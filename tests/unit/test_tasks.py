#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import contextlib
import subprocess
from os import chdir, makedirs
from pathlib import Path

import pytest
import tomlkit

from deode import GeneralConstants
from deode.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from deode.derived_variables import set_times
from deode.plugin import DeodePluginRegistry
from deode.tasks.archive import ArchiveHour, ArchiveStatic
from deode.tasks.base import Task
from deode.tasks.batch import BatchJob
from deode.tasks.collectlogs import CollectLogs
from deode.tasks.creategrib import CreateGrib
from deode.tasks.discover_task import available_tasks, get_task
from deode.tasks.e923 import E923
from deode.tasks.extractsqlite import ExtractSQLite
from deode.tasks.forecast import FirstGuess, Forecast
from deode.tasks.iomerge import IOmerge
from deode.tasks.marsprep import Marsprep
from deode.toolbox import ArchiveError, FileManager, ProviderError


def classes_to_be_tested():
    """Return the names of the task-related classes to be tested."""
    reg = DeodePluginRegistry()
    encountered_classes = available_tasks(reg)
    return encountered_classes.keys()


@pytest.fixture(scope="module")
def base_raw_config():
    """Return a raw config common to all tasks."""
    config = BasicConfig.from_file(ConfigParserDefaults.CONFIG_DIRECTORY / "config.toml")
    return config


@pytest.fixture(params=classes_to_be_tested(), scope="module")
def task_name_and_configs(request, base_raw_config, tmp_path_factory):
    """Return a ParsedConfig with a task-specific section according to `params`."""
    task_name = request.param
    task_config = ParsedConfig(
        base_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )
    task_config = task_config.copy(update=set_times(task_config))

    basetime = task_config["general.times.basetime"]
    config_patch = tomlkit.parse(
        f"""
        [general]
            keep_workdirs = false
        [system]
            wrk = "{tmp_path_factory.getbasetemp().as_posix()}"
            bindir = "{tmp_path_factory.getbasetemp().as_posix()}/bin"
        [platform]
            deode_home = "{GeneralConstants.PACKAGE_DIRECTORY}"
            scratch = "{tmp_path_factory.getbasetemp().as_posix()}"
            static_data = "{tmp_path_factory.getbasetemp().as_posix()}"
            climdata = "{tmp_path_factory.getbasetemp().as_posix()}"
            soilgrid_data_path = "{tmp_path_factory.getbasetemp().as_posix()}"
        [task.args]
            joboutdir = "foo"
            tarname= "foo"
            task_logs = "foo"
            bd_nr = 1
            bd_time = "{basetime}"
            basetime = "{basetime}"
            ionr = 0
        """
    )

    task_config = task_config.copy(update=config_patch)
    task_config = task_config.copy(update={"task": {"wrapper": "echo NPROC=@NPROC@;"}})

    return task_name, task_config


@pytest.fixture(scope="module")
def _mockers_for_task_run_tests(session_mocker, tmp_path_factory):
    """Define mockers used in the tests for the tasks' `run` methods."""
    # Keep reference to the original methods that will be replaced with wrappers
    original_batchjob_init_method = BatchJob.__init__
    original_batchjob_run_method = BatchJob.run
    original_toolbox_filemanager_input_method = FileManager.input
    original_task_forecast_forecast_execute_method = Forecast.execute
    original_task_forecast_firstguess_execute_method = FirstGuess.execute
    original_task_archive_archivehour_execute_method = ArchiveHour.execute
    original_task_archive_archivestatic_execute_method = ArchiveStatic.execute
    original_task_creategrib_creategrib_execute_method = CreateGrib.execute
    original_task_extractsqlite_extractsqlite_execute_method = ExtractSQLite.execute
    original_task_e923_constant_part_method = E923.constant_part
    original_task_e923_monthly_part_method = E923.monthly_part
    original_task_iomerge_iomerge_execute_method = IOmerge.execute
    original_task_marsprep_run_method = Marsprep.run
    original_task_collectlogs_collectlogs_execute_method = CollectLogs.execute

    # Define the wrappers that will replace some key methods
    def new_batchjob_init_method(self, *args, **kwargs):
        """Remove eventual `wrapper` settings, which are not used for tests."""
        original_batchjob_init_method(self, *args, **kwargs)
        self.wrapper = ""

    def new_batchjob_run_method(self, cmd):
        """Run the original method with a dummy cmd if the original cmd fails."""
        try:
            original_batchjob_run_method(self, cmd=cmd)
        except subprocess.CalledProcessError:
            original_batchjob_run_method(
                self, cmd="echo 'Running a dummy command' >| output"
            )

    def new_toolbox_filemanager_input_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(ArchiveError, ProviderError, NotImplementedError):
            original_toolbox_filemanager_input_method(*args, **kwargs)

    def new_task_forecast_forecast_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_forecast_forecast_execute_method(*args, **kwargs)

    def new_task_forecast_firstguess_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_forecast_firstguess_execute_method(*args, **kwargs)

    def new_task_archive_archivehour_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_archive_archivehour_execute_method(*args, **kwargs)

    def new_task_archive_archivestatic_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_archive_archivestatic_execute_method(*args, **kwargs)

    def new_task_cleaning_tasks_cleaner_execute_method(*args, **kwargs):  # noqa: ARG001
        """Skip any work."""

    def new_task_creategrib_creategrib_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_creategrib_creategrib_execute_method(*args, **kwargs)

    def new_task_extractsqlite_extractsqlite_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_extractsqlite_extractsqlite_execute_method(*args, **kwargs)

    def new_task_mars_batchjob_run_method(*args, **kwargs):  # noqa: ARG001
        """Skip any work."""

    def new_task_marsprep_run_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_marsprep_run_method(*args, **kwargs)

    def new_task_e923_constant_part_method(*args, **kwargs):
        """Create needed file "Const.Clim" before running the original method."""
        with open("Const.Clim", "w", encoding="utf8"):
            original_task_e923_constant_part_method(*args, **kwargs)

    def new_task_e923_monthly_part_method(self, constant_file):
        """Create needed file `constant_file` before running the original method."""
        Path(constant_file).parent.mkdir(parents=True, exist_ok=True)
        Path(constant_file).touch()
        Path("Const.Clim.01").touch()
        original_task_e923_monthly_part_method(self, constant_file)

    def new_task_iomerge_iomerge_execute_method(self):
        """Create needed file `ECHIS` before running the original method."""
        file1 = self.wdir + "/../Forecast/io_serv.000001.d/ECHIS"
        Path(file1).parent.mkdir(parents=True, exist_ok=True)
        with open(file1, "w", encoding="utf8") as f1:
            f1.write("01:00:00")
        with contextlib.suppress(FileNotFoundError, RuntimeError):
            original_task_iomerge_iomerge_execute_method(self)

    def new_task_collectlogs_collectlogs_execute_method(*args, **kwargs):
        """Suppress some errors so that test continues if they happen."""
        with contextlib.suppress(FileNotFoundError):
            original_task_collectlogs_collectlogs_execute_method(*args, **kwargs)

    def new_surfex_binary(_self, *_args, **_kwargs):
        """Create output."""
        Path("PGD.fa").touch()
        Path("PREP.fa").touch()

    # Do the actual mocking
    session_mocker.patch("shutil.chown")
    session_mocker.patch(
        "deode.tasks.batch.BatchJob.__init__", new=new_batchjob_init_method
    )
    session_mocker.patch("deode.tasks.batch.BatchJob.run", new=new_batchjob_run_method)
    session_mocker.patch(
        "deode.toolbox.FileManager.input", new=new_toolbox_filemanager_input_method
    )
    session_mocker.patch(
        "deode.tasks.forecast.Forecast.execute",
        new=new_task_forecast_forecast_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.forecast.FirstGuess.execute",
        new=new_task_forecast_firstguess_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.archive.ArchiveHour.execute",
        new=new_task_archive_archivehour_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.archive.ArchiveStatic.execute",
        new=new_task_archive_archivestatic_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.cleaning_tasks.Cleaning.execute",
        new=new_task_cleaning_tasks_cleaner_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.creategrib.CreateGrib.execute",
        new=new_task_creategrib_creategrib_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.extractsqlite.ExtractSQLite.execute",
        new=new_task_extractsqlite_extractsqlite_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.e923.E923.constant_part", new=new_task_e923_constant_part_method
    )
    session_mocker.patch(
        "deode.tasks.e923.E923.monthly_part", new=new_task_e923_monthly_part_method
    )
    session_mocker.patch(
        "deode.tasks.iomerge.IOmerge.execute",
        new=new_task_iomerge_iomerge_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.marsprep.BatchJob.run",
        new=new_task_mars_batchjob_run_method,
    )
    session_mocker.patch(
        "deode.tasks.marsprep.Marsprep.run",
        new=new_task_marsprep_run_method,
    )
    session_mocker.patch(
        "deode.tasks.collectlogs.CollectLogs.execute",
        new=new_task_collectlogs_collectlogs_execute_method,
    )

    # Create files needed by gmtedsoil tasks
    tif_files_dir = tmp_path_factory.getbasetemp() / "GMTED2010"
    makedirs(tif_files_dir, exist_ok=True)

    for fname in ["50N000E_20101117_gmted_mea075", "30N000E_20101117_gmted_mea075"]:
        fpath = tif_files_dir / f"{fname}.tif"
        fpath.touch()

    # Mock things that we don't want to test here (e.g., external binaries)
    session_mocker.patch("deode.tasks.gmtedsoil._import_gdal")


class TestTasks:
    """Test all tasks."""

    @pytest.mark.usefixtures("_mockers_for_task_run_tests")
    def test_task_can_be_instantiated(self, task_name_and_configs):
        class_name, task_config = task_name_and_configs
        assert isinstance(get_task(class_name, task_config), Task)

    @pytest.mark.usefixtures("_mockers_for_task_run_tests")
    def test_task_can_be_run(self, task_name_and_configs):
        class_name, task_config = task_name_and_configs
        my_task_class = get_task(class_name, task_config)
        org_cwd = Path.cwd()
        my_task_class.run()
        chdir(org_cwd)
