#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import subprocess
from pathlib import Path

import pytest
import tomlkit

import deode
from deode.config_parser import ParsedConfig, read_raw_config_file
from deode.discover_task import discover, get_task
from deode.tasks.base import Task
from deode.tasks.batch import BatchJob
from deode.tasks.e923 import E923
from deode.tasks.forecast import FirstGuess, Forecast
from deode.toolbox import ArchiveError, FileManager, ProviderError

WORKING_DIR = Path.cwd()


def classes_to_be_tested():
    """Return the names of the task-related classes to be tested."""
    encountered_classes = discover(deode.tasks, Task, attrname="__type_name__")
    return encountered_classes.keys()


@pytest.fixture(scope="module")
def base_raw_config():
    """Return a raw config common to all tasks."""
    return read_raw_config_file("config/config.toml")


@pytest.fixture(params=classes_to_be_tested())
def task_name_and_configs(request, base_raw_config, tmp_path):
    """Return a ParsedConfig with a task-specific section according to `params`."""
    task_name = request.param
    task_config = ParsedConfig.parse_obj(base_raw_config, json_schema={})

    config_patch = tomlkit.parse(
        f"""
        [general]
            case = "my_case"
            loglevel = "INFO"
            macros = []
            os_macros = ["USER", "HOME"]
            realization = -1
            cnmexp = "DEOD"
            tstep = 60
            keep_workdirs = false
        [general.times]
            list = ["2000-01-01T00:00:00Z"]
            basetime = "2000-01-01T00:00:00Z"
            validtime = "2000-01-02T00:00:00Z"
        [domain]
            name = "MYDOMAIN"
        [system]
            wrk = "{tmp_path.as_posix()}"
            bindir = "{tmp_path.as_posix()}/bin"
        [platform]
            deode_home = "{WORKING_DIR}"
            scratch = "{tmp_path.as_posix()}"
            static_data = "{tmp_path.as_posix()}"
            prep_input_file = "{tmp_path.as_posix()}/demo/ECMWF/archive/2023/02/18/18/fc20230218_18+006"
        """
    )

    task_config = task_config.copy(update=config_patch)
    task_config = task_config.copy(
        update={
            "task": {
                task_name: {
                    "wrapper": "",
                    "command": "echo Hello world && touch output",
                    "input_data": {"input_file": "/dev/null"},
                    "output_data": {"output": "archived_file"},
                }
            }
        }
    )

    return task_name, task_config


@pytest.fixture(scope="module")
def _module_mockers(session_mocker):
    original_batchjob_run_method = BatchJob.run
    original_toolbox_filemanager_input_method = FileManager.input
    original_task_forecast_forecast_execute_method = Forecast.execute
    original_task_forecast_firstguess_execute_method = FirstGuess.execute
    original_task_e923_constant_part_method = E923.constant_part
    original_task_e923_monthly_part_method = E923.monthly_part

    def new_batchjob_run_method(*args, **kwargs):
        try:
            original_batchjob_run_method(*args, **kwargs)
        except subprocess.CalledProcessError:
            pass

    def new_toolbox_filemanager_input_method(*args, **kwargs):
        try:
            original_toolbox_filemanager_input_method(*args, **kwargs)
        except (ArchiveError, ProviderError, NotImplementedError):
            pass

    def new_task_forecast_forecast_execute_method(*args, **kwargs):
        try:
            original_task_forecast_forecast_execute_method(*args, **kwargs)
        except FileNotFoundError:
            pass

    def new_task_forecast_firstguess_execute_method(*args, **kwargs):
        try:
            original_task_forecast_firstguess_execute_method(*args, **kwargs)
        except FileNotFoundError:
            pass

    def new_task_e923_constant_part_method(*args, **kwargs):
        with open("Const.Clim", "w"):
            original_task_e923_constant_part_method(*args, **kwargs)

    def new_task_e923_montly_part_method(self, constant_file):
        with open(constant_file, "w"):
            original_task_e923_monthly_part_method(self, constant_file)

    session_mocker.patch(
        "deode.toolbox.FileManager.input", new=new_toolbox_filemanager_input_method
    )
    session_mocker.patch("deode.tasks.batch.BatchJob.run", new=new_batchjob_run_method)
    session_mocker.patch(
        "deode.tasks.forecast.Forecast.execute",
        new=new_task_forecast_forecast_execute_method,
    )
    session_mocker.patch(
        "deode.tasks.forecast.FirstGuess.execute",
        new=new_task_forecast_firstguess_execute_method,
    )

    # TODO: Mock deode.tasks.data.OutputData.data instead
    session_mocker.patch("deode.tasks.data.OutputData.archive_files", return_value=None)
    session_mocker.patch(
        "deode.tasks.e923.E923.constant_part", new=new_task_e923_constant_part_method
    )
    session_mocker.patch(
        "deode.tasks.e923.E923.monthly_part", new=new_task_e923_montly_part_method
    )
    session_mocker.patch("surfex.SURFEXBinary")


class TestTasks:
    # pylint: disable=no-self-use
    """Test all tasks."""

    def test_task_can_be_instantiated(self, task_name_and_configs):
        class_name, task_config = task_name_and_configs
        assert isinstance(get_task(class_name, task_config), Task)

    @pytest.mark.usefixtures("_module_mockers")
    def test_task_can_be_run(self, task_name_and_configs):
        class_name, task_config = task_name_and_configs
        my_task_class = get_task(class_name, task_config)
        my_task_class.run()


if __name__ == "__main__":
    pytest.main()
