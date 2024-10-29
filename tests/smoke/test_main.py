#!/usr/bin/env python3
"""Smoke tests."""
import itertools
import os
import shutil
from contextlib import redirect_stderr, redirect_stdout, suppress
from io import StringIO
from unittest import mock

import pytest

from deode import GeneralConstants
from deode.__main__ import main
from deode.argparse_wrapper import get_parsed_args
from deode.config_parser import ConfigParserDefaults
from deode.submission import NoSchedulerSubmission, TaskSettings
from deode.toolbox import Platform


@pytest.fixture(scope="module")
def config_path(tmp_path_factory):
    main_configs_test_dir = tmp_path_factory.getbasetemp() / "config_files"
    main_configs_test_dir.mkdir()
    shutil.copy(ConfigParserDefaults.PACKAGE_CONFIG_PATH, main_configs_test_dir)

    config_includes_test_dir = (
        main_configs_test_dir / ConfigParserDefaults.PACKAGE_INCLUDE_DIR.name
    )
    shutil.copytree(ConfigParserDefaults.PACKAGE_INCLUDE_DIR, config_includes_test_dir)

    return main_configs_test_dir / ConfigParserDefaults.PACKAGE_CONFIG_PATH.name


@pytest.fixture(scope="module")
def _module_mockers(module_mocker, config_path, tmp_path_factory):
    # Patching ConfigParserDefaults.CONFIG_PATH so tests use the generated config
    module_mocker.patch(
        "deode.config_parser.ConfigParserDefaults.__class__.__setattr__",
        new=type.__setattr__,
    )
    module_mocker.patch(
        "deode.config_parser.ConfigParserDefaults.CONFIG_PATH", new=config_path
    )

    original_no_scheduler_submission_submit_method = NoSchedulerSubmission.submit
    original_submission_task_settings_parse_job = TaskSettings.parse_job
    original_platform_evaluate_function = Platform.evaluate

    def new_no_scheduler_submission_submit_method(*args, **kwargs):
        """Wrap the original method to catch ."""
        with suppress(RuntimeError):
            original_no_scheduler_submission_submit_method(*args, **kwargs)

    def new_submission_task_settings_parse_job(self, **kwargs):
        kwargs["task_job"] = (tmp_path_factory.getbasetemp() / "task_job.txt").as_posix()
        with suppress(RuntimeError):
            original_submission_task_settings_parse_job(self, **kwargs)

    def new_platform_evaluate_function(self, *args, **kwargs):
        with suppress(TypeError):
            original_platform_evaluate_function(self, *args, **kwargs)

    module_mocker.patch(
        "deode.submission.NoSchedulerSubmission.submit",
        new=new_no_scheduler_submission_submit_method,
    )
    module_mocker.patch("deode.scheduler.ecflow")
    module_mocker.patch("deode.scheduler.EcflowServer._select_host_from_list")
    module_mocker.patch(
        "deode.toolbox.Platform.evaluate", new=new_platform_evaluate_function
    )
    module_mocker.patch("deode.suites.base.ecflow")
    module_mocker.patch(
        "deode.submission.TaskSettings.parse_job",
        new=new_submission_task_settings_parse_job,
    )
    module_mocker.patch("shutil.chown")


def test_package_executable_is_in_path():
    assert shutil.which(GeneralConstants.PACKAGE_NAME)


@pytest.mark.parametrize("argv", [[], None])
def test_cannot_run_without_arguments(argv):
    with redirect_stderr(StringIO()), pytest.raises(SystemExit, match="2"):
        main(argv)


@pytest.mark.usefixtures("_module_mockers")
def test_correct_config_is_in_use(config_path, mocker):
    mocker.patch("sys.exit")
    args = get_parsed_args(argv=["run"])
    assert config_path.is_file()
    assert args.config_file == config_path


@pytest.mark.usefixtures("_module_mockers")
class TestMainShowCommands:
    def test_show_config_command(self):
        with redirect_stdout(StringIO()):
            main(["show", "config"])
            main(
                [
                    "show",
                    "config",
                    "--config-file",
                    ConfigParserDefaults.PACKAGE_CONFIG_PATH.as_posix(),
                ]
            )

    def test_show_config_schema_command(self):
        with redirect_stdout(StringIO()):
            main(["show", "config-schema"])

    def test_show_config_command_stretched_time(self):
        """Test again, mocking time.time so the total elapsed time is greater than 60s."""

        def fake_time():
            for new in itertools.count():
                yield 100 * new

        with mock.patch(
            "time.time", mock.MagicMock(side_effect=fake_time())
        ), redirect_stdout(StringIO()):
            main(["show", "config"])

    def test_show_namelist_command(self, tmp_path_factory):
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fort.4"
        main(["show", "namelist", "-t", "surfex", "-n", "forecast", "-o", output_file])


@pytest.mark.usefixtures("_module_mockers")
def test_run_task_command(tmp_path):
    main(
        [
            "run",
            "--task",
            "Forecast",
            "--template",
            str(GeneralConstants.PACKAGE_DIRECTORY / "deode/templates/stand_alone.py"),
            "--job",
            f"{tmp_path.as_posix()}/forecast.job",
            "-o",
            f"{tmp_path.as_posix()}/forecast.log",
        ]
    )


@pytest.mark.usefixtures("_module_mockers")
def test_start_suite_command():
    with suppress(FileNotFoundError):
        main(["start", "suite"])


@pytest.mark.usefixtures("_module_mockers")
def test_doc_config_command():
    with redirect_stdout(StringIO()):
        main(["doc", "config"])


def test_integrate_namelists_command():
    args = [
        "namelist",
        "integrate",
        "--namelist",
        "deode/data/namelists/unit_testing/nl_master_base",
        "--output",
        os.devnull,
    ]
    main(args)


@pytest.mark.usefixtures("_module_mockers")
def test_convert_namelists_command(tmp_path):
    output_yml = f"{tmp_path.as_posix()}/nl_master_base.49t2.yml"

    args = [
        "namelist",
        "convert",
        "--namelist",
        "deode/data/namelists/unit_testing/nl_master_base.yml",
        "--output",
        output_yml,
        "--from-cycle",
        "CY48t2",
        "--to-cycle",
        "CY49t2",
        "--format",
        "yaml",
    ]
    main(args)


@pytest.mark.usefixtures("_module_mockers")
def test_format_namelists_command(tmp_path):
    output_yml = f"{tmp_path.as_posix()}/nl_master_base.format.yml"

    args = [
        "namelist",
        "format",
        "--namelist",
        "deode/data/namelists/unit_testing/nl_master_base.yml",
        "--output",
        output_yml,
        "--format",
        "yaml",
    ]
    main(args)
