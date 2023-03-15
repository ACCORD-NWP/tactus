#!/usr/bin/env python3
"""Smoke tests."""
import itertools
import shutil
from contextlib import redirect_stderr, redirect_stdout, suppress
from io import StringIO
from pathlib import Path
from unittest import mock

import pytest

from deode import PACKAGE_NAME
from deode.argparse_wrapper import get_parsed_args
from deode.main import main
from deode.submission import NoSchedulerSubmission, TaskSettings

WORKING_DIR = Path.cwd()


@pytest.fixture(scope="module")
def config_path(tmp_path_factory):
    config_path = tmp_path_factory.getbasetemp() / "config.toml"
    shutil.copy(WORKING_DIR / "deode/data/config_files/config.toml", config_path)
    return config_path


@pytest.fixture(scope="module")
def _module_mockers(session_mocker, config_path, tmp_path_factory):
    # Monkeypatching DEODE_CONFIG_PATH so tests use the generated config.toml.
    # Otherwise, the program defaults to reading from ~/.deode/config.toml
    session_mocker.patch.dict("os.environ", {"DEODE_CONFIG_PATH": str(config_path)})

    original_no_scheduler_submission_submit_method = NoSchedulerSubmission.submit
    original_submission_task_settings_parse_job = TaskSettings.parse_job

    def new_no_scheduler_submission_submit_method(*args, **kwargs):
        """Wrap the original method to catch ."""
        with suppress(RuntimeError):
            original_no_scheduler_submission_submit_method(*args, **kwargs)

    def new_submission_task_settings_parse_job(self, **kwargs):
        kwargs["task_job"] = (tmp_path_factory.getbasetemp() / "task_job.txt").as_posix()
        original_submission_task_settings_parse_job(self, **kwargs)

    session_mocker.patch(
        "deode.submission.NoSchedulerSubmission.submit",
        new=new_no_scheduler_submission_submit_method,
    )
    session_mocker.patch("deode.scheduler.ecflow")
    session_mocker.patch("deode.suites.ecflow")
    session_mocker.patch(
        "deode.submission.TaskSettings.parse_job",
        new=new_submission_task_settings_parse_job,
    )


def test_package_executable_is_in_path():
    assert shutil.which(PACKAGE_NAME)


@pytest.mark.parametrize("argv", [[], None])
def test_cannot_run_without_arguments(argv):
    with redirect_stderr(StringIO()):
        with pytest.raises(SystemExit, match="2"):
            main(argv)


@pytest.mark.usefixtures("_module_mockers")
def test_correct_config_is_in_use(config_path, mocker):
    mocker.patch("sys.exit")
    args = get_parsed_args(argv=[])
    assert args.config_file == config_path


@pytest.mark.usefixtures("_module_mockers")
class TestMainShowCommands:
    # pylint: disable=no-self-use
    def test_show_config_command(self):
        with redirect_stdout(StringIO()):
            main(["show", "config"])

    def test_show_config_command_stretched_time(self):
        """Test again, mocking time.time so the total elapsed time is greater than 60s."""

        def fake_time():
            for new in itertools.count():
                yield 100 * new

        with mock.patch("time.time", mock.MagicMock(side_effect=fake_time())):
            with redirect_stdout(StringIO()):
                main(["show", "config"])


@pytest.mark.usefixtures("_module_mockers")
def test_run_task_command(tmp_path):
    main(
        [
            "run",
            "--task",
            "Forecast",
            "--template",
            f"{WORKING_DIR.as_posix()}/deode/templates/stand_alone.py",
            "--job",
            f"{tmp_path.as_posix()}/forecast.jo",
            "-o",
            f"{tmp_path.as_posix()}/forecast.log",
        ]
    )


@pytest.mark.usefixtures("_module_mockers")
def test_start_suite_command(tmp_path):
    main(
        [
            "start",
            "suite",
            "--joboutdir",
            tmp_path.as_posix(),
            "--ecf_files",
            f"{WORKING_DIR.as_posix()}/ecf",
        ]
    )
