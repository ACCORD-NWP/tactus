#!/usr/bin/env python3
"""Smoke tests."""
import datetime
import itertools
import shutil
from contextlib import redirect_stderr, redirect_stdout, suppress
from io import StringIO
from pathlib import Path
from unittest import mock

import pytest
import tomlkit

from deode import PACKAGE_NAME
from deode.argparse_wrapper import get_parsed_args
from deode.main import main
from deode.submission import NoSchedulerSubmission

WORKING_DIR = Path.cwd()


@pytest.fixture(scope="module")
def config_path(tmp_path_factory):
    config_path = tmp_path_factory.getbasetemp() / "config.toml"
    shutil.copy(WORKING_DIR / "config/config.toml", config_path)
    return config_path


@pytest.fixture(scope="module")
def _module_mockers(session_mocker, config_path):
    # Monkeypatching DEODE_CONFIG_PATH so tests use the generated config.toml.
    # Otherwise, the program defaults to reading from ~/.deode/config.toml
    session_mocker.patch.dict("os.environ", {"DEODE_CONFIG_PATH": str(config_path)})

    original_no_scheduler_submission_submit_method = NoSchedulerSubmission.submit

    def new_no_scheduler_submission_submit_method(*args, **kwargs):
        """Wrap the original method to catch ."""
        with suppress(RuntimeError):
            original_no_scheduler_submission_submit_method(*args, **kwargs)

    session_mocker.patch(
        "deode.submission.NoSchedulerSubmission.submit",
        new=new_no_scheduler_submission_submit_method,
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
def test_run_task_command():
    main(
        [
            "run",
            "--task",
            "Forecast",
            "--template",
            f"{WORKING_DIR.as_posix()}/ecf/stand_alone.py",
            "--job",
            "./forecast.jo",
            "-o",
            "./forecast.log",
        ]
    )


if __name__ == "__main__":
    pytest.main()
