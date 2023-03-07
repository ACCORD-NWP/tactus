#!/usr/bin/env python3
"""Smoke tests."""
import datetime
import itertools
import shutil
from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path
from unittest import mock

import pytest
import tomlkit

from deode import PACKAGE_NAME
from deode.argparse_wrapper import get_parsed_args
from deode.main import main


@pytest.fixture(scope="module")
def base_raw_config():
    return tomlkit.parse(
        """
        [general]
            times.list = ["2000-01-01T00:00:00Z"]
        """
    )


@pytest.fixture(scope="module")
def tmp_test_data_dir(tmpdir_factory, base_raw_config):
    return Path(tmpdir_factory.mktemp("deode_test_rootdir"))


@pytest.fixture(scope="module")
def config_path(base_raw_config, tmp_test_data_dir):
    config_path = tmp_test_data_dir / "config.toml"
    with open(config_path, "w") as config_file:
        tomlkit.dump(base_raw_config, config_file)
    return config_path


@pytest.fixture(scope="module")
def _module_mockers(session_mocker, config_path):
    # Monkeypatching DEODE_CONFIG_PATH so tests use the generated config.toml.
    # Otherwise, the program defaults to reading from ~/.deode/config.toml
    session_mocker.patch.dict("os.environ", {"DEODE_CONFIG_PATH": str(config_path)})


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


if __name__ == "__main__":
    pytest.main()
