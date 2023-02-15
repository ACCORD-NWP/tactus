#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pytest
import tomlkit

from deode.config_parser import ParsedConfig
from deode.discover_task import get_task
from deode.tasks.base import Task


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            loglevel = "DEBUG"
            macros = []
            os_macros = ["USER", "HOME"]
            realization = -1
            cnmexp = "DEOD"
            tstep = 60
        [general.times]
            list = ["2000-01-01T00:00:00Z"]
            basetime = "2000-01-01T00:00:00Z"
            validtime = "2000-01-02T00:00:00Z"
        [domain]
            name = "MYDOMAIN"
        [system]
            wrk = "/tmp/@YYYY@@MM@@DD@_@HH@"
        [platform]
        """
    )


@pytest.fixture()
def raw_config_with_task(minimal_raw_config):
    rtn = minimal_raw_config.copy()
    rtn.update(
        {
            "task": {
                "forecast": {
                    "wrapper": "",
                    "command": "echo Hello world && touch output",
                    "input_data": {"input_file": "/dev/null"},
                    "output_data": {"output": "archived_file"},
                }
            }
        }
    )
    return rtn


@pytest.fixture()
def parsed_config_with_task(raw_config_with_task):
    return ParsedConfig.parse_obj(raw_config_with_task)


class TestBaseTask:
    # pylint: disable=no-self-use
    """Test some base tasks."""

    def test_config_can_be_instantiated(self, parsed_config_with_task):
        assert isinstance(parsed_config_with_task, ParsedConfig)

    def test_task_can_be_instantiated(self, parsed_config_with_task):
        config = parsed_config_with_task
        task = get_task("forecast", config)
        assert isinstance(task, Task)

    def test_task_can_be_run(self, parsed_config_with_task):
        config = parsed_config_with_task
        get_task("forecast", config).run()


if __name__ == "__main__":
    pytest.main()
