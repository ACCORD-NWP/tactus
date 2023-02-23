#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pkgutil

import pytest
import tomlkit

from deode.config_parser import ParsedConfig
from deode.discover_task import get_task
from deode.tasks.base import Task


@pytest.fixture()
def base_raw_config():
    """Return a raw config common to all tasks."""
    return tomlkit.parse(
        """
        [general]
            case = "my_case"
            loglevel = "DEBUG"
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
            wrk = "/tmp/@YYYY@@MM@@DD@_@HH@"
        [platform]
        """
    )


@pytest.fixture(params=[_.name for _ in pkgutil.iter_modules(["deode/tasks"])])
def parsed_config_with_task(request, base_raw_config):
    """Return a ParsedConfig with a task-specific section according to `params`."""
    raw_config = base_raw_config.copy()
    raw_config.update(
        {
            "task": {
                request.param: {
                    "wrapper": "",
                    "command": "echo Hello world && touch output",
                    "input_data": {"input_file": "/dev/null"},
                    "output_data": {"output": "archived_file"},
                }
            }
        }
    )
    return ParsedConfig.parse_obj(raw_config)


class TestTasks:
    # pylint: disable=no-self-use
    """Test all tasks."""

    def test_task_can_be_instantiated(self, parsed_config_with_task):
        task = get_task("UnitTest", parsed_config_with_task)
        assert isinstance(task, Task)

    def test_task_can_be_run(self, parsed_config_with_task):
        get_task("UnitTest", parsed_config_with_task).run()


if __name__ == "__main__":
    pytest.main()
