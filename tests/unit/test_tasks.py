#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
from pathlib import Path

import pytest
import tomlkit

import deode
from deode.config_parser import ParsedConfig, read_raw_config_file
from deode.discover_task import discover, get_task
from deode.tasks.base import Task

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


class TestTasks:
    # pylint: disable=no-self-use
    """Test all tasks."""

    def test_task_can_be_instantiated(self, task_name_and_configs):
        class_name, task_config = task_name_and_configs
        assert isinstance(get_task(class_name, task_config), Task)

    def test_task_can_be_run(self, task_name_and_configs):
        class_name, task_config = task_name_and_configs
        my_task_class = get_task(class_name, task_config)
        my_task_class.run()


if __name__ == "__main__":
    pytest.main()
