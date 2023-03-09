#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pytest
import tomlkit

from deode.config_parser import ParsedConfig
from deode.submission import NoSchedulerSubmission, TaskSettings


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            times.list = ["2000-01-01T00:00:00Z"]
            loglevel = "DEBUG"
        [system]
            wrk = "/tmp/@YYYY@@MM@@DD@_@HH@"
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


@pytest.fixture()
def config_from_task_config_file():
    fname = "config/config.toml"
    return ParsedConfig.from_file(fname)


class TestSubmission:
    # pylint: disable=no-self-use

    def test_config_can_be_instantiated(self, parsed_config_with_task):
        assert isinstance(parsed_config_with_task, ParsedConfig)

    def test_submit(self, config_from_task_config_file):
        config = config_from_task_config_file
        config = config.copy(
            update={"submission": {"default_submit_type": "background_hpc"},
                    "troika": {"config_file": "config/troika.yml"}}
        )
        config = config.copy(update={"platform": {"SCRATCH": "/tmp"}})  # noqa
        task = "UnitTest"
        template_job = "ecf/stand_alone.py"
        task_job = f"/tmp/{task}.job"  # noqa
        output = f"/tmp/{task}.log"  # noqa

        assert config.get_value("submission.default_submit_type") == "background_hpc"
        background = TaskSettings(config)
        sub = NoSchedulerSubmission(background)
        sub.submit(
            task, config, template_job, task_job, output
        )

    def test_get_batch_info(self, config_from_task_config_file):
        config = config_from_task_config_file
        arg = "#SBATCH UNITTEST"
        config = config.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {
                        "BATCH": {
                            "TEST": arg
                        }
                    }
                }
            }
        )
        task = TaskSettings(config)
        settings = task.get_task_settings("unittest", key="BATCH")
        assert settings["TEST"] == arg

    def test_get_batch_info_exception(self, config_from_task_config_file):
        config = config_from_task_config_file
        arg = "#SBATCH UNITTEST"
        config = config.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {
                        "tasks": ["unittest"],
                        "BATCH": {
                            "TEST_INCLUDED": arg,
                            "TEST": "NOT USED"
                        }
                    },
                    "task_exceptions": {
                        "unittest": {
                            "BATCH": {
                                "TEST": arg
                            }
                        }
                    }
                }
            }
        )
        task = TaskSettings(config)
        settings = task.get_task_settings("unittest", key="BATCH")
        assert settings["TEST"] == arg
        assert settings["TEST"] != "NOT USED"
        assert settings["TEST_INCLUDED"] == arg

    def test_submit_non_existing_task(self, config_from_task_config_file):
        config = config_from_task_config_file
        task = "not_existing"
        template_job = "ecf/stand_alone.py"
        task_job = f"/tmp/{task}.job"  # noqa
        output = f"/tmp/{task}.log"  # noqa

        background = TaskSettings(config)
        sub = NoSchedulerSubmission(background)
        with pytest.raises(Exception, match="Task not found:"):
            sub.submit(
                task,
                config,
                template_job,
                task_job,
                output
            )
