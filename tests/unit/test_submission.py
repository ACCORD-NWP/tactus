#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pytest

from deode.config_parser import ParsedConfig
from deode.submission import NoSchedulerSubmission, TaskSettings


@pytest.fixture()
def minimal_raw_config():
    return {
        "general": {
            "assimilation_times": {"list": ["20000101T00"]},
            "loglevel": "DEBUG"
        },
        "system": {"wrk": "/tmp/@YYYY@@MM@@DD@_@HH@"} # noqa S108
    }


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
    fname = "docs/task_config_example.toml"
    return ParsedConfig.from_file(fname)


class TestSubmission:
    # pylint: disable=no-self-use

    def test_config_can_be_instantiated(self, parsed_config_with_task):
        assert isinstance(parsed_config_with_task, ParsedConfig)

    def test_submit(self, config_from_task_config_file):
        config = config_from_task_config_file
        task = "forecast"
        template_job = "ecf/stand_alone.py"
        task_job = f"/tmp/{task}.job"  # noqa
        output = f"/tmp/{task}.log"  # noqa
        settings = {
            "submit_types": ["background"],
            "default_submit_type": "background",
            "background": {"SCHOST": "localhost"},
        }
        background = TaskSettings(settings)
        sub = NoSchedulerSubmission(background)
        sub.submit(
            task, config, template_job, task_job, output, troika_config="config.yml"
        )

    def test_submit_non_existing_task(self, config_from_task_config_file):
        config = config_from_task_config_file
        task = "not_existing"
        template_job = "ecf/stand_alone.py"
        task_job = f"/tmp/{task}.job"  # noqa
        output = f"/tmp/{task}.log"  # noqa
        settings = {
            "submit_types": ["background"],
            "default_submit_type": "background",
            "background": {"SCHOST": "localhost"},
        }
        background = TaskSettings(settings)
        sub = NoSchedulerSubmission(background)

        with pytest.raises(Exception, match="Task not found:"):
            sub.submit(
                task,
                config,
                template_job,
                task_job,
                output,
                troika_config="config.yml",
            )


if __name__ == "__main__":
    pytest.main()
