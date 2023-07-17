#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pytest
import tomlkit

from deode.config_parser import ParsedConfig
from deode.derived_variables import derived_variables
from deode.submission import NoSchedulerSubmission, ProcessorLayout, TaskSettings


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
    rtn.update({"task": {}})
    return rtn


@pytest.fixture()
def parsed_config_with_task(raw_config_with_task):
    return ParsedConfig.parse_obj(raw_config_with_task)


@pytest.fixture()
def config_from_task_config_file():
    return ParsedConfig.from_file("deode/data/config_files/config.toml")


class TestSubmission:
    # pylint: disable=no-self-use

    def test_config_can_be_instantiated(self, parsed_config_with_task):
        assert isinstance(parsed_config_with_task, ParsedConfig)

    def test_submit(self, config_from_task_config_file):
        config = config_from_task_config_file
        config = config.copy(
            update={
                "submission": {"default_submit_type": "background_hpc"},
                "troika": {"config_file": "deode/data/config_files/troika.yml"},
            }
        )
        config = config.copy(update={"platform": {"SCRATCH": "/tmp"}})  # noqa
        task = "UnitTest"
        template_job = "deode/templates/stand_alone.py"
        task_job = f"/tmp/{task}.job"  # noqa
        output = f"/tmp/{task}.log"  # noqa

        assert config.get_value("submission.default_submit_type") == "background_hpc"
        background = TaskSettings(config)
        sub = NoSchedulerSubmission(background)
        sub.submit(task, config, template_job, task_job, output)

    def test_get_batch_info(self, config_from_task_config_file):
        config = config_from_task_config_file
        arg = "#SBATCH UNITTEST"
        argname = "job-name=@TASK_NAME@"
        config = config.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {"BATCH": {"TEST": arg, "NAME": argname}},
                }
            }
        )
        task = TaskSettings(config)

        settings = task.get_task_settings("unittest")

        assert settings["BATCH"]["TEST"] == arg
        assert settings["BATCH"]["NAME"] == "job-name=unittest"

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
                        "BATCH": {"TEST_INCLUDED": arg, "TEST": "NOT USED"},
                    },
                    "task_exceptions": {"unittest": {"BATCH": {"TEST": arg}}},
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
        template_job = "deode/templates/stand_alone.py"
        task_job = f"/tmp/{task}.job"  # noqa
        output = f"/tmp/{task}.log"  # noqa

        background = TaskSettings(config)
        sub = NoSchedulerSubmission(background)
        with pytest.raises(KeyError, match="Task not found:"):
            sub.submit(task, config, template_job, task_job, output)

    def test_wrapper_and_nproc(self, config_from_task_config_file):
        config = config_from_task_config_file
        config = config.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {"WRAPPER": "@NPROC@", "NPROC": 2, "NPROCX": 1},
                }
            }
        )
        task = TaskSettings(config)
        settings = task.get_task_settings("unittest")
        processor_layout = ProcessorLayout(settings)
        update = derived_variables(config, processor_layout=processor_layout)
        assert update["task"]["wrapper"] == "2"
        assert update["namelist"]["nproc"] == 2
        assert update["namelist"]["nprocx"] == 1
        assert update["namelist"]["nprocy"] is None
        config = config.copy(update=update)
        assert config.get_value("task.wrapper") == "2"
        assert config.get_value("namelist.nproc") == 2
        assert config.get_value("namelist.nprocx") == 1
        with pytest.raises(AttributeError, match="object has no attribute"):
            config.get_value("namelist.nprocy")

    def test_empty_wrapper_and_nproc(self, config_from_task_config_file):
        config = config_from_task_config_file
        config = config.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {"NPROC": 2, "NPROCX": 1},
                }
            }
        )
        task = TaskSettings(config)
        settings = task.get_task_settings("unittest")
        processor_layout = ProcessorLayout(settings)
        update = derived_variables(config, processor_layout=processor_layout)
        config = config.copy(update=update)
        with pytest.raises(AttributeError, match="object has no attribute"):
            config.get_value("task.wrapper")
        assert config.get_value("namelist.nproc") == 2
        with pytest.raises(AttributeError, match="object has no attribute"):
            config.get_value("namelist.nprocy")
