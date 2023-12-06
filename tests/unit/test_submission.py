#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
from contextlib import suppress

import pytest
import tomlkit

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.derived_variables import derived_variables, set_times
from deode.submission import NoSchedulerSubmission, ProcessorLayout, TaskSettings


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            times.list = ["2000-01-01T00:00:00Z"]
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
    return ParsedConfig(
        raw_config_with_task, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )


@pytest.fixture()
def config_from_task_config_file():
    """Return a raw config common to all tasks."""
    return ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


@pytest.fixture(scope="module")
def tmp_directory(tmp_path_factory):
    """Return a temp directory valid for this module."""
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def _module_mockers(module_mocker):
    # Patching ConfigParserDefaults.CONFIG_PATH so tests use the generated config
    original_submission_task_settings_parse_job = TaskSettings.parse_job

    def new_submission_task_settings_parse_job(self, **kwargs):
        with suppress(RuntimeError):
            original_submission_task_settings_parse_job(self, **kwargs)

    module_mocker.patch(
        "deode.submission.TaskSettings.parse_job",
        new=new_submission_task_settings_parse_job,
    )


@pytest.mark.usefixtures("_module_mockers")
class TestSubmission:
    def test_config_can_be_instantiated(self, parsed_config_with_task):
        assert isinstance(parsed_config_with_task, ParsedConfig)

    def test_submit(self, config_from_task_config_file, tmp_directory):
        config = config_from_task_config_file.copy(
            update={
                "submission": {"default_submit_type": "background_hpc"},
                "troika": {"config_file": "deode/data/config_files/troika.yml"},
            }
        )
        tmp = tmp_directory
        config = config.copy(update={"platform": {"SCRATCH": tmp, "unix_group": ""}})
        config = config.copy(update=set_times(config))
        task = "UnitTest"
        template_job = "deode/templates/stand_alone.py"
        task_job = f"{tmp}/{task}.job"
        output = f"{tmp}/{task}.log"

        assert config["submission.default_submit_type"] == "background_hpc"
        background = TaskSettings(config)
        sub = NoSchedulerSubmission(background)
        sub.submit(task, config, template_job, task_job, output)

    def test_get_batch_info(self, config_from_task_config_file):
        arg = "#SBATCH UNITTEST"
        argname = "job-name=@TASK_NAME@"
        config = config_from_task_config_file.copy(
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
        arg = "#SBATCH UNITTEST"
        config = config_from_task_config_file.copy(
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

    def test_cannot_submit_non_existing_task(
        self, config_from_task_config_file, tmp_directory
    ):
        config = config_from_task_config_file.copy()
        task = "not_existing"
        tmp = tmp_directory
        template_job = "deode/templates/stand_alone.py"
        task_job = f"{tmp}/{task}.job"
        output = f"{tmp}/{task}.log"

        background = TaskSettings(config)
        sub = NoSchedulerSubmission(background)
        with pytest.raises(NotImplementedError, match=f'Task "{task}" not implemented.'):
            sub.submit(task, config, template_job, task_job, output)

    def test_wrapper_and_nproc(self, config_from_task_config_file):
        config = config_from_task_config_file.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {"WRAPPER": "@NPROC@", "NPROC": 2, "NPROCX": 1},
                }
            }
        )
        config = config.copy(update=set_times(config))
        task = TaskSettings(config)
        settings = task.get_task_settings("unittest")
        processor_layout = ProcessorLayout(settings)
        update = derived_variables(config, processor_layout=processor_layout)
        assert update["submission"]["task"]["wrapper"] == "2"
        assert update["namelist"]["nproc"] == 2
        assert update["namelist"]["nprocx"] == 1
        assert update["namelist"]["nprocy"] is None
        config = config.copy(update=update)
        assert config["submission.task.wrapper"] == "2"
        assert config["namelist.nproc"] == 2
        assert config["namelist.nprocx"] == 1
        with pytest.raises(KeyError, match="'nprocy'"):
            config["namelist.nprocy"]

    def test_empty_wrapper_and_nproc(self, config_from_task_config_file):
        config = config_from_task_config_file.copy(
            update={
                "submission": {
                    "submit_types": ["unittest"],
                    "default_submit_type": "unittest",
                    "unittest": {"NPROC": 2, "NPROCX": 1},
                }
            }
        )
        config = config.copy(update=set_times(config))
        task = TaskSettings(config)
        settings = task.get_task_settings("unittest")
        processor_layout = ProcessorLayout(settings)
        update = derived_variables(config, processor_layout=processor_layout)
        config = config.copy(update=update)
        with pytest.raises(KeyError, match="'wrapper'"):
            config["submission.task.wrapper"]
        assert config["namelist.nproc"] == 2
        with pytest.raises(KeyError, match="'nprocy'"):
            config["namelist.nprocy"]
