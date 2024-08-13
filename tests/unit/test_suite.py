#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import os
from contextlib import suppress

import pytest
import tomlkit

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.derived_variables import set_times
from deode.submission import TaskSettings
from deode.suites.deode import DeodeSuiteDefinition


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            times.list = ["2000-01-01T00:00:00Z"]
        """
    )


@pytest.fixture()
def minimal_parsed_config(minimal_raw_config):
    return ParsedConfig(
        minimal_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )


@pytest.fixture()
def config_from_task_config_file():
    """Return a raw config common to all tasks."""
    return ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


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
class TestSuite:
    def test_config_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, ParsedConfig)

    @pytest.mark.parametrize(
        "param",
        [
            {"boundaries": {"bdmax": 1}},
            {"suite_control": {"create_static_data": False}},
            {"suite_control": {"create_time_dependent_suite": False, "do_soil": False}},
            {
                "suite_control": {
                    "do_archiving": True,
                    "do_cleaning": False,
                    "do_extractsqlite": True,
                    "do_marsprep": True,
                    "do_pgd": False,
                    "do_soil": False,
                    "interpolate_boundaries": False,
                    "cold_start": False,
                }
            },
            {"suite_control": {"interpolate_boundaries": False}},
            {"submission": {"max_ecf_tasks": 2}},
        ],
    )
    def test_suite(self, config_from_task_config_file, param, tmp_directory):
        config = config_from_task_config_file
        suite_name = "test_suite"
        config = config.copy(
            update={
                "general": {"case": suite_name},
                "scheduler": {
                    "ecfvars": {
                        "ecf_files": f"{tmp_directory}/ecf_files",
                        "ecf_jobout": f"{tmp_directory}/jobout",
                    }
                },
                "platform": {
                    "deode_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                },
            }
        )
        config = config.copy(update=set_times(config))
        config = config.copy(update=param)
        defs = DeodeSuiteDefinition(
            config,
            dry_run=True,
        )
        def_file = f"{tmp_directory}/{suite_name}.def"
        defs.save_as_defs(def_file)
