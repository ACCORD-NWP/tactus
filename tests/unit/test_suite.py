#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import os
from contextlib import suppress

import pytest

from deode.config_parser import ParsedConfig
from deode.derived_variables import set_times
from deode.submission import TaskSettings
from deode.suites.deode import DeodeSuiteDefinition


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
    def test_config_can_be_instantiated(self, default_config):
        assert isinstance(default_config, ParsedConfig)

    @pytest.mark.parametrize(
        "param",
        [
            {
                "suite_control": {
                    "do_marsprep": False,
                    "interpolate_boundaries": False,
                    "create_static_data": False,
                },
                "general": {
                    "times": {
                        "end": "2022-05-03T00:00:00Z",
                        "start": "2022-05-02T00:00:00Z",
                    },
                },
                "eps": {
                    "general": {"members": [0, 1]},
                },
            },
            {"boundaries": {"bdmax": 1}},
            {"suite_control": {"create_static_data": False}},
            {"suite_control": {"create_time_dependent_suite": False, "do_soil": False}},
            {
                "suite_control": {
                    "do_archiving": False,
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
    def test_suite(self, default_config, param, tmp_directory):
        config = default_config
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
                    "mirrorglobalDT": {"remote_path": "@YYYY@/@HH@mm@"},
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
