#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pytest
import tomlkit

from deode.config_parser import ParsedConfig
from deode.submission import TaskSettings
from deode.suites import SuiteDefinition


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            data_rootdir = "."
            assimilation_times.list = ["2000-01-01T00:00:00Z"]
        """
    )


@pytest.fixture()
def minimal_parsed_config(minimal_raw_config):
    return ParsedConfig.parse_obj(minimal_raw_config)


@pytest.fixture()
def config_from_task_config_file():
    fname = "docs/task_config_example.toml"
    return ParsedConfig.from_file(fname)


class TestSuite:
    # pylint: disable=no-self-use

    def test_config_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, ParsedConfig)

    def test_suite(self, config_from_task_config_file):
        config = config_from_task_config_file
        suite_name = "test_suite"
        loglevel = "debug"
        settings = {
            "submit_types": ["background"],
            "default_submit_type": "background",
            "background": {"SCHOST": "localhost"},
        }
        background = TaskSettings(settings)
        defs = SuiteDefinition(
            suite_name,
            "/tmp/joboutdir",  # noqa
            "ecf",
            config,
            background,
            loglevel,  # noqa
            dry_run=True,
        )
        def_file = f"/tmp/{suite_name}.def"  # noqa
        defs.save_as_defs(def_file)


if __name__ == "__main__":
    pytest.main()
