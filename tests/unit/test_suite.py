#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import pytest

from deode.config_parser import ParsedConfig
from deode.submission import TaskSettings
from deode.suites import SuiteDefinition


@pytest.fixture()
def minimal_raw_config():
    return {
        "general": {"assimilation_times": {"list": ["20000101T00"]}},
    }


@pytest.fixture()
def minimal_parsed_config(minimal_raw_config):
    return ParsedConfig.parse_obj(minimal_raw_config)


class TestSuite:
    # pylint: disable=no-self-use

    def test_config_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, ParsedConfig)

    def test_suite(self, minimal_parsed_config):
        config = minimal_parsed_config
        suite_name = "test_suite"
        loglevel = "debug"
        settings = {
            "submit_types": ["background"],
            "default_submit_type": "background",
            "background": {"SCHOST": "localhost"},
        }
        background = TaskSettings(settings)
        defs = SuiteDefinition(
            suite_name, "/tmp/joboutdir", "ecf", config, background, loglevel  # noqa
        )
        def_file = f"/tmp/{suite_name}.def"  # noqa
        defs.save_as_defs(def_file)


if __name__ == "__main__":
    pytest.main()
