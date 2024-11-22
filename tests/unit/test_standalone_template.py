#!/usr/bin/env python3
"""Unit tests for the standalone template script."""
import os

from deode import GeneralConstants
from deode.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from deode.logs import logger  # Use deode's own configs for logger
from deode.templates.stand_alone import default_main

logger.enable("deode")


class TestStandalone:
    def test_standalone_template(self, tmp_path_factory):
        deode_home = str(GeneralConstants.PACKAGE_DIRECTORY)
        config = BasicConfig.from_file(
            ConfigParserDefaults.CONFIG_DIRECTORY / "config.toml"
        )
        task_config = ParsedConfig(
            config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )

        task_config = task_config.copy(
            update={
                "submission": {"serial": {"tasks": ["UnitTest"]}},
                "platform": {
                    "scratch": f"{tmp_path_factory.getbasetemp().as_posix()}",
                    "unix_group": "",
                },
            }
        )

        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/config.toml"
        if os.path.exists(output_file):
            os.remove(output_file)

        task_config.save_as(output_file)
        base_dir = os.getcwd()
        default_main("UnitTest", output_file, deode_home)
        os.chdir(base_dir)
