#!/usr/bin/env python3
"""Unit tests for the standalone template script."""
import os

from deode import GeneralConstants

from tactus.logs import logger  # Use deode's own configs for logger
from tactus.templates.stand_alone import default_main

logger.enable("deode")


class TestStandalone:
    def test_standalone_template(self, tmp_directory, default_config):
        deode_home = str(GeneralConstants.PACKAGE_DIRECTORY)
        task_config = default_config.copy(
            update={
                "submission": {
                    "types": {
                        "pytest": {
                            "tasks": ["UnitTest"],
                            "SCHOST": "localhost",
                            "WRAPPER": "",
                        }
                    }
                },
                "platform": {
                    "scratch": f"{tmp_directory}",
                    "unix_group": "",
                },
            }
        )

        output_file = f"{tmp_directory}/config.toml"
        if os.path.exists(output_file):
            os.remove(output_file)

        task_config.save_as(output_file)
        prev_dir = os.getcwd()
        default_main("UnitTest", output_file, deode_home)
        os.chdir(prev_dir)
