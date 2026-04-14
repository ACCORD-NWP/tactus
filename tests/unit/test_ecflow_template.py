#!/usr/bin/env python3
"""Unit tests for the eccodes template script."""

import os
from unittest.mock import patch

import pytest

from tactus import GeneralConstants
from tactus.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from tactus.host_actions import TactusHost
from tactus.logs import logger
from tactus.scheduler import EcflowServer
from tactus.templates.ecflow.default import default_main

logger.enable("tactus")


# TODO: The mocked ecflow module is treated as the config, but it is not a config
@pytest.fixture
@patch("tactus.scheduler.ecflow")
def ecflow_server(parsed_config):
    config = parsed_config
    start_command = "start"
    with patch("tactus.scheduler.Platform"):
        return EcflowServer(config, start_command)


class FakeTask:
    def __init__(self, *_args, **_kwargs):
        self.ecf_task = "UnitTest"
        self.ecf_name = "UnitTest"


class TestScheduler:
    @patch("tactus.templates.ecflow.default.EcflowClient")
    @patch("tactus.templates.ecflow.default.EcflowServer")
    @patch("tactus.templates.ecflow.default.EcflowTask", FakeTask)
    def test_ecf_port_setting(
        self, mock_client, mock_server, ecflow_server: EcflowServer, tmp_path_factory
    ):
        offset = 100
        port = os.getuid() + offset
        ecf_port = ecflow_server._set_port_from_user(offset)
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/config_.toml"
        assert port == ecf_port
        kwargs = {
            "ECF_HOST": "localhost",
            "ECF_PORT": os.getuid(),
            "ECF_NAME": "UnitTest",
            "ECF_PASS": "abc123",
            "ECF_TRYNO": 1,
            "ECF_RID": None,
            "ECF_TIMEOUT": 20,
            "BASETIME": "2024-09-16T00:00:00Z",
            "VALIDTIME": "2024-09-16T00:00:00Z",
            "LOGLEVEL": "INFO",
            "ARGS": "",
            "WRAPPER": "",
            "CONFIG": output_file,
            "TACTUS_HOME": str(GeneralConstants.PACKAGE_DIRECTORY),
            "KEEP_WORKDIRS": False,
        }

        config = BasicConfig.from_file(
            ConfigParserDefaults.CONFIG_DIRECTORY / "config.toml"
        )
        tactus_host = TactusHost().detect_tactus_host()
        task_config = ParsedConfig(
            config,
            json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
            host=tactus_host,
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

        if os.path.exists(output_file):
            os.remove(output_file)

        task_config.save_as(output_file)
        mock_client.return_value.__enter__.return_value = None
        mock_client.return_value.__exit__.return_value = None
        assert mock_server is not None
        prev_dir = os.getcwd()
        default_main(kwargs)
        os.chdir(prev_dir)
