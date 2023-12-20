#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
from unittest.mock import patch

import pytest

from deode.logs import logger
from deode.scheduler import EcflowClient, EcflowServer, EcflowTask

logger.enable("deode")


def suite_name():
    return "test_suite"


@pytest.fixture()
@patch("deode.scheduler.ecflow")
def ecflow_task(__):
    ecf_name = f"/{suite_name}/family/Task"
    ecf_tryno = "1"
    ecf_pass = "abc123"  # noqa
    ecf_rid = None
    ecf_timeout = 20
    return EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, ecf_timeout=ecf_timeout)


@pytest.fixture()
@patch("deode.scheduler.ecflow")
def ecflow_server(parsed_config):
    config = parsed_config
    start_command = "start"
    return EcflowServer(config, start_command)


class TestScheduler:
    def test_ecflow_client(self, ecflow_server, ecflow_task):
        EcflowClient(ecflow_server, ecflow_task)

    @patch("deode.scheduler.ecflow")
    def test_start_suite(self, mock, ecflow_server):
        logger.debug("Print mock: {}", mock)
        def_file = f"/tmp/{suite_name()}.def"  # noqa
        ecflow_server.start_suite(suite_name(), def_file)
        logger.debug("Print mock: {}", mock)
