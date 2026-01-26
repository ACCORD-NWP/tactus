#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import os
from unittest.mock import patch

import pytest

from tactus.host_actions import AmbigiousHostError, HostNotFoundError, SelectHost
from tactus.logs import logger
from tactus.scheduler import EcflowClient, EcflowServer, EcflowTask

logger.enable("tactus")


def suite_name():
    return "test_suite"


@pytest.fixture()
@patch("tactus.scheduler.ecflow")
def ecflow_task(__):
    ecf_name = f"/{suite_name}/family/Task"
    ecf_tryno = "1"
    ecf_pass = "abc123"  # noqa
    ecf_rid = None
    ecf_timeout = 20
    return EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, ecf_timeout=ecf_timeout)


# TODO: The mocked ecflow module is treated as the config, but it is not a config
@pytest.fixture()
@patch("tactus.scheduler.ecflow")
def ecflow_server(parsed_config):
    config = parsed_config
    start_command = "start"
    with patch("tactus.scheduler.Platform"):
        return EcflowServer(config, start_command)


class TestScheduler:
    def test_ecf_port_setting(self, ecflow_server: EcflowServer):
        offset = 100
        port = os.getuid() + offset
        ecf_port = ecflow_server._set_port_from_user(offset)
        assert port == ecf_port

    def test_successfully_select_host_from_list(self):
        hostname = "localhost"
        hosts = ["foo", hostname]
        ecf_host = SelectHost._select_host_from_list(hosts)
        assert ecf_host == hostname

    def test_fail_to_find_host_from_list(self):
        hosts = ["foo"]
        host_list = ",".join(hosts)
        with pytest.raises(HostNotFoundError, match=f"No host found, tried:{host_list}"):
            SelectHost._select_host_from_list(hosts)

    def test_to_many_hosts_found_from_list(self):
        hostname = "localhost"
        hosts = [hostname, hostname]
        host_list = ",".join(hosts)
        msg = f"Ambigious host selection:{host_list}"
        with pytest.raises(AmbigiousHostError, match=msg):
            SelectHost._select_host_from_list(hosts)

    def test_ecflow_client(self, ecflow_server: EcflowServer, ecflow_task):
        EcflowClient(ecflow_server, ecflow_task)

    @patch("tactus.scheduler.ecflow")
    def test_start_suite(self, mock, ecflow_server: EcflowServer):
        logger.debug("Print mock: {}", mock)
        def_file = f"/tmp/{suite_name()}.def"  # noqa
        ecflow_server.start_suite(suite_name(), def_file)
        logger.debug("Print mock: {}", mock)
