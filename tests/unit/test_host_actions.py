#!/usr/bin/env python3
"""Unit tests for host actions."""

import os
import re

import pytest

from tactus.host_actions import TactusHost, set_tactus_home


def test_set_tactus_home(default_config):
    tactus_home = set_tactus_home(default_config)
    assert os.path.isdir(tactus_home)


def test_set_tactus_home_from_arg(default_config):
    tactus_home = set_tactus_home(default_config, "foo")
    assert tactus_home == "foo"


@pytest.fixture()
def _module_mockers(module_mocker):
    def new_socket_gethostname():
        return "tactus-test"

    module_mocker.patch("socket.gethostname", new=new_socket_gethostname)


@pytest.fixture()
def _module_mockers_yaml(module_mocker):
    def new_yaml_safe_load(infile):  # noqa ARG001
        return None

    module_mocker.patch("yaml.safe_load", new=new_yaml_safe_load)


@pytest.mark.usefixtures("_module_mockers")
def test_by_host():
    dh = TactusHost()
    dh.known_hosts = {"by_host": {"hostname": "tactus-test"}}
    tactus_host = dh.detect_tactus_host()
    assert tactus_host == "by_host"


def test_by_env():
    dh = TactusHost()
    dh.known_hosts = {"by_env": {"env": {"DEODE_HOST_TESTENV": "foo"}}}
    tactus_host = dh.detect_tactus_host()
    assert tactus_host == "by_env"


def test_from_env_tactus_host():
    os.environ["DEODE_HOST"] = "bar"
    dh = TactusHost()
    dh.known_hosts = {}
    tactus_host = dh.detect_tactus_host()
    assert tactus_host == "bar"
    del os.environ["DEODE_HOST"]


def test_ambiguous_host():
    dh = TactusHost()
    dh.hostname = "tactus-test"
    dh.known_hosts = {
        "by_host": {"hostname": "tactus-test"},
        "by_env": {"env": {"DEODE_HOST_TESTENV": "foo"}},
    }
    os.environ["DEODE_HOST_TESTENV"] = "foo"
    with pytest.raises(
        RuntimeError, match=re.escape("Ambiguous matches: ['by_host', 'by_env']")
    ):
        dh.detect_tactus_host()
    del os.environ["DEODE_HOST_TESTENV"]


def test_non_existing_detect_method():
    dh = TactusHost()
    dh.known_hosts = {"erroneous": {"foo": "bar"}}
    with pytest.raises(RuntimeError, match="No tactus-host detection using foo"):
        dh.detect_tactus_host()


@pytest.mark.usefixtures("_module_mockers_yaml")
def test_load_known_hosts_handles_none():
    with pytest.raises(RuntimeError, match="No hosts available in"):
        TactusHost()
