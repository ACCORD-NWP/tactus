#!/usr/bin/env python3
"""Unit tests for host actions."""

import os
import re

import pytest

from deode.host_actions import DeodeHost


@pytest.fixture()
def _module_mockers(module_mocker):
    def new_socket_gethostname():
        return "deode-test"

    module_mocker.patch("socket.gethostname", new=new_socket_gethostname)


@pytest.fixture()
def _module_mockers_yaml(module_mocker):
    def new_yaml_safe_load(infile):  # noqa ARG001
        return None

    module_mocker.patch("yaml.safe_load", new=new_yaml_safe_load)


@pytest.mark.usefixtures("_module_mockers")
def test_default_host():
    dh = DeodeHost()
    dh.known_hosts = {
        "by_host": {"hostname": "undefined"},
        "by_env": {"env": {"DEODE_HOST_TESTENV": "foo"}},
    }
    dh.detect_deode_host()


def test_ambiguous_host():
    dh = DeodeHost()
    dh.known_hosts = {
        "by_host": {"hostname": "deode-test"},
        "by_env": {"env": {"DEODE_HOST_TESTENV": "foo"}},
    }
    os.environ["DEODE_HOST_TESTENV"] = "foo"
    with pytest.raises(
        RuntimeError, match=re.escape("Ambiguous matches: ['by_host', 'by_env']")
    ):
        dh.detect_deode_host()


def test_non_existing_detect_method():
    dh = DeodeHost()
    dh.known_hosts = {"erroneous": {"foo": "bar"}}
    with pytest.raises(RuntimeError, match="No deode-host detection using foo"):
        dh.detect_deode_host()


@pytest.mark.usefixtures("_module_mockers_yaml")
def test_load_known_hosts_handles_none():
    with pytest.raises(RuntimeError, match="No hosts available in"):
        DeodeHost()
