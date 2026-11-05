#!/usr/bin/env python3
"""Unit tests for host actions."""

import os

import pytest

from tactus.config_parser import GeneralConstants
from tactus.host_actions import (
    AmbigiousHostError,
    HostNotFoundError,
    SelectHost,
    TactusHost,
    set_tactus_home,
)


def test_set_tactus_home(default_config):
    tactus_home = set_tactus_home(default_config)
    assert os.path.isdir(tactus_home)


def test_set_tactus_home_from_arg(default_config):
    tactus_home = set_tactus_home(default_config, "foo")
    assert tactus_home == "foo"


@pytest.fixture
def _module_mockers(mocker):
    def new_socket_gethostname():
        return "tactus-test"

    mocker.patch("socket.gethostname", new=new_socket_gethostname)


@pytest.fixture
def _module_mockers_yaml(mocker):
    def new_yaml_safe_load(infile):
        return None

    mocker.patch("yaml.safe_load", new=new_yaml_safe_load)


@pytest.mark.usefixtures("_module_mockers")
def test_by_host():
    dh = TactusHost()
    dh.known_hosts = {"by_host": {"hostname": "tactus-test"}}
    tactus_host = dh.detect_tactus_host()
    assert tactus_host == "by_host"


def test_by_env():
    dh = TactusHost()
    dh.known_hosts = {"by_env": {"env": {"TACTUS_HOST_TESTENV": "foo"}}}
    tactus_host = dh.detect_tactus_host()
    assert tactus_host == "by_env"


def test_from_env_tactus_host():
    os.environ["TACTUS_HOST"] = "bar"
    dh = TactusHost()
    dh.known_hosts = {}
    tactus_host = dh.detect_tactus_host()
    assert tactus_host == "bar"
    del os.environ["TACTUS_HOST"]


def test_incomplete_host():
    dh = TactusHost()
    dh.hostname = "tactus-test"
    dh.known_hosts = {
        "by_host": {"hostname": "tactus-test"},
        "by_env": {"env": {"TACTUS_HOST_TESTENV": "foo"}},
    }
    os.environ["TACTUS_HOST_TESTENV"] = "foo"
    with pytest.raises(RuntimeError, match="Ambiguous matches"):
        dh.detect_tactus_host()
    del os.environ["TACTUS_HOST_TESTENV"]


def test_non_existing_detect_method():
    dh = TactusHost()
    dh.known_hosts = {"erroneous": {"foo": "bar"}}
    with pytest.raises(RuntimeError, match="No tactus-host detection using foo"):
        dh.detect_tactus_host()


@pytest.mark.usefixtures("_module_mockers_yaml")
def test_load_known_hosts_handles_none():
    with pytest.raises(RuntimeError, match="No hosts available in"):
        TactusHost()


# Additional coverage for TactusHost, set_tactus_home and SelectHost


@pytest.fixture
def _no_tactus_host(monkeypatch):
    monkeypatch.delenv("TACTUS_HOST", raising=False)


@pytest.fixture
def tactus_host_factory(_no_tactus_host):
    def _make(known_hosts, hostname="tactus-test"):
        dh = TactusHost(known_hosts=known_hosts)
        dh.hostname = hostname
        return dh

    return _make


def test_load_known_hosts_from_dict(tactus_host_factory):
    known_hosts = {"first": {"hostname": "foo"}, "second": {"hostname": "bar"}}
    dh = tactus_host_factory(known_hosts)
    assert dh.known_hosts is known_hosts
    assert dh.available_hosts == ["first", "second"]
    assert dh.default_host == "first"


@pytest.mark.usefixtures("_no_tactus_host")
def test_load_known_hosts_from_file(tmp_path):
    known_hosts_file = tmp_path / "known_hosts.yml"
    known_hosts_file.write_text('from_file:\n  hostname: "foo\\\\d+"\n')
    dh = TactusHost(known_hosts_file=str(known_hosts_file))
    assert dh.known_hosts == {"from_file": {"hostname": "foo\\d+"}}


def test_detect_by_hostname_list(tactus_host_factory):
    dh = tactus_host_factory(
        {
            "default": {"hostname": "nomatch"},
            "listed": {"hostname": ["uan\\d\\d", "nid\\d+"]},
        },
        hostname="nid001234",
    )
    assert dh.detect_tactus_host() == "listed"


def test_detect_by_env_list(tactus_host_factory, monkeypatch):
    monkeypatch.setenv("TACTUS_HOST_TESTENV", "bar2")
    dh = tactus_host_factory({
        "default": {"hostname": "nomatch"},
        "listed": {"env": {"TACTUS_HOST_TESTENV": ["foo\\d", "bar\\d"]}},
    })
    assert dh.detect_tactus_host() == "listed"


def test_detect_by_env_value_mismatch(tactus_host_factory, monkeypatch):
    monkeypatch.setenv("TACTUS_HOST_TESTENV", "other")
    dh = tactus_host_factory({"by_env": {"env": {"TACTUS_HOST_TESTENV": "foo"}}})
    assert dh._detect_by_env({"TACTUS_HOST_TESTENV": "foo"}) is False


def test_no_match_returns_default(tactus_host_factory):
    dh = tactus_host_factory({
        "first": {"hostname": "foo"},
        "second": {"hostname": "bar"},
    })
    assert dh.detect_tactus_host() == "first"


def test_no_match_without_default_returns_none(tactus_host_factory):
    dh = tactus_host_factory({
        "first": {"hostname": "foo"},
        "second": {"hostname": "bar"},
    })
    assert dh.detect_tactus_host(use_default=False) is None


def test_ambiguous_single_method(tactus_host_factory):
    dh = tactus_host_factory({
        "first": {"hostname": "tactus-.*"},
        "second": {"hostname": ".*-test"},
    })
    with pytest.raises(RuntimeError, match="Ambiguous matches"):
        dh.detect_tactus_host()


def test_incomplete_single_method_match(tactus_host_factory, monkeypatch):
    """A host with two rules where only one of them matches."""
    monkeypatch.delenv("TACTUS_HOST_TESTENV", raising=False)
    dh = tactus_host_factory({
        "default": {"hostname": "nomatch"},
        "two_rules": {
            "env": {"TACTUS_HOST_TESTENV": "foo"},
            "hostname": "tactus-test",
        },
    })
    with pytest.raises(RuntimeError, match="Incomplete match"):
        dh.detect_tactus_host()


def test_complete_two_rule_match(tactus_host_factory, monkeypatch):
    """A host with two rules where both match (the foo case)."""
    monkeypatch.setenv("TACTUS_HOST_TESTENV", "foo1")
    dh = tactus_host_factory(
        {
            "default": {"hostname": "nomatch"},
            "foo": {
                "env": {"TACTUS_HOST_TESTENV": "foo\\d+"},
                "hostname": ".*.bar",
            },
        },
        hostname="host1.bar",
    )
    assert dh.detect_tactus_host() == "foo"


def test_ambiguous_two_rule_match(tactus_host_factory, monkeypatch):
    """Two hosts that both match on both rules."""
    monkeypatch.setenv("TACTUS_HOST_TESTENV", "foo")
    rules = {"env": {"TACTUS_HOST_TESTENV": "foo"}, "hostname": "tactus-test"}
    dh = tactus_host_factory({"first": dict(rules), "second": dict(rules)})
    with pytest.raises(RuntimeError, match="Ambiguous matches"):
        dh.detect_tactus_host()


def test_set_tactus_home_from_config():
    assert set_tactus_home({"platform.tactus_home": "/my/tactus"}) == "/my/tactus"


def test_set_tactus_home_missing_in_config():
    tactus_home = set_tactus_home({})
    assert tactus_home == str(GeneralConstants.PACKAGE_DIRECTORY)


def test_set_tactus_home_set_by_the_system():
    tactus_home = set_tactus_home({"platform.tactus_home": "set-by-the-system"})
    assert tactus_home == str(GeneralConstants.PACKAGE_DIRECTORY)


@pytest.fixture
def mock_ping(monkeypatch):
    """Replace ping and sleep; ping answers from a per-try list of live hosts."""
    state = {"responses": [], "calls": 0, "sleeps": 0}

    def fake_ping(host):
        state["calls"] += 1
        return host in state["responses"][0]

    def fake_sleep(_delay):
        state["sleeps"] += 1
        state["responses"].pop(0)

    monkeypatch.setattr("tactus.host_actions.ping", fake_ping)
    monkeypatch.setattr("tactus.host_actions.time.sleep", fake_sleep)
    return state


def test_select_host_single(mock_ping):
    mock_ping["responses"] = [{"b"}]
    assert SelectHost._select_host_from_list(["a", " b "]) == "b"
    assert mock_ping["sleeps"] == 0


def test_select_host_after_retry(mock_ping):
    mock_ping["responses"] = [set(), {"a"}]
    assert SelectHost._select_host_from_list(["a", "b"], tries=3) == "a"
    assert mock_ping["sleeps"] == 1


def test_select_host_not_found(mock_ping):
    mock_ping["responses"] = [set(), set(), set()]
    with pytest.raises(HostNotFoundError, match="No host found, tried:a,b"):
        SelectHost._select_host_from_list(["a", "b"], tries=3)
    assert mock_ping["sleeps"] == 2


def test_select_host_ambiguous(mock_ping):
    mock_ping["responses"] = [{"a", "b"}]
    with pytest.raises(AmbigiousHostError, match="Ambigious host selection:a,b"):
        SelectHost._select_host_from_list(["a", "b"])


def test_complete_match_wins_over_partial(tactus_host_factory, monkeypatch):
    """One host matches all its rules, another only some: the complete one is used."""
    monkeypatch.delenv("TACTUS_HOST_TESTENV", raising=False)
    dh = tactus_host_factory({
        "foo": {"env": {"TACTUS_HOST_TESTENV": "foo\\d+"}, "hostname": "tactus-.*"},
        "other": {"hostname": "tactus-test"},
    })
    assert dh.detect_tactus_host() == "other"


def test_empty_tactus_host_env_is_ignored(monkeypatch):
    monkeypatch.setenv("TACTUS_HOST", "")
    dh = TactusHost(known_hosts={"first": {"hostname": "nomatch"}})
    assert dh.detect_tactus_host() == "first"


def test_default_host_follows_known_hosts(tactus_host_factory):
    dh = tactus_host_factory({"first": {"hostname": "foo"}})
    dh.known_hosts = {"second": {"hostname": "bar"}, "third": {"hostname": "baz"}}
    assert dh.default_host == "second"
    assert dh.available_hosts == ["second", "third"]


@pytest.mark.parametrize("content", ["[]\n", "- a\n- b\n", "just a string\n"])
@pytest.mark.usefixtures("_no_tactus_host")
def test_load_known_hosts_not_a_mapping(tmp_path, content):
    known_hosts_file = tmp_path / "known_hosts.yml"
    known_hosts_file.write_text(content)
    with pytest.raises(RuntimeError, match="(No hosts available|must be a mapping)"):
        TactusHost(known_hosts_file=str(known_hosts_file))


@pytest.mark.parametrize(
    "rules",
    [
        {"hostname": 42},
        {"hostname": ["ok", 42]},
        {"env": "not-a-mapping"},
        {"env": {"TACTUS_HOST_TESTENV": 42}},
    ],
)
def test_invalid_rule_patterns(tactus_host_factory, monkeypatch, rules):
    monkeypatch.setenv("TACTUS_HOST_TESTENV", "foo")
    dh = tactus_host_factory({"bad": rules})
    with pytest.raises(TypeError):
        dh.detect_tactus_host()


def test_select_host_empty_list(mock_ping):
    with pytest.raises(HostNotFoundError, match="No host found"):
        SelectHost._select_host_from_list([])
    assert mock_ping["calls"] == 0


def test_select_host_zero_tries(mock_ping):
    with pytest.raises(HostNotFoundError, match="No host found, tried:a"):
        SelectHost._select_host_from_list(["a"], tries=0)
    assert mock_ping["calls"] == 0
