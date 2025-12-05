#!/usr/bin/env python3
"""Unit tests for the impact methods."""

import json
import os
from dataclasses import dataclass
from pathlib import Path

import pytest
import tomli
import tomlkit
import xmltodict
import yaml

from tactus.derived_variables import set_times
from tactus.tasks.batch import BatchJob
from tactus.tasks.impacts import ImpactModel, ImpactModels, get_fdb_info

WORKING_DIR = Path.cwd()


@dataclass()
class UnitTest(ImpactModel):
    """Test method."""

    name = "unittest"

    def run(self):
        """Only for unit testing."""
        path = self.platform.substitute(self.config["path"])
        args = self.platform.substitute(self.config["arguments"])
        cmd = f"echo {args} > {path}/txtfile"
        BatchJob(os.environ, wrapper="").run(cmd)


@pytest.fixture(scope="module")
def basic_config(tmp_directory, default_config):
    config = default_config
    config = config.copy(update=set_times(config))
    config_patch = tomlkit.parse(
        f"""
        [general]
            keep_workdirs = false
        [system]
            wrk = "{tmp_directory}"
        [submission.task]
            wrapper = "echo"
        [platform]
            deode_home = "{WORKING_DIR}"
        [impact.unittest]
            active = true
            arguments = "hello world"
            config_name = "{tmp_directory}/unittest.json"
            path = "{tmp_directory}"
            task = "test"
        [impact.unittest.communicate]
            user_ecf_port = "bar"
            user_ecf_host = "bar"
        [impact.unittest.test]
            arguments = "hello world"
        """
    )
    config = config.copy(update=config_patch)

    return config


@pytest.fixture(scope="module")
def basic_config_installed(basic_config, tmp_directory):
    config_patch = tomlkit.parse(
        f"""
        [platform.impact]
            unittest = "{tmp_directory}"
        """
    )
    config = basic_config.copy(update=config_patch)

    return config


def test_impact_inactive_not_installed(basic_config):
    model = ImpactModels(basic_config, "test")
    assert len(model.impact) == 0


def test_impact_inactive_active_false(basic_config_installed):
    basic_config = basic_config_installed.copy(
        update={"impact": {"unittest": {"active": False}}}
    )
    model = ImpactModels(basic_config, "test")
    assert len(model.impact) == 0


def test_impact_inactive_wrong_task(basic_config_installed):
    model = ImpactModels(basic_config_installed)
    assert len(model.impact) == 0


def test_impact_run_cmd(basic_config_installed, tmp_directory):
    model = ImpactModels(basic_config_installed, "test")
    assert len(model.impact) > 0
    model.execute()

    txtfile = f"{tmp_directory}/txtfile"
    with open(txtfile, "r", encoding="utf-8") as f:
        line = f.read()
    assert line.strip() == "hello world"


@pytest.mark.parametrize("filetype", ["yml", "json", "toml", "xml"])
def test_impact_different_configs(basic_config_installed, tmp_directory, filetype):
    filename = f"{tmp_directory}/unittest.{filetype}"
    basic_config = basic_config_installed.copy(
        update={"impact": {"unittest": {"config_name": filename}}}
    )
    model = ImpactModels(basic_config, "test")
    model.execute()

    with open(filename, "rb") as f:
        if filename.endswith(".json"):
            config_data = json.load(f)
        if filename.endswith(".toml"):
            config_data = tomli.load(f)
        if filename.endswith((".yaml", ".yml")):
            config_data = yaml.safe_load(f)
        if filename.endswith((".xml")):
            config_data = xmltodict.parse(f.read())["root"]

    assert config_data == basic_config_installed.get("impact.unittest.communicate").dict()


@pytest.mark.parametrize("members", [[0], [0, 1]])
def test_get_fdb_info(default_config, members):
    stream = "enfo" if len(members) > 1 else "oper"
    truth = {
        "class": "d1",
        "dataset": "on-demand-extremes-dt",
        "date": "20251103",
        "expver": "fomo",
        "georef": "u15rxs",
        "step": [0, 1, 2, 3],
        "stream": stream,
        "time": "0000",
    }

    if len(members) > 1:
        truth["number"] = members

    basic_config = default_config.copy(
        update={
            "general": {
                "times": {"basetime": "2025-11-03T00:00:00Z", "forecast_range": "PT3H"}
            },
            "fdb": {"grib_set": {"expver": "fomo", "georef": "u15rxs", "stream": stream}},
            "eps": {"general": {"members": members}},
        }
    )

    fdb_keys = get_fdb_info(basic_config)

    assert fdb_keys == truth
