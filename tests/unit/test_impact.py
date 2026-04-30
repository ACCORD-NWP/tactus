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

from deode.derived_variables import set_times
from deode.tasks.batch import BatchJob
from deode.tasks.impacts import BaseImpactModel, ImpactModels

WORKING_DIR = Path.cwd()


@dataclass()
class UnitTest(BaseImpactModel):
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
        """
    )
    return config.copy(update=config_patch)


@pytest.fixture(scope="module")
def basic_config_installed(basic_config, tmp_directory):
    return basic_config.update("platform.impact.unittest", tmp_directory)


@pytest.fixture(scope="module")
def basic_config_installed_extended(basic_config_installed):
    config_patch = tomlkit.parse(
        """
        [topsection]
            key1 = "value1"
            key2 = 2
            key3 = 3.1415

        [topsection.subsection]
            use_me = "maybe"
        """
    )
    return basic_config_installed.copy(update=config_patch)


@pytest.fixture(scope="module")
def basic_config_installed_test2(basic_config_installed):
    config_patch = tomlkit.parse(
        """
        [impact.unittest.test2]
            arguments = "hello world, from task test2"
        """
    )
    return basic_config_installed.copy(update=config_patch)


def test_impact_inactive_not_installed(basic_config):
    model = ImpactModels(basic_config, "test")
    assert len(model.impact) == 0


def test_impact_inactive_active_false(basic_config_installed):
    config = basic_config_installed.update("impact.unittest.active", False)
    model = ImpactModels(config, "test")
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


def test_impact_run_cmd_second_task(basic_config_installed_test2, tmp_directory):
    model = ImpactModels(basic_config_installed_test2, "test2")
    assert len(model.impact) > 0
    model.execute()

    txtfile = f"{tmp_directory}/txtfile"
    with open(txtfile, "r", encoding="utf-8") as f:
        line = f.read()
    assert line.strip() == "hello world, from task test2"


@pytest.mark.parametrize("filetype", ["yml", "json", "toml", "xml"])
def test_impact_different_configs(basic_config_installed, tmp_directory, filetype):
    filename = f"{tmp_directory}/unittest.{filetype}"
    config = basic_config_installed.update("impact.unittest.config_name", filename)

    model = ImpactModels(config, "test")
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

    assert config_data == basic_config_installed.get_as_dict(
        "impact.unittest.communicate"
    )


def test_impact_communicate_copy(basic_config_installed_extended, tmp_directory):
    filename = f"{tmp_directory}/unittest.json"
    config_patch = tomlkit.parse(
        f"""
        [impact.unittest]
            config_name = "{filename}"

        [impact.unittest.communicate.topsection.COPY]
        """
    )
    config = basic_config_installed_extended.copy(update=config_patch)

    model = ImpactModels(config, "test")
    model.execute()

    with open(filename, "rb") as f:
        config_data = json.load(f)

    ref_data = basic_config_installed_extended.get_as_dict("topsection")
    del ref_data["subsection"]

    assert config_data["topsection"] == ref_data


def test_impact_communicate_copyall(basic_config_installed_extended, tmp_directory):
    filename = f"{tmp_directory}/unittest.json"
    config_patch = tomlkit.parse(
        f"""
        [impact.unittest]
            config_name = "{filename}"

        [impact.unittest.communicate.topsection.COPYALL]
        """
    )
    config = basic_config_installed_extended.copy(update=config_patch)

    model = ImpactModels(config, "test")
    model.execute()

    with open(filename, "rb") as f:
        config_data = json.load(f)

    assert config_data["topsection"] == basic_config_installed_extended.get_as_dict(
        "topsection"
    )
