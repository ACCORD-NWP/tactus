#!/usr/bin/env python3
"""Unit tests for commands_functions.py."""
import os
from argparse import ArgumentParser
from pathlib import Path

import pytest

from deode.commands_functions import namelist_integrate, set_deode_home, show_namelist
from deode.config_parser import PACKAGE_CONFIG_PATH, ParsedConfig


@pytest.fixture()
def parsed_config():
    """Return a raw config common to all tasks."""
    return ParsedConfig.from_file(PACKAGE_CONFIG_PATH, json_schema={})


def test_set_deode_home(parsed_config):
    arg = ArgumentParser()
    arg.deode_home = None
    deode_home = set_deode_home(arg, parsed_config)
    assert os.path.isdir(deode_home)


@pytest.fixture()
def set_arg():
    arg = ArgumentParser()
    arg.deode_home = None
    arg.namelist_type = "master"
    arg.namelist = "forecast_bdmodel_ifs"
    arg.namelist_name = None
    arg.loglevel = "DEBUG"
    arg.domain = "test"
    arg.no_substitute = True
    return arg


@pytest.fixture()
def nlint_arg():
    arg = ArgumentParser()
    arg.deode_home = None
    arg.namelist = ["deode/data/namelists/unit_testing/nl_master_integrate"]
    arg.yaml = "deode/data/namelists/unit_testing/nl_master_base.yml"
    arg.tag = "nl_master_base"
    arg.output = "/tmp/nl_master_integrated.yml"  # noqa S108
    arg.loglevel = "DEBUG"
    arg.domain = "test"
    return arg


@pytest.mark.parametrize(
    "param",
    [
        {
            "config": {
                "general": {"accept_static_namelists": False},
            },
            "path": "/tmp/test1",  # noqa S108
            "clean": False,
        },
        {
            "config": {
                "general": {"accept_static_namelists": True},
                "system": {"namelists": "/tmp/test1"},  # noqa S108
            },
            "path": "/tmp/test2",  # noqa S108
            "clean": True,
        },
        {
            "config": {
                "general": {"accept_static_namelists": True},
                "system": {"namelists": "/tmp/test2"},  # noqa S108
            },
            "path": "/tmp/test3",  # noqa S108
            "clean": False,
        },
    ],
)
def test_show_namelist(set_arg, parsed_config, param):
    config = parsed_config.copy(update=param["config"])
    prev_cwd = Path.cwd()
    outpath = param["path"]
    os.makedirs(outpath, exist_ok=True)
    os.chdir(outpath)
    show_namelist(set_arg, config)
    os.chdir(prev_cwd)
    assert os.path.isfile(f"{outpath}/namelist_master_forecast_bdmodel_ifs")
    assert os.path.isfile(f"{outpath}/xxt00000000")
    assert os.path.isfile(f"{outpath}/xxtddddhhmm")
    if param["clean"]:
        os.remove(f"{outpath}/xxt00000000")
        os.remove(f"{outpath}/xxtddddhhmm")


def test_namelist_integrate(nlint_arg, parsed_config):
    if os.path.exists(nlint_arg.output):
        os.remove(nlint_arg.output)
    namelist_integrate(nlint_arg, parsed_config)
    assert os.path.isfile(nlint_arg.output)


if __name__ == "__main__":
    pytest.main()
