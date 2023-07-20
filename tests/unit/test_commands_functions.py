#!/usr/bin/env python3
"""Unit tests for commands_functions.py."""
import os
from argparse import ArgumentParser
from pathlib import Path

import pytest
import tomlkit

from deode.commands_functions import set_deode_home, show_namelist
from deode.config_parser import ParsedConfig


@pytest.fixture()
def config_platform():
    """Set the platform specific configuration."""
    task_configs = tomlkit.parse(
        """
        [general]
            tstep = 1
            bdint = "PT3H"
            forecast_range = "PT3H"
            cycle = "CY48t3"
            accept_static_namelists = false
        [system]
            fullpos_config_file = "@DEODE_HOME@/deode/namelist_generation_input/CY48t3/fullpos_namelist.yml"
            namelists = "/tmp"
        [general.times]
            list = ["2000-01-01T00:00:00Z"]
            basetime = "2000-01-01T00:00:00Z"
            validtime = "2000-01-02T00:00:00Z"
        [general.output_settings]
            surfex = ["PT0H:PT12H:PT1H"]
        [macros]
            os_macros = ["USER", "HOME", "PWD"]
            group_macros = ["platform","system"]
            gen_macros = []
        [domain]
            name = "DEMO_60x80_2500m"
            nimax = 49
            njmax = 69
            xloncen = 4.9
            xlatcen = 51.967
            xlon0 = 0.0
            xlat0 = 52.5
            xbeta  = 0.0
            xdx = 2500.0
            xdy = 2500.0
            ilone = 11
            ilate = 11
            nbzong = 8
            nbzonl = 8
            gridtype = "linear"
        [vertical_levels]
            nlev = 65
            ahalf = [0]
            bhalf = [1]
        """
    )

    return task_configs


@pytest.fixture()
def parsed_config(config_platform):
    return ParsedConfig.parse_obj(config_platform)


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
    arg.namelist = "forecast"
    arg.namelist_name = None
    arg.loglevel = "DEBUG"
    arg.domain = "test"
    arg.no_substitute = True
    return arg


@pytest.mark.parametrize(
    "param",
    [
        {
            "config": {"general": {"accept_static_namelists": False}},
            "path": "/tmp/test1",  # noqa S108
        },
        {
            "config": {
                "general": {"accept_static_namelists": True},
                "system": {"namelist": "/tmp/test1"},  # noqa S108
            },
            "path": "tmp/test2",  # noqa S108
        },
    ],
)
def test_show_namelist(set_arg, parsed_config, param):
    config = parsed_config
    config = config.copy(update=param["config"])
    prev_cwd = Path.cwd()
    outpath = param["path"]
    os.makedirs(outpath, exist_ok=True)
    os.chdir(outpath)
    show_namelist(set_arg, config)
    os.chdir(prev_cwd)
    assert os.path.isfile(f"{outpath}/namelist_master_forecast")
    assert os.path.isfile(f"{outpath}/xxt00000000")
    assert os.path.isfile(f"{outpath}/xxtddddhhmm")


if __name__ == "__main__":
    pytest.main()
