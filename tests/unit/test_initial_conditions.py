#!/usr/bin/env python3
"""Unit tests for the initial_conditions."""

import contextlib
from pathlib import Path

import pytest
import tomlkit
from deode import GeneralConstants

from tactus.derived_variables import set_times
from tactus.initial_conditions import InitialConditions


@pytest.fixture(params=[False, True])
def parsed_config(request, tmp_directory, default_config):
    """Return a raw config common to all tasks."""
    if request.param:
        for f in [
            "foo",
            "ELSCFTESTALBC000",
            "ICMSHTEST+0003h00m00s",
            "ICMSHTEST+0003h00m00s.sfx",
            "ICMSHTESTINIT.sfx",
        ]:
            Path(f"{tmp_directory}/{f}").touch()

    config = default_config
    config = config.copy(update=set_times(config))

    config_patch = tomlkit.parse(
        f"""
        [file_templates.initfile]
            archive = "@INTP_BDDIR@/@HISTORY_TEMPLATE@"
        [file_templates.initfile_sfx]
            archive = "@INTP_BDDIR@/@SURFEX_TEMPLATE@"
        [general]
            cnmexp = "TEST"
            bogus = "{request.param}"
        [system]
            intp_bddir = "{tmp_directory}"
            archive = "{tmp_directory}"
        [platform]
            deode_home = "{GeneralConstants.PACKAGE_DIRECTORY}"
        """
    )

    config = config.copy(update=config_patch)
    return config


@pytest.fixture(params=[True, False])
def set_surfex(request):
    return {"general": {"surfex": request.param}}


@pytest.fixture(params=["start", "cold_start", "restart"])
def set_mode(request):
    return {"suite_control": {"mode": request.param}}


@pytest.mark.parametrize(
    "param",
    [
        {"general": {"deode_home": str(GeneralConstants.PACKAGE_DIRECTORY)}},
        {
            "file_templates": {
                "initfile": {"archive": "@INTP_BDDIR@/@HISTORY_TEMPLATE@"},
                "initfile_sfx": {"archive": "@INTP_BDDIR@/@SURFEX_TEMPLATE@"},
            }
        },
        {"general": {"times": {"start": "2023-10-15T15:32:24Z"}}},
    ],
)
def test_find_initial_files(tmp_directory, parsed_config, set_surfex, set_mode, param):
    """Test load of the yml files."""
    if set_mode["suite_control"]["mode"] == "start":
        truth = f"{tmp_directory}/ELSCFTESTALBC000"
        truth_sfx = f"{tmp_directory}/ICMSHTESTINIT.sfx"
        with contextlib.suppress(KeyError):
            if "times" in param["general"]:
                truth = f"{tmp_directory}/ICMSHTEST+0003h00m00s"
                truth_sfx = f"{tmp_directory}/ICMSHTEST+0003h00m00s.sfx"
    elif set_mode["suite_control"]["mode"] == "cold_start":
        truth = f"{tmp_directory}/ELSCFTESTALBC000"
        truth_sfx = f"{tmp_directory}/ICMSHTESTINIT.sfx"
    elif set_mode["suite_control"]["mode"] == "restart":
        truth = f"{tmp_directory}/ICMSHTEST+0003h00m00s"
        truth_sfx = f"{tmp_directory}/ICMSHTEST+0003h00m00s.sfx"
        with contextlib.suppress(KeyError):
            if "initfile" in param["file_templates"]:
                truth = f"{tmp_directory}/foo"
                truth_sfx = f"{tmp_directory}/foo"

    for key in ["initfile", "initfile_sfx"]:
        with contextlib.suppress(KeyError):
            param["file_templates"][key]["archive"] = f"{tmp_directory}/foo"

    config = parsed_config
    config = config.copy(update=set_surfex)
    config = config.copy(update=set_mode)
    config = config.copy(update=param)
    if parsed_config["general"]["bogus"] == "True":
        initfile, initfile_sfx = InitialConditions(config).find_initial_files()
        assert initfile == truth
        assert initfile_sfx == truth_sfx
    else:
        with contextlib.suppress(FileNotFoundError):
            initfile, initfile_sfx = InitialConditions(config).find_initial_files()
            assert initfile == truth
            assert initfile_sfx == truth_sfx


if __name__ == "__main__":
    pytest.main()
