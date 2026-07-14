#!/usr/bin/env python3
"""Unit tests for the initial_conditions."""

import contextlib
from pathlib import Path

import pytest
import tomlkit

from tactus import GeneralConstants
from tactus.derived_variables import set_times
from tactus.initial_conditions import InitialConditions
from tactus.toolbox import Platform


@pytest.fixture
def parsed_config(tmp_directory, default_config):
    """Return a raw config common to all tasks."""
    config = default_config
    config = config.copy(update=set_times(config))

    config_patch = tomlkit.parse(f"""
        [file_templates.initfile]
            archive = "@INTP_BDDIR@/@HISTORY_TEMPLATE@"
        [file_templates.initfile_sfx]
            archive = "@INTP_BDDIR@/@SURFEX_TEMPLATE@"
        [general]
            cnmexp = "TEST"
        [system]
            intp_bddir = "{tmp_directory}"
            archive = "{tmp_directory}"
        [platform]
            tactus_home = "{GeneralConstants.PACKAGE_DIRECTORY}"
        """)

    config = config.copy(update=config_patch)
    platform = Platform(config)
    for f in [
        platform.substitute(config["file_templates"]["pertana"]["archive"]),
        platform.substitute(config["file_templates"]["pertsurf"]["archive"]),
        "foo",
        "ELSCFTESTALBC000",
        "ICMSHTEST+0003h00m00s",
        "ICMSHTEST+0003h00m00s.sfx",
        "ICMSHTESTINIT.sfx",
    ]:
        Path(f"{tmp_directory}/{f}").touch()

    return config


@pytest.fixture(params=["start", "cold_start", "restart"])
def set_mode(request):
    return {"suite_control": {"mode": request.param}}


@pytest.fixture(
    params=[
        {
            "general": {"tactus_home": str(GeneralConstants.PACKAGE_DIRECTORY)},
        },
        {
            "file_templates": {
                "initfile": {"archive": "@INTP_BDDIR@/@HISTORY_TEMPLATE@"},
                "initfile_sfx": {"archive": "@INTP_BDDIR@/@SURFEX_TEMPLATE@"},
            },
        },
    ]
)
def truth_from_set_mode(set_mode, tmp_directory, request):
    if (
        set_mode["suite_control"]["mode"] == "start"
        or set_mode["suite_control"]["mode"] == "cold_start"
    ):
        truth = f"{tmp_directory}/ELSCFTESTALBC000"
        truth_sfx = f"{tmp_directory}/ICMSHTESTINIT.sfx"
    elif set_mode["suite_control"]["mode"] == "restart":
        truth = f"{tmp_directory}/ICMSHTEST+0003h00m00s"
        truth_sfx = f"{tmp_directory}/ICMSHTEST+0003h00m00s.sfx"
        with contextlib.suppress(KeyError):
            if "initfile" in request.param["file_templates"]:
                truth = f"{tmp_directory}/foo"
                truth_sfx = f"{tmp_directory}/foo"

    return [truth, truth_sfx, request.param]


def test_find_initial_files_pertana(
    tmp_directory, parsed_config, set_mode, truth_from_set_mode
):
    """Test input to the Pertana task."""
    truth = truth_from_set_mode[0]

    for key in ["initfile"]:
        with contextlib.suppress(KeyError):
            truth_from_set_mode[2]["file_templates"][key]["archive"] = (
                f"{tmp_directory}/foo"
            )

    config = parsed_config
    config = config.copy(update=set_mode)
    config = config.copy(update=truth_from_set_mode[2])
    initfile, _, status = InitialConditions(config).find_initial_files("Pertana", False)
    assert initfile == truth


def test_find_initial_files_pertsurf(
    tmp_directory, parsed_config, set_mode, truth_from_set_mode
):
    """Test input to the Pertsurf task."""
    truth_sfx = truth_from_set_mode[1]

    for key in ["initfile_sfx"]:
        with contextlib.suppress(KeyError):
            truth_from_set_mode[2]["file_templates"][key]["archive"] = (
                f"{tmp_directory}/foo"
            )

    config = parsed_config
    config = config.copy(update=set_mode)
    config = config.copy(update=truth_from_set_mode[2])
    _, initfile_sfx, status = InitialConditions(config).find_initial_files(
        "Pertsurf", False
    )
    assert initfile_sfx == truth_sfx


@pytest.mark.parametrize(
    "param",
    [
        {
            "general": {"tactus_home": str(GeneralConstants.PACKAGE_DIRECTORY)},
        },
        {
            "perturbations": {
                "pertana": {"active": True},
                "pertsurf": {"active": True},
            },
        },
    ],
)
def test_find_initial_files_forecast(
    tmp_directory, parsed_config, set_mode, truth_from_set_mode, param
):
    """Test input to the Forecast task."""
    platform = Platform(parsed_config)

    truth = truth_from_set_mode[0]
    truth_sfx = truth_from_set_mode[1]

    with contextlib.suppress(KeyError):
        if param["perturbations"]["pertana"]["active"]:
            truth = platform.substitute(
                f"{tmp_directory}/{parsed_config['file_templates']['pertana']['archive']}"
            )
        if param["perturbations"]["pertsurf"]["active"]:
            truth_sfx = platform.substitute(
                f"{tmp_directory}/{parsed_config['file_templates']['pertsurf']['archive']}"
            )

    for key in ["initfile", "initfile_sfx"]:
        with contextlib.suppress(KeyError):
            truth_from_set_mode[2]["file_templates"][key]["archive"] = (
                f"{tmp_directory}/foo"
            )

    config = parsed_config.copy(update=set_mode)
    config = config.copy(update=truth_from_set_mode[2])
    config = config.copy(update=param)
    initfile, initfile_sfx, status = InitialConditions(config).find_initial_files(
        "Forecast", False
    )
    assert initfile == truth
    assert initfile_sfx == truth_sfx
    assert status


if __name__ == "__main__":
    pytest.main()
