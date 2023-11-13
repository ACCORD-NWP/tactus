#!/usr/bin/env python3
"""Unit tests for the initial_conditions."""

import contextlib
from pathlib import Path

import pytest
import tomlkit

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.initial_conditions import InitialConditions

WORKING_DIR = Path.cwd()


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(params=[False, True])
def parsed_config(request, tmpdir):
    """Return a raw config common to all tasks."""
    if request.param:
        for f in [
            "foo",
            "ELSCFTESTALBC000",
            "ICMSHTEST+0006h00m00s",
            "ICMSHTEST+0006h00m00s.sfx",
            "ICMSHTESTINIT.sfx",
        ]:
            Path(f"{tmpdir}/{f}").touch()

    config = ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )

    try:
        basetime = config["general.times.basetime"]
    except KeyError:
        basetime = config["general.times.start"]
    try:
        validtime = config["general.times.validtime"]
    except KeyError:
        validtime = basetime

    config_patch = tomlkit.parse(
        f"""
        [general]
            cnmexp = "TEST"
            bogus = "{request.param}"
            initfile = "{tmpdir}/@HISTORY_TEMPLATE@"
            initfile_sfx = "{tmpdir}/@SURFEX_TEMPLATE@"
        [general.times]
            basetime = "{basetime}"
            validtime = "{validtime}"
        [system]
            intp_bddir = "{tmpdir}"
            archive = "{tmpdir}"
        [platform]
            deode_home = "{WORKING_DIR}"
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
        {"general": {"deode_home": "{WORKING_DIR}"}},
        {"general": {"initfile": "foo", "initfile_sfx": "foo"}},
        {"general": {"times": {"start": "2023-10-15T15:32:24Z"}}},
    ],
)
def test_find_initial_files(tmpdir, parsed_config, set_surfex, set_mode, param):
    """Test load of the yml files."""
    if parsed_config["general"]["bogus"] == "True":
        if set_mode["suite_control"]["mode"] == "start":
            truth = f"{tmpdir}/ELSCFTESTALBC000"
            truth_sfx = f"{tmpdir}/ICMSHTESTINIT.sfx"
            if "times" in param["general"]:
                truth = f"{tmpdir}/ICMSHTEST+0006h00m00s"
                truth_sfx = f"{tmpdir}/ICMSHTEST+0006h00m00s.sfx"
        elif set_mode["suite_control"]["mode"] == "cold_start":
            truth = f"{tmpdir}/ELSCFTESTALBC000"
            truth_sfx = f"{tmpdir}/ICMSHTESTINIT.sfx"
        elif set_mode["suite_control"]["mode"] == "restart":
            truth = f"{tmpdir}/ICMSHTEST+0006h00m00s"
            truth_sfx = f"{tmpdir}/ICMSHTEST+0006h00m00s.sfx"
            if "initfile" in param["general"]:
                truth = f"{tmpdir}/foo"
                truth_sfx = f"{tmpdir}/foo"

    for key in ["initfile", "initfile_sfx"]:
        if key in param["general"]:
            param["general"][key] = f"{tmpdir}/foo"

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
