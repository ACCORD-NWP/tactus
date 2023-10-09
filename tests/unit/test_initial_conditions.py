#!/usr/bin/env python3
"""Unit tests for the initial_conditions."""

import contextlib
import os
from pathlib import Path

import pytest
import tomlkit

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.initial_conditions import InitialConditions

WORKING_DIR = Path.cwd()


@pytest.fixture(scope="session")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module", params=[False, True])
def parsed_config(request, tmpdir):
    """Return a raw config common to all tasks."""
    if request.param:
        for f in [
            "foo",
            "ELSCFTESTALBC000",
            "ICMSHTEST+0006:00:00",
            "ICMSHTEST+0006:00:00.sfx",
            "ICMSHTESTINIT.sfx",
        ]:
            Path(f"{tmpdir}/{f}").touch()

    config = ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH, json_schema={}
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


@pytest.mark.parametrize(
    "param",
    [
        {"general": {"deode_home": "{WORKING_DIR}"}},
        {"general": {"surfex": False}},
        {"suite_control": {"cold_start": True}},
        {"general": {"surfex": True}},
        {"general": {"surfex": False}, "suite_control": {"cold_start": True}},
        {"general": {"initfile": "foo", "initfile_sfx": "foo"}},
    ],
)
def test_find_initial_files(parsed_config, tmpdir, param):
    """Test load of the yml files."""
    config = parsed_config
    config = config.copy(update=param)
    prev_cwd = Path.cwd()
    with contextlib.suppress(FileNotFoundError):
        os.chdir(tmpdir)
        InitialConditions(config).find_initial_files()
    os.chdir(prev_cwd)


if __name__ == "__main__":
    pytest.main()
