#!/usr/bin/env python3
"""Unit tests for the namelist generation module."""
import os

import pytest
import tomli
import tomlkit

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.namelist import (
    InvalidNamelistKindError,
    InvalidNamelistTargetError,
    NamelistGenerator,
    NamelistIntegrator,
)


@pytest.fixture()
def config_platform():
    """Set the platform specific configuration."""
    task_configs = tomli.loads(
        """
        [boundaries]
            bdmodel = "ifs"
        [general]
            case = "test_case"
            realization = ""
            cnmexp = "HARM"
            bdint = "PT3H"
            cycle = "CY46h1"
            accept_static_namelists = false
        [general.times]
            basetime = "2000-01-01T00:00:00Z"
            validtime = "2000-01-02T00:00:00Z"
            list = ["2000-01-01T00:00:00Z"]
        [platform]
            foo = "bar"
        [system]
            hei = "hopp"
        [domain]
            name = "DEMO_100_2500m"
            nimax = 89
            njmax = 109
            ilone = 11
            ilate = 11
            gridtype = "linear"
            tstep = 72
        [macros.select.default]
            gen_macros = ["boundaries.bdmodel"]
            group_macros = ["platform", "system"]
            os_macros = ["USER", "HOME", "PWD"]
        [namelist_update.master.all_targets]
            namct0 = { bar = "foo" }
        [namelist_update.master.forecast]
            namct0 = { foo = "bar" }
        """
    )
    return task_configs


@pytest.fixture()
def parsed_config(config_platform):
    return ParsedConfig(
        config_platform, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )


@pytest.fixture(params=["pgd", "prep", "forecast"])
def _nlgen_surfex(parsed_config, tmp_path_factory, request):
    """Test namelist generation for surfex."""
    nam_type = request.param
    nlgen = NamelistGenerator(parsed_config, "surfex")
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/EXSEG1.nam"
    if os.path.exists(output_file):
        os.remove(output_file)
    nlgen.load(nam_type)
    nlres = nlgen.assemble_namelist(nam_type)
    assert int(nlres["NAM_IO_OFFLINE"]["NHALO"]) == 0
    if nam_type == "pgd":
        assert int(nlres["NAM_CONF_PROJ_GRID"]["NIMAX"]) == 89


class TestNamelistGenerator:
    """Test NamelistGenerator."""

    def test_nlgen_master(self, parsed_config, tmp_path_factory):
        """Test namelist generation for master."""
        nlgen = NamelistGenerator(parsed_config, "master")
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fort.4"
        if os.path.exists(output_file):
            os.remove(output_file)
        nlgen.generate_namelist("forecast", output_file)
        assert os.path.exists(output_file)
        nl = NamelistIntegrator(parsed_config).ftn2dict(output_file)
        assert nl["NAMCT0"]["FOO"] == "bar"
        assert nl["NAMCT0"]["BAR"] == "foo"

    @pytest.mark.usefixtures("_nlgen_surfex")
    def test_nlgen_surfex(self):
        """Test namelist generation for surfex."""

    def test_nlgen_invalid_type(self, parsed_config):
        """Test namelist generation for non-existing kind."""
        with pytest.raises(InvalidNamelistKindError):
            _ = NamelistGenerator(parsed_config, "slave")

    def test_nlgen_invalid_target(self, parsed_config, tmp_path_factory):
        """Test namelist generation for non-existing target."""
        nlgen = NamelistGenerator(parsed_config, "master")
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fort.4"
        with pytest.raises(InvalidNamelistTargetError):
            nlgen.generate_namelist("analysis", output_file)

    def test_nlgen_timesteps(self, tmp_path_factory, default_config):
        # basic config file from config.toml
        task_config = default_config

        # modify time intervals
        config_patch = tomlkit.parse(
            """
        [general.output_settings]
            history = ["PT0H:PT4H:PT2H", "PT4H:PT6H:PT1H"]
            fullpos = "PT1H"
        """
        )
        task_config = task_config.copy(update=config_patch)

        # generate and write namelist
        nlgen = NamelistGenerator(task_config, "master")
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fort.4"
        if os.path.exists(output_file):
            os.remove(output_file)
        nlgen.generate_namelist("forecast", output_file)

        # Check if output exists and is as expected
        assert os.path.exists(output_file)
        nl = NamelistIntegrator(task_config).ftn2dict(output_file)

        assert nl["NAMCT0"]["NHISTS"] == [5, 0, 96, 192, 240, 288]
        assert nl["NAMCT0"]["NPOSTS"] == [7, 0, 48, 96, 144, 192, 240, 288]


if __name__ == "__main__":
    pytest.main()
