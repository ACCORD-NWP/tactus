#!/usr/bin/env python3
"""Unit tests for the namelist generation module."""
import os

import pytest
import tomlkit

from deode.config_parser import MAIN_CONFIG_JSON_SCHEMA, ParsedConfig
from deode.namelist import (
    InvalidNamelistKindError,
    InvalidNamelistTargetError,
    NamelistGenerator,
)


@pytest.fixture()
def config_platform():
    """Set the platform specific configuration."""
    task_configs = tomlkit.parse(
        """
        [general]
            case = "test_case"
            os_macros = ["USER", "HOME", "PWD"]
            realization = -1
            cnmexp = "HARM"
            tstep = 72
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
        """
    )
    return task_configs


@pytest.fixture()
def parsed_config(config_platform):
    return ParsedConfig(config_platform, json_schema=MAIN_CONFIG_JSON_SCHEMA)


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
    # pylint: disable=no-self-use
    """Test NamelistGenerator."""

    def test_nlgen_master(self, parsed_config, tmp_path_factory):
        """Test namelist generation for master."""
        nlgen = NamelistGenerator(parsed_config, "master")
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fort.4"
        if os.path.exists(output_file):
            os.remove(output_file)
        nlgen.generate_namelist("forecast_bdmodel_ifs", output_file)
        assert os.path.exists(output_file)

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


if __name__ == "__main__":
    pytest.main()
