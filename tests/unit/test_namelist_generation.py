#!/usr/bin/env python3
"""Unit tests for the namelist generation module."""
# TODO: import logging
import os

import pytest
import tomlkit

from deode.config_parser import ParsedConfig
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
            loglevel = "DEBUG"
            case = "test_case"
            os_macros = ["USER", "HOME", "PWD"]
            realization = -1
            cnmexp = "HARM"
            tstep = 72
        [general.times]
            basetime = "2000-01-01T00:0:00Z"
            validtime = "2000-01-02T00:00:00Z"
            list = ["2000-01-01T00:00:00Z"]
        [platform]
            foo = "bar"
        [system]
            hei = "hopp"
        [domain]
            name = "DEMO_100_2500m"
        """
    )
    return task_configs


@pytest.fixture()
def parsed_config(config_platform):
    return ParsedConfig.parse_obj(config_platform)


class TestNamelistGenerator:
    # pylint: disable=no-self-use
    """Test NamelistGenerator."""

    def test_nlgen_master(self, parsed_config, tmp_path_factory):
        """Test namelist generation for master."""
        nlgen = NamelistGenerator(parsed_config, "master")
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fort.4"
        if os.path.exists(output_file):
            os.remove(output_file)
        nlgen.generate_namelist("forecast", output_file)
        assert os.path.exists(output_file)

    def test_nlgen_surfex(self, parsed_config, tmp_path_factory):
        """Test namelist generation for surfex."""
        nlgen = NamelistGenerator(parsed_config, "surfex")
        output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/EXSEG1.nam"
        if os.path.exists(output_file):
            os.remove(output_file)
        nlgen.generate_namelist("forecast", output_file)
        assert os.path.exists(output_file)

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
