#!/usr/bin/env python3
"""Unit tests for the namelist generation module."""

import os
from pathlib import Path
from unittest.mock import patch

import pytest
import tomli
import tomlkit

from tactus.config_parser import ConfigParserDefaults, ParsedConfig
from tactus.namelist import (
    InvalidNamelistTargetError,
    NamelistGenerator,
    NamelistIntegrator,
    _resolve_namelist_path,
)


@pytest.fixture
def config_platform():
    """Set the platform specific configuration."""
    return tomli.loads(
        """
        [boundaries]
            bdmodel = "ifs"
        [general]
            case = "test_case"
            os_macros = ["USER", "HOME", "PWD"]
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
        """
    )


@pytest.fixture
def parsed_config(config_platform):
    return ParsedConfig(
        config_platform, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )


@pytest.fixture(params=["pgd", "prep", "forecast"])
def _nlgen_surfex(parsed_config, tmp_directory, request):
    """Test namelist generation for surfex."""
    nam_type = request.param
    nlgen = NamelistGenerator(parsed_config, "surfex")
    output_file = f"{tmp_directory}/EXSEG1.nam"
    if os.path.exists(output_file):
        os.remove(output_file)
    nlgen.load(nam_type)
    nlres = nlgen.assemble_namelist(nam_type)
    assert int(nlres["NAM_IO_OFFLINE"]["NHALO"]) == 0
    if nam_type == "pgd":
        assert int(nlres["NAM_CONF_PROJ_GRID"]["NIMAX"]) == 89


class TestNamelistGenerator:
    """Test NamelistGenerator."""

    def test_nlgen_master(self, parsed_config, tmp_directory):
        """Test namelist generation for master."""
        config_patch = tomlkit.parse(
            """
        [namelist_update.master.all_targets]
            namct0 = { bar = "foo" }
        [namelist_update.master.forecast]
            namct0 = { foo = "bar" }
        """
        )
        config = parsed_config.copy(update=config_patch)
        nlgen = NamelistGenerator(config, "master")
        output_file = f"{tmp_directory}/fort.4"
        nlgen.generate_namelist("forecast", output_file)
        assert os.path.exists(output_file)
        nl = NamelistIntegrator(config).ftn2dict(output_file)
        assert nl["NAMCT0"]["FOO"] == "bar"
        assert nl["NAMCT0"]["BAR"] == "foo"

    def test_nlgen_master_from_static(self, parsed_config, tmp_directory):

        # Create the static namelist
        config_patch = tomlkit.parse(
            """
        [general]
            accept_static_namelists = false
        [namelist_update.master.forecast.namct0]
            larome = false
        """
        )
        config = parsed_config.copy(update=config_patch)
        nlgen = NamelistGenerator(config, "master")
        output_file = f"{tmp_directory}/namelist_master_forecast"
        nlgen.generate_namelist("forecast", output_file)

        # Create the new namelist from the static namelist
        # and modify larome
        config_patch = tomlkit.parse(
            f"""
        [general]
            accept_static_namelists = true
        [namelist_update.master.forecast.namct0]
            larome = true
        [system]
          namelists = "{tmp_directory}"
        """
        )
        config = parsed_config.copy(update=config_patch)
        nlgen = NamelistGenerator(config, "master")
        output_file = f"{tmp_directory}/output_from_static_namelist"
        nlgen.generate_namelist("forecast", output_file)
        nl = NamelistIntegrator(config).ftn2dict(output_file)
        assert nl["NAMCT0"]["LAROME"]

    @pytest.mark.usefixtures("_nlgen_surfex")
    def test_nlgen_surfex(self):
        """Test namelist generation for surfex."""

    def test_nlgen_invalid_target(self, parsed_config, tmp_directory):
        """Test namelist generation for non-existing target."""
        nlgen = NamelistGenerator(parsed_config, "master")
        output_file = f"{tmp_directory}/fort.4"
        with pytest.raises(InvalidNamelistTargetError):
            nlgen.generate_namelist("analysis", output_file)

    def test_nlgen_timesteps(self, tmp_directory, default_config):
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
        output_file = f"{tmp_directory}/fort.4"
        if os.path.exists(output_file):
            os.remove(output_file)
        nlgen.generate_namelist("forecast", output_file)

        # Check if output exists and is as expected
        assert os.path.exists(output_file)
        nl = NamelistIntegrator(task_config).ftn2dict(output_file)

        assert nl["NAMCT0"]["NHISTS"] == [5, 0, 96, 192, 240, 288]
        assert nl["NAMCT0"]["NPOSTS"] == [7, 0, 48, 96, 144, 192, 240, 288]


class TestResolveNamelistPath:
    """Tests for _resolve_namelist_path."""

    def test_returns_config_path_when_found(self, tmp_path):
        """ConfigPaths.path_from_subpath succeeds — its result is returned directly."""
        expected = tmp_path / "namelists" / "master.yml"
        expected.parent.mkdir(parents=True)
        expected.touch()

        with patch(
            "tactus.namelist.ConfigPaths.path_from_subpath", return_value=expected
        ):
            result = _resolve_namelist_path("master.yml")

        assert result == expected

    def test_falls_back_to_package_path_when_config_raises(self, tmp_path):
        """RuntimeError from ConfigPaths triggers fallback to resolve_path_relative_to_package."""
        fallback = tmp_path / "package" / "master.yml"
        fallback.parent.mkdir(parents=True)
        fallback.touch()

        with (
            patch(
                "tactus.namelist.ConfigPaths.path_from_subpath",
                side_effect=RuntimeError("not found"),
            ),
            patch(
                "tactus.namelist.resolve_path_relative_to_package",
                return_value=fallback,
            ),
        ):
            result = _resolve_namelist_path("master.yml")

        assert result == fallback

    def test_accepts_path_object_as_input(self, tmp_path):
        """A Path object is accepted in addition to a plain string."""
        expected = tmp_path / "x.yml"
        expected.touch()

        with patch(
            "tactus.namelist.ConfigPaths.path_from_subpath", return_value=expected
        ):
            result = _resolve_namelist_path(Path("x.yml"))

        assert result == expected

    def test_propagates_error_when_fallback_also_fails(self):
        """FileNotFoundError from the fallback is not swallowed."""
        with (
            patch(
                "tactus.namelist.ConfigPaths.path_from_subpath",
                side_effect=RuntimeError("not found"),
            ),
            patch(
                "tactus.namelist.resolve_path_relative_to_package",
                side_effect=FileNotFoundError("not in package either"),
            ),
            pytest.raises(FileNotFoundError),
        ):
            _resolve_namelist_path("nonexistent.yml")


if __name__ == "__main__":
    pytest.main()
