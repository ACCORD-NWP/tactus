#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""

import os
from pathlib import Path

import pytest
import tomlkit

from tactus.config_parser import ConfigParserDefaults, ParsedConfig
from tactus.logs import logger
from tactus.toolbox import FileManager

logger.enable("tactus")


@pytest.fixture
def config_platform(tmp_directory):
    """Set the platform specific configuration."""
    return tomlkit.parse(
        """
        [general]
            case = "mytest"
            cnmexp = "DEOD"
        [macros.select.default]
            os_macros = ["USER", "HOME"]
            group_macros = ["platform","system"]
            gen_macros = ["general.cnmexp",
                          { domain = "domain.name" }]
        [domain]
            name = "MYDOMAIN"
            tstep = 60
        [pgd]
            ond_decade = true
        [general.times]
            basetime = "2000-01-01T00:00:00Z"
            validtime = "2000-01-02T00:00:00Z"
            list = ["2000-01-01T00:00:00Z"]"""
        + f"""
        [system]
            bindir = "{tmp_directory}/bindir"
            archive = "{tmp_directory}/archive/@YYYY@/@MM@/@DD@/@HH@"
        [platform]
        """
    )


@pytest.fixture
def parsed_config_with_paths(config_platform):
    return ParsedConfig(
        config_platform,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


class TestFileManager:
    """Test FileManager."""

    def test_non_existing_provider(self, parsed_config_with_paths):
        fmanager = FileManager(parsed_config_with_paths)
        with pytest.raises(NotImplementedError):
            provider, resource = fmanager.get_input(
                "foo", "bar", provider_id="does_not_exist"
            )

    def test_input_files(self, tmp_directory: str, parsed_config_with_paths):
        """Test input files."""
        tmp_directory = Path(tmp_directory)
        (tmp_directory / "archive/2000/01/01/00").mkdir(parents=True, exist_ok=True)
        (tmp_directory / "archive/2000/01/01/00/ICMSHDEOD+0024").touch()
        fmanager = FileManager(parsed_config_with_paths)
        provider, resource = fmanager.get_input(
            "@ARCHIVE@/ICMSH@CNMEXP@+@LLLL@",
            str(tmp_directory / "ICMSH@CNMEXP@INIT"),
            check_archive=False,
        )
        logger.debug("identifier={}", provider.identifier)
        logger.info(provider.identifier)
        assert provider.identifier == str(
            tmp_directory / "archive/2000/01/01/00/ICMSHDEOD+0024"
        )
        assert resource.identifier == str(tmp_directory / "ICMSHDEODINIT")

        (tmp_directory / "bindir").mkdir(parents=True, exist_ok=True)
        (tmp_directory / "bindir/MASTERODB").touch()
        provider, resource = fmanager.get_input(
            "@BINDIR@/MASTERODB",
            str(tmp_directory / "MASTERODB"),
        )
        assert provider.identifier == str(tmp_directory / "bindir/MASTERODB")
        assert resource.identifier == str(tmp_directory / "MASTERODB")
        assert (tmp_directory / "MASTERODB").exists()
        (tmp_directory / "MASTERODB").unlink()
        (tmp_directory / "bindir/MASTERODB").unlink()
        (tmp_directory / "archive/2000/01/01/00/ICMSHDEOD+0024").unlink()
        (tmp_directory / "bindir").rmdir()

        res_dict = {
            "input": {
                "/dev/null": {
                    "destination": str(tmp_directory / "test"),
                    "provider_id": "symlink",
                }
            }
        }
        fmanager.set_resources_from_dict(res_dict)

    def test_output_files(self, tmp_directory: str, parsed_config_with_paths):
        """Test output files."""
        tmp_directory = Path(tmp_directory)
        fmanager = FileManager(parsed_config_with_paths)
        (tmp_directory / "archive/2000/01/01/00").mkdir(parents=True, exist_ok=True)
        (tmp_directory / "ICMSHDEOD+0024").touch()
        provider, aprovider, resource = fmanager.get_output(
            str(tmp_directory / "ICMSH@CNMEXP@+@LLLL@"),
            "@ARCHIVE@/OUT_ICMSH@CNMEXP@+@LLLL@",
            archive=False,
        )
        assert resource.identifier == str(tmp_directory / "ICMSHDEOD+0024")
        assert provider.identifier == str(
            tmp_directory / "archive/2000/01/01/00/OUT_ICMSHDEOD+0024"
        )
        assert (tmp_directory / "archive/2000/01/01/00/OUT_ICMSHDEOD+0024").exists()
        assert aprovider is None
        (tmp_directory / "archive/2000/01/01/00/OUT_ICMSHDEOD+0024").unlink()

    def test_case_insensitive(self, parsed_config_with_paths):
        """Test input files."""
        fmanager = FileManager(parsed_config_with_paths)
        test = fmanager.platform.sub_value("t/@ARCHIVE@/a@T@b", "ARCHIVE", "found")
        assert test == "t/found/a@T@b"
        test = fmanager.platform.sub_value("t/@ARCHIVE@/a@T@b", "archive", "found")
        assert test == "t/found/a@T@b"
        test = fmanager.platform.sub_value("@TA@t/@ARCHIVE@/a@T@", "archive", "found")
        assert test == "@TA@t/found/a@T@"

    def test_substitution(self, parsed_config_with_paths):
        """Test input files."""
        config = parsed_config_with_paths
        platform_value = "platform_value"
        os.environ["FILE_TEST"] = "test"
        test_config = {
            "general": {
                "cnmexp": "DEOD",
                "times": {
                    "basetime": "2023-02-15T01:30:00Z",
                    "validtime": "2023-02-15T03:30:00Z",
                },
            },
            "macros": {
                "select": {
                    "default": {
                        "os_macros": ["FILE_TEST"],
                        "groups_macros": ["platform"],
                        "gen_macros": ["general.cnmexp", {"domain": "domain.name"}],
                    },
                },
            },
            "domain": {"name": "DOMAIN"},
            "system": {"climdir": "my_dir"},
            "platform": {"test": platform_value},
        }
        config = config.copy(update=test_config)
        fmanager = FileManager(config)
        istring = "@FILE_TEST@:@NOT_FOUND@:@TeST@:@CLimDiR@:@domain@:@cnmexp@:@YYYY@:@MM@:@DD@:@HH@:@mm@:@LLLL@"
        ostring = (
            f"test:@NOT_FOUND@:{platform_value}:my_dir:DOMAIN:DEOD:2023:02:15:01:30:0002"
        )
        test = fmanager.platform.substitute(istring)
        assert test == ostring

    def test_input_data_iterator(self, parsed_config_with_paths):
        prev_cwd = Path.cwd()
        config = parsed_config_with_paths
        fmanager = FileManager(config)
        input_dir = Path("/tmp/test_in")  # noqa S108
        output_dir = Path("/tmp/test_out")  # noqa S108
        infile = str(input_dir / "test")

        input_dir.mkdir(parents=True, exist_ok=True)
        output_dir.mkdir(parents=True, exist_ok=True)
        (input_dir / "test").touch()
        try:
            os.chdir(output_dir)

            truth = {
                "test_list": {"path": str(input_dir), "files": ["test"]},
                "test_dict": {"path": str(input_dir), "files": {"test_out": "test"}},
            }

            fmanager.input_data_iterator(truth)
        finally:
            os.chdir(prev_cwd)

        for outfile in [str(output_dir / "test"), str(output_dir / "test_out")]:
            assert Path(outfile).exists()
            link = os.readlink(outfile)
            assert link == infile
