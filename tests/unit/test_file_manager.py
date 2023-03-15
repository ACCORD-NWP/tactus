#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import logging
import os

import pytest
import tomlkit

from deode.config_parser import ParsedConfig
from deode.toolbox import FileManager


@pytest.fixture()
def config_platform():
    """Set the platform specific configuration."""
    task_configs = tomlkit.parse(
        """
        [general]
            case = "mytest"
            os_macros = ["USER", "HOME"]
            realization = -1
            cnmexp = "DEOD"
            tstep = 60
            loglevel = "DEBUG"
        [domain]
            name = "MYDOMAIN"
        [general.times]
            basetime = "2000-01-01T00:0:00Z"
            validtime = "2000-01-02T00:00:00Z"
            list = ["2000-01-01T00:00:00Z"]
        [system]
            bindir = "/tmp/bindir"
            archive = "/tmp/archive/@YYYY@/@MM@/@DD@/@HH@"
        [platform]
        """
    )
    return task_configs


@pytest.fixture()
def parsed_config_with_paths(config_platform):
    return ParsedConfig.parse_obj(config_platform)


class TestFileManager:
    # pylint: disable=no-self-use
    """Test FileManager."""

    def test_input_files(self, parsed_config_with_paths):
        """Test input files."""
        fmanager = FileManager(parsed_config_with_paths)
        provider, resource = fmanager.get_input(
            "@ARCHIVE@/ICMSH@CNMEXP@+@LLLL@",
            "/tmp/ICMSH@CNMEXP@INIT",  # noqa S108
            check_archive=True,
        )
        logging.debug("identifier=%s", provider.identifier)
        assert provider.identifier == "ectmp:/2000/01/01/00/ICMSHDEOD+0024"  # noqa S108
        assert resource.identifier == "/tmp/ICMSHDEODINIT"  # noqa S108

        os.makedirs("/tmp/bindir", exist_ok=True)  # noqa S108
        os.system("touch /tmp/bindir/MASTERODB")  # noqa S108, S605, S607
        provider, resource = fmanager.get_input(
            "@BINDIR@/MASTERODB", "/tmp/MASTERODB"  # noqa S108, E501
        )
        assert provider.identifier == "/tmp/bindir/MASTERODB"  # noqa S108
        assert resource.identifier == "/tmp/MASTERODB"  # noqa S108
        assert os.path.exists("/tmp/MASTERODB")  # noqa S108
        os.remove("/tmp/MASTERODB")  # noqa S108
        os.remove("/tmp/bindir/MASTERODB")  # noqa S108
        os.rmdir("/tmp/bindir")  # noqa S108

        res_dict = {
            "input": {
                "/dev/null": {
                    "destination": "/tmp/test",  # noqa S108, E501
                    "provider_id": "symlink",
                }
            }
        }
        fmanager.set_resources_from_dict(res_dict)

    def test_output_files(self, parsed_config_with_paths):
        """Test input files."""
        fmanager = FileManager(parsed_config_with_paths)
        os.makedirs("/tmp/archive/2000/01/01/00/", exist_ok=True)  # noqa S108
        os.system("touch /tmp/ICMSHDEOD+0024")  # noqa S108
        provider, aprovider, resource = fmanager.get_output(
            "/tmp/ICMSH@CNMEXP@+@LLLL@",  # noqa S108
            "@ARCHIVE@/OUT_ICMSH@CNMEXP@+@LLLL@",
            archive=True,
        )
        print(provider)
        print(aprovider)
        print(resource)
        assert resource.identifier == "/tmp/ICMSHDEOD+0024"  # noqa S108
        assert (
            provider.identifier
            == "/tmp/archive/2000/01/01/00/OUT_ICMSHDEOD+0024"  # noqa S108, E501
        )
        assert os.path.exists(
            "/tmp/archive/2000/01/01/00/OUT_ICMSHDEOD+0024"  # noqa S108, E501
        )
        assert (
            aprovider.identifier == "ectmp:/2000/01/01/00/OUT_ICMSHDEOD+0024"
        )  # noqa S108, E501
        os.remove("/tmp/archive/2000/01/01/00/OUT_ICMSHDEOD+0024")  # noqa S108

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
        test_config = {
            "general": {
                "cnmexp": "DEOD",
                "times": {
                    "basetime": "2023-02-15T01:30:00Z",
                    "validtime": "2023-02-15T03:30:00Z",
                },
            },
            "domain": {"name": "DOMAIN"},
            "system": {"climdir": "my_dir"},
            "platform": {"test": platform_value},
        }
        config = config.copy(update=test_config)
        fmanager = FileManager(config)
        istring = "@TeST@:@CLimDiR@:@domain@:@cnmexp@:@YYYY@:@MM@:@DD@:@HH@:@mm@:@LLLL@"
        ostring = f"{platform_value}:my_dir:DOMAIN:DEOD:2023:02:15:01:30:0002"
        test = fmanager.platform.substitute(istring)
        assert test == ostring
