#!/usr/bin/env python3
"""Unit tests for the archiving methods."""

import os
from pathlib import Path

import pytest

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.derived_variables import set_times
from deode.tasks.archive import Archive


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def basic_config(tmpdir):
    tmp1 = Path(tmpdir, "inpath")
    tmp2 = Path(tmpdir, "outpath")
    os.makedirs(tmp1, exist_ok=True)
    os.system("touch " + str(tmp1) + "/copy")  # noqa S108
    os.system("touch " + str(tmp1) + "/xtra")  # noqa S108
    os.system("touch " + str(tmp1) + "/move")  # noqa S108
    config = ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )
    config = config.copy(update=set_times(config))
    config = config.copy(
        update={
            "archiving": {
                "test": {
                    "copy": {
                        "copy_file": {
                            "active": True,
                            "inpath": str(tmp1),
                            "outpath": str(tmp2),
                            "pattern": ["c*", "x*"],
                        }
                    },
                    "move": {
                        "move_file": {
                            "active": True,
                            "inpath": str(tmp1),
                            "outpath": str(tmp2),
                            "pattern": "m*",
                        }
                    },
                }
            },
            "platform": {"archive_types": ["copy", "move"], "unix_group": ""},
        }
    )
    return config


def test_defaults(basic_config):
    a = Archive(basic_config, "test")
    assert a.choices == basic_config["archiving.test"]


def test_copy(basic_config):
    tmp2 = basic_config["archiving.test.copy.copy_file.outpath"]

    a = Archive(basic_config, "test")
    choice = a.choices["copy"]["copy_file"]
    a.archive(choice["pattern"], choice["inpath"], choice["outpath"], "copy")

    assert os.path.isfile(f"{tmp2}/copy")
    assert os.path.isfile(f"{tmp2}/xtra")


def test_move(basic_config):
    tmp1 = basic_config["archiving.test.move.move_file.inpath"]
    tmp2 = basic_config["archiving.test.move.move_file.outpath"]

    a = Archive(basic_config, "test")
    choice = a.choices["move"]["move_file"]
    a.archive(choice["pattern"], choice["inpath"], choice["outpath"], "move")

    assert os.path.isfile(f"{tmp2}/move")
    assert not os.path.isfile(f"{tmp1}/move")
