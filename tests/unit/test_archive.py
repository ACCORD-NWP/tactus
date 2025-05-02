#!/usr/bin/env python3
"""Unit tests for the archiving methods."""

import os
import sys
from pathlib import Path

import pytest

from deode.archive import Archive
from deode.derived_variables import set_times
from deode.toolbox import FDB, compute_georef


class MockFDB:
    def archive(self, a):
        pass

    def flush(self):
        pass


class PyFDB:
    FDB = MockFDB


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def basic_config(tmpdir, default_config):
    tmp1 = Path(tmpdir, "inpath")
    tmp2 = Path(tmpdir, "outpath")
    os.makedirs(tmp1, exist_ok=True)
    os.system("touch " + str(tmp1) + "/copy")  # noqa S108
    os.system("touch " + str(tmp1) + "/xtra")  # noqa S108
    os.system("touch " + str(tmp1) + "/move")  # noqa S108
    config = default_config
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
                    "fdb": {
                        "fdb_file": {
                            "active": True,
                            "inpath": str(tmp1),
                            "outpath": str(tmp2),
                            "pattern": "x*",
                        }
                    },
                }
            },
            "platform": {"archive_types": ["copy", "move", "fdb"], "unix_group": ""},
        }
    )
    return config


def test_defaults(basic_config):
    a = Archive(basic_config, "test")
    assert a.choices == basic_config["archiving.test"]


def test_method_is_included(basic_config):
    with pytest.raises(RuntimeError):
        Archive(basic_config, "test", include=["copy"])


def test_method_is_excluded(basic_config):
    with pytest.raises(RuntimeError):
        Archive(basic_config, "test", exclude=["fdb"])


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


def test_fdb(monkeypatch, basic_config):
    if "USER" not in os.environ:
        os.environ["USER"] = "foo"
    config = basic_config.copy(update={"fdb": {"grib_set": {"expver": "test"}}})
    tmp1 = basic_config["archiving.test.move.move_file.inpath"]

    os.system("touch xtra_temp1.grib")  # noqa S108
    os.system("touch xtra_temp2.grib")  # noqa S108
    a = Archive(config, "test")
    choice = a.choices["fdb"]["fdb_file"]
    output = []
    with monkeypatch.context() as mp:
        mp.setitem(sys.modules, "pyfdb", PyFDB)
        mp.setattr(os, "system", output.append)
        a.archive(choice["pattern"], choice["inpath"], choice["outpath"], "fdb")

    assert f"grib_filter temp_rules {tmp1}/xtra -o xtra_temp1.grib" == output[0]
    assert output[1].startswith("grib_set -s")
    assert "expver=test" in output[1]
    assert "georef=u15rxs" in output[1]
    assert output[1].endswith("xtra_temp1.grib xtra_temp2.grib")


def test_fdb_user_restriction(monkeypatch, basic_config):
    user = os.environ["USER"] if "USER" in os.environ else "foo"
    user = os.environ.get("USER", "foo")
    nouser = "no" + user
    config = basic_config.copy(
        update={"fdb": {"expver_restrictions": {"test": [user], "tset": [nouser]}}}
    )
    with monkeypatch.context() as mp:
        mp.setitem(sys.modules, "pyfdb", PyFDB)
        FDB(config, "").check_expver_restrictions("test")
        with pytest.raises(RuntimeError):
            FDB(config, "").check_expver_restrictions("tset")


def test_fdb_compute_georef():
    # Demo domain
    config_demo = {"xlatcen": 52.0, "xloncen": 4.9}
    # Very Precise lon and lat center
    # But we hardcode precision = 6, so this will truncate
    config_precise = {"xlatcen": 37.7749, "xloncen": -122.4194}
    georef = compute_georef(config_demo)
    georef2 = compute_georef(config_precise)
    assert georef == "u15rzd"
    assert georef2 == "9q8yyk"
