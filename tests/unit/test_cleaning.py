#!/usr/bin/env python3
"""Unit tests for the fullpos."""

import contextlib
import glob
import os
from pathlib import Path

import pytest

from tactus.cleaning import CleanDeode, wipe_ecfs
from tactus.datetime_utils import as_datetime
from tactus.derived_variables import set_times


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def basic_config(default_config):
    config = default_config
    config = config.copy(update=set_times(config))
    return config


@pytest.fixture()
def _module_mockers(module_mocker):
    def new_subprocess_check_output(infile, text):  # noqa ARG001
        return "foo"

    module_mocker.patch("subprocess.check_output", new=new_subprocess_check_output)


def test_defaults(basic_config):
    config = basic_config.copy(
        update={"cleaning": {"defaults": {"ncycle_delay": 0, "cleaning_delay": "P1D"}}}
    )
    CleanDeode(config)
    CleanDeode(config, {})
    with contextlib.suppress(RuntimeError):
        CleanDeode(config, config.get("cleaning.defaults"))


def test_check_choice1(basic_config):
    defaults = {"active": True, "cleaning_delay": "P1D"}
    cleaner = CleanDeode(basic_config, defaults)
    choices = {"test": {"ncycles_delay": 0}}
    cleaner.prep_cleaning(choices)


def test_check_choice2(basic_config):
    defaults = {"active": True, "ncycles_delay": 0}
    cleaner = CleanDeode(basic_config, defaults)
    choices = {"test": {"cleaning_delay": "P1D", "cleaning_max_delay": "P2D"}}
    cleaner.prep_cleaning(choices)


def test_cycle_length_exception(basic_config):
    config = basic_config
    cleaner = CleanDeode(config, config.get("cleaning.defaults"))
    choices = {"test": {"step": "PT27M"}}
    with contextlib.suppress(RuntimeError):
        cleaner.prep_cleaning(choices)


def test_basetime(basic_config):
    config = basic_config
    basetime = as_datetime("2024-06-13T00:00:00Z")
    cleaner = CleanDeode(config, config.get("cleaning.defaults"), basetime)
    cleaner.prep_cleaning({}, basetime)


@pytest.mark.usefixtures("_module_mockers")
def test_wipe_ecfs():
    with pytest.raises(RuntimeError, match="Error running command"):
        wipe_ecfs("foo")


def test_full_cleaning(tmpdir, basic_config):
    config = basic_config
    path = f"{tmpdir}/deode"
    os.makedirs(path, exist_ok=True)
    path2 = f"{tmpdir}/deode_remove_dir"
    os.makedirs(path2, exist_ok=True)

    for f in ["ELS", "ICMSHTEST"]:
        Path(f"{path}/{f}").touch()

    choices = {
        "dry_test": {
            "active": True,
            "dry_run": True,
            "path": path,
            "exclude": "(.*)ELS(.*)",
            "include": "(.*)",
        },
        "ecfs_test": {
            "active": True,
            "dry_run": True,
            "ecfs_prefix": "ecfoo",
            "wipe": True,
        },
        "ecflow_tests": {
            "active": True,
            "dry_run": True,
            "remove_from_scheduler": True,
        },
        "full_test": {
            "active": True,
            "dry_run": False,
            "path": path,
            "exclude": "(.*)ELS(.*)",
            "include": "(.*)",
        },
        "wipe_test": {
            "active": True,
            "dry_run": False,
            "path": path2,
            "wipe": True,
        },
    }

    # Test the actual cleaning
    cleaner = CleanDeode(config, config.get("cleaning.defaults"))
    cleaner.has_ecfs = True
    cleaner.prep_cleaning(choices)
    cleaner.clean()
    files_left = list(glob.glob(f"{path}/*"))

    assert len(files_left) == 1
    assert os.path.basename(files_left[0]) == "ELS"
    assert not os.path.isdir(path2)
