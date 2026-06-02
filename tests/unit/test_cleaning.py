#!/usr/bin/env python3
"""Unit tests for the fullpos."""

import contextlib
import glob
import os
from pathlib import Path

import pytest

from tactus.cleaning import CleanTactus, wipe_ecfs
from tactus.datetime_utils import as_datetime
from tactus.derived_variables import set_times


@pytest.fixture(scope="module")
def basic_config(default_config):
    return default_config.copy(update=set_times(default_config))


@pytest.fixture
def _module_mockers(module_mocker):
    def new_subprocess_check_output(infile, text):
        return "foo"

    module_mocker.patch("subprocess.check_output", new=new_subprocess_check_output)


@pytest.mark.skip()
def test_defaults(basic_config):
    config = basic_config.copy(
        update={"cleaning": {"defaults": {"ncycle_delay": 0, "cleaning_delay": "P1D"}}}
    )
    CleanTactus(config)
    CleanTactus(config, {})
    with contextlib.suppress(RuntimeError):
        CleanTactus(config, config.get_as_dict("cleaning.defaults"))


@pytest.mark.skip()
def test_check_choice1(basic_config):
    defaults = {"active": True, "cleaning_delay": "P1D"}
    cleaner = CleanTactus(basic_config, defaults)
    choices = {"test": {"ncycles_delay": 0}}
    cleaner.prep_cleaning(choices)


@pytest.mark.skip()
def test_check_choice2(basic_config):
    defaults = {"active": True, "ncycles_delay": 0}
    cleaner = CleanTactus(basic_config, defaults)
    choices = {"test": {"cleaning_delay": "P1D", "cleaning_max_delay": "P2D"}}
    cleaner.prep_cleaning(choices)


@pytest.mark.skip()
def test_cycle_length_exception(basic_config):
    config = basic_config
    cleaner = CleanTactus(config, config.get_as_dict("cleaning.defaults"))
    choices = {"test": {"step": "PT27M"}}
    with contextlib.suppress(RuntimeError):
        cleaner.prep_cleaning(choices)


@pytest.mark.skip()
def test_basetime(basic_config):
    config = basic_config
    basetime = as_datetime("2024-06-13T00:00:00Z")
    cleaner = CleanTactus(config, config.get_as_dict("cleaning.defaults"), basetime)
    cleaner.prep_cleaning({}, basetime)


@pytest.mark.skip()
@pytest.mark.usefixtures("_module_mockers")
def test_wipe_ecfs():
    with pytest.raises(RuntimeError, match="Error running command"):
        wipe_ecfs("foo")


@pytest.mark.skip()
def test_full_cleaning(tmp_directory, basic_config):
    config = basic_config
    path = f"{tmp_directory}/tactus"
    os.makedirs(path, exist_ok=True)
    path2 = f"{tmp_directory}/tactus_remove_dir"
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
            "cleaning_delay": "P0D",
            "wipe": True,
        },
        "ecflow_tests": {
            "active": True,
            "dry_run": True,
            "remove_from_scheduler": True,
            "ncycles_delay": 0,
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
    cleaner = CleanTactus(config, config.get_as_dict("cleaning.defaults"))
    cleaner.has_ecfs = True
    cleaner.prep_cleaning(choices)
    cleaner.clean()
    num_files_left = [f for f in glob.glob(f"{path}/*") if os.path.isfile(f)]

    assert len(num_files_left) == 1
    assert os.path.basename(num_files_left[0]) == "ELS"
    assert not os.path.isdir(path2)
