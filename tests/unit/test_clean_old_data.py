#!/usr/bin/env python3
"""Unit tests for the fullpos."""

import glob
import os
from datetime import datetime, timedelta
from pathlib import Path

import pytest
import tomlkit

from tactus.datetime_utils import as_timedelta
from tactus.tasks.clean_old_data import CleanScratchData


@pytest.fixture(name="parsed_config", scope="module")
def fixture_parsed_config(tmp_directory, default_config):
    """Return a raw config common to tasks."""
    config = default_config

    config_patch = tomlkit.parse(
        f"""
        [system]
            wrk = "{tmp_directory}"

        """
    )

    config = config.copy(update=config_patch)
    return config


def test_remove_old(tmp_directory, parsed_config):
    # Prepare dir with old files
    dir_old = Path(f"{tmp_directory}/clean/dir_old")
    dir_old.mkdir(parents=True)
    file_old = dir_old / "file_old"
    file_old.touch()
    timestamp_old = (datetime.now() - timedelta(days=3)).timestamp()
    os.utime(dir_old, (timestamp_old, timestamp_old))
    os.utime(file_old, (timestamp_old, timestamp_old))

    # Prepare dir with new files
    dir_new = Path(f"{tmp_directory}/clean/dir_new")
    dir_new.mkdir(parents=True)
    file_new = dir_new / "file_new"
    file_new.touch()
    timestamp_new = (datetime.now() - timedelta(days=1)).timestamp()
    os.utime(dir_new, (timestamp_new, timestamp_new))
    os.utime(file_new, (timestamp_new, timestamp_new))

    cleanolddata = CleanScratchData(parsed_config)
    dic_old_file = cleanolddata.get_old(
        os.path.join(tmp_directory, "clean"),
        "/([^/]+)/([^/]+)",
        cleanolddata.cutoff(as_timedelta("P2D")),
    )
    dic_old_dir = cleanolddata.get_old(
        os.path.join(tmp_directory, "clean"),
        "/([^/]+)",
        cleanolddata.cutoff(as_timedelta("P2D")),
    )
    cleanolddata.remove_list(dic_old_file, files=True)
    left_files = list(glob.glob(f"{tmp_directory}/clean/*/*"))
    assert len(left_files) == 1
    assert Path(left_files[0]) == file_new
    cleanolddata.remove_list(dic_old_dir)
    left_dir = list(glob.glob(f"{tmp_directory}/clean/*/"))
    assert len(left_dir) == 1
    assert Path(left_dir[0]) == dir_new
