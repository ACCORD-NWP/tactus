#!/usr/bin/env python3
"""Unit tests for the fullpos."""

import glob
import os
from datetime import datetime, timedelta
from pathlib import Path

import pytest
import tomlkit

from deode.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from deode.datetime_utils import as_timedelta
from deode.tasks.clean_old_data import CleanScratchData


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def base_raw_config():
    """Return a raw config common to all tasks."""
    config = BasicConfig.from_file(ConfigParserDefaults.CONFIG_DIRECTORY / "config.toml")
    return config


@pytest.fixture(scope="module")
def parsed_config(base_raw_config, tmp_path_factory):
    """Return a raw config common to tasks."""
    config = ParsedConfig(
        base_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )

    config_patch = tomlkit.parse(
        f"""
        [system]
            wrk = "{tmp_path_factory.getbasetemp().as_posix()}"

        """
    )

    config = config.copy(update=config_patch)
    return config


def test_remove_old(tmpdir, parsed_config):
    dir_old = f"{tmpdir}/clean/dir_old"
    os.makedirs(dir_old)
    file_old = f"{dir_old}/file_old"
    Path(file_old).touch()
    timestamp_old = (datetime.now() - timedelta(days=3)).timestamp()
    os.utime(dir_old, (timestamp_old, timestamp_old))
    os.utime(file_old, (timestamp_old, timestamp_old))

    dir_new = f"{tmpdir}/clean/dir_new"
    os.makedirs(dir_new)
    file_new = f"{dir_new}/file_new"
    Path(file_new).touch()
    timestamp_new = (datetime.now() - timedelta(days=1)).timestamp()
    os.utime(dir_new, (timestamp_new, timestamp_new))
    os.utime(file_new, (timestamp_new, timestamp_new))

    config = parsed_config
    cleanolddata = CleanScratchData(config)
    dic_old_file = cleanolddata.get_old(
        os.path.join(tmpdir, "clean"),
        cleanolddata.cutoff(as_timedelta("P2D")),
        return_files=True,
    )
    dic_old_dir = cleanolddata.get_old(
        os.path.join(tmpdir, "clean"), cleanolddata.cutoff(as_timedelta("P2D"))
    )
    cleanolddata.remove_dic(dic_old_file, files=True)
    left_files = list(glob.glob(f"{tmpdir}/clean/*/*"))
    assert len(left_files) == 1
    assert left_files[0] == f"{tmpdir}/clean/dir_new/file_new"
    cleanolddata.remove_dic(dic_old_dir)
    left_dir = list(glob.glob(f"{tmpdir}/clean/*/"))
    assert len(left_dir) == 1
    assert left_dir[0] == f"{tmpdir}/clean/dir_new/"
