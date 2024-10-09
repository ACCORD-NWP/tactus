#!/usr/bin/env python3
"""Unit tests for the eccodes_path settings."""
import contextlib
import os
from pathlib import Path, PosixPath

import pytest
import tomlkit

from deode.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from deode.derived_variables import set_times
from deode.tasks.base import Task

WORKING_DIR = Path.cwd()
DEODE_DEFS = ConfigParserDefaults.DATA_DIRECTORY / "eccodes/definitions"


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def task(tmpdir):
    raw_config = BasicConfig.from_file(
        ConfigParserDefaults.CONFIG_DIRECTORY / "config.toml"
    )
    config = ParsedConfig(
        raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )
    config = config.copy(update=set_times(config))

    config_patch = tomlkit.parse(
        f"""
        [general]
            keep_workdirs = false
        [system]
            wrk = "{tmpdir}"
        [platform]
            deode_home = "{WORKING_DIR}"
        """
    )

    config = config.copy(update=config_patch)
    task = Task(config, "test")

    return task


@pytest.fixture()
def _clean_env():
    # Control environment
    with contextlib.suppress(KeyError):
        del os.environ["ECCODES_DEFINITION_PATH"]
        del os.environ["ECCODES_VERSION"]


@pytest.mark.usefixtures("_clean_env")
class TestEccodesDefPath:
    def test_preset_path(self, task):
        # Preset path
        os.environ["ECCODES_DEFINITION_PATH"] = "foo"
        task._set_eccodes_environment()
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        assert eccodes_definition_path == "foo"

    def test_no_preset_path_no_version(self, task):
        # No preset path, no ECCODES_VERSION
        task._set_eccodes_environment()
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        assert PosixPath(eccodes_definition_path) == DEODE_DEFS

    def test_no_preset_path_old_version(self, task):
        # No preset path, older ECCODES_VERSION
        os.environ["ECCODES_VERSION"] = "2.28.0"
        eccodes_dir = "foo/" + os.getenv("ECCODES_VERSION")
        os.environ["ECCODES_DIR"] = eccodes_dir
        eccodes_share = f"{eccodes_dir}/share/eccodes/definitions"
        task._set_eccodes_environment()
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        assert eccodes_definition_path == f"{DEODE_DEFS}:{eccodes_share}"

    def test_no_preset_path_new_version(self, task):
        # No preset path, newer ECCODES_VERSION
        os.environ["ECCODES_VERSION"] = "2.30.0"
        eccodes_dir = "foo/" + os.getenv("ECCODES_VERSION")
        os.environ["ECCODES_DIR"] = eccodes_dir
        task._set_eccodes_environment()
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        assert PosixPath(eccodes_definition_path) == DEODE_DEFS
