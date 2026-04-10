#!/usr/bin/env python3
"""Unit tests for the eccodes_path settings."""

import contextlib
import os
import re
from pathlib import Path

import pytest
import tomlkit
import yaml

from deode.config_parser import ConfigParserDefaults
from deode.derived_variables import set_times
from deode.tasks.base import Task

WORKING_DIR = Path.cwd()
DEODE_DEFS = ConfigParserDefaults.DATA_DIRECTORY / "eccodes/definitions"


@pytest.fixture(scope="module")
def task(tmp_directory, default_config):
    config = default_config
    config = config.copy(update=set_times(config))

    config_patch = tomlkit.parse(
        f"""
        [general]
            keep_workdirs = false
        [system]
            wrk = "{tmp_directory}"
        [platform]
            deode_home = "{WORKING_DIR}"
        """
    )

    config = config.copy(update=config_patch)
    return Task(config, "test")


@pytest.fixture
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
        assert str(DEODE_DEFS) in eccodes_definition_path

    def test_no_preset_path_old_version(self, task):
        # No preset path, older ECCODES_VERSION
        os.environ["ECCODES_VERSION"] = "2.28.0"
        eccodes_dir = "foo/" + os.getenv("ECCODES_VERSION")
        os.environ["ECCODES_DIR"] = eccodes_dir
        eccodes_share = f"{eccodes_dir}/share/eccodes/definitions"
        task._set_eccodes_environment()
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        assert eccodes_share in eccodes_definition_path
        assert str(DEODE_DEFS) in eccodes_definition_path

    def test_no_preset_path_new_version(self, task):
        # No preset path, newer ECCODES_VERSION
        os.environ["ECCODES_VERSION"] = "2.30.0"
        eccodes_dir = "foo/" + os.getenv("ECCODES_VERSION")
        os.environ["ECCODES_DIR"] = eccodes_dir
        task._set_eccodes_environment()
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        eccodes_share = f"{eccodes_dir}/share/eccodes/definitions"
        assert str(DEODE_DEFS) in eccodes_definition_path
        assert eccodes_share not in eccodes_definition_path


class TestFaFieldNameDefinitions:
    """Test that all FA variables from fullpos namelists are in faFieldName.def."""

    def _extract_fa_variables_from_yaml(self, yaml_file_path):
        """Extract FA field names from a fullpos YAML file.

        Returns a set of FA variable names found in the file.
        """
        fa_variables = set()

        with open(yaml_file_path, "r") as f:
            try:
                data = yaml.safe_load(f)
            except yaml.YAMLError as e:
                pytest.fail(f"Failed to parse fullpos YAML file {yaml_file_path}: {e}")

        if not data or "selection" not in data:
            return fa_variables

        # Recursively search for lists of FA variable names
        def extract_from_dict(d):
            if isinstance(d, dict):
                for key, value in d.items():
                    # Skip keys that contain numeric levels (RFP3H, RFP3P, etc.) or other config
                    if key in ["RFP3H", "RFP3P", "RFP3I", "NRFP3S"]:
                        continue
                    if isinstance(value, list):
                        # This is a list of FA variables
                        for item in value:
                            if isinstance(item, str) and not item.startswith("${"):
                                # It's an FA variable name (not a template variable)
                                fa_variables.add(item)
                            elif isinstance(item, dict):
                                extract_from_dict(item)
                    elif isinstance(value, dict):
                        extract_from_dict(value)
            elif isinstance(d, list):
                for item in d:
                    if isinstance(item, dict):
                        extract_from_dict(item)

        extract_from_dict(data["selection"])
        return fa_variables

    def _extract_fa_variables_from_def(self, def_file_path):
        """Extract FA field names from faFieldName.def file.

        Returns a set of FA variable names defined in the file.
        """
        fa_variables = set()

        # Pattern to match: "VARIABLE_NAME" = {
        pattern = re.compile(r'^"([^"]+)"\s*=\s*{', re.MULTILINE)

        with open(def_file_path, "r") as f:
            content = f.read()
            matches = pattern.findall(content)
            fa_variables.update(matches)

        return fa_variables

    def _normalize_fa_variable_name(self, name):
        """Normalize FA variable name by replacing spaces with underscores.

        The FA library accepts spaces and underscores interchangeably,
        so we normalize to underscores for comparison.
        """
        return name.replace(" ", "_")

    def _find_matching_def_variable(self, yaml_var, def_vars):
        """Check if a YAML variable has a matching definition.

        Matches if:
        1. Exact match exists (after normalization)
        2. A truncated version exists in the def (FA files have name length limits)

        Returns True if a match is found, False otherwise.
        """
        # Check exact match
        if yaml_var in def_vars:
            return True

        # Set truncation length as height and pressure prefixes are 6 characters long
        # and FA names have a limit of 16 characters
        fa_truncated_match_min_length = 10

        # Check if any def variable is a prefix of the YAML variable (truncation case)
        # FA field names are often truncated to fit character limits
        for def_var in def_vars:
            # Allow truncated matches if the def var is at least 10 chars
            # and the yaml var starts with it
            if len(def_var) >= fa_truncated_match_min_length and yaml_var.startswith(
                def_var
            ):
                return True

        return False

    def test_all_fullpos_fa_variables_in_def(self):
        """Test that all FA variables from fullpos namelists are in faFieldName.def."""
        # Get paths
        namelist_input_dir = (
            ConfigParserDefaults.DATA_DIRECTORY / "namelist_generation_input"
        )
        fa_field_def_path = (
            ConfigParserDefaults.DATA_DIRECTORY
            / "eccodes/definitions/grib2/localConcepts/lfpw/faFieldName.def"
        )

        # Extract all FA variables from fullpos YAML files
        all_fa_variables = set()
        fullpos_dirs = list(namelist_input_dir.glob("*/fullpos"))

        # Ensure we found fullpos directories
        assert len(fullpos_dirs) > 0, (
            f"No fullpos directories found in {namelist_input_dir}"
        )

        yaml_file_count = 0
        for fullpos_dir in fullpos_dirs:
            yaml_files = list(fullpos_dir.glob("*.yml"))
            yaml_file_count += len(yaml_files)
            for yaml_file in yaml_files:
                fa_vars = self._extract_fa_variables_from_yaml(yaml_file)
                all_fa_variables.update(fa_vars)

        # Ensure we found at least one YAML file
        assert yaml_file_count > 0, "No YAML files found in any fullpos directories"

        # Extract all FA variables from faFieldName.def
        def_fa_variables = self._extract_fa_variables_from_def(fa_field_def_path)

        # Normalize variable names (FA library accepts spaces/underscores interchangeably)
        normalized_yaml_vars = {
            self._normalize_fa_variable_name(v) for v in all_fa_variables
        }
        normalized_def_vars = {
            self._normalize_fa_variable_name(v) for v in def_fa_variables
        }

        # Check that all FA variables from YAML files are in the definition file
        # (accounting for truncation)
        missing_variables = set()
        for yaml_var in normalized_yaml_vars:
            if not self._find_matching_def_variable(yaml_var, normalized_def_vars):
                missing_variables.add(yaml_var)

        assert len(missing_variables) == 0, (
            f"The following {len(missing_variables)} FA variables from fullpos "
            f"namelists are missing in faFieldName.def:\n"
            + "\n".join(sorted(missing_variables))
        )
