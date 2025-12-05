#!/usr/bin/env python3
"""Unit tests for the experiment.py."""

from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

import pytest
import tomli
import tomlkit

from tactus.config_parser import ParsedConfig
from tactus.experiment import ExpFromFiles, case_setup


@pytest.fixture(name="tmpdir", scope="module")
def fixture_tmpdir(tmp_path_factory: pytest.TempPathFactory) -> Path:
    """Fixture that provides a temporary directory for testing."""
    return tmp_path_factory.getbasetemp()


@pytest.fixture(name="nonexisting_file")
def fixture_nonexisting_file(tmpdir: Path):
    """Fixture that provides a path to a non-existing TOML file for testing."""
    return tmpdir / "nonexisting_file.toml"


@pytest.fixture(name="nontoml_file")
def fixture_nontoml_file(tmpdir: Path):
    """Fixture that provides a path to a non-TOML file for testing."""
    file = tmpdir / "nontoml_file.toml"
    file.write_text("This is not a TOML file.")
    return file


@pytest.fixture(name="test_toml_section")
def fixture_test_toml_section():
    """Fixture that provides a test TOML section as a string for testing."""
    doc = tomlkit.document()
    doc.append("testentry", "test")
    return doc.as_string()


@pytest.fixture(name="toml_file")
def fixture_toml_file(tmpdir: Path, test_toml_section: str):
    """Fixture that provides a path to a TOML file with a test section for testing."""
    file = tmpdir / "config.toml"
    file.write_text(test_toml_section)
    return file


@pytest.fixture(name="output_file")
def fixture_output_file(tmpdir: Path):
    """Fixture that provides a path to an output TOML file for testing."""
    return tmpdir / "output.toml"


@pytest.fixture(name="exp_dependencies")
def fixture_exp_dependencies(output_file: Path):
    """Fixture that provides a dictionary of experiment dependencies for testing."""
    return ExpFromFiles.setup_files(
        str(output_file),
        case="testcase",
    )


@pytest.fixture(name="config")
def fixture_config(default_config):
    """Fixture that provides a parsed configuration object for testing."""
    update = {
        "macros": {
            "case": {
                "gen_macros": ["general.csc"],
                "group_macros": [],
                "os_macros": [],
            }
        }
    }
    config = default_config
    config = config.copy(update=update)
    return config


@pytest.fixture(name="mock_exp_from_files")
def fixture_mock_exp_from_files():
    """Fixture that provides a mock instance of ExpFromFiles."""
    with patch("tactus.experiment.ExpFromFiles.__new__") as mock_new:
        mock_exp = MagicMock()
        mock_exp.config = MagicMock()
        mock_new.return_value = mock_exp
        yield mock_exp


class TestExpFromFiles:
    """Unit tests for the ExpFromFiles class."""

    def test_exp_from_nonexisting_file(
        self, config: ParsedConfig, nonexisting_file: Path, exp_dependencies: dict
    ):
        """Test function for creating an experiment from a non-existing file."""
        ExpFromFiles(config, exp_dependencies, [nonexisting_file])

    def test_exp_from_nontoml_file(
        self, config: ParsedConfig, nontoml_file: Path, exp_dependencies: dict
    ):
        """Test function for creating an experiment from a non-TOML file."""
        with pytest.raises((RuntimeError, tomli.TOMLDecodeError)):
            ExpFromFiles(config, exp_dependencies, [nontoml_file])

    def test_exp_from_toml_file(
        self,
        config: ParsedConfig,
        toml_file: Path,
        exp_dependencies: dict,
        output_file: Path,
    ):
        """Test function for creating an experiment from a TOML file."""
        exp = ExpFromFiles(config, exp_dependencies, [toml_file])
        exp.config.save_as(output_file)


class TestCaseSetup:
    """Unit tests for the case_setup function."""

    @patch("tactus.experiment.EPSExp")
    def test_eps_not_activated(
        self,
        epsexp_mock: Mock,
        config: ParsedConfig,
        output_file: Path,
        mock_exp_from_files: MagicMock,
    ):
        """Test that EPS setup is not activated."""
        mock_exp_from_files.config.get.return_value = None

        case_setup(config=config, output_file=output_file, mod_files=[])

        epsexp_mock.assert_not_called()

    def test_eps_activated(
        self,
        config: ParsedConfig,
        output_file: Path,
        mock_exp_from_files: MagicMock,
    ):
        """Test that EPS setup is activated."""
        mock_exp_from_files.config = {"eps": {}}

        with patch("tactus.experiment.EPSExp.__new__") as epsexp_mock_new:
            epsexp_mock = MagicMock()
            epsexp_mock.config.get.return_value = None
            epsexp_mock_new.return_value = epsexp_mock

            case_setup(config=config, output_file=output_file, mod_files=[])

            epsexp_mock_new.assert_called_once()
