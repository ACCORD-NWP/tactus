"""Module with tests for the base Task class."""

import os
from pathlib import Path

import pytest

from tactus.config_parser import ParsedConfig
from tactus.derived_variables import set_times
from tactus.tasks.base import Task
from tactus.toolbox import Platform


@pytest.fixture(scope="module")
def tmp_path(tmp_directory: str):
    return Path(tmp_directory)


@pytest.fixture(scope="module")
def task_bindir(tmp_path: Path):
    return str(tmp_path / "task_bin")


@pytest.fixture(scope="module")
def gen_bindir(tmp_path: Path):
    return str(tmp_path / "gen_bin")


@pytest.fixture(scope="module")
def binaries_bindir(tmp_path: Path):
    return str(tmp_path / "binaries_bin")


@pytest.fixture(scope="module")
def basic_config(tmp_directory: str, default_config: ParsedConfig):
    config = default_config.copy(update=set_times(default_config))
    return config.copy(
        update={
            "platform": {
                "scratch": tmp_directory,
                "unix_group": "",
            },
        }
    )


class TestBaseTask:
    """Unit tests for the base Task class."""

    def test_prep(self, tmp_path: Path, basic_config: ParsedConfig):
        """Test the prep method of the Task class."""
        with open(tmp_path / "config.toml", "w", encoding="utf8"):
            task = Task(basic_config, "TestPrep")
            task.prep()

            # Check that the working directory was created
            assert os.path.exists(task.wdir)

            # Check that the config file was saved in the working directory
            config_file = f"{task.wdir}/config.toml"
            assert os.path.exists(config_file)

        # Clean up
        os.remove(config_file)


class TestGetBinary:
    """Unit tests for Task.get_binary."""

    def test_task_specific_bindir(self, task_bindir: Path, basic_config: ParsedConfig):
        """Task-specific bindir from task_exceptions is returned."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "task_exceptions": {"GetBinaryTest": {"bindir": task_bindir}}
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{task_bindir}/MASTERODB"

    def test_general_bindir(self, gen_bindir: str, basic_config: ParsedConfig):
        """submission.bindir is used when the binary file exists there."""
        task_config = basic_config.copy(
            update={
                "submission": {"bindir": gen_bindir},
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{gen_bindir}/MASTERODB"

    def test_binary_name_override(self, task_bindir: Path, basic_config: ParsedConfig):
        """Binary name set in task_exceptions overrides the requested binary name."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "task_exceptions": {
                        "GetBinaryTest": {
                            "bindir": task_bindir,
                            "binary": "ALTERNATE_BIN",
                        }
                    }
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{task_bindir}/ALTERNATE_BIN"

    def test_binaries_section_binary_name_and_bindir(
        self,
        task_bindir: Path,
        gen_bindir: str,
        binaries_bindir: Path,
        basic_config: ParsedConfig,
    ):
        """Binary under task_exceptions.binaries.{binary} overrides the binary name."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "bindir": gen_bindir,
                    "task_exceptions": {
                        "bindir": task_bindir,
                        "GetBinaryTest": {
                            "binaries": {
                                "MASTERODB": {
                                    "bindir": binaries_bindir,
                                    "binary": "MASTERODB_DBG",
                                }
                            }
                        },
                    },
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{binaries_bindir}/MASTERODB_DBG"

    def test_binaries_section_bindir_only(
        self, binaries_bindir: Path, basic_config: ParsedConfig
    ):
        """Bindir under task_exceptions.binaries.{binary} takes precedence."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "task_exceptions": {
                        "GetBinaryTest": {
                            "binaries": {"MASTERODB": {"bindir": binaries_bindir}}
                        }
                    }
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{binaries_bindir}/MASTERODB"

    def test_binaries_section_binary_name_only(self, basic_config: ParsedConfig):
        """Only binary under binaries overrides the name; fallback returns the new name."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "task_exceptions": {
                        "GetBinaryTest": {
                            "binaries": {"MASTERODB": {"binary": "MASTERODB_DBG"}}
                        }
                    }
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        default_bindir = Platform(basic_config).substitute(
            basic_config.get("submission.bindir")
        )
        assert task.get_binary("MASTERODB") == f"{default_bindir}/MASTERODB_DBG"

    def test_binaries_section_binary_name_and_task_bindir(
        self,
        task_bindir: Path,
        gen_bindir: str,
        basic_config: ParsedConfig,
    ):
        """Binary under task_exceptions.binaries.{binary} overrides the binary name."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "bindir": gen_bindir,
                    "task_exceptions": {
                        "GetBinaryTest": {
                            "bindir": task_bindir,
                            "binaries": {
                                "MASTERODB": {
                                    "binary": "MASTERODB_DBG",
                                }
                            },
                        },
                    },
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{task_bindir}/MASTERODB_DBG"

    def test_binaries_section_binary_name_and_gen_bindir(
        self, gen_bindir: str, basic_config: ParsedConfig
    ):
        """Binary under task_exceptions.binaries.{binary} overrides the binary name."""
        task_config = basic_config.copy(
            update={
                "submission": {
                    "bindir": gen_bindir,
                    "task_exceptions": {
                        "GetBinaryTest": {
                            "binaries": {
                                "MASTERODB": {
                                    "binary": "MASTERODB_DBG",
                                }
                            }
                        }
                    },
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        print(task.get_binary("MASTERODB"))
        assert task.get_binary("MASTERODB") == f"{gen_bindir}/MASTERODB_DBG"
