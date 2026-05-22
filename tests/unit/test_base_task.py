"""Module with tests for the base Task class."""

import os
from pathlib import Path

from tactus.config_parser import ParsedConfig
from tactus.tasks.base import Task


class TestBaseTask:
    """Unit tests for the base Task class."""

    def test_prep(self, tmp_path: Path, default_config: ParsedConfig):
        """Test the prep method of the Task class."""
        task_config = default_config.copy(
            update={
                "submission": {"serial": {"tasks": ["UnitTest"]}},
                "platform": {
                    "scratch": f"{tmp_path}",
                    "unix_group": "",
                },
            }
        )

        with open(tmp_path / "config.toml", "w", encoding="utf8"):
            task = Task(task_config, "TestPrep")
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

    def test_task_specific_bindir(self, tmp_path: Path, default_config: ParsedConfig):
        """Task-specific bindir from task_exceptions is returned."""
        task_bindir = str(tmp_path / "task_bin")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
                "submission": {
                    "task_exceptions": {"GetBinaryTest": {"bindir": task_bindir}}
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{task_bindir}/MASTERODB"

    def test_general_bindir(self, tmp_path: Path, default_config: ParsedConfig):
        """submission.bindir is used when the binary file exists there."""
        gen_bindir = tmp_path / "gen_bin"
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
                "submission": {"bindir": str(gen_bindir)},
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == str(gen_bindir / "MASTERODB")

    def test_sys_bindir(self, tmp_path: Path, default_config: ParsedConfig):
        """submission.sys_bindir is used when the binary file exists there."""
        sys_bindir = tmp_path / "install" / "bin"
        sys_bindir.mkdir(parents=True)
        (sys_bindir / "MASTERODB").touch()
        task_config = default_config.copy(
            update={
                "system": {"casedir": str(tmp_path)},
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
                "submission": {"sys_bindir": str(sys_bindir)},
            },
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == str(sys_bindir / "MASTERODB")

    def test_fallback_returns_binary_name(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """Binary name is returned unchanged when no bindir is configured."""
        task_config = default_config.copy(
            update={"platform": {"scratch": str(tmp_path), "unix_group": ""}},
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == "MASTERODB"

    def test_binary_name_override(self, tmp_path: Path, default_config: ParsedConfig):
        """Binary name set in task_exceptions overrides the requested binary name."""
        task_bindir = str(tmp_path / "task_bin")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
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

    def test_binaries_section_bindir_only(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """Only bindir under binaries is used; binary name stays unchanged."""
        binaries_bindir = str(tmp_path / "binaries_bin")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
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

    def test_binaries_section_bindir_only(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """bindir under task_exceptions.binaries.{binary} takes precedence."""
        binaries_bindir = str(tmp_path / "binaries_bin")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
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

    def test_binaries_section_binary_name_and_bindir(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """binary under task_exceptions.binaries.{binary} overrides the binary name."""
        binaries_bindir = str(tmp_path / "binaries_bin")
        task_bindir = str(tmp_path / "task_bindir")
        gen_bindir = str(tmp_path / "gen_bindir")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
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

    def test_binaries_section_binary_name_only(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """Only binary under binaries overrides the name; fallback returns the new name."""
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
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
        assert task.get_binary("MASTERODB") == "MASTERODB_DBG"

    def test_binaries_section_binary_name_and_task_bindir(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """binary under task_exceptions.binaries.{binary} overrides the binary name."""
        binaries_bindir = str(tmp_path / "binaries_bin")
        task_bindir = str(tmp_path / "task_bindir")
        gen_bindir = str(tmp_path / "gen_bindir")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
                "submission": {
                    "bindir": gen_bindir,
                    "task_exceptions": {
                        "bindir": task_bindir,
                        "GetBinaryTest": {
                            "binaries": {
                                "MASTERODB": {
                                    "binary": "MASTERODB_DBG",
                                }
                            }
                        },
                    },
                },
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == f"{task_bindir}/MASTERODB_DBG"

    def test_binaries_section_binary_name_and_gen_bindir(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """binary under task_exceptions.binaries.{binary} overrides the binary name."""
        binaries_bindir = str(tmp_path / "binaries_bin")
        gen_bindir = str(tmp_path / "gen_bindir")
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
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
        assert task.get_binary("MASTERODB") == f"{gen_bindir}/MASTERODB_DBG"

    def test_binaries_section_binary_name_and_sys_bindir(
        self, tmp_path: Path, default_config: ParsedConfig
    ):
        """binary under binaries overrides the name; sys_bindir is checked with the new name."""
        sys_bindir = tmp_path / "install" / "bin"
        sys_bindir.mkdir(parents=True)
        (sys_bindir / "MASTERODB_DBG").touch()
        task_config = default_config.copy(
            update={
                "platform": {"scratch": str(tmp_path), "unix_group": ""},
                "submission": {
                    "task_exceptions": {
                        "GetBinaryTest": {
                            "binaries": {
                                "MASTERODB": {
                                    "binary": "MASTERODB_DBG",
                                }
                            }
                        }
                    }
                },
                "system": {"casedir": str(tmp_path)},
            }
        )
        task = Task(task_config, "GetBinaryTest")
        assert task.get_binary("MASTERODB") == str(sys_bindir / "MASTERODB_DBG")
