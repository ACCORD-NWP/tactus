"""Module with tests for the base Task class."""

import os
from pathlib import Path

from deode.config_parser import ParsedConfig
from deode.tasks.base import Task


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
