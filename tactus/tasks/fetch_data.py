"""Tasks fetching static data managed with dvc."""

import os
import shlex
import sys
from pathlib import Path

from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class IALDataFetch(Task):
    """Clone the IAL-data repository and pull its dvc-managed data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.git_repo = self.config["fetch_data.ial_data_git_repo"]
        self.git_version = self.config["fetch_data.ial_data_git_version"]
        self.data_dir = self.platform.substitute(self.config["fetch_data.ial_data_dir"])
        self.dvc_targets = [
            self.platform.substitute(target)
            for target in self.config.get("fetch_data.dvc_targets", [])
        ]
        self.dvc_jobs = self.config.get("fetch_data.dvc_jobs", 0)
        # Always use the dvc installed with tactus
        self.dvc_bin = f"{os.path.dirname(sys.executable)}/dvc"

    def git_commands(self):
        """Return the git commands cloning/updating the IAL-data repository.

        Returns:
            list: Shell commands to run

        Raises:
            RuntimeError: If the target directory exists and is not a git clone
        """
        data_dir = shlex.quote(self.data_dir)
        version = shlex.quote(self.git_version)
        cmds = []
        if os.path.isdir(os.path.join(self.data_dir, ".git")):
            logger.info("IAL-data clone {} already exists, updating it", self.data_dir)
            cmds.append(f"cd {data_dir}; git fetch --tags origin")
        elif os.path.exists(self.data_dir) and os.listdir(self.data_dir):
            raise RuntimeError(
                f"{self.data_dir} exists, is not empty and is not a git repository"
            )
        else:
            tactusmakedirs(Path(self.data_dir).parent, unixgroup=self.unix_group)
            cmds.append(f"git clone {shlex.quote(self.git_repo)} {data_dir}")

        cmds.append(f"cd {data_dir}; git checkout {version}")
        # Fast-forward when the version is a branch (no-op for tags and commits)
        cmds.append(
            f"cd {data_dir}; "
            "if git symbolic-ref -q HEAD > /dev/null; then git pull --ff-only; fi"
        )
        return cmds

    def expand_targets(self):
        """Expand the dvc targets, to be done once the repository is cloned.

        IAL-data tracks every file with its own .dvc file, which dvc does not
        accept a directory as target for. Directory targets are hence replaced
        by all the .dvc files found below them.

        Returns:
            list: dvc targets, relative to the clone

        Raises:
            FileNotFoundError: If a directory target contains no .dvc file
        """
        targets = []
        for target in self.dvc_targets:
            path = Path(self.data_dir) / target
            if path.is_dir():
                dvc_files = sorted(
                    str(dvc_file.relative_to(self.data_dir))
                    for dvc_file in path.rglob("*.dvc")
                )
                if not dvc_files:
                    raise FileNotFoundError(f"No dvc tracked data found in {path}")
                targets.extend(dvc_files)
            else:
                targets.append(target)
        return targets

    def dvc_command(self):
        """Return the dvc command pulling the data.

        Returns:
            str: Shell command to run
        """
        cmd = f"cd {shlex.quote(self.data_dir)}; {shlex.quote(self.dvc_bin)} pull"
        if self.dvc_jobs:
            cmd += f" --jobs {int(self.dvc_jobs)}"
        for target in self.expand_targets():
            cmd += f" {shlex.quote(target)}"
        return cmd

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        for cmd in self.git_commands():
            batch_job.run(cmd)

        logger.info("Pulling dvc data into {}", self.data_dir)
        batch_job.run(self.dvc_command())
        logger.info("IAL-data {} available in {}", self.git_version, self.data_dir)
