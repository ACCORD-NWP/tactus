"""Compialtion tasks."""

import os
import sys

import yaml

from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class TactusBundleCreate(Task):
    """tactus create bundle."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        compile_dir = self.config["compile.dir"]
        self.compile_dir = self.platform.substitute(compile_dir)
        tactusmakedirs(self.compile_dir)

        git_token = self.config["compile.git_token"]
        git_token_str = ""
        if git_token:
            git_token_str = f"--github-token {git_token}"
        self.git_token_str = git_token_str

        bundle_file = self.config["compile.bundle_file"]
        bundle_file = self.platform.substitute(bundle_file)

        if self.config["compile.ial_dir"]:
            with open(bundle_file, "r", encoding="utf-8") as f:
                data = yaml.safe_load(f)

            bundle_file = "@CASEDIR@/bundle-local-ial.yaml"
            bundle_file = self.platform.substitute(bundle_file)

            for item in data["projects"]:
                if isinstance(item, dict) and "ial-source" in item:
                    ial = item["ial-source"]

                    if isinstance(ial, dict):
                        # Remove unwanted keys if they exist
                        ial.pop("git", None)
                        ial.pop("version", None)

                        # Add the new key
                        ial["dir"] = self.platform.substitute(
                            self.config["compile.ial_dir"]
                        )

            with open(bundle_file, "w", encoding="utf-8") as f:
                yaml.safe_dump(data, f, sort_keys=False)

        self.bundle_file_str = f"--bundle {bundle_file}"

        self.ecbundle_bin = f"{os.path.dirname(sys.executable)}/ecbundle"

        self.compile_dir = self.platform.substitute(compile_dir)

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        # Assume git ssh access unless token is set
        if not self.git_token_str:
            os.environ["GITHUB"] = "git@github.com:"

        batch_job.run(
            f"cd {self.compile_dir}; {self.ecbundle_bin} create "
            + f"{self.git_token_str} {self.bundle_file_str} --update"
        )


class TactusBundleBuild(Task):
    """tactus bundle build."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        compile_dir = self.config["compile.dir"]
        self.compile_dir = self.platform.substitute(compile_dir)
        self.ecbundle_bin = f"{os.path.dirname(sys.executable)}/ecbundle"

        self.arch = self.config["compile.arch"]
        bindir = "@CASEDIR@/install"
        builddir = "@CASEDIR@/build"
        bindir = self.platform.substitute(bindir)
        bindir = os.path.realpath(bindir)
        builddir = self.platform.substitute(builddir)
        builddir = os.path.realpath(builddir)
        self.exp_bindir = bindir
        self.exp_builddir = builddir
        tactusmakedirs(self.exp_bindir)
        tactusmakedirs(self.exp_builddir)
        self.ninja_arg = ""
        if self.config["compile"].get("ninja"):
            self.ninja_arg = "--ninja "

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        batch_job.run(
            f"cd {self.compile_dir};  {self.ecbundle_bin} build "
            + f"--arch {self.arch} {self.ninja_arg} --forecast-only "
            + f"--install-dir={self.exp_bindir} --install "
            + f"--build-dir={self.exp_builddir}"
        )
