"""Compialtion tasks."""

import hashlib
import json
import os
import sys
from pathlib import Path

import yaml
from git import InvalidGitRepositoryError, Repo

from ..logs import logger
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

        bundle_dir = self.config["compile.dir"]
        self.bundle_dir = self.platform.substitute(bundle_dir)
        self.ecbundle_bin = f"{os.path.dirname(sys.executable)}/ecbundle"

        self.arch = self.config["compile.arch"]

        # check for existing builds in cache_dir
        if self.config["compile.cache"]:
            self.bundle_hash = self.get_bundle_hash(f"{self.bundle_dir}/source")

            # get arch to build install path
            arch_dir = Path(f"{self.bundle_dir}/{self.arch}")
            default_link = arch_dir / "default"
            if default_link.exists() and default_link.is_symlink():
                arch = str(default_link.resolve())
            else:
                arch = str(arch_dir)
            arch = arch.split("arch")[-1]
            compile_dir = f"{self.config['compile.cache_dir']}/{arch}/{self.bundle_hash}"

        else:
            compile_dir = "@CASEDIR@"

        bindir = f"{compile_dir}/install/@PRECISION@"
        builddir = f"{compile_dir}/build/@PRECISION@"
        local_bindir = "@CASEDIR@/install/@PRECISION@"
        bindir = self.platform.substitute(bindir)
        bindir = os.path.realpath(bindir)
        builddir = self.platform.substitute(builddir)
        builddir = os.path.realpath(builddir)
        self.local_bindir = self.platform.substitute(local_bindir)
        self.exp_bindir = bindir
        self.exp_builddir = builddir
        self.do_build = self.config["compile.force_rebuild"] or not os.path.exists(
            f"{self.exp_bindir}/MASTERODB"
        )

        tactusmakedirs(self.exp_bindir)
        tactusmakedirs(self.exp_builddir)
        tactusmakedirs(os.path.dirname(self.local_bindir))
        self.ninja_arg = ""
        if self.config["compile"].get("ninja"):
            self.ninja_arg = "--ninja "

        self.rebuild_args = ""
        if self.config["compile.force_rebuild"]:
            self.rebuild_args = "--clean"

    def get_bundle_hash(self, source_dir):
        """Build a unique hash for the bundle source combination."""
        logger.debug("Build a hash for the source bundle")

        manifest = {
            "repositories": {},
            "dirty": False,
        }

        source_path = Path(source_dir)

        # Iterate through source folders
        for folder in sorted(source_path.iterdir()):
            if not folder.is_dir():
                continue

            try:
                repo = Repo(folder)
            except InvalidGitRepositoryError:
                logger.info("[SKIP] Not a git repo: {}", folder.name)
                continue

            logger.info("[CHECK] {}", folder.name)

            # test for modified/staged/untracked files:
            dirty = repo.is_dirty(untracked_files=True)

            repo_info = {
                "commit": repo.head.commit.hexsha,
                "dirty": dirty,
            }

            manifest["repositories"][folder.name] = repo_info

            if repo_info["dirty"]:
                manifest["dirty"] = True

        # Deterministic serialization
        serialized = json.dumps(
            manifest,
            sort_keys=True,
            separators=(",", ":"),
        )

        # Combined deterministic hash
        build_hash = hashlib.sha256(serialized.encode("utf-8")).hexdigest()

        # Mark hash as dirty if any repo is dirty
        if manifest["dirty"]:
            build_hash += "-dirty"

        logger.info(f"hash for {source_path}: {build_hash}")

        return build_hash

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        if self.do_build:
            logger.info("Building bundle sources at {}", self.exp_builddir)

            batch_job.run(
                f"cd {self.bundle_dir};  {self.ecbundle_bin} build "
                + f"--arch {self.arch} {self.ninja_arg} --forecast-only "
                + f" {self.rebuild_args} "
                + f"--install-dir={self.exp_bindir} --install "
                + f"--build-dir={self.exp_builddir}"
            )
        if self.config["compile.cache"]:
            if os.path.islink(self.local_bindir):
                logger.debug("Removing old link.")
                os.unlink(self.local_bindir)
            os.symlink(self.exp_bindir, self.local_bindir)

        else:
            logger.info("found existing install for this bundle at {}", self.exp_bindir)
