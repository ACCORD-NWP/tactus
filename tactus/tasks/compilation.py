"""Compialtion tasks."""

import copy
import hashlib
import json
import os
import shutil
import sys
from pathlib import Path

from git import InvalidGitRepositoryError, Repo
from ruamel.yaml import YAML

from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class IALClone(Task):
    """Bundle Fetch task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.git_ial_repo = self.config["compile.ial_git_repo"]
        self.git_ial_branch = self.config["compile.ial_git_branch"]
        git_token = self.config["compile.git_token"]
        self.git_token = git_token
        ial_dir = self.config["compile.ial_dir"]
        self.ial_dir = self.platform.substitute(ial_dir)

    def execute(self):
        """Execute task."""
        if os.path.exists(self.ial_dir):
            logger.info("IAL dir {} alreadys exists", self.ial_dir)
        else:
            batch_job = BatchJob(os.environ)
            cmd = f"git clone {self.git_ial_repo} {self.ial_dir}"
            cmd = cmd.replace("[TOKEN]", self.git_token)
            batch_job.run(cmd)
            batch_job.run(f"cd {self.ial_dir}; git checkout {self.git_ial_branch}")


class TactusBundleCreate(Task):
    """tactus create bundle."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
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

        orig_bundle_file = self.config["compile.bundle_file"]
        self.orig_bundle_file = self.platform.substitute(orig_bundle_file)

        if self.config["compile.bundle_update"]:
            bundle_file = "@CASEDIR@/bundle-local-ial.yaml"
            self.bundle_file = self.platform.substitute(bundle_file)
            update_bundle_file = self.config["compile.update_bundle_file"]
            self.update_bundle_file = self.platform.substitute(update_bundle_file)
        else:
            self.bundle_file = self.orig_bundle_file

        self.bundle_file_str = f"--bundle {self.bundle_file}"

        self.ecbundle_bin = f"{os.path.dirname(sys.executable)}/ecbundle"

        self.compile_dir = self.platform.substitute(compile_dir)

    def deep_merge(self, original, updates):
        """Recursively merge `updates` into `original`.

        Rules:
        - Dictionaries and lists of dictionaries are merged recursively.
        - Other lists and scalar values are replaced entirely.
        - Keys missing from `updates` remain unchanged.
        """
        if isinstance(original, dict) and isinstance(updates, dict):
            merged = copy.deepcopy(original)
            for key, value in updates.items():
                if key in merged:
                    if isinstance(merged[key], dict) and isinstance(value, dict):
                        merged[key] = self.deep_merge(merged[key], value)

                    elif isinstance(merged[key], list) and isinstance(value, list):
                        merged[key] = {k: v for d in merged[key] for k, v in d.items()}
                        new_val = {k: v for d in value for k, v in d.items()}
                        merged[key] = self.deep_merge(merged[key], new_val)
                    else:
                        merged[key] = copy.deepcopy(value)

                else:
                    merged[key] = copy.deepcopy(value)

        else:
            return copy.deepcopy(updates)

        if "projects" in merged:
            merged["projects"] = [
                {key: value} for key, value in merged["projects"].items()
            ]

        return merged

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        # Assume git ssh access unless token is set
        if not self.git_token_str:
            os.environ["GITHUB"] = "git@github.com:"

        ial_dir = self.config["compile.ial_dir"]
        os.environ["IAL_DIR"] = self.platform.substitute(ial_dir)

        if self.config["compile.bundle_update"]:
            yaml = YAML()

            # Formatting preservation settings
            yaml.preserve_quotes = True
            yaml.indent(mapping=4, sequence=4, offset=2)
            yaml.width = 4096
            try:
                with open(self.orig_bundle_file, "r", encoding="utf-8") as f:
                    orig_bundle_dict = yaml.load(f) or {}

                with open(self.update_bundle_file, "r", encoding="utf-8") as f:
                    upd_bundle_dict = yaml.load(f) or {}

            except FileNotFoundError:
                orig_bundle_dict = {}
                upd_bundle_dict = {}

            merged_dict = self.deep_merge(orig_bundle_dict, upd_bundle_dict)

            with open(self.bundle_file, "w", encoding="utf-8") as f:
                yaml.dump(merged_dict, f)

        batch_job.run(
            f"cd {self.compile_dir}; {self.ecbundle_bin} create "
            + f"{self.git_token_str} {self.bundle_file_str} --update"
        )


class TactusBundleBuild(Task):
    """tactus bundle build."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        bundle_dir = self.config["compile.dir"]
        self.bundle_dir = self.platform.substitute(bundle_dir)
        self.ecbundle_bin = f"{os.path.dirname(sys.executable)}/ecbundle"

        self.precision = self.config.get("task.args.prec", "prec")

        self.arch = self.config["compile.arch"]

        # check for existing builds in cache_dir
        if self.config["compile.cache"]:
            try:
                self.bundle_hash = self.get_bundle_hash(f"{self.bundle_dir}/source")
            except FileNotFoundError:
                self.bundle_hash = "unknown"

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

        bindir = f"{compile_dir}/install/{self.precision}"
        builddir = f"{compile_dir}/build/{self.precision}"
        local_bindir = f"@CASEDIR@/install/{self.precision}"
        bindir = self.platform.substitute(bindir)
        bindir = os.path.realpath(bindir)
        builddir = self.platform.substitute(builddir)
        builddir = os.path.realpath(builddir)
        self.local_bindir = self.platform.substitute(local_bindir)
        self.exp_bindir = bindir
        self.exp_builddir = builddir
        self.skip_build = self.config["compile.skip_build"] and os.path.exists(
            f"{self.exp_bindir}/MASTERODB"
        )

        tactusmakedirs(self.exp_bindir)
        tactusmakedirs(self.exp_builddir)
        tactusmakedirs(os.path.dirname(self.local_bindir))
        try:
            logger.info(
                "Backing up bundle from {}", f"{self.bundle_dir}/source/bundle.yml"
            )
            shutil.copyfile(
                f"{self.bundle_dir}/source/bundle.yml",
                f"{self.platform.substitute(compile_dir)}/bundle.yml",
            )
        except FileNotFoundError:
            logger.info("Unable to find {}", self.platform.substitute(compile_dir))

        self.ninja_arg = ""
        if self.config["compile"].get("ninja"):
            self.ninja_arg = "--ninja "

        self.rebuild_args = ""
        if self.config["compile.clean_build"]:
            self.rebuild_args = "--clean"

        self.prec_arg = ""
        if self.precision == "R32":
            self.prec_arg = "--without-double-precision"

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
        if not self.skip_build:
            logger.info("Building bundle sources at {}", self.exp_builddir)
            batch_job = BatchJob(os.environ)
            nthreads = os.environ.get("OMP_NUM_THREADS")
            batch_job.run(
                f"cd {self.bundle_dir};  {self.ecbundle_bin} build "
                + f"--arch {self.arch} {self.ninja_arg} --forecast-only "
                + f" {self.rebuild_args} {self.prec_arg} -j{nthreads} "
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
