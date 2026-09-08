"""Compialtion tasks."""

import copy
import os
import shutil
import sys
from pathlib import Path

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
        self.git_ial_version = self.config["compile.ial_git_version"]
        git_token = self.config["compile.git_token"]
        self.git_token = git_token
        ial_dir = self.config["compile.ial_dir"]
        self.ial_dir = self.platform.substitute(ial_dir)

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        if os.path.exists(self.ial_dir):
            logger.info("IAL dir {} already exists", self.ial_dir)
        else:
            cmd = f"git clone {self.git_ial_repo} {self.ial_dir}"
            cmd = cmd.replace("[TOKEN]", self.git_token)
            batch_job.run(cmd)
        batch_job.run(f"cd {self.ial_dir}; git checkout {self.git_ial_version}")


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

        self.arch_dir = self.platform.substitute(self.config["compile.arch_dir"])

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
            + f"{self.git_token_str} {self.bundle_file_str} --update "
            + f"--arch-dir {self.arch_dir}"
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
        self.compiler = self.platform.substitute("@COMPILER@")
        self.precision = self.config.get("task.args.prec", "prec")
        self.case_dir = self.platform.substitute("@CASEDIR@")
        self.arch = self.platform.substitute(self.config["submission.arch"])

        local_install_dir = f"{self.case_dir}/install/{self.precision}"
        self.local_install_dir = self.platform.substitute(local_install_dir)

        if self.config["compile.install"]:
            self.git_ial_branch = self.config["compile.ial_git_version"]

            install_subpath = self.get_install_subpath()

            install_dir_root = (
                f"@INSTALL_DIR@/{self.git_ial_branch}/{self.precision}/{self.compiler}"
            )
            self.install_dir_root = self.platform.substitute(install_dir_root)

            install_dir = f"{self.install_dir_root}/{install_subpath}"
            self.install_dir = self.platform.substitute(install_dir)

            install_dir_latest = "@INSTALL_DIR@/latest"
            self.install_dir_latest = self.platform.substitute(install_dir_latest)

        else:
            self.install_dir = self.local_install_dir

        builddir = f"{self.case_dir}/build/{self.precision}"
        self.exp_bindir = f"{self.install_dir}"
        self.exp_builddir = builddir
        self.skip_build = self.config["compile.skip_build"] and os.path.exists(
            f"{self.exp_bindir}/bin/MASTERODB"
        )

        tactusmakedirs(self.exp_bindir)
        tactusmakedirs(self.exp_builddir)
        try:
            logger.info(
                "Backing up bundle from {}", f"{self.bundle_dir}/source/bundle.yml"
            )
            shutil.copyfile(
                f"{self.bundle_dir}/source/bundle.yml",
                f"{self.platform.substitute(self.case_dir)}/bundle.yml",
            )
        except FileNotFoundError:
            logger.info("Unable to find {}", self.platform.substitute(self.case_dir))

        self.ninja_arg = ""
        if self.config["compile"].get("ninja"):
            self.ninja_arg = "--ninja "

        self.rebuild_args = ""
        if self.config["compile.clean_build"]:
            self.rebuild_args = "--clean"

        self.prec_arg = ""
        if self.precision == "R32":
            self.prec_arg = "--without-double-precision"

    def get_install_subpath(self):
        """Build install subpath by using the location of the env.sh file.

        The `arch` build directory (``<bundle_dir>/<arch>``) may contain a
        ``default`` symlink pointing at the actual build used, e.g. one
        selected by ecbundle based on compiler/toolchain. When that symlink
        exists, it is resolved and the path components coming after the
        `arch` directory name are returned, giving the subpath under which
        the build was actually installed (e.g. ``<toolchain>/<build_type>``).
        When there is no such symlink, the subpath is empty (`.`), meaning
        installs go directly under the `arch` directory.

        Returns:
            Path: Subpath (relative to the `arch` directory) to append to
                the install/build directories.

        """
        arch_dir = Path(f"{self.bundle_dir}/source/arch/{self.arch}")
        default_link = arch_dir
        if default_link.exists() and default_link.is_symlink():
            arch = default_link.resolve()
        else:
            arch = arch_dir

        parts = arch.parts

        if self.compiler in parts:
            compiler_idx = parts.index(self.compiler)
            return Path(*parts[compiler_idx + 1 :])
        return None

    def make_install_arch_symlink(self):
        """Creates a "default" symlink in the install tree.

        Creates a symlink named default in the compilers installation folder
        to point to the full path of the compiler's default architecture if
        the architecture provided was a symlink.

        """
        arch_dir = Path(f"{self.bundle_dir}/source/arch/{self.arch}")
        default_link = arch_dir

        install_root = Path(self.install_dir_root)
        default_root_link = install_root / "default"

        if default_link.exists() and default_link.is_symlink():
            if default_root_link.exists() and default_root_link.is_symlink:
                logger.debug("Removing old link.")
                os.unlink(default_root_link)
            shutil.copy(str(default_link), str(default_root_link), follow_symlinks=False)

    def execute(self):
        """Execute task."""
        if not self.skip_build:
            if os.path.exists(self.local_install_dir):
                if os.path.islink(self.local_install_dir):
                    logger.debug("Removing old link.")
                    os.unlink(self.local_install_dir)
                else:
                    shutil.rmtree(self.local_install_dir)

            logger.info("Building bundle sources at {}", self.exp_builddir)
            batch_job = BatchJob(os.environ)
            nthreads = os.environ.get("OMP_NUM_THREADS")
            batch_job.run(
                f"cd {self.bundle_dir};  {self.ecbundle_bin} build "
                + f"--arch {self.arch} {self.ninja_arg} --forecast-only "
                + f" {self.rebuild_args} {self.prec_arg} -j{nthreads} "
                + f"--install-dir={self.install_dir} --install "
                + f"--build-dir={self.exp_builddir}"
            )
            logger.info("Installed bundle at  {}", {self.install_dir})

        if self.config["compile.install"]:
            self.make_install_arch_symlink()
            if os.path.exists(self.install_dir_latest) and os.path.islink(
                self.install_dir_latest
            ):
                logger.debug("Removing old link.")
                os.unlink(self.install_dir_latest)

            latest_install = f"@INSTALL_DIR@/{self.git_ial_branch}"
            latest_install = self.platform.substitute(latest_install)
            os.symlink(latest_install, self.install_dir_latest)
            logger.info(
                "Symlinked installtion from {} to {}",
                self.install_dir,
                self.install_dir_latest,
            )

            os.symlink(self.install_dir, self.local_install_dir)
            logger.info(
                "Symlinked installtion from {} to {}",
                self.install_dir,
                self.local_install_dir,
            )
