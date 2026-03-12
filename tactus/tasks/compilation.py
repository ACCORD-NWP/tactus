"""Compialtion tasks."""
import os

from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class IALClone(Task):
    """IAL clone task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.git_ial_repo = self.config["compile.git_repo"]
        self.git_ial_branch = self.config["compile.git_branch"]
        ial_dir = self.config["compile.ial_dir"]
        self.ial_dir = self.platform.substitute(ial_dir)

    def execute(self):
        """Execute task."""
        if os.path.exists(self.ial_dir):
            logger.info("IAL dir {} alreadys exists", self.ial_dir)
        else:
            batch_job = BatchJob(os.environ)
            batch_job.run(f"git clone {self.git_ial_repo} {self.ial_dir}")
            batch_job.run(f"cd {self.ial_dir}; git checkout {self.git_ial_branch}")


class IALBundleCreate(Task):
    """IAL create bundle."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        ial_dir = self.config["compile.ial_dir"]
        self.ial_dir = self.platform.substitute(ial_dir)

    def execute(self):
        """Execute task."""
        os.environ["GITHUB"] = "git@github.com:"
        batch_job = BatchJob(os.environ)
        batch_job.run(f"cd {self.ial_dir}/bundle; ./ial-bundle create")


class IALBundleBuild(Task):
    """IAL bundle build."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        ial_dir = self.config["compile.ial_dir"]
        self.ial_dir = self.platform.substitute(ial_dir)
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

    def execute(self):
        """Execute task."""
        batch_job = BatchJob(os.environ)
        batch_job.run(
            f"cd {self.ial_dir}/bundle; ./ial-bundle build "
            + f"--arch arch/{self.arch} --ninja --forecast-only "
            + f"--install-dir={self.exp_bindir} --install "
            + f"--build-dir={self.exp_builddir}"
        )
