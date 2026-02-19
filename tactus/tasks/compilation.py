"""Compialtion tasks."""
import os

# from ..logs import logger

# from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class IALBundle(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.ial_dir = self.config["compile.ial_dir"]
        self.arch = "ecmwf/hpc2020"

    def execute(self):

        batch_job = BatchJob(os.environ)
        batch_job.run(f"cd {self.ial_dir}/bundle; ./ial-bundle create")
        batch_job.run(f"cd {self.ial_dir}/bundle; ./ial-bundle build --arch arch/{self.arch} --ninja --forecast-only")
