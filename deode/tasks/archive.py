"""ARCHIVEHOUR // ARCHIVESTATIC."""

import os

from ..logs import logger
from ..toolbox import Platform
from .base import Task


class ArchiveStatic(Task):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.config = config
        self.platform = Platform(self.config)
        self.climdir = self.platform.get_value("system.climdir")
        self.apath = self.platform.get_value("archiving.apath")
        self.loc = self.platform.get_value("archiving.loc")

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # List all files in current location
        files = os.listdir(self.climdir)
        arch_loc = f"{self.loc}/{self.apath}/climate/"
        logger.info("Proceed to store: {}, {}", self.climdir, files)
        logger.info("in this archive location: {}", arch_loc)
        for f in files:
            # Locate files and store to ECFS
            self.fmanager.input(
                f"{self.climdir}/{f}",
                f"{arch_loc}/{f}",
                check_archive=True,
                provider_id="ecfs",
            )


class ArchiveHour(Task):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.apath = self.platform.get_value("archiving.apath")
        self.aloc = self.platform.get_value("archiving.loc")
        self.archive = self.platform.get_value("system.archive")
        self.arch_cdate = self.platform.get_value("archiving.arch_cdate")

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Locate files and store on ECFS - Stores only initial files now. Needs strategy to store as many files as needed.
        files = os.listdir(f"{self.archive}")
        arch_loc = f"{self.aloc}/{self.apath}/{self.arch_cdate}/"
        logger.info("Storing: {}, {}", self.archive, files)
        logger.info("in this archive location: {}", arch_loc)

        for f in files:
            self.fmanager.input(
                f"{self.archive}/{f}",
                arch_loc,
                check_archive=True,
                provider_id="ecfs",
            )
