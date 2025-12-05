"""ARCHIVEHOUR // ARCHIVESTATIC."""

import os

from tactus.archive import Archive
from tactus.tasks.base import Task


class ArchiveTask(Task):
    """Archving data."""

    def __init__(self, config, datatype=None, include=None, exclude=None):
        """Construct the archive object."""
        Task.__init__(self, config, __class__.__name__)
        self.da = Archive(config, datatype, include=include, exclude=exclude)

    def execute(self):
        """Loops over archive choices."""
        self.da.execute()


class ArchiveStaticMember(ArchiveTask):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "staticmember")


class ArchiveStatic(ArchiveTask):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "static")


class ArchiveHour(ArchiveTask):
    """Archving task for time dependent data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "hour", exclude=["fdb"])


class ArchiveFDB(ArchiveTask):
    """Archving task for time dependent data dedicated for FDB."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "FDB", include=["fdb"])


class ArchiveDataBridge(ArchiveTask):
    """Archving task for time dependent data dedicated for the databridge."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "DataBridge", include=["fdb"])
        self._check_user()

    def _check_user(self):
        """Check if we are allowed to archive to the databridge."""
        user = os.environ.get("USER")
        try:
            databridge_user = self.platform.substitute(self.config["fdb.databridge_user"])
        except KeyError as error:
            raise KeyError("No databridge user defined") from error

        if user != databridge_user:
            raise ValueError(
                f"Archiving to the databridge is only allowed for user {databridge_user}"
            )


class ArchiveMergedSQLites(ArchiveTask):
    """Archving task for time dependent data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "merged_sqlite", exclude=["fdb"])
