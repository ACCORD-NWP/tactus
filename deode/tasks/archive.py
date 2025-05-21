"""ARCHIVEHOUR // ARCHIVESTATIC."""

from deode.archive import Archive
from deode.tasks.base import Task


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
            config (deode.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "staticmember")


class ArchiveStatic(ArchiveTask):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "static")


class ArchiveHour(ArchiveTask):
    """Archving task for time dependent data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "hour", exclude=["fdb"])


class ArchiveFDB(ArchiveTask):
    """Archving task for time dependent data dedicated for FDB."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        ArchiveTask.__init__(self, config, "FDB", include=["fdb"])
