"""ARCHIVEHOUR // ARCHIVESTATIC."""

import glob
import os
import pathlib

from ..logs import logger
from .base import Task


class Archive(Task):
    """Archving data."""

    def __init__(self, config, datatype=None):
        """Construct the archive object.

        Loop over archive types (e.g. ecfs,fdb) for the current platform
        and store the active selections from config.

        Args:
            config (deode.ParsedConfig): Configuration
            datatype (str): Indicating data type (climate, hour)
        """
        Task.__init__(self, config, __class__.__name__)

        self.archive_types = self.config.get("platform.archive_types", [])

        self.choices = {}
        self.archive_loc = {}
        if datatype is not None:
            choices_for_type = self.config[f"archiving.{datatype}"].dict()
            skipped_types = []
            for archive_type, choices in choices_for_type.items():
                d = {
                    name: choice
                    for name, choice in choices.items()
                    if self.trigger(choice["active"])
                }
                if len(d) > 0:
                    if archive_type in self.archive_types:
                        self.choices[archive_type] = d
                        self.archive_loc[archive_type] = self.platform.get_value(
                            f"archiving.prefix.{archive_type}", ""
                        )
                    else:
                        skipped_types.append(archive_type)

            if len(skipped_types) > 0:
                logger.warning(
                    "Skipped archive types not defined for this host: {}", skipped_types
                )

    def trigger(self, trigger):
        """Return trigger."""
        if isinstance(trigger, bool):
            return trigger
        return self.config[trigger]

    def execute(self):
        """Loops over archive choices."""
        for archive_type, choices in self.choices.items():
            logger.info("Archiving type: {}", archive_type)
            for name, choice in choices.items():
                choice.pop("active")
                logger.info("Archiving {} with: {}", name, choice)
                outpath = choice["outpath"] if "outpath" in choice else ""
                self.archive(
                    choice["pattern"],
                    choice["inpath"],
                    outpath,
                    archive_type,
                )

    def archive(self, pattern, inpath, outpath, archive_type=None):
        """Send files to the file manager.

        Args:
            pattern (str,list): string of list of patterns to search for
            inpath (str): Full path on the input archive
            outpath (str): relative path on the output archive
            archive_type (str, optional): Archive type. Defaults to None.
        """
        out = self.platform.substitute(outpath)
        inp = self.platform.substitute(inpath)

        if isinstance(pattern, str):
            pattern = [pattern]

        for ptrn in pattern:
            search = str(pathlib.PurePath(inp, self.platform.substitute(ptrn)))
            files = [x for x in glob.glob(search) if os.path.isfile(x)]

            for filename in sorted(files):
                self.fmanager.output(
                    filename,
                    pathlib.PurePath(
                        self.archive_loc[archive_type], out, os.path.basename(filename)
                    ),
                    provider_id=archive_type,
                )


class ArchiveStatic(Archive):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Archive.__init__(self, config, "static")


class ArchiveHour(Archive):
    """Archving task for time dependent data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Archive.__init__(self, config, "hour")
