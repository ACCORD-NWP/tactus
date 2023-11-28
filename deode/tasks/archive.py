"""ARCHIVEHOUR // ARCHIVESTATIC."""

import glob
import os
import pathlib

from ..datetime_utils import as_datetime, oi2dt_list
from ..logs import logger
from .base import Task


class Archive(Task):
    """Archving data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        apath = self.platform.get_value("archiving.paths.apath")
        aloc = self.platform.get_value("archiving.paths.aloc")
        self.arch_loc = f"{aloc}{apath}"

    def trigger(self, trigger):
        """Return trigger."""
        if isinstance(trigger, bool):
            return trigger
        return self.config[trigger]

    def archive(self, pattern, outpath, inpath):
        """Send files to the file manager.

        Args:
            pattern (str,list): string of list of patterns to search for
            outpath (str): relative path on the output archive
            inpath (str): Full path on the input archive

        """
        out = self.platform.substitute(outpath)
        inp = self.platform.substitute(inpath)

        if isinstance(pattern, str):
            pattern = [pattern]

        for ptrn in pattern:
            search = str(pathlib.PurePath(inp, self.platform.substitute(ptrn)))
            files = [x for x in glob.glob(search) if os.path.isfile(x)]

            for filename in files:
                self.fmanager.input(
                    filename,
                    pathlib.PurePath(self.arch_loc, out, os.path.basename(filename)),
                    check_archive=True,
                    provider_id="ecfs",
                )


class ArchiveStatic(Archive):
    """Archving task for static data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Archive.__init__(self, config)

        self.choices = self.config["archiving.static"]

    def execute(self):
        """Run task.

        Define run sequence.

        """
        for name, choice in self.choices.items():
            if self.trigger(choice["active"]):
                logger.info("{}: {}", name, choice)
                self.archive(choice["pattern"], choice["outpath"], choice["inpath"])


class ArchiveHour(Archive):
    """Archving task for time dependent data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Archive.__init__(self, config)

        self.default = self.config["archiving.default"]
        self.choices = self.config["archiving.hour"]

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]
        self.file_templates = self.config["file_templates"]

        try:
            self.archiving_settings = self.config["archiving.settings"]
            if len(self.archiving_settings) == 0:
                self.archiving_settings = None
        except KeyError:
            self.archiving_settings = None

        try:
            self.conversions = self.config["task.creategrib.conversions"]
        except KeyError:
            self.conversions = {}

        if self.archiving_settings is None:
            self.archiving_settings = {
                x: y
                for x, y in self.config["general.output_settings"].items()
                if x != "nrazts"
            }

    def archive_loop(self, ftype, conv_type, inpath, outpath):
        """Loop over a list fo files and archive them.

        Args:
            ftype(str): Filetype
            conv_type(str): Converted file type
            inpath: Absolute path on the input archive
            outpath: Relatve path on the output archive

        """
        files = []
        dt_list = oi2dt_list(self.archiving_settings[ftype], self.forecast_range)
        for dt in dt_list:
            fi = self.platform.substitute(
                self.file_templates[ftype][conv_type], validtime=self.basetime + dt
            )
            files.append(fi)

        self.archive(files, outpath, inpath)

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Loop to find files based on template then rename for ECFS to accept
        if self.trigger(self.default["active"]):
            outpath = self.platform.substitute(self.default["outpath"])
            inpath = self.platform.substitute(self.default["inpath"])
            logger.info("default: {}", {"inpath": inpath, "outpath": outpath})

            for filetype in self.archiving_settings:
                self.archive_loop(filetype, "archive", inpath, outpath)
                # Archive explicitly converted grib files
                if filetype in self.conversions:
                    self.archive_loop(filetype, "grib", inpath, outpath)

        for name, choice in self.choices.items():
            if self.trigger(choice["active"]):
                logger.info("{}: {}", name, choice)
                self.archive(choice["pattern"], choice["outpath"], choice["inpath"])
