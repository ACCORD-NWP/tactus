"""ARCHIVEHOUR // ARCHIVESTATIC."""

import os
import pathlib

from ..datetime_utils import as_datetime, as_timedelta, oi2dt_list
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

        self.climdir = self.platform.get_value("system.climdir")
        self.logs = self.platform.get_value("system.logs")

        self.apath = self.platform.get_value("archiving.paths.apath")
        self.aloc = self.platform.get_value("archiving.paths.aloc")

    def execute(self):
        """Run task.

        Define run sequence.

        """

        arch_loc = f"{self.aloc}{self.apath}/climate/"
        logarch_loc = f"{self.aloc}{self.apath}/logs/"
        clim_files = os.listdir(self.climdir)
        logger.info("store these files: {}", clim_files)

        for fi in clim_files:
            self.fmanager.input(
                pathlib.PurePath(self.climdir, fi),
                pathlib.PurePath(arch_loc, fi),
                check_archive=True,
                provider_id="ecfs",
            )

        # Static log
        for fi in os.listdir(self.logs):
            p2 = pathlib.Path(self.logs, fi)

            if p2.is_file():
                logger.info(
                    "Log file storage ArchiveStatic: {},{}",
                    p2,
                    pathlib.PurePath(logarch_loc, fi),
                )
                self.fmanager.input(
                    pathlib.PurePath(self.logs, fi),
                    pathlib.PurePath(logarch_loc, fi),
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

        try:
            self.archiving_settings = self.config["archiving.aoutput_settings"].items()
            logger.info(
                "Archiving_settings from archive.toml: {}", self.archiving_settings
            )
            if len(self.archiving_settings) == 0:
                self.archiving_settings = None
        except KeyError:
            self.archiving_settings = None

        self.apath = self.platform.get_value("archiving.paths.apath")
        self.aloc = self.platform.get_value("archiving.paths.aloc")
        self.arch_cdate = self.platform.get_value("archiving.paths.arch_cdate")

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.file_templates = self.config["file_templates"]
        self.forecast_range = self.config["general.times.forecast_range"]
        self.logs = self.platform.get_value("system.logs")
        self.archive = self.platform.get_value("system.archive")

        self.grib_temp = self.config["task.creategrib.conversions.surfex"][
            "output_template"
        ]

        logger.info("Archiving_settings: {}", self.archiving_settings)
        logger.info("CreateGrib template: {}", self.grib_temp)

        if self.archiving_settings is None:
            self.archiving_settings = {
                x: y
                for x, y in self.config["general.output_settings"].items()
                if x != "nrazts"
            }

    def execute(self):
        """Run task.

        Define run sequence.

        """

        arch_loc = f"{self.aloc}{self.apath}/{self.arch_cdate}/"
        logarch_loc = f"{self.aloc}{self.apath}/logs/"

        # Loop to find files based on template then rename for ECFS to accept
        for ftype, oi in self.archiving_settings:
            logger.info("archiving_settings loop: {},{}", ftype, oi)
            dt_list = oi2dt_list(oi, self.forecast_range)

            if ftype == "grib":
                # For ftype=grib only
                for dt in dt_list:
                    fi = self.platform.substitute(
                        self.grib_temp, validtime=self.basetime + dt
                    )
                    self.fmanager.input(
                        pathlib.PurePath(self.archive, fi),
                        pathlib.PurePath(arch_loc, fi),
                        check_archive=True,
                        provider_id="ecfs",
                    )
            else:
                # Store each file in turn
                for dt in dt_list:
                    fi = self.platform.substitute(
                        self.file_templates[ftype]["archive"],
                        validtime=self.basetime + dt,
                    )
                    self.fmanager.input(
                        pathlib.PurePath(self.archive, fi),
                        pathlib.PurePath(arch_loc, fi),
                        check_archive=True,
                        provider_id="ecfs",
                    )

        # Find the log files and archive each file in turn
        log = f"{self.basetime.strftime('%Y%m%d_%H%M')}.tar.gz"
        logger.info(
            "Log file storage ArchiveHour: {},{}",
            pathlib.Path(self.logs, log),
            pathlib.PurePath(logarch_loc, log),
        )
        self.fmanager.input(
            pathlib.Path(self.logs, log),
            pathlib.PurePath(logarch_loc, log),
            check_archive=True,
            provider_id="ecfs",
        )
