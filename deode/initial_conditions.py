"""Initial_conditions."""
import os

from .datetime_utils import as_datetime, as_timedelta
from .logs import logger
from .toolbox import Platform


class InitialConditions(object):
    """FirstGuess."""

    def __init__(self, config):
        """Construct FirstGuess object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        self.config = config
        self.platform = Platform(self.config)
        self.wrk = self.platform.get_value("system.wrk")
        self.intp_bddir = self.config["system.intp_bddir"]
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.starttime = as_datetime(self.config["general.times.start"])
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        self.archive = self.config["system.archive"]
        self.file_templates = self.config["file_templates"].dict()
        self.surfex = self.config["general.surfex"]
        self.mode = self.config["suite_control.mode"]

        self.source = ""
        self.source_sfx = ""

    def nosuccess(self):
        """Report of not success."""
        if self.surfex:
            logger.warning("Could not find:\n  {}\n  {}", self.source, self.source_sfx)
        else:
            logger.warning("Could not find:\n  {}", self.source)

    def success(self):
        """Report of success."""
        if self.surfex:
            logger.info(
                "Found initial files for mode={}\n  {}\n  {}",
                self.mode,
                self.source,
                self.source_sfx,
            )
        else:
            logger.info("Found initial file for mode={}\n  {}", self.mode, self.source)

    def check_if_found(self):
        """Check if files are present."""
        found = os.path.exists(self.source)
        if self.surfex:
            found = found and os.path.exists(self.source_sfx)
        if found:
            self.success()
        else:
            self.nosuccess()

        return found

    def find_initial_files(self):
        """Find initial file."""
        # Find data explicitly defined
        if self.mode == "restart" and self.starttime == self.basetime:
            pdtg = self.basetime - self.cycle_length
            initfile = self.config["general.initfile"]
            initfile_sfx = self.config["general.initfile_sfx"]
            self.source = self.platform.substitute(
                initfile,
                basetime=pdtg,
                validtime=self.basetime,
            )
            self.source_sfx = self.platform.substitute(
                initfile_sfx,
                basetime=pdtg,
                validtime=self.basetime,
            )

        # Find data prepared by Prep and the boundary interpolation
        elif self.mode == "cold_start" or self.starttime == self.basetime:
            self.source = self.platform.substitute(
                f"{self.intp_bddir}/ELSCF@CNMEXP@ALBC000"
            )
            self.source_sfx = self.platform.substitute(
                f"{self.archive}/ICMSH@CNMEXP@INIT.sfx"
            )

        else:
            # Find data from previous forecast
            pdtg = self.basetime - self.cycle_length

            self.source = self.platform.substitute(
                f"{self.archive}/{self.file_templates['history']['archive']}",
                basetime=pdtg,
                validtime=self.basetime,
            )
            self.source_sfx = self.platform.substitute(
                f"{self.archive}/{self.file_templates['surfex']['archive']}",
                basetime=pdtg,
                validtime=self.basetime,
            )

        if not self.check_if_found():
            raise FileNotFoundError(
                "Could not find initial files for "
                f"mode={self.mode}, {self.source}, {self.source_sfx}"
            )

        return self.source, self.source_sfx
