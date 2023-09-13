"""Initial_conditions."""
import contextlib
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
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        self.archive = self.config["system.archive"]
        self.file_templates = self.config["file_templates"].dict()

    def nosuccess(self, f1, f2, fail=True):
        """Report of not success.

        Args:
            f1 (str) : file1
            f2 (str) : file1
            fail (boolean) : Create exception or not

        Raises:
            FileNotFoundError : if fail is True

        """
        logger.warning("Could not find:\n  {}\n  {}", f1, f2)
        if fail:
            raise FileNotFoundError("Could not find any initial files")

    def find_initial_files(self):
        """Find initial file."""
        # Find data explicitly defined
        init_defined = False
        with contextlib.suppress(KeyError):
            source = self.config["general.initfile"]
            source_sfx = self.config["general.initfile_sfx"]
            logger.debug("Defined source {}", source)
            logger.debug("Defined source_sfx {}", source_sfx)
            init_defined = True

        if init_defined:
            if os.path.exists(source) and os.path.exists(source_sfx):
                return source, source_sfx
            else:
                self.nosuccess(source, source_sfx)

        # Find data prepared by Prep and the boundary interpolation
        source = self.platform.substitute(f"{self.intp_bddir}/ELSCF@CNMEXP@ALBC000")
        source_sfx = self.platform.substitute(f"{self.archive}/ICMSH@CNMEXP@INIT.sfx")

        if os.path.exists(source) and os.path.exists(source_sfx):
            logger.info("Found initial files\n  {}\n  {}", source, source_sfx)
            return source, source_sfx

        self.nosuccess(source, source_sfx, False)

        # Find data from previous forecast
        pdtg = self.basetime - self.cycle_length

        source = self.platform.substitute(
            f"{self.archive}/{self.file_templates['history']}",
            basetime=pdtg,
            validtime=self.basetime,
        )
        source_sfx = self.platform.substitute(
            f"{self.archive}/{self.file_templates['surfex']}",
            basetime=pdtg,
            validtime=self.basetime,
        )

        if os.path.exists(source) and os.path.exists(source_sfx):
            logger.info("Found initial files\n  {}\n  {}", source, source_sfx)
            return source, source_sfx

        self.nosuccess(source, source_sfx)
        return "", ""
