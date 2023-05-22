"""Initial_conditions."""


import os

from .datetime_utils import as_datetime, as_timedelta
from .logs import get_logger_from_config
from .toolbox import Platform


class InitialConditions(object):
    """FirstGuess."""

    def __init__(self, config):
        """Construct FirstGuess object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        self.config = config
        self.logger = get_logger_from_config(config)
        self.platform = Platform(self.config)
        self.wrk = self.platform.get_value("system.wrk")

        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.cycle_length = as_timedelta(
            self.config.get_value("general.times.cycle_length")
        )
        self.archive = self.config.get_value("system.archive")
        self.file_templates = self.config.get_value("file_templates").dict()

    def nosuccess(self, f1, f2, fail=True):
        """Report of not success.

        Args:
            f1 (str) : file1
            f2 (str) : file1
            fail (boolean) : Create exception or not

        Raises:
            FileNotFoundError : if fail is True

        """
        self.logger.warning("Could not find:\n  %s\n  %s", f1, f2)
        if fail:
            raise FileNotFoundError("Could not find any initial files")

    def find_initial_files(self):
        """Find initial file."""
        # Find data explicitly defined
        init_defined = False
        try:
            source = self.config.get_value("general.initfile")
            source_sfx = self.config.get_value("general.initfile_sfx")
            self.logger.debug("Defined source %s", source)
            self.logger.debug("Defined source_sfx %s", source_sfx)
            init_defined = True
        except AttributeError:  # noqa
            pass

        if init_defined:
            if os.path.exists(source) and os.path.exists(source_sfx):
                return source, source_sfx
            else:
                self.nosuccess(source, source_sfx)

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
            self.logger.info("Found initial files\n  %s\n  %s", source, source_sfx)
            return source, source_sfx

        self.nosuccess(source, source_sfx, False)

        # Find data prepared by Prep and the boundary interpolation
        source = self.platform.substitute(f"{self.wrk}/ELSCF@CNMEXP@ALBC000")
        source_sfx = self.platform.substitute(f"{self.archive}/ICMSH@CNMEXP@INIT.sfx")

        if os.path.exists(source) and os.path.exists(source_sfx):
            self.logger.info("Found initial files\n  %s\n  %s", source, source_sfx)
            return source, source_sfx

        self.nosuccess(source, source_sfx)
        return "", ""
