"""E927."""

import os

from ..datetime_utils import as_datetime, as_timedelta
from ..namelist import NamelistGenerator
from .base import Task
from .batch import BatchJob


class E927(Task):
    """E927 task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")
        self.climdir = self.platform.get_system_value("climdir")

        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.bdint = self.config.get_value("general.bdint")
        self.forecast_range = self.config.get_value("general.forecast_range")

        self.cnmexp = self.config.get_value("general.cnmexp")
        self.bdclimdir = self.platform.get_system_value("bdclimdir")

        self.nlgen = NamelistGenerator(config, "master")
        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

    def remove_links(self, link):
        """Remove link.

        Args:
            link (list) : List of links to remove
        """
        self.logger.info("clean %s", link)
        for x in link:
            try:
                os.unlink(x)
            except FileNotFoundError:
                self.logger.warning("Could not remove file '%s'.", x, exc_info=True)

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # RTTM input
        for ifile in ["MCICA", "RADSRTM"]:
            self.fmanager.input(f"@RRTM_DIR@/{ifile}", ifile)

        # Climate files
        mm = self.basetime.strftime("%m")
        self.fmanager.input("{}/Const.Clim.{}".format(self.climdir, mm), "const.clim.000")
        self.fmanager.input("{}/Const.Clim.{}".format(self.bdclimdir, mm), "Const.Clim")

        # Namelist
        self.nlgen.generate_namelist("e927", "fort.4")

        # Forecast range
        cdtg = self.basetime
        dtgend = self.basetime + as_timedelta(self.forecast_range)
        i = 0

        # Fix basetime for PT00H,PT12H only
        basetime = self.basetime
        offset = int(basetime.strftime("%H")) % 12
        time_period = f"PT{offset}H"
        basetime = basetime - as_timedelta(time_period)
        bddir = self.config.get_value("system.bddir")
        bdfile_template = self.config.get_value("system.bdfile_template")

        while cdtg <= dtgend:

            # Input file
            initfile = f"ICMSH{self.cnmexp}INIT"
            self.fmanager.input(
                f"{bddir}/{bdfile_template}", initfile, basetime=basetime, validtime=cdtg
            )

            # Run masterodb
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.master)

            target = f"{self.wrk}/ELSCF{self.cnmexp}ALBC{i:03d}"
            self.fmanager.output(f"PF{self.cnmexp}000+0000", target)
            self.remove_links([initfile, "ncf927"])
            cdtg += as_timedelta(self.bdint)
            i += 1
