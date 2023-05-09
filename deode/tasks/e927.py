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
        try:
            self.bdnr = config.get_value("task.args.bd_nr")
            self.bd_time = config.get_value("task.args.bd_time")
        except AttributeError:
            self.bdnr = "1"
            self.bd_time = "2023-02-19T09:00:00Z"

        self.name = f"{self.name}_{self.bdnr}"

        self.cnmexp = self.config.get_value("general.cnmexp")
        self.bdclimdir = self.platform.get_system_value("bdclimdir")

        self.nlgen = NamelistGenerator(self.config, "master")
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

        # Fix basetime for PT00H,PT12H only
        basetime = self.basetime
        offset = int(basetime.strftime("%H")) % 12
        time_period = f"PT{offset}H"
        bd_basetime = basetime - as_timedelta(time_period)
        bddir = self.config.get_value("system.bddir")
        bdfile_template = self.config.get_value("system.bdfile_template")

        # Input file
        bdnr = int(self.bdnr)
        print("e927_bdnr: ", bdnr)
        print("e927_bd_time: ", self.bd_time)
        print("e927_bd_basetime: ", bd_basetime)
        initfile = f"ICMSH{self.cnmexp}INIT"
        self.fmanager.input(
            f"{bddir}/{bdfile_template}",
            initfile,
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        target = f"{self.wrk}/ELSCF{self.cnmexp}ALBC{bdnr:03d}"
        self.fmanager.output(f"PF{self.cnmexp}000+0000", target)
        self.remove_links([initfile, "ncf927"])
