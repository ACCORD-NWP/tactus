"""E927."""

import os

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
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

        self.cnmexp = self.config.get_value("general.cnmexp")
        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.forecast_range = self.config.get_value("general.forecast_range")

        self.bdint = self.config.get_value("general.bdint")
        self.bdcycle = as_timedelta(config.get_value("general.bdcycle"))
        self.bdnr = config.get_value("task.args.bd_nr")
        self.bd_time = config.get_value("task.args.bd_time")
        self.bddir = self.config.get_value("system.bddir")
        self.bdfile_template = self.config.get_value("system.bdfile_template")
        self.bdclimdir = self.platform.get_system_value("bdclimdir")

        self.name = f"{self.name}_{self.bdnr}"

        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

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

        bd_basetime = self.basetime - cycle_offset(self.basetime, self.bdcycle)

        # Input file
        bdnr = int(self.bdnr)
        initfile = f"ICMSH{self.cnmexp}INIT"
        self.fmanager.input(
            f"{self.bddir}/{self.bdfile_template}",
            initfile,
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        target = f"{self.wrk}/ELSCF{self.cnmexp}ALBC{bdnr:03d}"
        self.fmanager.output(f"PF{self.cnmexp}000+0000", target)
