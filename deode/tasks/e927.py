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
        Task.__init__(self, config, __class__.__name__)

        self.climdir = self.platform.get_system_value("climdir")
        self.archive = self.platform.get_system_value("archive")

        self.cnmexp = self.config["general.cnmexp"]
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]

        self.bdmodel = self.config["boundaries.bdmodel"]
        self.bdint = self.config["boundaries.bdint"]
        bdcycle = as_timedelta(config["boundaries.bdcycle"])
        bdcycle_start = as_timedelta(config["boundaries.bdcycle_start"])
        bdshift = as_timedelta(config["boundaries.bdshift"])
        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime, bdcycle, bdcycle_start=bdcycle_start, bdshift=-bdshift
        )

        self.intp_bddir = self.config["system.intp_bddir"]
        self.bd_index = config["task.args.bd_index"]
        self.bd_time = config["task.args.bd_time"]
        self.bddir = self.config["system.bddir"]
        self.bdfile_template = self.config["system.bdfile_template"]
        self.bdclimdir = self.platform.get_system_value("bdclimdir")

        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = self.get_binary("MASTERODB")

        self.name = f"{self.name}_{self.bd_index}"

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

        # Input file
        bd_index = int(self.bd_index)
        initfile = f"ICMSH{self.cnmexp}INIT"
        self.fmanager.input(
            f"{self.bddir}/{self.bdfile_template}",
            initfile,
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        target = f"{self.intp_bddir}/ELSCF{self.cnmexp}ALBC{bd_index:03d}"
        self.fmanager.output(f"PF{self.cnmexp}000+0000", target)
        self.archive_logs("NODE.001_01")
