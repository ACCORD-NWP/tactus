"""E927."""

import os

import f90nml

from ..datetime_utils import as_datetime, as_timedelta
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

        self.iterator = self.config.get_value("general.iterator")
        print(f"ITERATOR:{self.iterator}")

        self.cnmexp = self.config.get_value("general.cnmexp")
        self.bdclimdir = self.platform.get_system_value("bdclimdir")

        self.namelist_path = self.platform.get_platform_value("NAMELISTS")
        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

    def load_namelist(self, namelist):
        """Read and adjust namelist.

        Args:
            namelist (str) : namelist file

        Returns :
            nam (f90nml object): loaded namelist
        """
        self.logger.info("Read namelist: %s", namelist)
        nam = f90nml.read(namelist)
        nam.uppercase = True
        nam.end_comma = True
        return nam

    def write_namelist(self, nam):
        """Write namelist with uppercase and commas.

        Args:
            nam (f90nml object) : namelist object to write
        """
        nam.uppercase = True
        nam.end_comma = True
        with open("fort.4", "w", encoding="utf-8") as nml_file:
            f90nml.write(nam, nml_file)

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
        namelist = f"{self.namelist_path}/fort.4_e927"
        nam = self.load_namelist(namelist)
        self.write_namelist(nam)

        # Forecast range
        cdtg = self.basetime

        # Fix basetime for PT00H,PT12H only
        basetime = self.basetime
        offset = int(basetime.strftime("%H")) % 12
        time_period = f"PT{offset}H"
        basetime = basetime - as_timedelta(time_period)
        bddir = self.config.get_value("system.bddir")
        bdfile_template = self.config.get_value("system.bdfile_template")

        # Iterates (controlled from suites.py)
        iterator = int(self.iterator)
        # Input file
        initfile = f"ICMSH{self.cnmexp}INIT"
        self.fmanager.input(f"{bddir}/{bdfile_template}", initfile, basetime=basetime, validtime=cdtg)

        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        target = f"{self.wrk}/ELSCF{self.cnmexp}ALBC{iterator:03d}"
        self.fmanager.output(f"PF{self.cnmexp}000+0000", target)
        self.remove_links([initfile, "ncf927"])
