"""E923Update."""

import glob
import os

from ..datetime_utils import as_datetime
from ..logs import logger
from .base import Task
from .batch import BatchJob


class E923Update(Task):
    """Methods for updating climfile for ALARO.

    For mor informations see README in bindir

    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, "E923Update")

        self.climdir = self.platform.get_system_value("climdir")
        self.bindir = self.platform.get_system_value("fa_sfx2clim_bin")
        logger.info(f"Name: {self.name}")
        self.fa_sfx2clim = self.get_binary("fa_sfx2clim")

        self.archive = self.config["system.archive"]
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.month = self.basetime.strftime("%m")

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Old climfile
        climfile = f"{self.climdir}/Const.Clim.{self.month}"

        # PGD
        pgd_file = self.platform.substitute(
            self.config["file_templates.pgd.archive"], basetime=self.basetime
        )
        self.fmanager.input(f"{self.climdir}/{pgd_file}", "pgd_file")
        # sfx init
        sfx_init = self.platform.substitute(f"{self.archive}/ICMSH@CNMEXP@INIT.sfx")
        self.fmanager.input(sfx_init, "input_sfx")
        # Old climfile
        climfile = f"{self.climdir}/Const.Clim.{self.month}"

        # namelist
        with open("nam", "w") as namelist:
            namelist.write(
                """
&NAM
  L_Z0=.T.,
  LZ0THER=.F.,
  FACZ0=0.53,
  FACZ0_VEG=1.00,
  NLISSZ=3,
  NLISSZ_VEG=3,
/
"""
            )
            namelist.close()

        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(f"{self.fa_sfx2clim} nam pgd_file input_sfx {climfile}")

        self.archive_logs(glob.glob("NODE.*"), target=self.climdir)
