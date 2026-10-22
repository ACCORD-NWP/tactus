"""E923Update."""

import contextlib
import os

from ..datetime_utils import get_month_list
from .base import Task
from .batch import BatchJob


class E923Update(Task):
    """Methods for updating climfile for ALARO.

    For more information see doc/markdown/e923_update.md.

    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)
        self.months = [
            f"{mm:02d}"
            for mm in get_month_list(
                config["general.times.start"], config["general.times.end"], extend=True
            )
        ]

        self.climdir = self.platform.get_system_value("climdir")
        self.intp_bddir_sfx = self.config["system.intp_bddir_sfx"]
        self.fa_sfx2clim = self.get_binary("fa_sfx2clim")

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # PGD
        pgd_file = self.config["file_templates.pgd.archive"]
        self.fmanager.input(f"{self.climdir}/{pgd_file}", "pgd_file")
        # sfx init
        self.fmanager.input(f"{self.intp_bddir_sfx}/ICMSH@CNMEXP@INIT.sfx", "input_sfx")

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

        # Climate file to be updated
        for month in self.months:
            with contextlib.suppress(FileNotFoundError):
                os.unlink("climfile")
            self.fmanager.input(f"{self.climdir}/Const.Clim.{month}", "climfile")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(f"{self.fa_sfx2clim} nam pgd_file input_sfx climfile")
