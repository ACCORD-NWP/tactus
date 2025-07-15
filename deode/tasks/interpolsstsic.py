"""InterpolSstSic."""

import os

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..logs import logger
from .base import Task
from .batch import BatchJob
from .marsprep import Marsprep


class InterpolSstSic(Task):
    """Interpolate SST/SIC from the host model to the model geometry.

    Supported host models:
        - IFS
    """

    def __init__(self, config):
        """Construct InterpolSstSic object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.basetime = as_datetime(self.config["general.times.basetime"])

        mars = Marsprep.mars_selection(
            selection=self.platform.substitute(self.config["boundaries.ifs.selection"]),
            config=self.config,
        )
        bdcycle = as_timedelta(mars["ifs_cycle_length"])
        bdcycle_start = as_timedelta(mars["ifs_cycle_start"])
        bdshift = as_timedelta(self.config["boundaries.bdshift"])
        # Boundary basetime
        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime, bdcycle, bdcycle_start=bdcycle_start, bdshift=-bdshift
        )

        self.bd_index = self.config["task.args.bd_index"]
        self.bd_time = self.config["task.args.bd_time"]

        self.gl = self.get_binary("gl")

        self.name = f"{self.name}_{self.bd_index}"

    def execute(self):
        """Run task.

        Run sequence: set input files, create gl namelist and run gl.

        Raises:
            NotImplementedError: If an unsupported SST model is used
        """
        # Climate file
        climdir = self.platform.get_system_value("climdir")
        climfile = self.platform.substitute(
            self.config["file_templates.pgd.archive"], basetime=self.basetime
        )

        self.fmanager.input(f"{climdir}/{climfile}", climfile)

        # Boundary input file(s)
        marsdir = self.config["system.marsdir"]

        merge_ocean_models = ""
        merge_ocean_files = ""
        for sstmodel in self.config["boundaries.sstmodels"]:
            if sstmodel == "IFS":
                infile = self.platform.substitute(
                    self.config["system.bdfile_sst_template"],
                    basetime=self.bd_basetime,
                    validtime=as_datetime(self.bd_time),
                )
                self.fmanager.input(
                    f"{marsdir}/{infile}",
                    infile,
                    basetime=self.bd_basetime,
                    validtime=as_datetime(self.bd_time),
                )
            else:
                raise NotImplementedError(f"SST model '{sstmodel}' not implemented")

            merge_ocean_models = f"{merge_ocean_models}'{sstmodel}',"
            merge_ocean_files = f"{merge_ocean_files}'{infile}',"

        sst_is_lsm = self.config["boundaries.sst_is_lsm"]

        # ADJUST_SST_UNDER_ICE must be TRUE only if sice is used
        if self.config["general.surfex_sea_ice"] == "sice":
            adjust_sst_under_ice = ".TRUE."
        else:
            adjust_sst_under_ice = ".FALSE."

        # Output file, name hard-coded in source code
        sstfile_template = self.config["system.sstfile_template"]

        intp_bddir = self.platform.get_system_value("intp_bddir")
        outfile = sstfile_template
        target = f"{intp_bddir}/{sstfile_template}"

        # Create namelist for gl
        with open("namgl", "w") as namelist:
            namelist.write(
                f"""&naminterp
  MERGE_OCEAN_MODELS={merge_ocean_models}
  MERGE_OCEAN_FILES={
      self.platform.substitute(
          merge_ocean_files,
          basetime=self.bd_basetime,
          validtime=as_datetime(self.bd_time)
      )
  }
  CLIMATE_FILE='{climfile}',
  OUTKEY%DATE={self.basetime.strftime("%Y%m%d")},
  OUTKEY%TIME={self.basetime.strftime("%H%M%S")},
  OUTKEY%ENDSTEP={
      self.platform.substitute(
          "@LL@",
          basetime=self.bd_basetime,
          validtime=as_datetime(self.bd_time)
      )
  },
  ADJUST_SST_UNDER_ICE={adjust_sst_under_ice},
  SST_IS_LSM='{sst_is_lsm}'
/
                """
            )
            namelist.close()

        # Run gl
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(f"{self.gl} -sst3 -n namgl -o {self.platform.substitute(outfile)}")

        logger.debug("WRKDIR: {}", self.wrk)
        logger.debug("OUTPUT {}", outfile)
        self.fmanager.output(outfile, target)
