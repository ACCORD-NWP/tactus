"""InterpolSstSic."""

import os

from deode.boundary_utils import Boundary

from ..datetime_utils import as_datetime
from ..logs import logger
from .base import Task
from .batch import BatchJob


class InterpolSstSic(Task):
    """Interpolate SST/SIC from the host model to the model geometry.

    Supported host models:
        - IFS
    """

    def __init__(self, config):
        """Construct InterpolSstSic object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.boundary = Boundary(config)

        self.outfile = self.config["file_templates.sstfile.archive"]
        self.target = (
            f"{self.platform.get_system_value('intp_bddir')}" + "/" + f"{self.outfile}"
        )
        self.gl = self.get_binary("gl")

        self.name = f"{self.name}"

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
        bddir_sst = self.config["system.bddir_sst"]

        for bd_index, bd_time in self.boundary.bd_index_time_dict.items():
            merge_ocean_models = ""
            merge_ocean_files = ""
            for sstmodel in self.config["boundaries.sstmodels"]:
                if sstmodel == "IFS":
                    infile: str = self.config["file_templates.bdfile_sst.archive"]
                    # Default to 0 for bdmember if no bdmember specified. This is to
                    # be able to reference files created by marsprep, which contains
                    # bdmember = 0 even for "deterministic" runs.
                    if not self.config["boundaries.ifs.bdmember"]:
                        infile = infile.replace("@BDMEMBER@", "0")

                    infile = self.platform.substitute(
                        infile,
                        basetime=self.boundary.bd_basetime,
                        validtime=as_datetime(bd_time),
                    )
                    self.fmanager.input(
                        f"{bddir_sst}/{infile}",
                        infile,
                        basetime=self.boundary.bd_basetime,
                        validtime=as_datetime(bd_time),
                    )
                else:
                    raise NotImplementedError(f"SST model '{sstmodel}' not implemented")

                merge_ocean_models = f"{merge_ocean_models}'{sstmodel}',"
                merge_ocean_files = f"{merge_ocean_files}'{infile}',"

            sst_is_lsm = self.config["boundaries.sst_is_lsm"]

            # ADJUST_SST_UNDER_ICE must be TRUE only if sice is used
            adjust_sst_under_ice = (
                ".TRUE." if self.config["general.surfex_sea_ice"] == "sice" else ".FALSE."
            )

            # Create namelist for gl
            with open("namgl", "w") as namelist:
                namelist.write(
                    f"""&naminterp
      MERGE_OCEAN_MODELS={merge_ocean_models}
      MERGE_OCEAN_FILES={
          self.platform.substitute(
              merge_ocean_files,
              basetime=self.boundary.bd_basetime,
              validtime=as_datetime(bd_time)
          )
      }
      CLIMATE_FILE='{climfile}',
      OUTKEY%DATE={self.basetime.strftime("%Y%m%d")},
      OUTKEY%TIME={self.basetime.strftime("%H%M%S")},
      OUTKEY%ENDSTEP={
          self.platform.substitute(
              "@LL@",
              basetime=self.boundary.bd_basetime,
              validtime=as_datetime(bd_time)
          )
      },
      ADJUST_SST_UNDER_ICE={adjust_sst_under_ice},
      SST_IS_LSM='{sst_is_lsm}'
    /
                        """
                )
                namelist.close()

            # Run gl
            outfile = self.platform.substitute(self.outfile, bd_index=bd_index)
            target = self.platform.substitute(self.target, bd_index=bd_index)
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(f"{self.gl} -sst3 -n namgl -o {outfile}")

            logger.debug("WRKDIR: {}", self.wrk)
            logger.debug("OUTPUT {}", outfile)
            self.fmanager.output(outfile, target)
