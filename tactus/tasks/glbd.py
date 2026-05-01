"""GlBd."""

import os

from tactus.boundary_utils import Boundary
from tactus.datetime_utils import as_datetime, as_timedelta
from tactus.logs import logger
from tactus.namelist import NamelistGenerator
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob


class GlBd(Task):
    """Interpolate boundary files with GL."""

    def __init__(self, config):
        """Construct GlBd object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.boundary = Boundary(config)

        self.basetime = as_datetime(self.config["general.times.basetime"])

        self.target = (
            f"{self.platform.get_system_value('intp_bddir')}"
            + "/"
            + f"{self.config['file_templates.interpolated_boundaries.archive']}"
        )

        self.climdir = self.platform.get_system_value("climdir")
        self.bddir = self.config["system.bddir"]
        self.bdfile_template = self.config.get("file_templates.bdfile.archive")
        self.bdtype = self.config.get("system.bdtype", "HRES")

        self.nlgen = NamelistGenerator(self.config, "gl")
        self.binary = self.get_binary("gl")

        self.name = (
            f"{self.name}_{self.boundary.min_index}-{self.boundary.max_index}"
        ).upper()

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # File templates, etc.
        bd_path_template = f"{self.bddir}/{self.bdfile_template}"
        logger.info("bd_path_template = {}", bd_path_template)
        gl_infile = "gl_input_file"
        gl_outfile = "tmpfile"
        s3_path_template = self.config.get("system.s3_path_template")
        logger.info("s3_path_template = {}", s3_path_template)
        slaflag = self.config.get("boundaries.slaflag", "PT0H")
        slafdiff = self.config.get("boundaries.slafdiff", "PT0H")
        bdtype = self.bdtype
        if slaflag != "PT0H" and slafdiff != "PT0H":
            logger.info("slaflag={}, slafdiff={}", slaflag, slafdiff)
            bdtype = "SLAF"
            basetime2 = as_datetime(self.boundary.bd_basetime) - as_timedelta(slaflag)
            basetime1 = basetime2 + as_timedelta(slafdiff)
            logger.info("basetime1={}", basetime1)
            logger.info("basetime2={}", basetime2)

        # Climate file, inside loop??
        mm = self.basetime.strftime("%m")
        self.fmanager.input("{}/Const.Clim.{}".format(self.climdir, mm), "climate_aladin")

        # Namelist
        nlfile = "naminterp"
        nltarget = f"gl_bd_{bdtype}"
        self.nlgen.generate_namelist(nltarget, nlfile)

        # Loop over boundary files for this batch
        for bd_index, bd_time in self.boundary.bd_index_time_dict.items():
            validtime = as_datetime(bd_time)
            # Remove eventual old symlinks
            for f in [gl_infile, "oro_file", "file1", "file2", gl_outfile]:
                if os.path.exists(f):
                    os.remove(f)
            # Get input file, possibly via S3
            self.get_bdfile(
                bd_path_template,
                gl_infile,
                self.boundary.bd_basetime,
                validtime,
                s3_path_template,
            )
            # Any extra files needed (orography or for SLAF)?
            if bdtype == "IFSENS":
                oro_file = "oro_file"
                self.get_bdfile(
                    bd_path_template,
                    oro_file,
                    self.boundary.bd_basetime,
                    self.boundary.bd_basetime,
                    s3_path_template,
                )
            elif bdtype == "SLAF":
                self.get_bdfile(
                    bd_path_template, "file1", basetime1, validtime, s3_path_template
                )
                self.get_bdfile(
                    bd_path_template, "file2", basetime2, validtime, s3_path_template
                )

            # Run binary
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            cmd = f"{self.binary} -lbc ifs {gl_infile} -o {gl_outfile} -d -s -n {nlfile}"
            batch.run(cmd)

            # Store output
            this_target = self.target.replace("@NNN@", f"{bd_index:03}")
            self.fmanager.output(gl_outfile, this_target)

    def get_bdfile(
        self, bdfile_template, local_file, basetime, validtime, s3_path_template=None
    ):
        """Helper function to possibly download a file via S3 before trying to find it.

        Args:
           bdfile_template:  str
           local_file:  Path
           basetime:  datetime
           validtime:  datetime
           s3_path_template:  str

        """
        if s3_path_template:
            self.fmanager.input(
                s3_path_template,
                bdfile_template,
                basetime=basetime,
                validtime=validtime,
                provider_id="s3",
            )
        self.fmanager.input(
            bdfile_template,
            local_file,
            basetime=basetime,
            validtime=validtime,
        )
