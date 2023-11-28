"""CreateGrib."""


import os

from ..datetime_utils import as_datetime, oi2dt_list
from ..logs import logger
from .base import Task
from .batch import BatchJob


class CreateGrib(Task):
    """Create grib files."""

    def __init__(self, config):
        """Construct create grib object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.archive = self.platform.get_system_value("archive")

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]

        try:
            self.conversions = self.config[f"task.{self.name}.conversions"]
        except KeyError:
            self.conversions = {}

        self.output_settings = self.config["general.output_settings"]
        self.file_templates = self.config["file_templates"]

        self.gl = self.get_binary("gl")

    def create_list(self, input_template, output_settings):
        """Create list of files to process."""
        files = {}
        # Store the output
        dt_list = oi2dt_list(output_settings, self.forecast_range)
        for dt in dt_list:
            validtime = self.basetime + dt
            fname = self.platform.substitute(input_template, validtime=validtime)
            files[validtime] = f"{self.archive}/{fname}"

        return files

    def convert2grib(self, infile, outfile):
        """Convert FA to grib.

        Args:
            infile (str): Input file
            outfile (str): Output file

        Raises:
            FileNotFoundError: Could not find file
        """
        if not os.path.isfile(infile):
            raise FileNotFoundError(f" missing {infile}")

        cmd = f"{self.gl} -p {infile} -o {outfile}"
        if infile.endswith(".sfx"):
            # Convert surfex files
            namelist_file = "namsfx"
            with open(namelist_file, "w") as namelist:
                namelist.write(
                    """&naminterp
  stepunits = 'h'
/
              """
                )
                namelist.close()

            cmd = f"{cmd} -igs -of GRIB -n {namelist_file}"
        else:
            # Convert other files files
            cmd = f"{cmd} -of GRIB2"

        # Run gl
        BatchJob(os.environ, wrapper=self.wrapper).run(cmd)

    def execute(self):
        """Execute creategrib."""
        for filetype in self.conversions:
            logger.info("***", self.conversions)
            file_handle = self.create_list(
                self.file_templates[filetype]["archive"], self.output_settings[filetype]
            )

            for validtime, fname in file_handle.items():
                output = self.platform.substitute(
                    self.file_templates[filetype]["grib"], validtime=validtime
                )
                logger.info("Convert: {} to {}", fname, output)
                self.convert2grib(fname, output)
                self.fmanager.output(output, f"{self.archive}/{output}")
