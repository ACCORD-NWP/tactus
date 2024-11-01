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
        Task.__init__(self, config, __class__.__name__)

        self.archive = self.platform.get_system_value("archive")

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]

        self.conversions = self.config.get(f"task.{self.name}.conversions", {})

        self.rules = {}
        for filetype in self.conversions:
            try:
                self.rules[filetype] = self.config[f"task.{self.name}.{filetype}"].dict()
            except KeyError:
                self.rules[filetype] = {"namelist": []}
        self.output_settings = self.config["general.output_settings"]
        self.file_templates = self.config["file_templates"]

        self.gl = self.get_binary("gl")
        self.csc = self.config["general.csc"]

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

    def convert2grib(self, infile, outfile, filetype):
        """Convert FA to grib.

        Namelist arguments are given in the task.creategrib config part
        per filetype
        Args:
            infile (str): Input file
            outfile (str): Output file
            filetype (str): filetype to look for in rules
        Raises:
            FileNotFoundError: Could not find file
        """
        if not os.path.isfile(infile):
            raise FileNotFoundError(f" missing {infile}")

        cmd = f"{self.gl} -p {infile} -o {outfile}"
        try:
            of = self.rules[filetype]["output_format"]
            cmd += f" -of {of}"
        except KeyError:
            pass

        gl_namelist = (
            self.rules[filetype][self.csc]["namelist"]
            if self.csc in self.rules[filetype]
            else self.rules[filetype]["namelist"]
        )

        if len(gl_namelist) > 0:
            # Write namelist as given in rules
            namelist_file = "namelist"
            with open(namelist_file, "w") as namelist:
                namelist.write("&naminterp\n")
                for x in gl_namelist:
                    y = self.platform.substitute(x)
                    namelist.write(f"{y}\n")
                namelist.write("/\n")
                namelist.close()
            cmd += f" -n {namelist_file}"

        # Run gl
        BatchJob(os.environ, wrapper=self.wrapper).run(cmd)

    def execute(self):
        """Execute creategrib."""
        for filetype in self.conversions:
            file_handle = self.create_list(
                self.file_templates[filetype]["archive"],
                self.output_settings[filetype],
            )
            for validtime, fname in file_handle.items():
                output = self.platform.substitute(
                    self.file_templates[filetype]["grib"], validtime=validtime
                )
                logger.info("Convert: {} to {}", fname, output)
                self.convert2grib(fname, output, filetype)
                self.fmanager.output(output, f"{self.archive}/{output}")
