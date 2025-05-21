"""CreateGrib."""

import contextlib
import os

from ..datetime_utils import as_datetime, oi2dt_list
from ..logs import logger
from .base import Task
from .batch import BatchJob


class GlGrib(Task):
    """Create grib files."""

    def __init__(self, config, name=""):
        """Construct create grib object.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Task name
        """
        Task.__init__(self, config, name)

        self.tasknr = int(config.get("task.args.tasknr", "0"))
        self.ntasks = int(config.get("task.args.ntasks", "1"))
        self.archive = self.platform.get_system_value("archive")
        self.file_templates = self.config["file_templates"]
        self.csc = self.config["general.csc"]
        self.gl = self.get_binary("gl")

        if self.name == "CreateGribStatic":
            self.rules = self.config.get(f"creategrib.{self.name}", {})

        elif self.name == "CreateGrib":
            self.conversions = self.config.get(f"creategrib.{self.name}.conversions", {})
            self.rules = {
                filetype: self.config.get(
                    f"creategrib.{self.name}.{filetype}", {"namelist": []}
                )
                for filetype in self.conversions
            }
            self.output_settings = self.config["general.output_settings"]
            self.forecast_range = self.config["general.times.forecast_range"]
            self.basetime = as_datetime(self.config["general.times.basetime"])

            self.name = f"{self.name}_{self.tasknr:02}"

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

        Namelist arguments are given in the creategrib.NAME config part
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

        gl_namelist = None

        with contextlib.suppress(KeyError):
            gl_namelist = (
                self.rules[filetype][self.csc]["namelist"]
                if self.csc in self.rules[filetype]
                else self.rules[filetype]["namelist"]
            )

        if gl_namelist is not None:
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
        if self.name == "CreateGribStatic":
            for filetype, rules in self.rules.items():
                for i, _fname in enumerate(rules["files_in"]):
                    fname = self.platform.substitute(_fname)
                    output = self.platform.substitute(rules["files_out"][i])
                    outfile = os.path.basename(output)
                    logger.info("Convert: {} to {}", fname, outfile)
                    self.convert2grib(fname, outfile, filetype)
                    self.fmanager.output(outfile, output)

        elif "CreateGrib_" in self.name:
            convert_dict = {}
            for filetype in self.conversions:
                if "output_frequency_reference" not in self.rules[filetype]:
                    file_handle = self.create_list(
                        self.file_templates[filetype]["archive"],
                        self.output_settings[filetype],
                    )
                else:
                    file_handle = self.create_list(
                        self.file_templates[filetype]["archive"],
                        self.output_settings[
                            self.rules[filetype]["output_frequency_reference"]
                        ],
                    )
                for validtime, fname in file_handle.items():
                    output = self.platform.substitute(
                        self.file_templates[filetype]["grib"], validtime=validtime
                    )
                    if validtime not in convert_dict:
                        convert_dict[validtime] = []
                    convert_dict[validtime].append(
                        {"fname": fname, "output": output, "filetype": filetype}
                    )

            convert_list = [x for t in sorted(convert_dict) for x in convert_dict[t]]
            for items in convert_list[self.tasknr :: self.ntasks]:
                fname, output, filetype = (
                    items["fname"],
                    items["output"],
                    items["filetype"],
                )
                logger.info("Convert: {} to {} for {}", fname, output, filetype)
                self.convert2grib(fname, output, filetype)
                self.fmanager.output(output, f"{self.archive}/{output}")

        else:
            raise NotImplementedError(f"Not implemented name={self.name}")


class CreateGrib(GlGrib):
    """Convert time dependent files."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        GlGrib.__init__(self, config, __class__.__name__)


class CreateGribStatic(GlGrib):
    """Convert static files."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        GlGrib.__init__(self, config, __class__.__name__)
