"""CreateGrib."""


import os
import re

from ..datetime_utils import as_datetime, as_timedelta, dt2str
from .base import Task
from .batch import BatchJob


class Fileconv(BatchJob):
    """FA conversions."""

    def __init__(
        self,
        basetime,
        forecast_range=None,
        output_interval=None,
        input_template="ICMSHHARM+duration.sfx",
        output_template="sfx_basetime+duration.grib",
        src_dir=".",
        target_dir=None,
        binary="gl",
        wrapper="",
    ):
        """Construct the FA conversion object.

        Args:
            basetime (str): Initital time for forecast
            forecast_range (str): Forecast range
            output_interval (str): output interval
            input_template (str): template for input files
            output_template (str): template for output files
            src_dir (str): location of input files
            target_dir (str): location of output files
            binary (str): Link to binary to be used
            wrapper (str): prefix to binary
        """
        self.wrapper = wrapper
        self.rte = os.environ
        BatchJob.__init__(self.rte, self.wrapper)

        self.basetime = as_datetime(basetime)
        self.src_dir = src_dir
        if target_dir is None:
            self.target_dir = self.src_dir
        self.input_template = input_template
        self.output_template = output_template
        if forecast_range is not None:
            self.forecast_range = as_timedelta(forecast_range)
        if output_interval is not None:
            self.output_interval = as_timedelta(output_interval)
        self.binary = binary

        # Create the list of files to work with
        if forecast_range is None and output_interval is None:
            self.files = self.find_files(
                src_dir, input_template.replace("+duration", r"\+(.*)")
            )
        else:
            self.files = self.create_list()

    def find_files(self, path, pattern):
        """Find files with pattern in path.

        Args:
            path (str): path to files
            pattern (str): pattern to match

        Returns:
            result (dict): dict of duration and file names
        """

        def fname2dur(filename):
            pattern = re.compile("(\\d{4})\\:(\\d{2})\\:(\\d{2})")
            try:
                h, m, s = [int(x) for x in pattern.findall(filename)[0]]
                return as_timedelta(f"PT{h}H{m}M{s}S")
            except IndexError:
                err = f"Cannot find duration from {filename}"
                raise IndexError(err)

        result = []
        it = os.scandir(path)
        for entry in it:
            if not entry.name.startswith(".") and entry.is_file():
                if re.search(pattern, entry.name):
                    result.append(entry.name)
            if not entry.name.startswith(".") and entry.is_dir():
                subresult = self.find_files(os.path.join(path, entry.name), pattern)
                subresult = [entry.name + "/" + e for e in subresult]
                result.extend(subresult)

        result = {fname2dur(x): f"{path}/{x}" for x in result}
        return result

    def create_list(self):
        """Create list of files to process."""
        cdtg = self.basetime
        dtgend = self.basetime + self.forecast_range
        files = {}
        while cdtg <= dtgend:
            dt = cdtg - self.basetime
            duration = dt2str(dt)
            files[
                dt
            ] = f"{self.src_dir}/{self.input_template.replace('duration',duration)}"
            cdtg += self.output_interval

        return files

    def convert2grib(self, steps=None):
        """Convert FA to grib.

        Args:
            steps (str,list): Steps to process

        Raises:
            FileNotFoundError: Could not find file
        """

        def find_cmd(infile, outfile):
            glcmd = f"{self.binary} -p {infile} -o {outfile} -"
            if "sfx" in self.input_template:
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

                glcmd = f"{glcmd} -igs -of GRIB -n {namelist_file}"
            else:
                # Convert other files files
                glcmd = f"{glcmd} -of GRIB2"

            return glcmd

        if steps is None:
            step = list(self.files.keys())
        elif isinstance(steps, str):
            step = [as_timedelta(steps)]
        else:
            step = [as_timedelta(k) for k in steps]

        # Loop over all given steps and convert
        for k in step:
            output = self.output_template.replace(
                "basetime", self.basetime.strftime("%Y-%m-%dT%H:%M:%S")
            )
            output = output.replace("duration", dt2str(k))
            output = f"{self.target_dir}/{output}"
            if os.path.isfile(self.files[k]):
                cmd = find_cmd(self.files[k], output)
                self.run(cmd)
            else:
                raise FileNotFoundError(f" missing {self.files[k]}")


class CreateGrib(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct create grib object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.cnmexp = self.config.get_value("general.cnmexp")
        self.domain = self.config.get_value("domain.name")
        self.archive = self.platform.get_system_value("archive")

        self.basetime = self.config.get_value("general.times.basetime")
        self.forecast_range = self.config.get_value("general.forecast_range")

        self.conversions = self.config.get_value(f"task.{self.name}.conversions").dict()

        conversions_default = {
            "his": {
                "input_template": f"ICMSH{self.cnmexp}+duration",
                "output_template": "his_basetime+duration.grib2",
            },
            "sfx": {
                "input_template": f"ICMSH{self.cnmexp}+duration.sfx",
                "output_template": "sfx_basetime+duration.grib",
            },
            "sfx_sel": {
                "input_template": "ICMSHSELE+duration.sfx",
                "output_template": "sfx_sel_basetime+duration.grib",
            },
        }
        for opt in ["input_template", "output_template"]:
            for k, v in self.conversions.items():
                if opt not in v:
                    self.conversions[k][opt] = conversions_default[k][opt]

        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")
        self.gl = f"{self.platform.get_system_value('bindir')}/gl"  # noqa

    def execute(self):
        """Execute creategrib."""
        for k, v in self.conversions.items():
            output_interval = f"general.output_interval_{k}"

            handle = Fileconv(
                self.basetime,
                self.forecast_range,
                self.config.get_value(output_interval),
                src_dir=self.archive,
                input_template=v["input_template"],
                output_template=v["output_template"],
                binary=self.gl,
                wrapper=self.wrapper,
            )

            handle.convert2grib()
