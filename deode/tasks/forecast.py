"""Forecast."""
import atexit
import glob
import json
import os

from pysurfex.namelist import InputDataFromNamelist
from pysurfex.platform_deps import SystemFilePathsFromFile

from ..config_parser import ConfigPaths
from ..datetime_utils import as_datetime, as_timedelta, oi2dt_list
from ..derived_variables import check_fullpos_namelist
from ..initial_conditions import InitialConditions
from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import deodemakedirs
from .base import Task
from .batch import BatchJob
from .iomerge import IOMERGE_FILETYPES
from .sfx import PySurfexBaseTask


class Forecast(PySurfexBaseTask):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        PySurfexBaseTask.__init__(self, config, __class__.__name__)

        self.cycle = self.config["general.cycle"]
        self.cnmexp = self.config["general.cnmexp"]
        self.domain = self.config["domain.name"]
        self.windfarm = self.config.get("general.windfarm", False)
        self.forecast_dir_link = self.config["system.forecast_dir_link"]

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        self.intp_bddir = self.config["system.intp_bddir"]
        self.forecast_range = self.config["general.times.forecast_range"]

        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config["platform.deode_home"]
        self.output_settings = self.config["general.output_settings"]
        self.surfex = self.config["general.surfex"]

        self.accelerator_device = self.config.get("accelerator_device", None)

        self.io_server = int(os.environ.get("NPROC_IO", "0")) > 0
        self.iomerge_is_external = (
            self.io_server and self.config["suite_control.n_io_merge"] > 0
        )

        # Update namelist settings
        self.nlgen_master = NamelistGenerator(self.config, "master")
        self.nlgen_surfex = NamelistGenerator(self.config, "surfex")

        self.master = self.get_binary("MASTERODB")

        self.file_templates = self.config["file_templates"]

        self.unix_group = self.platform.get_platform_value("unix_group")

    def post(self):
        """Do special post for Foreast."""
        logger.debug("Forecast class post")
        if not self.iomerge_is_external:
            # Clean workdir
            if self.config["general.keep_workdirs"]:
                self.rename_wdir(prefix="Finished_task_")
            else:
                self.remove_wdir()

    def archive_output(self, filetype, periods):
        """Archive forecast model output.

        Args:
            filetype (str): Filename template
            periods (str): Output list
        """
        dt_list = oi2dt_list(periods, self.forecast_range)
        logger.info("Input template: {}", filetype["model"])
        logger.info("Output template: {}", filetype["archive"])
        for dt in dt_list:
            filename_in = self.platform.substitute(
                filetype["model"], validtime=self.basetime + dt
            )
            filename_out = self.platform.substitute(
                filetype["archive"], validtime=self.basetime + dt
            )
            self.fmanager.output(filename_in, f"{self.archive}/{filename_out}")

    def wfp_input(self):
        """Add wind turbine files to forecast directory."""
        self.wfp_dir = self.platform.get_platform_value("windfarm_path")

        yy = self.basetime.strftime("%Y")
        self.fmanager.input(
            f"{self.wfp_dir}/wind_turbine_coordinates_{self.domain}_{yy}.tab",
            "wind_turbine_coordinates.tab",
        )

        turbine_list = glob.glob(f"{self.wfp_dir}/wind_turbine_[0-9][0-9][0-9].tab")
        for ifile in turbine_list:
            infile = os.path.basename(ifile)
            self.fmanager.input(ifile, infile)

    def sst_sic_input(self, intp_bddir):
        """Link SST & SIC input file to forecast directory."""
        current_datetime = self.basetime
        end_datetime = self.basetime + as_timedelta(self.forecast_range)
        i = 0
        while current_datetime <= end_datetime:
            source = self.platform.substitute(
                self.config["file_templates.sstfile.archive"], bd_index=i
            )
            self.fmanager.input(f"{intp_bddir}/{source}", source)
            current_datetime += self.bdint
            i += 1

    def accelerator_device_input(self):
        """Copy the input files for GPU execution.

        - parallel_method: input file with parallelisation technique for each algorithm
        - synchost: input file defining optional device-to-host memory transfers
        - select_gpu: wrapper file for sbatch, binding GPU to MPI rank
        """
        for key, file_definition in self.accelerator_device.items():
            if key in ["parallel_method", "sync_host", "select_gpu"]:
                input_file = file_definition[0]
                output_file = file_definition[1]
                if input_file and output_file:
                    self.fmanager.input(input_file, output_file)

    def merge_output(self, filetype, periods):
        """Merge distributed forecast model output.

        Args:
            filetype (str): File type (history, surfex, fullpos)
            periods (str): Output list

        Final result is the expected output file name in the working directory
        (as if there was no IO server).
        NOTE: This function has been replaced by io_merge tasks.
        It is only called if n_io_merge=0.
        """
        dt_list = oi2dt_list(periods, self.forecast_range)

        for dt in dt_list:
            ftemplate = self.file_templates[filetype]["model"]
            filename = self.platform.substitute(ftemplate, validtime=self.basetime + dt)
            logger.debug("Merging file {}", filename)
            if filetype == "history":
                lfitools = self.get_binary("lfitools")
                cmd = f"{lfitools} facat all io_serv*.d/{filename}.gridall "
                cmd += f"io_serv*.d/{filename}.speca* {filename}"
                logger.debug(cmd)
                BatchJob(os.environ, wrapper="").run(cmd)

            elif filetype == "surfex":
                # NOTE: .sfx also has a part in the working directory,
                #        so you *must* change the name
                lfitools = self.get_binary("lfitools")
                os.rename(filename, filename + ".part")
                cmd = f"{lfitools} facat all {filename}.part "
                cmd += f"io_serv*.d/{filename} {filename}"
                logger.debug(cmd)
                BatchJob(os.environ, wrapper="").run(cmd)

            else:
                # Fullpos (grib2) output has .hpf as extra file extension
                cmd = f"cat io_serv*.d/{filename}*.hfp > {filename}"
                logger.debug(cmd)
                BatchJob(os.environ, wrapper="").run(cmd)

    def execute(self):
        """Execute forecast."""
        # Fetch forecast model static input data
        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value("forecast_input_definition")
        )
        logger.info("Read static data spec from: {}", input_definition)
        with open(input_definition, "r", encoding="utf-8") as f:
            input_data = json.load(f)
        self.fmanager.input_data_iterator(input_data)

        # wind farm input data
        if self.windfarm:
            self.wfp_input()

        # Construct master namelist and include fullpos config
        forecast_namelist = "forecast"
        self.nlgen_master.load(forecast_namelist)
        self.nlgen_master = check_fullpos_namelist(self.config, self.nlgen_master)

        nlres = self.nlgen_master.assemble_namelist(forecast_namelist)
        self.nlgen_master.write_namelist(nlres, "fort.4")

        # SURFEX: Namelists and input data
        self.nlgen_surfex.load("forecast")
        settings = self.nlgen_surfex.assemble_namelist("forecast")
        self.nlgen_surfex.write_namelist(settings, "EXSEG1.nam")

        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value("sfx_input_definition")
        )
        with open(input_definition, "r", encoding="utf-8") as f:
            input_data = json.load(f)
        binput_data = InputDataFromNamelist(
            settings,
            input_data,
            "forecast",
            SystemFilePathsFromFile(self.pysurfex_system_file),
        ).data

        for dest, target in binput_data.items():
            logger.debug("target={}, dest={}", target, dest)
            self.fmanager.input(target, dest)

        # Initial files
        initfile, initfile_sfx = InitialConditions(self.config).find_initial_files()
        self.fmanager.input(initfile, f"ICMSH{self.cnmexp}INIT")
        if not self.surfex:
            initfile_sfx = None

        if initfile_sfx is not None:
            self.fmanager.input(initfile_sfx, f"ICMSH{self.cnmexp}INIT.sfx")

        # Use explicitly defined boundary dir if defined
        intp_bddir = self.config.get("system.intp_bddir", self.wrk)

        # Link the boundary files, use initial file as first boundary file
        initfile_model = self.platform.substitute(
            self.file_templates["interpolated_boundaries"]["model"],
            validtime=self.basetime,
            bd_index=0,
        )

        self.fmanager.input(initfile, initfile_model)

        current_datetime = self.basetime + self.bdint
        end_datetime = self.basetime + as_timedelta(self.forecast_range)
        i = 1
        while current_datetime <= end_datetime:
            source = self.platform.substitute(
                self.file_templates["interpolated_boundaries"]["archive"],
                validtime=current_datetime,
                bd_index=i,
            )
            target = self.platform.substitute(
                self.file_templates["interpolated_boundaries"]["model"],
                validtime=current_datetime,
                bd_index=i,
            )
            self.fmanager.input(f"{intp_bddir}/{source}", target)
            current_datetime += self.bdint
            i += 1

        if self.config.get("general.upd_sst_sic", False):
            self.sst_sic_input(intp_bddir)

        if self.accelerator_device:
            logger.info("Processing accelerator_device section")
            self.accelerator_device_input()
        else:
            logger.info("No accelerator_device section found")

        # Create a link to working directory for IO_merge tasks
        if self.iomerge_is_external:
            if os.path.islink(self.forecast_dir_link):
                logger.debug("Removing old link.")
                os.unlink(self.forecast_dir_link)
            os.symlink(os.getcwd(), self.forecast_dir_link)

        # Store the output
        # Must happen before the forecast starts, so the io_merge tasks
        #   can write to it.
        deodemakedirs(self.archive, unixgroup=self.unix_group)

        # Run MASTERODB
        batch = BatchJob(os.environ, wrapper=self.platform.substitute(self.wrapper))
        batch.run(self.master)

        # Merge files and move to archive
        if self.iomerge_is_external:
            atexit.unregister(self.rename_wdir)
        else:
            for filetype, oi in self.output_settings.items():
                if filetype in self.file_templates:
                    if self.io_server and filetype in IOMERGE_FILETYPES:
                        self.merge_output(filetype, oi)
                    self.archive_output(self.file_templates[filetype], oi)

        self.archive_logs(["fort.4", "EXSEG1.nam", "NODE.001_01"])


class PrepareCycle(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, self.__class__.__name__)

        self.archive = self.platform.get_system_value("archive")
        deodemakedirs(
            self.archive, unixgroup=self.platform.get_platform_value("unix_group")
        )


class FirstGuess(Task):
    """FirstGuess."""

    def __init__(self, config):
        """Construct FirstGuess object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __class__.__name__)

    def execute(self):
        """Find initial file."""
        initfile, initfile_sfx = InitialConditions(self.config).find_initial_files()
