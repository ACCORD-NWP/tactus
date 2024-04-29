"""Forecast."""
import glob
import json
import os
import shutil
import subprocess

from ..datetime_utils import as_datetime, as_timedelta, oi2dt_list
from ..derived_variables import check_fullpos_namelist
from ..initial_conditions import InitialConditions
from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import deodemakedirs
from .base import Task
from .batch import BatchJob
from .sfx import InputDataFromNamelist


class Forecast(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, "Forecast")

        self.cycle = self.config["general.cycle"]
        self.cnmexp = self.config["general.cnmexp"]
        self.domain = self.config["domain.name"]
        self.windfarm = self.config.get("general.windfarm", False)

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        self.intp_bddir = self.config["system.intp_bddir"]
        self.forecast_range = self.config["general.times.forecast_range"]

        self.climdir = self.platform.get_system_value("climdir")
        self.rrtm_dir = self.platform.get_platform_value("RRTM_DIR")
        self.ncdir = self.config["platform.ncdir"]
        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config["platform.deode_home"]
        self.output_settings = self.config["general.output_settings"]
        self.surfex = self.config["general.surfex"]

        try:
            self.accelerator_device = self.config["accelerator_device"]
        except KeyError:
            self.accelerator_device = None

        # Update namelist settings
        self.nlgen_master = NamelistGenerator(self.config, "master")
        self.nlgen_surfex = NamelistGenerator(self.config, "surfex")

        self.master = self.get_binary("MASTERODB")

        self.file_templates = self.config["file_templates"]

        self.unix_group = self.platform.get_platform_value("unix_group")

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

    def convert_namelist_between_cycles(self, namelist_filename, tnt_directive_yaml):
         command = ["tnt.py", "-d", tnt_directive_yaml, namelist_filename]
         subprocess.call(command)

         file_name = os.path.basename(tnt_directive_yaml)
         index = file_name.find("_to_")
         cycle_from = file_name[0:index]
         shutil.move(namelist_filename, namelist_filename+"."+ cycle_from)
         shutil.move(namelist_filename + ".tnt", namelist_filename)

    def upgrade_namelist_to_49t2(self, namelist_filename):
        self.convert_namelist_between_cycles(namelist_filename,"/leonardo/home/userexternal/dhaumont/accord/thenamelisttool/tnt_directives/cy48t2_to_cy49.yaml")
        self.convert_namelist_between_cycles(namelist_filename,"/leonardo/home/userexternal/dhaumont/accord/thenamelisttool/tnt_directives/cy49_to_cy49t1.yaml")

    def execute(self):
        """Execute forecast."""
        # CY48t3 input files not used in CY46
        # *.nc files and ecoclimap.bin files
        input_files = [
            "greenhouse_gas_climatology_46r1.nc",
            "greenhouse_gas_climatology_48r1.nc",
            "greenhouse_gas_timeseries_CMIP3_A1B_46r1.nc",
            "greenhouse_gas_timeseries_CMIP3_A2_46r1.nc",
            "greenhouse_gas_timeseries_CMIP3_B1_46r1.nc",
            "greenhouse_gas_timeseries_CMIP5_RCP3PD_46r1.nc",
            "greenhouse_gas_timeseries_CMIP5_RCP45_46r1.nc",
            "greenhouse_gas_timeseries_CMIP5_RCP6_46r1.nc",
            "greenhouse_gas_timeseries_CMIP5_RCP85_46r1.nc",
            "greenhouse_gas_timeseries_CMIP6_SSP126_CFC11equiv_47r1.nc",
            "greenhouse_gas_timeseries_CMIP6_SSP245_CFC11equiv_47r1.nc",
            "greenhouse_gas_timeseries_CMIP6_SSP370_CFC11equiv_47r1.nc",
            "greenhouse_gas_timeseries_CMIP6_SSP585_CFC11equiv_47r1.nc",
        ]
        if self.cycle in ["CY48t3"]:
            for ifile in input_files:
                self.fmanager.input(f"{self.ncdir}/{ifile}", ifile)

        # RRTM files
        for ifile in [
            "C11CLIM",
            "C12CLIM",
            "C22CLIM",
            "CCL4CLIM",
            "CH4CLIM",
            "CO2CLIM",
            "ECOZC",
            "GCH4CLIM",
            "GCO2CLIM",
            "GOZOCLIM",
            "MCH4CLIM",
            "MCICA",
            "MCO2CLIM",
            "MOZOCLIM",
            "N2OCLIM",
            "NO2CLIM",
            "OZOCLIM",
            "RADAIOP",
            "RADRRTM",
            "RADSRTM",
            "SO4_A1B2000",
            "SO4_A1B2010",
            "SO4_A1B2020",
            "SO4_A1B2030",
            "SO4_A1B2040",
            "SO4_A1B2050",
            "SO4_A1B2060",
            "SO4_A1B2070",
            "SO4_A1B2080",
            "SO4_A1B2090",
            "SO4_A1B2100",
            "SO4_OBS1920",
            "SO4_OBS1930",
            "SO4_OBS1940",
            "SO4_OBS1950",
            "SO4_OBS1960",
            "SO4_OBS1970",
            "SO4_OBS1980",
            "SO4_OBS1990",
        ]:
            self.fmanager.input(f"{self.rrtm_dir}/{ifile}", ifile)

        # Climate files
        mm = self.basetime.strftime("%m")
        self.fmanager.input(f"{self.climdir}/Const.Clim.{mm}", "Const.Clim")
        self.fmanager.input(
            f"{self.climdir}/Const.Clim.{mm}", f"const.clim.{self.domain}"
        )
        self.fmanager.input(
            f"{self.climdir}/{self.file_templates['pgd']['archive']}",
            f"{self.file_templates['pgd']['model']}",
        )

        # wind farm input data
        if self.windfarm:
            self.wfp_input()

        # Construct master namelist and include fullpos config
        forecast_namelist = "forecast"
        self.nlgen_master.load(forecast_namelist)
        logger.info(self.nlgen_master)
        self.nlgen_master = check_fullpos_namelist(self.config, self.nlgen_master)

        nlres = self.nlgen_master.assemble_namelist(forecast_namelist)
        self.nlgen_master.write_namelist(nlres, "fort.4")
        
        self.upgrade_namelist_to_49t2("fort.4")

        #shutil.copy("/leonardo/home/userexternal/dhaumont/accord/namelists/fort.4.gpupack", "fort.4")

        # SURFEX: Namelists and input data
        self.nlgen_surfex.load("forecast")
        settings = self.nlgen_surfex.assemble_namelist("forecast")
        self.nlgen_surfex.write_namelist(settings, "EXSEG1.nam")

        sfx_input_defs = self.platform.get_system_value("sfx_input_defs")
        with open(sfx_input_defs, "r", encoding="utf-8") as f:
            input_data = json.load(f)
        binput_data = InputDataFromNamelist(
            settings, input_data, "forecast", self.platform
        ).get()

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
        try:
            intp_bddir = self.config["system.intp_bddir"]
        except KeyError:
            intp_bddir = self.wrk

        # Link the boundary files, use initial file as first boundary file
        self.fmanager.input(initfile, f"ELSCF{self.cnmexp}ALBC000")

        cdtg = self.basetime + self.bdint
        dtgend = self.basetime + as_timedelta(self.forecast_range)
        i = 1
        while cdtg <= dtgend:
            source = f"ELSCF{self.cnmexp}ALBC{i:03d}"
            self.fmanager.input(f"{intp_bddir}/{source}", source)
            cdtg += self.bdint
            i += 1

        if self.accelerator_device:
            logger.info("Processing accelerator_device section")
            self.accelerator_device_input()
        else:
            logger.info("No accelerator_device section found")

        # Run MASTERODB
        batch = BatchJob(os.environ, wrapper=self.platform.substitute(self.wrapper))
        batch.run(self.master)

        # Store the output
        deodemakedirs(self.archive, unixgroup=self.unix_group)

        io_server = os.path.exists("io_serv.000001.d")
        if io_server:
            logger.debug("IO_SERVER detected!")

        for filetype, oi in self.output_settings.items():
            if filetype in self.file_templates:
                if io_server:
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
        Task.__init__(self, config, "FirstGuess")

    def execute(self):
        """Find initial file."""
        initfile, initfile_sfx = InitialConditions(self.config).find_initial_files()
