"""Forecast."""
import json
import os

from ..datetime_utils import as_datetime, as_timedelta, oi2dt_list
from ..derived_variables import check_fullpos_namelist
from ..initial_conditions import InitialConditions
from ..logs import logger
from ..namelist import NamelistGenerator
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
        Task.__init__(self, config, __name__)

        self.cycle = self.config["general.cycle"]
        self.cnmexp = self.config["general.cnmexp"]
        self.domain = self.config["domain.name"]

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        self.bdmodel = self.config["boundaries.bdmodel"]
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        self.intp_bddir = self.config["system.intp_bddir"]
        self.forecast_range = self.config["general.forecast_range"]

        self.climdir = self.platform.get_system_value("climdir")
        self.rrtm_dir = self.platform.get_platform_value("RRTM_DIR")
        self.ncdir = self.config["platform.ncdir"]
        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config["platform.deode_home"]
        self.output_settings = self.config["general.output_settings"]

        # Update namelist settings
        self.nlgen_master = NamelistGenerator(self.config, "master")
        self.nlgen_surfex = NamelistGenerator(self.config, "surfex")

        self.master = self.get_binary("MASTERODB")

        self.file_templates = self.config["file_templates"]

    def archive_output(self, fname, periods):
        """Archive forecast model output.

        Args:
            fname (str): Filename template
            periods (str): Output list
        """
        dt_list = oi2dt_list(periods, self.forecast_range)
        for dt in dt_list:
            filename = self.platform.substitute(fname, validtime=self.basetime + dt)
            self.fmanager.output(filename, f"{self.archive}/{filename}")

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
        if self.cycle in ["CY46t1", "CY48t3"]:
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
        self.fmanager.input(f"{self.climdir}/Const.Clim.sfx", "Const.Clim.sfx")

        # Construct master namelist and include fullpos config
        forecast_namelist = f"forecast_bdmodel_{self.bdmodel}"
        self.nlgen_master.load(forecast_namelist)
        self.nlgen_master = check_fullpos_namelist(self.config, self.nlgen_master)

        nlres = self.nlgen_master.assemble_namelist(forecast_namelist)
        self.nlgen_master.write_namelist(nlres, "fort.4")

        # SURFEX: Namelists and input data
        self.nlgen_surfex.load("forecast")
        settings = self.nlgen_surfex.assemble_namelist("forecast")
        self.nlgen_surfex.write_namelist(settings, "EXSEG1.nam")

        sfx_input_defs = self.platform.get_system_value("sfx_input_defs")
        input_data = json.load(open(sfx_input_defs, "r", encoding="utf-8"))
        binput_data = InputDataFromNamelist(
            settings, input_data, "forecast", self.platform
        ).get()

        for dest, target in binput_data.items():
            logger.debug("target={}, dest={}", target, dest)
            self.fmanager.input(target, dest)

        # Initial files
        initfile, initfile_sfx = InitialConditions(self.config).find_initial_files()
        self.fmanager.input(initfile, f"ICMSH{self.cnmexp}INIT")
        if initfile_sfx is not None:
            self.fmanager.input(initfile_sfx, f"ICMSH{self.cnmexp}INIT.sfx")

        # Use explicitly defined boundary dir if defined
        try:
            intp_bddir = self.config["system.intp_bddir"]
        except KeyError:  # noqa
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

        # Run MASTERODB
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store the output
        os.makedirs(self.archive, exist_ok=True)

        for filetype, oi in self.output_settings.items():
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
        os.makedirs(self.archive, exist_ok=True)


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
