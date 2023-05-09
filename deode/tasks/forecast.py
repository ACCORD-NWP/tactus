"""Forecast."""
import os
from pathlib import Path

from ..datetime_utils import as_datetime, as_timedelta, dt2str, oi2dt_list
from ..fullpos import Fullpos
from ..initial_conditions import InitialConditions
from ..namelist import NamelistGenerator
from .base import Task
from .batch import BatchJob


class Forecast(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.cycle = self.config.get_value("general.cycle")
        self.cnmexp = self.config.get_value("general.cnmexp")
        self.domain = self.config.get_value("domain.name")

        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.cycle_length = as_timedelta(
            self.config.get_value("general.times.cycle_length")
        )
        self.bdint = as_timedelta(self.config.get_value("general.bdint"))
        self.forecast_range = self.config.get_value("general.forecast_range")

        self.climdir = self.platform.get_system_value("climdir")
        self.rrtm_dir = self.platform.get_platform_value("RRTM_DIR")
        self.ncdir = self.config.get_value("platform.ncdir")
        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config.get_value("platform.deode_home")

        # Update namelist settings
        self.namelists = self.platform.get_platform_value("NAMELISTS")
        self.nlgen_master = NamelistGenerator(self.config, "master")
        self.nlgen_surfex = NamelistGenerator(self.config, "surfex")
        self.fullpos_config = (
            Path(__file__).parent
            / "../namelist_generation_input"
            / f"{self.cycle}"
            / "fullpos_namelist.yml"
        )

        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")
        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

    def archive_output(self, fname, periods, suffix=""):
        """Archive forecast model output.

        Args:
            fname (str): Filename
            periods (str): Output list
            suffix (str): Suffix
        """
        dt_list = oi2dt_list(periods, self.forecast_range)
        for dt in dt_list:
            duration = dt2str(dt)

            self.fmanager.output(
                f"{fname}+{duration}{suffix}",
                f"{self.archive}/{fname}+{duration}{suffix}",
            )

    def execute(self):
        """Execute forecast."""
        # CY48t3/CY46t1 input files not used in CY46h1
        # *.nc files and ecoclimap.bin files
        input_files = [
            "aerosol_cams_climatology_43R3.nc",
            "aerosol_cams_climatology_43R3a.nc",
            "aerosol_ifs_rrtm.nc",
            "aerosol_ifs_rrtm_42R1.nc",
            "aerosol_ifs_rrtm_43R1.nc",
            "aerosol_ifs_rrtm_43R1a.nc",
            "aerosol_ifs_rrtm_43R3.nc",
            "aerosol_ifs_rrtm_45R2.nc",
            "aerosol_ifs_rrtm_46R1.nc",
            "aerosol_ifs_rrtm_46R1_with_NI_AM.nc",
            "aerosol_ifs_rrtm_AB.nc",
            "aerosol_ifs_rrtm_tegen.nc",
            "baran_ice_scattering_rrtm.nc",
            "es_droplet_scattering_rrtm.nc",
            "fu_ice_scattering_rrtm.nc",
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
            "mcica_gamma.nc",
            "mcica_lognormal.nc",
            "slingo_droplet_scattering_rrtm.nc",
            "socrates_droplet_scattering_rrtm.nc",
            "total_solar_irradiance_CMIP6_47r1.nc",
            "ecoclimapI_covers_param.bin",
            "ecoclimapII_eu_covers_param.bin",
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

        # Namelists
        self.nlgen_surfex.generate_namelist("forecast", "EXSEG1.nam")

        # Contstruct master namelist and include fullpos config
        self.nlgen_master.load("forecast")
        namfpc, selections = Fullpos(self.fullpos_config, self.domain).construct()
        self.nlgen_master.update(namfpc, "fullpos")
        nlres = self.nlgen_master.assemble_namelist("forecast")
        self.nlgen_master.write_namelist(nlres, "fort.4")

        for head, body in selections.items():
            self.nlgen_master.write_namelist(body, head)

        # Link the boundary files
        cdtg = self.basetime
        dtgend = self.basetime + as_timedelta(self.forecast_range)
        i = 0

        # Use explicitly defined boundary dir if defined
        try:
            intp_bddir = self.config.get_value("system.intp_bddir")
        except AttributeError:  # noqa
            intp_bddir = self.wrk

        # Link the boundary files
        while cdtg <= dtgend:
            source = f"ELSCF{self.cnmexp}ALBC{i:03d}"
            self.fmanager.input(f"{intp_bddir}/{source}", source)
            cdtg += self.bdint
            i += 1

        # Initial files
        initfile, initfile_sfx = InitialConditions(self.config).find_initial_files()
        self.fmanager.input(initfile, f"ICMSH{self.cnmexp}INIT")
        self.fmanager.input(initfile_sfx, f"ICMSH{self.cnmexp}INIT.sfx")

        # Run MASTERODB
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store the output
        os.makedirs(self.archive, exist_ok=True)
        self.archive_output(
            f"ICMSH{self.cnmexp}", self.config.get_value("general.output_interval_his")
        )
        self.archive_output(
            f"ICMSH{self.cnmexp}",
            self.config.get_value("general.output_interval_sfx"),
            suffix=".sfx",
        )
        self.archive_output(
            "ICMSHSELE",
            self.config.get_value("general.output_interval_sfx_sel"),
            suffix=".sfx",
        )
        self.archive_output(
            f"GRIBPF{self.cnmexp}{self.domain}",
            self.config.get_value("general.output_interval_fp"),
        )
        self.fmanager.output("NODE.001_01", f"{self.archive}/NODE.001_01")


class PrepareCycle(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


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
