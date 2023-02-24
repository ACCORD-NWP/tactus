"""Forecast."""


from .base import Task
from deode.tasks.batch import BatchJob
from deode.datetime_utils import as_datetime, as_timedelta
import os


class Forecast(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.cnmexp = self.config.get_value("general.cnmexp")
        self.domain = self.config.get_value("domain.name")

        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.cycle_length = as_timedelta(self.config.get_value("general.times.cycle_length"))
        self.bdint = as_timedelta(self.config.get_value("general.bdint"))
        self.forecast_range = as_timedelta(self.config.get_value("general.forecast_range"))

        self.climdir = self.platform.get_system_value('climdir')
        self.rrtm_dir = self.platform.get_platform_value("RRTM_DIR")
        self.initdata = self.platform.get_platform_value("FORECAST_DATA")
        self.archive = self.platform.get_system_value('archive')

        self.namelists = self.platform.get_platform_value("NAMELISTS")

        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")
        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

    def archive_output(self, fname, period, suffix=""):
        """Archive forecast model output.

        Args:
            fname (str): Filename
            period (str): Output frequency
            suffix (str): Suffix
        """
        # Store the output
        cdtg = self.basetime
        dtgend = self.basetime + self.forecast_range
        while cdtg <= dtgend:
            dt = cdtg - self.basetime
            h = int(dt.seconds / 3600)
            m = int((dt.seconds % 3600 - dt.seconds % 60) / 60)
            s = int(dt.seconds % 60)
            source = f"{fname}+{h:04d}:{m:02d}:{s:02d}{suffix}"
            self.fmanager.output(source, f"{self.archive}/{source}")
            cdtg += as_timedelta(period)

    def firstguess(self):
        """Find initial file."""
        # Find data from previous forecast
        pdtg = self.basetime - self.cycle_length
        dt = self.basetime - pdtg

        h = int(dt.seconds / 3600)
        m = int((dt.seconds % 3600 - dt.seconds % 60) / 60)
        s = int(dt.seconds % 60)

        archive = self.platform.substitute(self.config.get_value("system.archive"),
                                           basetime=pdtg)
        source = f"{archive}/ICMSH{self.cnmexp}+{h:04d}:{m:02d}:{s:02d}"
        source_sfx = f"{archive}/ICMSH{self.cnmexp}+{h:04d}:{m:02d}:{s:02d}.sfx"

        if os.path.exists(source) and os.path.exists(source_sfx):
            self.logger.info("Found initial files:\n  %s\n  %s", source, source_sfx)
            return source, source_sfx

        self.logger.info("Could not find:\n  %s\n  %s", source, source_sfx)

        # Find data prepared by Prep and the boundary interpolation
        source = f"{self.wrk}/ELSCF{self.cnmexp}ALBC000"
        source_sfx = f"{self.archive}/ICMSH{self.cnmexp}INIT.sfx"
        if os.path.exists(source) and os.path.exists(source_sfx):
            self.logger.info("Found initial files\n  %s\n  %s", source, source_sfx)
            return source, source_sfx

        self.logger.info("Could not find:\n  %s\n  %s", source, source_sfx)

        raise Exception("Could not find any initial files")

    def execute(self):
        """Execute forecast.

        # Boundaries assumed to be in archive
        CY48T3 input files not used in CY46
        input_files = ["aerosol_cams_climatology_43R3.nc",
                       "aerosol_cams_climatology_43R3a.nc", "aerosol_ifs_rrtm.nc",
                       "aerosol_ifs_rrtm_42R1.nc", "aerosol_ifs_rrtm_43R1.nc",
                       "aerosol_ifs_rrtm_43R1a.nc", "aerosol_ifs_rrtm_43R3.nc",
                       "aerosol_ifs_rrtm_45R2.nc", "aerosol_ifs_rrtm_46R1.nc",
                       "aerosol_ifs_rrtm_46R1_with_NI_AM.nc", "aerosol_ifs_rrtm_AB.nc",
                       "aerosol_ifs_rrtm_tegen.nc", "baran_ice_scattering_rrtm.nc",
                       "es_droplet_scattering_rrtm.nc", "fu_ice_scattering_rrtm.nc",
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
                       ]
        """
        # RRTM files
        for ifile in ["C11CLIM", "C12CLIM", "C22CLIM", "CCL4CLIM", "CH4CLIM", "CO2CLIM",
                      "ECOZC", "GCH4CLIM", "GCO2CLIM", "GOZOCLIM",
                      "MCH4CLIM", "MCICA", "MCO2CLIM", "MOZOCLIM",
                      "N2OCLIM", "NO2CLIM", "OZOCLIM", "RADAIOP", "RADRRTM", "RADSRTM",
                      "SO4_A1B2000", "SO4_A1B2010", "SO4_A1B2020", "SO4_A1B2030",
                      "SO4_A1B2040", "SO4_A1B2050", "SO4_A1B2060", "SO4_A1B2070",
                      "SO4_A1B2080", "SO4_A1B2090", "SO4_A1B2100", "SO4_OBS1920",
                      "SO4_OBS1930", "SO4_OBS1940", "SO4_OBS1950", "SO4_OBS1960",
                      "SO4_OBS1970", "SO4_OBS1980", "SO4_OBS1990"]:
            self.fmanager.input(f"{self.rrtm_dir}/{ifile}", ifile)

        # Climate files
        mm = self.basetime.strftime("%m")
        self.fmanager.input(f"{self.climdir}/Const.Clim.{mm}", "Const.Clim")
        self.fmanager.input(f"{self.climdir}/Const.Clim.{mm}", f"const.clim.{self.domain}")
        self.fmanager.input(f"{self.climdir}/Const.Clim.sfx", "Const.Clim.sfx")

        # Namelists
        # The fullpos output should be configured elsewhere
        for ifile in ['xxt00000000', 'xxtddddhhmm', 'dirlst', 'EXSEG1.nam']:
            namelist = f"{self.namelists}/{ifile}"
            self.fmanager.input(namelist, ifile, provider_id="copy")
        self.fmanager.input(f"{self.namelists}/fort.4_arome", "fort.4", provider_id="copy")

        # Link the boundary files
        cdtg = self.basetime
        dtgend = self.basetime + self.forecast_range
        i = 0
        while cdtg <= dtgend:
            source = f"ELSCF{self.cnmexp}ALBC{i:03d}"
            self.fmanager.input(f"{self.wrk}/{source}", source)
            cdtg += self.bdint
            i += 1

        # Initial files
        initfile, initfile_sfx = self.firstguess()
        self.fmanager.input(initfile, f"ICMSH{self.cnmexp}INIT")
        self.fmanager.input(initfile_sfx, f"ICMSH{self.cnmexp}INIT.sfx")

        # Run MASTERODB
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store the output
        os.makedirs(self.archive, exist_ok=True)
        self.archive_output(f"ICMSH{self.cnmexp}", self.config.get_value("general.output_interval_his"))
        self.archive_output(f"ICMSH{self.cnmexp}", self.config.get_value("general.output_interval_sfx"), suffix=".sfx")
        self.archive_output("ICMSHSELE", self.config.get_value("general.output_interval_sfx_sel"), suffix=".sfx")
        self.archive_output(f"PF{self.cnmexp}{self.domain}", self.config.get_value("general.output_interval_fp"))
        self.fmanager.output("NODE.001_01", f"{self.archive}/NODE.001_01")


class PgdInput(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


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
        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.cycle_length = as_timedelta(self.config.get_value("general.times.cycle_length"))
        self.archive = self.config.get_value('system.archive')
        self.cnmexp = self.config.get_value("general.cnmexp")

    def execute(self):
        """Find initial file."""
        # Find data from previous forecast
        pdtg = self.basetime - self.cycle_length
        dt = self.basetime - pdtg
        h = int(dt.seconds / 3600)
        m = int((dt.seconds % 3600 - dt.seconds % 60) / 60)
        s = int(dt.seconds % 60)

        archive = self.platform.substitute(self.archive, basetime=pdtg)
        source = f"{archive}/ICMSH{self.cnmexp}+{h:04d}:{m:02d}:{s:02d}"
        source_sfx = f"{archive}/ICMSH{self.cnmexp}+{h:04d}:{m:02d}:{s:02d}.sfx"

        if os.path.exists(source) and os.path.exists(source_sfx):
            self.logger.info("Found initial files\n  %s\n  %s", source, source_sfx)
            return source, source_sfx

        self.logger.info("Could not find:\n  %s\n  %s", source, source_sfx)

        # Find data prepared by Prep and the boundary interpolation
        archive = self.platform.substitute(self.archive)
        source = f"{self.wrk}/ELSCF{self.cnmexp}ALBC000"
        source_sfx = f"{archive}/ICMSH{self.cnmexp}INIT.sfx"

        if os.path.exists(source) and os.path.exists(source_sfx):
            self.logger.info("Found initial files\n  %s\n  %s", source, source_sfx)
            return source, source_sfx

        self.logger.info("Could not find:\n  %s\n  %s", source, source_sfx)

        raise Exception("Could not find any initial files")
