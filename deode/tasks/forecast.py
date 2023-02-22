"""Forecast."""

import os
# from datetime import timedelta  # noqa
from deode.tasks.batch import BatchJob
# from deode.datetime_utils import as_datetime  # noqa
from .base import Task


class Forecast(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)
        self.logger.debug("Construct forecast task")

    def execute(self):
        """Execute forecast."""
        # Boundaries assumed to be in archive
        # validtime = as_datetime(self.config.get_value("general.times.validtime")) # noqa
        # basetime = as_datetime(self.config.get_value("general.times.basetime"))  # noqa
        # basetime = validtime - timedelta(hours=3)  # noqa
        directory_path = self.platform.get_platform_value("FORECAST_DATA")
        input_files = ["C11CLIM", "C12CLIM", "C22CLIM", "CCL4CLIM", "CH4CLIM", "CO2CLIM",
                       "Const.Clim", "Const.Clim.sfx", "ECOZC", "ELSCFARPEALBC000",
                       "ELSCFARPEALBC001", "ELSCFARPEALBC002", "ELSCFARPEALBC003",
                       "ELSCFARPEALBC004", "ELSCFARPEALBC005", "ELSCFARPEALBC006",
                       "EXSEG1.nam", "GCH4CLIM", "GCO2CLIM", "GOZOCLIM",
                       "ICMSHARPEINIT", "ICMSHARPEINIT.sfx",
                       "MCH4CLIM", "MCICA", "MCO2CLIM", "MOZOCLIM", "N2OCLIM", "NO2CLIM",
                       "OZOCLIM", "RADRRTM", "RADSRTM", "SO4_A1B2000", "SO4_A1B2010",
                       "SO4_A1B2020", "SO4_A1B2030", "SO4_A1B2040", "SO4_A1B2050",
                       "SO4_A1B2060", "SO4_A1B2070", "SO4_A1B2080", "SO4_A1B2090",
                       "SO4_A1B2100", "SO4_OBS1920", "SO4_OBS1930", "SO4_OBS1940",
                       "SO4_OBS1950", "SO4_OBS1960", "SO4_OBS1970", "SO4_OBS1980",
                       "SO4_OBS1990", "aerosol_cams_climatology_43R3.nc",
                       "aerosol_cams_climatology_43R3a.nc", "aerosol_ifs_rrtm.nc",
                       "aerosol_ifs_rrtm_42R1.nc", "aerosol_ifs_rrtm_43R1.nc",
                       "aerosol_ifs_rrtm_43R1a.nc", "aerosol_ifs_rrtm_43R3.nc",
                       "aerosol_ifs_rrtm_45R2.nc", "aerosol_ifs_rrtm_46R1.nc",
                       "aerosol_ifs_rrtm_46R1_with_NI_AM.nc", "aerosol_ifs_rrtm_AB.nc",
                       "aerosol_ifs_rrtm_tegen.nc", "baran_ice_scattering_rrtm.nc",
                       "const.clim.FRANXL0013",
                       "ecoclimapII_eu_covers_param.bin",
                       "ecoclimapI_covers_param.bin",
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
                       "selectfp0", "selectfp3", "selectfp6", "selectfp9", "selectfp12",
                       "slingo_droplet_scattering_rrtm.nc",
                       "socrates_droplet_scattering_rrtm.nc",
                       "total_solar_irradiance_CMIP6_47r1.nc",
                       "xxt00000000", "xxt00000300", "xxt00000600", "xxt00000900",
                       "xxt00001200"
                       ]
        # for now copy all input files from directory_path
        for ifile in input_files:
            file_name = f"{directory_path}/{ifile}"
            self.fmanager.input(file_name, ifile, provider_id="copy")
        # Prepared namelist
        namelist = self.platform.get_platform_value("FORECAST_NAMELIST")
        # Binary assumed to be in bindir
        # binary = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa
        binary = "/home/snh02/pack/48t3_fix.01.OMPIIFC2104.x/bin/MASTERODB"
        wrapper = self.config.get_value("task.forecast.wrapper")
        self.fmanager.input(namelist, "fort.4", provider_id="copy")
        batch = BatchJob(os.environ, wrapper=wrapper)
        batch.run(binary)
        for i in range(7):
            self.fmanager.output(f"ICMSH@CNMEXP@+000{i}",
                                 f"@ARCHIVE@/ICMSH@CNMEXP@+000{i}")
        self.fmanager.output("NODE.001_01", "@ARCHIVE@/NODE.001_01")


class PgdInput(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


class Pgd(Task):
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


class E927(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


class FirstGuess(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)
