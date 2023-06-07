"""Forecast."""
import os

from ..datetime_utils import as_datetime, as_timedelta, oi2dt_list
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
        self.intp_bddir = self.config.get_value("system.intp_bddir")
        self.forecast_range = self.config.get_value("general.forecast_range")

        sfx_config = self.config.get_value("SURFEX").dict()
        self.second_generation = sfx_config["COVER"]["SG"]

        self.climdir = self.platform.get_system_value("climdir")
        self.rrtm_dir = self.platform.get_platform_value("RRTM_DIR")
        self.ncdir = self.config.get_value("platform.ncdir")
        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config.get_value("platform.deode_home")
        self.output_settings = self.config.get_value("general.output_settings").dict()

        # Update namelist settings
        self.namelists = self.platform.get_platform_value("NAMELISTS")
        self.nlgen_master = NamelistGenerator(self.config, "master")
        self.nlgen_surfex = NamelistGenerator(self.config, "surfex")
        self.fullpos_config_file = self.platform.get_system_value("fullpos_config_file")

        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

        self.file_templates = self.config.get_value("file_templates").dict()

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

        # Surfex files
        if not self.second_generation:
            eco_dir = self.fmanager.platform.get_platform_value("ECOCLIM_DATA_PATH")
            for ifile in [
                "ecoclimapI_covers_param.bin",
                "ecoclimapII_eu_covers_param.bin",
            ]:
                self.fmanager.input(f"{eco_dir}/{ifile}", ifile)

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
        namfpc, selections = Fullpos(
            self.domain, nlfile=self.fullpos_config_file
        ).construct()
        self.nlgen_master.update(namfpc, "fullpos")
        nlres = self.nlgen_master.assemble_namelist("forecast")
        self.nlgen_master.write_namelist(nlres, "fort.4")

        for head, body in selections.items():
            self.nlgen_master.write_namelist(body, head)

        # Initial files
        initfile, initfile_sfx = InitialConditions(self.config).find_initial_files()
        self.fmanager.input(initfile, f"ICMSH{self.cnmexp}INIT")
        self.fmanager.input(initfile_sfx, f"ICMSH{self.cnmexp}INIT.sfx")

        # Use explicitly defined boundary dir if defined
        try:
            intp_bddir = self.config.get_value("system.intp_bddir")
        except AttributeError:  # noqa
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

        self.fmanager.output("NODE.001_01", f"{self.archive}/NODE.001_01")


class PrepareCycle(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, self.__class__.__name__)


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
