"""ExtractSQLite."""

import json
import os

import pandas

from ..datetime_utils import as_datetime, oi2dt_list
from ..logs import logger
from ..sqlite_utils import parse_grib_file
from .base import Task


class ExtractSQLite(Task):
    """Extract sqlite point files."""

    def __init__(self, config):
        """Construct ExtractSQLite object.

        Args:
            config (deode.ParsedConfig): Configuration

        Raises:
            FileNotFoundError: Required file not fount
        """
        Task.__init__(self, config, __name__)

        self.archive = self.platform.get_system_value("archive")
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]
        try:
            self.infile_dt = self.config["extractsqlite.selection"]
        except KeyError:
            self.infile_dt = self.config["general.output_settings.fullpos"]
        self.infile_template = self.config["file_templates.fullpos.archive"]

        self.sqlite_path = self.platform.substitute(
            self.config["extractsqlite.sqlite_path"]
        )
        self.sqlite_template = self.platform.substitute(
            self.config["extractsqlite.sqlite_template"]
        )
        self.model_name = self.platform.substitute(
            self.config["extractsqlite.sqlite_model_name"]
        )
        stationfile = self.platform.substitute(self.config["extractsqlite.station_list"])
        if not os.path.isfile(stationfile):
            raise FileNotFoundError(f" missing {stationfile}")
        logger.info("Station list: {}", stationfile)
        self.station_list = pandas.read_csv(stationfile, skipinitialspace=True)
        paramfile = self.platform.substitute(self.config["extractsqlite.parameter_list"])
        if not os.path.isfile(paramfile):
            raise FileNotFoundError(f" missing {paramfile}")
        logger.info("Parameter list: {}", paramfile)
        with open(paramfile) as pf:
            self.parameter_list = json.load(pf)
            pf.close()
        self.weights = None
        self.output_settings = self.config["general.output_settings"]

    def execute(self):
        """Execute ExtractSQLite on all output files."""
        # split into "combined" and "direct" parameters

        # loop over lead times
        dt_list = oi2dt_list(self.infile_dt, self.forecast_range)
        for dt in dt_list:
            infile = self.platform.substitute(
                self.archive + self.infile_template, validtime=self.basetime + dt
            )
            if not os.path.isfile(infile):
                raise FileNotFoundError(f" missing {infile}")
            logger.info("SQLITE EXTRACTION: {}", infile)

            parse_grib_file(
                infile=infile,
                param_list=self.parameter_list,
                station_list=self.station_list,
                sqlite_template=self.sqlite_path + "/" + self.sqlite_template,
                model_name=self.model_name,
                weights=self.weights,
            )
