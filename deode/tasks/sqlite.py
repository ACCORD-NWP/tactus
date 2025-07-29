"""ExtractSQLite."""

import json
import os
import sqlite3
from pathlib import Path
from typing import Dict

import pandas as pd
from grib2sqlite import logger as sqlite_logger
from grib2sqlite import parse_grib_file

from deode.datetime_utils import as_datetime, oi2dt_list
from deode.eps.eps_setup import get_member_config
from deode.logs import LogDefaults, logger
from deode.tasks.base import Task
from deode.toolbox import Platform


class ExtractSQLite(Task):
    """Extract sqlite point files."""

    def __init__(self, config):
        """Construct ExtractSQLite object.

        Args:
            config (deode.ParsedConfig): Configuration

        Raises:
            FileNotFoundError: Required file not fount
        """
        Task.__init__(self, config, __class__.__name__)

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
        self.stationfile_sfc = self.platform.substitute(
            self.config["extractsqlite.station_list_sfc"]
        )
        if not os.path.isfile(self.stationfile_sfc):
            raise FileNotFoundError(f" missing {self.stationfile_sfc}")
        logger.info("Station list: {}", self.stationfile_sfc)
        self.stationfile_ua = self.platform.substitute(
            self.config["extractsqlite.station_list_ua"]
        )
        if not os.path.isfile(self.stationfile_ua):
            raise FileNotFoundError(f" missing {self.stationfile_ua}")
        logger.info("Station list: {}", self.stationfile_ua)
        paramfile_sfc = self.platform.substitute(
            self.config["extractsqlite.parameter_list_sfc"]
        )
        if not os.path.isfile(paramfile_sfc):
            raise FileNotFoundError(f" missing {paramfile_sfc}")
        logger.info("Surface parameter list: {}", paramfile_sfc)
        with open(paramfile_sfc, "r", encoding="utf-8") as pf:
            self.parameter_list_sfc = json.load(pf)
        paramfile_ua = self.platform.substitute(
            self.config["extractsqlite.parameter_list_ua"]
        )
        if not os.path.isfile(paramfile_ua):
            raise FileNotFoundError(f" missing {paramfile_ua}")
        logger.info("Upper air parameter list: {}", paramfile_ua)
        with open(paramfile_ua, "r", encoding="utf-8") as pf:
            self.parameter_list_ua = json.load(pf)
        self.output_settings = self.config["general.output_settings"]

    def execute(self):
        """Execute ExtractSQLite on all output files."""
        # split into "combined" and "direct" parameters
        # loop over lead times
        # Also split extraction between UA and SFC parameters
        # with different station lists.
        dt_list = oi2dt_list(self.infile_dt, self.forecast_range)
        station_list_sfc = pd.read_csv(self.stationfile_sfc, skipinitialspace=True)
        station_list_ua = pd.read_csv(self.stationfile_ua, skipinitialspace=True)
        for dt in dt_list:
            infile = self.platform.substitute(
                os.path.join(self.archive, self.infile_template),
                validtime=self.basetime + dt,
            )
            if not os.path.isfile(infile):
                raise FileNotFoundError(f" missing {infile}")
            logger.info("SQLITE EXTRACTION: {}", infile)
            loglevel = self.config.get("general.loglevel", LogDefaults.LEVEL).upper()
            sqlite_logger.setLevel(loglevel)

            parse_grib_file(
                infile=infile,
                param_list=self.parameter_list_sfc,
                station_list=station_list_sfc,
                sqlite_template=self.sqlite_path + "/" + self.sqlite_template,
                model_name=self.model_name,
                weights=None,
            )
            parse_grib_file(
                infile=infile,
                param_list=self.parameter_list_ua,
                station_list=station_list_ua,
                sqlite_template=self.sqlite_path + "/" + self.sqlite_template,
                model_name=self.model_name,
                weights=None,
            )


class MergeSQLites(Task):
    """Extract sqlite point files."""

    def __init__(self, config):
        """Construct ExtractSQLite object.

        Args:
            config (deode.ParsedConfig): Configuration

        Raises:
            FileNotFoundError: Required file not found
        """
        Task.__init__(self, config, __class__.__name__)

    def execute(self):
        """Execute MergeSQLite on all member FC files."""
        sqlite_template = (
            self.platform.substitute(self.config["extractsqlite.sqlite_template"])
            .replace("{", "@")
            .replace("}", "@")
        )
        merged_sqlite_path = Path(
            self.platform.substitute(self.config["extractsqlite.merged_sqlite_path"])
        )
        model_name = self.platform.substitute(
            self.config["extractsqlite.sqlite_model_name"]
        )
        paramfile_sfc = self.platform.substitute(
            self.config["extractsqlite.parameter_list_sfc"]
        )
        if not os.path.isfile(paramfile_sfc):
            raise FileNotFoundError(f"Missing parameter file: {paramfile_sfc}")
        paramfile_ua = self.platform.substitute(
            self.config["extractsqlite.parameter_list_ua"]
        )
        if not os.path.isfile(paramfile_ua):
            raise FileNotFoundError(f"Missing parameter file: {paramfile_ua}")
        logger.info("Parameter list sfc: {}", paramfile_sfc)
        logger.info("Parameter list ua: {}", paramfile_ua)
        for paramfile in [paramfile_sfc, paramfile_ua]:
            with open(paramfile, "r", encoding="utf-8") as file_:
                parameter_list = json.load(file_)

            for param in parameter_list:
                harp_param = param["harp_param"]
                # Get param specific sqlite template and path
                sqlite_param_template = sqlite_template.replace("@PP@", harp_param)
                sqlite_file = Path(self.platform.substitute(sqlite_param_template))
                merged_sqlite_file_path = merged_sqlite_path / sqlite_file
                # Prepare sqlite file paths for every member
                sqlite_file_dict: Dict[int, str] = {}
                for member in self.config["eps.general.members"]:
                    member_config = get_member_config(self.config, member)
                    member_path = Platform(member_config).substitute(
                        member_config["extractsqlite.sqlite_path"]
                    )
                    full_sqlite_path = Path(member_path) / sqlite_file
                    sqlite_file_dict[member] = str(full_sqlite_path)
                logger.info(
                    f"Files to merge for parameter {harp_param} are:\n"
                    + json.dumps(sqlite_file_dict, indent=4)
                )
                logger.info(f"Merged filepath: {merged_sqlite_file_path}")

                # Initialize the merged DataFrame
                df_merged: pd.DataFrame | None = None
                for member, file_path in sqlite_file_dict.items():
                    if not os.path.exists(file_path):
                        logger.warning(
                            f"SQLite file not found for member {member}: {file_path}"
                        )
                        continue

                    with sqlite3.connect(file_path) as connection:
                        df_member = pd.read_sql_query("SELECT * FROM FC", connection)

                    # Find column that contains model_name
                    value_cols = [col for col in df_member.columns if model_name in col]
                    if not value_cols:
                        logger.warning(
                            f"No value column containing '{model_name}' \
                                    found in {file_path}"
                        )
                        continue
                    if len(value_cols) > 1:
                        raise ValueError(f"Multiple value columns found in {file_path}")

                    df_member = df_member.rename(
                        columns={value_cols[0]: f"{model_name}_mbr{member:03d}"}
                    )

                    # Merge dataframes by keys that do not contain model_name
                    merge_keys = [
                        col for col in df_member.columns if model_name not in col
                    ]
                    if df_merged is None:
                        df_merged = df_member
                    else:
                        df_merged = df_merged.merge(df_member, on=merge_keys, how="inner")

                if df_merged is not None:
                    # Reorder columns: keys first, *_mbrXXX columns at the end
                    key_cols = [
                        col
                        for col in df_merged.columns
                        if not col.endswith(
                            tuple(f"_mbr{mbr:03d}" for mbr in sqlite_file_dict)
                        )
                    ]
                    mbr_cols = [col for col in df_merged.columns if col not in key_cols]
                    df_merged = df_merged[key_cols + mbr_cols]

                    # Write merged dataframe to file
                    os.makedirs(merged_sqlite_file_path.parent, exist_ok=True)
                    with sqlite3.connect(merged_sqlite_file_path) as connection:
                        df_merged.to_sql(
                            "FC", connection, if_exists="replace", index=False
                        )
                    logger.info(f"Merged file written to: {merged_sqlite_file_path}")
                else:
                    logger.warning(f"No data merged for parameter {harp_param}")
