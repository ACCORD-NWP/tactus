"""ExtractSQLite."""

import json
import os
import sqlite3
from pathlib import Path
from typing import Dict

import pandas as pd
from grib2sqlite import logger as sqlite_logger
from grib2sqlite import parse_grib_file

from tactus.datetime_utils import as_datetime, oi2dt_list
from tactus.eps.eps_setup import get_member_config
from tactus.logs import LogDefaults, logger
from tactus.tasks.base import Task
from tactus.toolbox import Platform


class ExtractSQLite(Task):
    """Extract sqlite point files."""

    def __init__(self, config):
        """Construct ExtractSQLite object.

        Args:
            config (tactus.ParsedConfig): Configuration

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

        extraction_list = self.config["extractsqlite.parameter_list"]
        # NOTE: If you provide an empty list, it is replaced by the default!
        #       If you don't want SQLite extraction, turn off the switch in stead.
        #       Not a perfect situation, but I think its OK for now.
        if len(extraction_list) == 0:
            is_ensemble = len(self.config["eps.general.members"]) > 1
            if is_ensemble:
                logger.info("Using default eps parameter extraction.")
                extraction_list = self.config["extractsqlite.parameter_list_default_eps"]
            else:
                logger.info("Using default deterministic parameter extraction.")
                extraction_list = self.config["extractsqlite.parameter_list_default_det"]

        self.n_list = len(extraction_list)
        logger.info("Found {} extraction lists.", self.n_list)
        self.param_file = []
        self.station_file = []
        for i in range(self.n_list):
            station_file = self.platform.substitute(extraction_list[i]["location_file"])
            if not os.path.isfile(station_file):
                raise FileNotFoundError(f" missing {station_file}")
            self.station_file.append(station_file)

            param_file = self.platform.substitute(extraction_list[i]["param_file"])
            if not os.path.isfile(param_file):
                raise FileNotFoundError(f" missing {param_file}")
            self.param_file.append(param_file)

        self.output_settings = self.config["general.output_settings"]

    def execute(self):
        """Execute ExtractSQLite on all output files."""
        # split into "combined" and "direct" parameters
        # loop over lead times
        # Also allow multiple extraction lists, typically UA and SFC parameters
        # with different station lists.
        dt_list = oi2dt_list(self.infile_dt, self.forecast_range)
        for i in range(self.n_list):
            logger.info("SQLITE: parameter file {}", self.param_file[i])
            logger.info("SQLITE: station file {}", self.station_file[i])
            station_list = pd.read_csv(self.station_file[i], skipinitialspace=True)
            with open(self.param_file[i], "r", encoding="utf-8") as pf:
                param_list = json.load(pf)
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
                    param_list=param_list,
                    station_list=station_list,
                    sqlite_template=self.sqlite_path + "/" + self.sqlite_template,
                    model_name=self.model_name,
                    weights=None,
                )


class MergeSQLites(Task):
    """Extract sqlite point files."""

    def __init__(self, config):
        """Construct ExtractSQLite object.

        Args:
            config (tactus.ParsedConfig): Configuration

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

        extraction_list = self.config["extractsqlite.parameter_list"]
        if len(extraction_list) == 0:
            is_ensemble = len(self.config["eps.general.members"]) > 1
            if is_ensemble:
                # NOTE: in this context, we expect eps data, of course.
                logger.info("Using default eps parameter extraction.")
                extraction_list = self.config["extractsqlite.parameter_list_default_eps"]
            else:
                logger.info("Using default deterministic parameter extraction.")
                extraction_list = self.config["extractsqlite.parameter_list_default_det"]

        n_list = len(extraction_list)
        logger.info("Found {} extraction lists.", n_list)

        for i in range(n_list):
            paramfile = self.platform.substitute(extraction_list[i]["param_file"])
            if not os.path.isfile(paramfile):
                raise FileNotFoundError(f"Missing parameter file: {paramfile}")
            logger.info("Parameter list {}: {}", i + 1, paramfile)
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
