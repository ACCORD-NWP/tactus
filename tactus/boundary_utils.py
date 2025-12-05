"""Boundary handling support classes."""

import ast

from deode.datetime_utils import as_datetime, as_timedelta, cycle_offset
from deode.mars_utils import mars_selection
from deode.toolbox import Platform


class Boundary:
    """Boundary class."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        platform = Platform(config)

        basetime = as_datetime(config["general.times.basetime"])
        self.forecast_range = as_timedelta(config["general.times.forecast_range"])

        try:
            bd_index_time_dict = config["task.args.bd_index_time_dict"]
            self.bd_index_time_dict = ast.literal_eval(bd_index_time_dict)
        except KeyError:
            self.bd_index_time_dict = {0: basetime}

        self.min_index = min(self.bd_index_time_dict.keys())
        self.max_index = max(self.bd_index_time_dict.keys())

        self.bdint = as_timedelta(config["boundaries.bdint"])

        bdmodel_map = {"lam": "e927", "ifs": "c903"}
        self.bdmodel = config["boundaries.bdmodel"]
        self.method = bdmodel_map[self.bdmodel]

        if self.method == "e927":
            self.bdcycle = as_timedelta(config["boundaries.lam.bdcycle"])
            self.bdcycle_start = as_timedelta(config["boundaries.lam.bdcycle_start"])
        elif self.method == "c903":
            mars = mars_selection(
                selection=platform.substitute(
                    config[f"boundaries.{self.bdmodel}.selection"]
                ),
                config=config,
            )
            self.bdcycle = as_timedelta(mars["ifs_cycle_length"])
            self.bdcycle_start = as_timedelta(mars["ifs_cycle_start"])

        # Boundary basetime
        self.bdshift = as_timedelta(config["boundaries.bdshift"])
        self.bdshift_sfx = as_timedelta(config["boundaries.bdshift_sfx"])

        if self.bdshift.total_seconds() % self.bdcycle.total_seconds() != 0:
            raise ValueError("bdshift needs to be a multiple of bdcycle!")

        if self.bdshift_sfx.total_seconds() % self.bdcycle.total_seconds() != 0:
            raise ValueError("bdshift needs to be a multiple of bdcycle!")

        self.bd_offset = cycle_offset(
            basetime,
            self.bdcycle,
            bdcycle_start=self.bdcycle_start,
            bdshift=self.bdshift,
        )
        self.bd_offset_sfx = cycle_offset(
            basetime,
            self.bdcycle,
            bdcycle_start=self.bdcycle_start,
            bdshift=self.bdshift_sfx,
        )
        self.bd_basetime = basetime - self.bd_offset
        self.bd_basetime_sfx = basetime - self.bd_offset_sfx
