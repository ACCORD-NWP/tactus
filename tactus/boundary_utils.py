"""Boundary handling support classes."""

import ast

from tactus.datetime_utils import as_datetime, as_timedelta, cycle_offset
from tactus.mars_utils import mars_selection
from tactus.toolbox import Platform


def use_cams_aerosols(config) -> bool:
    """Check if CAMS aerosols are used in the boundaries.

    Args:
        config (ParsedConfig): Configuration

    Returns:
        bool: The value of nrt_aerosols.active

    Raises:
        ValueError: If active for another cycle than CY49t2
    """
    if not config.get("nrt_aerosols.active", False):
        return False
    cycle = config["general.cycle"]
    if cycle != "CY49t2":
        raise ValueError(
            f"nrt_aerosols.active is only available for CY49t2, not for {cycle}"
        )
    return True


class Boundary:
    """Boundary class."""

    def __init__(self, config, cams: bool = False):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
            cams (bool): Use the CAMS forecast (nrt_aerosols) as host model
                instead of boundaries.bdmodel. Defaults to False.

        Raises:
            ValueError: If bdshift is not a multiple of bdcycle.
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

        bdmodel_map = {"lam": "e927", "ifs": "c903", "cams": "c903"}
        self.cams = cams
        self.bdmodel = "cams" if cams else config["boundaries.bdmodel"]
        self.method = bdmodel_map[self.bdmodel]

        if self.method == "e927":
            self.bdcycle = as_timedelta(config["boundaries.lam.bdcycle"])
            self.bdcycle_start = as_timedelta(config["boundaries.lam.bdcycle_start"])
        elif cams:
            mars = mars_selection(
                selection=platform.substitute(config["nrt_aerosols.selection"]),
                config=config,
            )
            self.bdcycle = as_timedelta(mars["ifs_cycle_length"])
            self.bdcycle_start = as_timedelta(mars["ifs_cycle_start"])
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
        bdshift_key = "nrt_aerosols.bdshift" if cams else "boundaries.bdshift"
        self.bdshift = as_timedelta(config[bdshift_key])
        extra_bdshift = config.get("task.args.extra_bdshift", "PT0H")
        self.bdshift += as_timedelta(extra_bdshift)
        self.bdshift_sfx = as_timedelta(config["boundaries.bdshift_sfx"])

        if self.bdshift.total_seconds() % self.bdcycle.total_seconds() != 0:
            raise ValueError("bdshift needs to be a multiple of bdcycle!")

        # The SURFEX initial file never comes from CAMS
        if (
            not cams
            and self.bdshift_sfx.total_seconds() % self.bdcycle.total_seconds() != 0
        ):
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
