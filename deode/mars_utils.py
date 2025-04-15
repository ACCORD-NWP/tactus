"""Utilsty for marsprep."""

import contextlib
import os
from dataclasses import dataclass, field, replace

import pandas as pd

from .datetime_utils import as_datetime
from .geo_utils import Projection, Projstring
from .logs import logger


def write_mars_req(req, name, method):
    """Write a request for MARS.

    Args:
       req (BaseRequest):    namelist object to write
       name     (string):   request file name
       method   (string): selected method, retrieve or read
    """
    sep0 = "{:<2}".format("")
    sep1 = " = "
    sep2 = ","

    df_req = req.to_dataframe()
    with open(name, "w") as f:
        f.write(str(method.upper()) + ",\n")

        for col in df_req.columns:
            for _, row in df_req.iterrows():
                # do not use the comma in the last line
                if col == df_req.columns[-1]:
                    row_str = sep0 + str(col) + sep1 + str(row[col])

                else:
                    row_str = sep0 + str(col) + sep1 + str(row[col]) + sep2

            f.write(row_str + "\n")


def get_steps_and_members_to_retrieve(steps, path, file_name, members):
    """Check which mars file already exist and returns steps and members which missing.

    Args:
        steps       (list): list of steps
        path      (string): path to global files
        file_name (string): name of file to check
        members     (list): members to retrieve
    Returns:
        steps             (list): steps to retrieve
        members           (list): members to retrieve
        is_control_member (bool): true control (0) is needed
    """
    step_list = []
    member_list = []
    for step in steps:
        step1 = int(step)
        step_str = str(step1) if path == "" else f"{step1:02d}"
        if not members:
            mars_file_check = path / f"{file_name}+{step_str}"
            if not os.path.exists(mars_file_check):
                step_list.append(step)
                logger.info("Missing file:{}", mars_file_check)
        else:
            for member in members:
                mars_file_check = os.path.join(path, f"{file_name}_{member}+{step_str}")
                if not os.path.exists(mars_file_check):
                    step_list.append(step)
                    logger.info("Missing file:{}", mars_file_check)
                    member_list.append(member)

    is_control_member = bool(0 in members)
    members = [member for member in sorted(set(member_list)) if member != 0]

    steps = sorted(set(step_list))
    return steps, members, is_control_member


def check_data_available(basetime, mars):
    """Check if there is data for basetime for choosen expver.

    Args:
        basetime (str): basetime
        mars    (dict): mars config section
    Raises:
        ValueError: No data for this date.
    """
    start_date = as_datetime(mars["start_date"])
    with contextlib.suppress(KeyError):
        end_date = as_datetime(mars["end_date"])
        if basetime > end_date:
            raise ValueError(
                f"No data for {basetime}! "
                f"The data is available between {start_date} and {end_date}."
            )
    if basetime < start_date:
        raise ValueError(
            f"No data for {basetime}! The data is available after {start_date}"
        )


def get_domain_data(config):
    """Read and return domain data.

    Args:
        config (deode.ParsedConfig): Configuration from which we get the domain data
    Returns:
        String containing the domain info for MARS
    """
    # Get domain specs
    domain_spec = {
        "nlon": config["domain.nimax"],
        "nlat": config["domain.njmax"],
        "latc": config["domain.xlatcen"],
        "lonc": config["domain.xloncen"],
        "lat0": config["domain.xlat0"],
        "lon0": config["domain.xlon0"],
        "gsize": config["domain.xdx"],
    }

    # Get domain properties
    projstring = Projstring()
    projection = Projection(
        projstring.get_projstring(lon0=domain_spec["lon0"], lat0=domain_spec["lat0"])
    )
    domain_properties = projection.get_domain_properties(domain_spec)
    fdomainstr = "/".join(
        [
            str(domain_properties["maxlat"]),
            str(domain_properties["minlon"]),
            str(domain_properties["minlat"]),
            str(domain_properties["maxlon"]),
        ]
    )

    return fdomainstr


def get_value_from_dict(dict_, key_orig):
    """Check value according to key.

    - If a string returns the value itself
    - If key is a date search for the most suitable match in value
    - Else return the value matching the key.

    Args:
        dict_ (str, BaseConfig object): Values to select
        key_orig (str): key for value checking

    Returns:
        value (str): Found value

    Raises:
        ValueError: Exception
    """
    if isinstance(dict_, str):
        return dict_

    try:
        ref_date = as_datetime(key_orig)
        for key, val in sorted(dict_.items(), reverse=True):
            if ref_date >= as_datetime(key):
                return val
    except ValueError:
        key = str(key_orig)
        if key in dict_:
            return dict_[key]

    raise ValueError(f"Value not found for {key_orig} within {dict_}")


def get_date_time_info(date, fc_cycle, bdint, bdcycle, bdshift):
    """Manipulate the dates for Mars request.

    Args:
       date         (datetime):   full date provided for spliting into parts
       fc_cycle    (timedelta):   forecast cycle length in hours
       bdint       (timedelta):   boundary interval in hours "1H","3H","6H"
       bdcycle     (timedelta):   length of IFS cycle
       bdshift     (timedelta):   shift of boundaries in hours

    Returns:
        init_date_str (string): initial date
        init_hour_str (string): initial hour
        str_steps       (list): list of steps in string

    """
    # Check if we're dealing with 00 or 03 ... etc.
    init_hour = date.hour % (bdcycle.total_seconds() // 3600)

    # Create Pandas date series for MARS extraction
    interval_int = bdint.total_seconds() // 3600
    forecast_range_int = fc_cycle.total_seconds() // 3600

    forecast_datetimes_series = pd.Series(
        pd.date_range(
            date - pd.Timedelta(hours=init_hour),
            periods=(forecast_range_int + init_hour) // interval_int + 1,
            freq=bdint,
        )
    )
    request_datetimes_series = pd.concat(
        [
            forecast_datetimes_series.iloc[int(init_hour) :],
        ]
    )

    init_date_str = forecast_datetimes_series.iloc[0].strftime("%Y%m%d")
    init_hour_str = forecast_datetimes_series.iloc[0].strftime("%H")

    step = int(bdint.total_seconds() // 3600)
    # Get steps according to bdshift and bdcycle
    str_steps = [
        "{0:02d}".format(
            int(
                bdshift.total_seconds() // 3600
                + (i * step)
                + date.hour % int(bdcycle.total_seconds()) // 3600
            )
        )
        for i in (request_datetimes_series.index.tolist())
    ]

    return init_date_str, init_hour_str, str_steps


@dataclass(kw_only=True)
class BaseRequest:
    """Represents a structured data request with configurable parameters.

    This class facilitates the creation of data requests by encapsulating
    request attributes and providing methods for modifying and enhancing
    the request structure. The request data is stored in a dictionary
    accessible via the `request` attribute.

    Parameters:
        class_ (str): The class of the request, e.g., "D1".
        data_type (str): The type of data requested (e.g., analysis, forecast).
        expver (str): Experiment version identifier.
        levtype (str): Level type (e.g., surface, pressure levels).
        date (str): The date range for the request in YYYYMMDD format.
        time (str): The time range for the request in HHMM format.
        steps (str): Forecast steps or time intervals.
        param (str): Parameter codes for the requested data.
        target (str): Target file or identifier for the output.
        request (dict): A dictionary representation of the request, initialized
            automatically upon object creation.
    """

    class_: str
    data_type: str
    expver: str
    levtype: str
    date: str
    time: str
    steps: str
    param: str
    target: str
    request: dict = field(init=False)

    def __post_init__(self):
        self.request = {
            "CLASS": [self.class_],
            "TYPE": [self.data_type],
            "EXPVER": [self.expver],
            "LEVTYPE": [self.levtype],
            "DATE": [self.date],
            "TIME": [self.time],
            "STEP": [self.steps],
            "PARAM": [self.param],
            "TARGET": [self.target],
        }

    def add_grid(self, grid):
        """Add grid key.

        Args:
            grid (string): resolution of the grid
        """
        self.request["GRID"] = [grid]

    def add_eps_members(self, members: list, prefetch: bool):
        """Fix parameters in case of eps members.

        Args:
            members    (list): members to retrieve
            prefetch (string): type of retrieve
        """
        if prefetch:
            if "+[STEP]" in self.target:
                self.target = self.target.replace("+[STEP]", "_[NUMBER]+[STEP]")
            else:
                self.target = f'"{self.target}_[NUMBER]"'

            self.request.update(
                {
                    "NUMBER": ["/".join(map(str, members))],
                    "TARGET": [self.target],
                }
            )
        else:
            target_ = self.target + f"_{members[0]}"
            self.request["TARGET"] = [target_]
            if members[0] == 0:
                self.request["TYPE"] = ["CF"]

    def add_process(self):
        """Add process if needed."""
        self.request["PROCESS"] = ["LOCAL"]

    def add_levelist(self, levelist: str):
        """Set multilevel if needed.

        Args:
            levelist (string): list of levels
        Returns:
            None
        """
        if self.levtype == "ML" and self.param == "129":
            local_levelist = 1

        elif self.levtype == "ML":
            local_levelist = get_value_from_dict(levelist, self.date)
            logger.info("levelist:{}", local_levelist)

        elif self.levtype == "SOL":
            local_levelist = "1/2/3/4/5"
            logger.info("levelist:{}", local_levelist)

        else:
            return

        # Add levelist to request
        self.request["LEVELIST"] = [local_levelist]

    def update_based_on_target(self, source: str):
        """Update request based on target.

        Args:
            source      (Path): Path to source
        Raises:
            ValueError (error): wrong method used
        """
        logger.info("Target: {}", self.target)
        if "mars_latlonZ" in self.target:
            self.request.update(
                {
                    "SOURCE": [source],
                    "REPRESS": ["GG"],
                    "STEP": ["00"],
                }
            )
            if self.class_ == "D1":
                self.request.update(
                    {
                        "DATE": ["20201019"],
                        "TIME": ["1200"],
                        "CLASS": ["OD"],
                    }
                )

        elif "mars_latlonGG" in self.target:
            self.request["SOURCE"] = [source]
        else:
            raise ValueError("Wrong call of read")

    def add_database_options(self):
        """Add database options."""
        if self.class_ == "D1":
            self.request.update(
                {
                    "DATASET": "extremes-dt",
                }
            )
            if "latlon" not in self.target:
                self.request.update(
                    {
                        "DATABASE": "fdb",
                    }
                )

    def to_dataframe(self):
        """Write request to dataframe."""
        return pd.DataFrame(data=self.request)

    def replace(self, **kwargs):
        """Return new instance with updated values."""
        return replace(self, **kwargs)
