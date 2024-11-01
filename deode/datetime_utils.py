#!/usr/bin/env python3
"""Implement helper routines to deal with dates and times."""
from datetime import timezone
from typing import List, Tuple, Union

import dateutil.parser
import pandas as pd
from dateutil.utils import default_tzinfo

from .aux_types import QuasiConstant


class DatetimeConstants(QuasiConstant):
    """Datetime-related constants."""

    # The regex in a json schema's "pattern" must use JavaScript syntax (ECMA 262). See
    # https://json-schema.org/understanding-json-schema/reference/regular_expressions.html
    ISO_8601_TIME_DURATION_REGEX = (
        "^P(?!$)(\\d+Y)?(\\d+M)?(\\d+W)?(\\d+D)?"
        + "(T(?=\\d+[HMS])(\\d+H)?(\\d+M)?(\\d+S)?)?$"
    )
    DEFAULT_SHIFT = pd.Timedelta(0)


def as_datetime(obj):
    """Convert obj to string, parse into datetime and add UTC timezone iff naive."""
    return default_tzinfo(dateutil.parser.parse(str(obj)), tzinfo=timezone.utc)


def as_timedelta(obj):
    """Convert obj to string and parse into pd.Timedelta."""
    return pd.Timedelta(str(obj))


def dt2str(dt):
    """Convert timdelta object to file name suitable string.

    Args:
        dt (timedelta object): duration

    Returns:
        duration (str): string representation of duration
                        suitable for FA files

    """
    h = int(dt.seconds / 3600) + int(dt.days * 24)
    m = int((dt.seconds % 3600 - dt.seconds % 60) / 60)
    s = int(dt.seconds % 60)

    duration = f"{h:04d}:{m:02d}:{s:02d}"
    return duration


def check_syntax(output_settings: Union[Tuple[str], List[str]], length: int):
    """Check syntax of output_settings.

    Args:
        output_settings (Union[Tuple[str], List[str]]): Specifies the output steps
        length (integer): length to check on

    Raises:
        SystemExit: General system handler

    """
    for x in output_settings:
        if x.count(":") != length:
            raise SystemExit(
                f"Invalid argument {output_settings} for output_settings.\n"
                "Please provide single time increment as a string "
                "or a list of 'starttime:endtime:interval' choices"
            )


def expand_output_settings(
    output_settings: Union[str, Tuple[str], List[str]], forecast_range: str
) -> List[List[pd.Timedelta]]:
    """Expand the output_settings coming from config.

    Args:
        output_settings (Union[str, Tuple[str], List[str]]):
            Specifies the output steps
        forecast_range (str): Forecast range in duration syntax

    Raises:
        RuntimeError: Handle erroneous time increment

    Returns:
        sections (List[List[pd.Timedelta]]) : List of output subsections.
            Can be empty in case of empty output_settings

    """
    output_intervals = []
    if isinstance(output_settings, str):
        check_syntax([output_settings], 0)
        # Infer the output intervals from the forecast range and output settings
        if output_settings != "":
            output_intervals = [":".join(["PT0H", forecast_range, output_settings])]
        else:
            return output_intervals
    elif isinstance(output_settings, (tuple, list)):
        check_syntax(output_settings, 2)
        output_intervals = output_settings

    # Check for zero size time increments
    zero_increment = as_timedelta("PT0H")
    for interval in output_intervals:
        if as_timedelta(interval.split(":")[2]) == zero_increment:
            raise RuntimeError(f"Zero size time increments not allowed:{interval}")

    # Convert the output intervals to a list of lists of timedelta objects
    sections = [
        [as_timedelta(item) for item in interval.split(":")]
        for interval in output_intervals
    ]

    return sections


def oi2dt_list(
    output_settings: Union[str, Tuple[str], List[str]], forecast_range: str
) -> List[pd.Timedelta]:
    """Build list of output occurences.

    Args:
        output_settings (Union[str, Tuple[str], List[str]]):
            Specifies the output steps
        forecast_range (str): Forecast range in duration syntax

    Returns:
        dt (List[pd.Timedelta]) : Sorted list of output occurences
    """
    sections = expand_output_settings(output_settings, forecast_range)

    output_dt = set()
    forecast_timedelta = as_timedelta(forecast_range)
    for start, end, step in sections:
        while start <= end and start <= forecast_timedelta:
            output_dt.add(start)
            start += step

    return sorted(output_dt)


def cycle_offset(
    basetime,
    bdcycle,
    bdcycle_start=DatetimeConstants.DEFAULT_SHIFT,
    bdshift=DatetimeConstants.DEFAULT_SHIFT,
):
    """Calculcate offset from a reference time.

    Args:
        basetime (datetime): Reference time
        bdcycle (timedelta): Interval between cycles
        bdcycle_start (timedelta): Time of day when bdcycle starts
        bdshift (timedelta): shift of boundary usage

    Returns:
        timedelta : a timdelta object of the offset

    """
    reftime = basetime.hour * 3600 + basetime.minute * 60 + basetime.second
    bdcycle_shift = (
        reftime - bdcycle_start.total_seconds() % bdcycle.total_seconds()
    ) % bdcycle.total_seconds()
    final_shift = bdcycle_shift - int(bdshift.total_seconds())
    return pd.Timedelta(seconds=final_shift)


def get_decade(dt) -> str:
    """Return the decade given a datetime object."""
    # Extract month and day from datetime object
    dtg_mm = int(dt.month)
    dtg_dd = int(dt.day)

    # Determine decades_mm and decades_dd based on dtg_dd
    if dtg_dd < 9:
        decades_mm = dtg_mm
        decades_dd = 5
    elif 8 < dtg_dd < 19:
        decades_mm = dtg_mm
        decades_dd = 15
    elif 18 < dtg_dd < 29:
        decades_mm = dtg_mm
        decades_dd = 25
    else:
        decades_mm = dtg_mm + 1
        if decades_mm == 13:
            decades_mm = 1
        decades_dd = 5

    decades_mm = f"{decades_mm:02d}"
    decades_dd = f"{decades_dd:02d}"

    return f"{decades_mm}{decades_dd}"


def get_decadal_list(dt_start, dt_end) -> list:
    """Return a list of dates for which decadal pgd files have to be created."""
    # check decade of start and end of period
    start_decade = get_decade(dt_start)
    end_decade = get_decade(dt_end)

    if start_decade != end_decade:
        # More than one decade is covered by period.
        if (dt_end - dt_start).days > 10:
            for x in range(0, (dt_end - dt_start).days, 10):
                if x == 0:
                    list_of_decades = [dt_start]
                else:
                    list_of_decades.append(dt_start + as_timedelta(f"P{x}D"))
        else:
            list_of_decades = [dt_start, dt_end]
    else:
        list_of_decades = [dt_start]
    return list_of_decades


def get_month_list(start, end) -> list:
    """Get list of months between to given dates (input as string)."""
    str_month_list = pd.date_range(start, end, freq="MS").strftime("%m").tolist()
    month_list = [int(i) for i in str_month_list]
    if len(month_list) == 0:
        month_list = [int(as_datetime(start).month)]
    else:
        month_list.insert(0, int(as_datetime(start).month))

    return month_list
