#!/usr/bin/env python3
"""Implement helper routines to deal with dates and times."""
from datetime import timezone

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


def check_syntax(output_settings, length):
    """Check syntax of output_settings.

    Args:
        output_settings (tuple, list, str): Specifies the output steps
        length (integer): length to check on

    Raises:
        SystemExit: General system handler

    """
    for x in output_settings:
        if x.count(":") != length:
            raise SystemExit(
                f"Invalid argument {output_settings} for output_settings.\nPlease provide single time increment as a string or a list of 'starttime:endtime:interval' choices"
            )


def expand_output_settings(output_settings, forecast_range):
    """Expand the output_settings coming from config.

    Args:
        output_settings (tuple, list, str): Specifies the output steps
        forecast_range (str): Forecast range in duration syntax

    Returns:
        sections (list) : List of output subsections

    """
    oi = []
    if isinstance(output_settings, str):
        check_syntax([output_settings], 0)
        oi = ["PT0H:" + forecast_range + ":" + output_settings]

    elif isinstance(output_settings, (tuple, list)):
        check_syntax(output_settings, 2)
        oi = output_settings

    sections = []
    for x in oi:
        sections.append([as_timedelta(y) for y in x.split(":")])

    return sections


def oi2dt_list(output_settings, forecast_range):
    """Build list of output occurences.

    Args:
        output_settings (tuple,list,str): Specifies the output steps
        forecast_range (str): Forecast range in duration syntax

    Returns:
        dt (list) : List of output occurences
    """

    sections = expand_output_settings(output_settings, forecast_range)

    dt = []
    cdt = as_timedelta("PT0H")
    fdt = as_timedelta(forecast_range)
    for s in sections:
        cdt = s[0]
        edt = s[1]
        while cdt <= edt and cdt <= fdt:
            if cdt not in dt:
                dt.append(cdt)
            cdt += s[2]

    dt.sort()
    return dt


def cycle_offset(basetime, dt, shift=DatetimeConstants.DEFAULT_SHIFT):
    """Calculcate offset from a reference time.

    Args:
        basetime (datetime): Reference time
        dt (timedelta): duration
        shift (timedelta): shift

    Returns:
        timedelta : a timdelta object of the offset

    """
    reftime = basetime.hour * 3600 + basetime.minute * 60 + basetime.second
    t = dt.days * 3600 * 24 + dt.seconds
    shift_seconds = shift.days * 3600 * 24 + shift.seconds
    k = reftime % t - shift_seconds
    return pd.Timedelta(seconds=k)


def get_decade(dt) -> str:
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
