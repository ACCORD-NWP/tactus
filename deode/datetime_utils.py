#!/usr/bin/env python3
"""Implement helper routines to deal with dates and times."""
from datetime import timezone

import dateutil.parser
import pandas as pd
from dateutil.utils import default_tzinfo

# The regex in a json schema's "pattern" must use JavaScript syntax (ECMA 262).
# <https://json-schema.org/understanding-json-schema/reference/regular_expressions.html>
ISO_8601_TIME_DURATION_REGEX = "^P(?!$)(\\d+Y)?(\\d+M)?(\\d+W)?(\\d+D)?"
ISO_8601_TIME_DURATION_REGEX += "(T(?=\\d+[HMS])(\\d+H)?(\\d+M)?(\\d+S)?)?$"
IRX = ISO_8601_TIME_DURATION_REGEX
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


def expand_output_settings(output_settings, forecast_range):
    """Expand the output_settings coming from config.

    Args:
        output_settings (tuple, list, str): Specifies the output steps
        forecast_range (str): Forecast range in duration syntax

    Returns:
        sections (list) : List of output subsections
    """
    if isinstance(output_settings, str):
        if output_settings.count(":") == 0:
            oi = ["PT0H:" + forecast_range + ":" + output_settings]
        elif output_settings.count(":") == 1:
            oi = ["PT0H:" + output_settings]
        else:
            oi = [output_settings]
    else:
        oi = output_settings

    z = ["PT0H:PT0H:PT0H"]
    for x in oi:
        if x.count(":") == 0:
            z.append(":".join([z[-1].split(":")[1], forecast_range, x]))
        elif x.count(":") == 1:
            z.append(z[-1].split(":")[1] + ":" + x)
        else:
            z.append(x)

    sections = []
    for x in z[1:]:
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

    return dt


def cycle_offset(basetime, dt, shift=DEFAULT_SHIFT):
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
