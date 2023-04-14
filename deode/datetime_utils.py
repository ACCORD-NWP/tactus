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
