#!/usr/bin/env python3
"""Implement TimeWindow object, TimeWindow container, and related routines."""
from datetime import datetime, timezone

import attrs
import dateutil.parser
import humanize
import numpy as np
import pandas as pd
from dateutil.utils import default_tzinfo

from .logs import get_logger

logger = get_logger(__name__)

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


def as_time_offset(obj):
    """Convert obj to a timedelta obj in a format that can be added to datetime objs."""
    return pd.tseries.frequencies.to_offset(as_timedelta(obj))


@np.vectorize
def datetime2epoch(dt):
    """Convert datetime object into unix epoch."""
    reference = datetime(1970, 1, 1, tzinfo=timezone.utc)
    epoch = int((dt - reference).total_seconds())
    return epoch


class TimeWindow(pd.Interval):
    """A single time window."""

    def __init__(self, mid, length, closed="left"):
        """Initialise a pd.Interval-like obj given 'mid' and 'length'."""
        mid = as_datetime(mid)
        length = as_time_offset(length)
        left = mid - 0.5 * length
        right = left + length
        super().__init__(left, right, closed)

    def __str__(self):
        return f"<{self.mid} \u00B1 {humanize.precisedelta(0.5 * self.length)}>"

    def __reduce__(self):
        """Tell pickle how to serialise objects of this class."""
        return type(self), (self.mid, self.length, self.closed)

    @classmethod
    def from_interval(cls, interval: pd.Interval):
        """Construct an instance from a pd.Interval object."""
        return cls(interval.mid, interval.length, interval.closed)


@attrs.frozen(kw_only=True)
class TimeWindowContainer:
    """A container for instances of the TimeWindow class."""

    data = attrs.field()
    cycle_length = attrs.field()

    @classmethod
    def from_start_end_and_length(cls, start, end, cycle_length):
        """Build the data in the container taking (start, end, cycle_length) as args."""
        return cls(
            data=pd.date_range(start, end, freq=cycle_length), cycle_length=cycle_length
        )

    def __getitem__(self, item):
        return TimeWindow(mid=self.data[item], length=self.cycle_length)

    def __len__(self):
        return len(self.data)
