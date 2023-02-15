#!/usr/bin/env python3
"""Implement TimeWindow object, TimeWindow container, and related routines."""
from datetime import datetime, timezone

import attrs
import dateutil.parser
import humanize
import numpy as np
import pandas as pd
from dateutil.utils import default_tzinfo
from pandas.tseries.frequencies import to_offset

from .logs import get_logger

logger = get_logger(__name__)


def as_datetime(obj):
    """Convert dt to string, parse into datetime and add UTC timezone iff naive."""
    return default_tzinfo(dateutil.parser.parse(str(obj)), tzinfo=timezone.utc)


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
        length = to_offset(length)
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
