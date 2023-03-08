#!/usr/bin/env python3
"""Unit tests for datetime_utils.py."""
import datetime

from deode.datetime_utils import as_datetime, as_timedelta


def test_as_datetime():
    dt = as_datetime("20181010T21")
    assert dt == datetime.datetime(2018, 10, 10, 21, tzinfo=datetime.timezone.utc)


def test_as_timedelta():
    assert as_timedelta("PT3H") == datetime.timedelta(hours=3)
