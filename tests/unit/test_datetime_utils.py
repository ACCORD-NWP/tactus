#!/usr/bin/env python3
"""Unit tests for datetime_utils.py."""
import datetime

import pytest

from deode.datetime_utils import as_datetime, as_timedelta, dt2str, oi2dt_list


def test_as_datetime():
    dt = as_datetime("20181010T21")
    assert dt == datetime.datetime(2018, 10, 10, 21, tzinfo=datetime.timezone.utc)


def test_as_timedelta():
    assert as_timedelta("PT3H") == datetime.timedelta(hours=3)


def test_as_dt2str():
    assert dt2str(as_timedelta("PT3H30M10S")) == "0003:30:10"


@pytest.fixture(params=["PT3H", "PT6H:PT3H", "PT0H:PT6H:PT3H"])
def _test_as_oi2dt_list():
    assert oi2dt_list(param, "PT6H") == [  # noqa
        datetime.timedelta(0),
        datetime.timedelta(seconds=10800),
        datetime.timedelta(seconds=21600),
    ]
