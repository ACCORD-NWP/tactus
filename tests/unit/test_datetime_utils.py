#!/usr/bin/env python3
"""Unit tests for datetime_utils.py."""
import datetime

import pytest

from deode.datetime_utils import (
    as_datetime,
    as_timedelta,
    cycle_offset,
    dt2str,
    get_decade,
    oi2dt_list,
)


def test_as_datetime():
    dt = as_datetime("20181010T21")
    assert dt == datetime.datetime(2018, 10, 10, 21, tzinfo=datetime.timezone.utc)


def test_as_timedelta():
    assert as_timedelta("PT3H") == datetime.timedelta(hours=3)


def test_as_dt2str():
    assert dt2str(as_timedelta("PT3H30M10S")) == "0003:30:10"


@pytest.mark.parametrize("param", ["05", "15", "25", "29", "31"])
def test_get_decade(param):
    truth = {"05": "1205", "15": "1215", "25": "1225", "29": "0105", "31": "0105"}
    dt = as_datetime(f"202312{param}T00")
    assert get_decade(dt) == truth[param]


@pytest.mark.parametrize("param", ["PT3H", "PT0H", "-PT3H"])
def test_offsetparam(param):
    truth = {"PT3H": -3, "PT0H": 0, "-PT3H": 3}
    basetime = as_datetime("20181010T21")
    bdcycle = as_timedelta("PT3H")
    shift = as_timedelta(param)
    assert datetime.timedelta(hours=truth[param]) == cycle_offset(
        basetime, bdcycle, shift=shift
    )


@pytest.mark.parametrize("param", ["PT3H", ["PT0H:PT6H:PT3H"]])
def test_oi2dt_list(param):
    assert oi2dt_list(param, "PT6H") == [
        datetime.timedelta(seconds=0),
        datetime.timedelta(seconds=10800),
        datetime.timedelta(seconds=21600),
    ]
