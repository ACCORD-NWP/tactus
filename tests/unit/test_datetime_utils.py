#!/usr/bin/env python3
"""Unit tests for datetime_utils.py."""

import datetime
from typing import List, Literal, Union

import pytest
from isodate.duration import Duration
from pytest_mock import MockFixture

from tactus.datetime_utils import (
    as_datetime,
    as_timedelta,
    check_syntax,
    cycle_offset,
    dt2str,
    expand_output_settings,
    get_decadal_list,
    get_decade,
    get_month_list,
    oi2dt_list,
    since_str,
)


def test_as_datetime():
    dt = as_datetime("20181010T21")
    assert dt == datetime.datetime(2018, 10, 10, 21, tzinfo=datetime.timezone.utc)


@pytest.mark.parametrize(
    ("param", "ref"),
    [
        ("PT3H", datetime.timedelta(hours=3)),
        ("PT144H", datetime.timedelta(hours=144)),
        ("P4M", Duration(months=4)),
        ("P5D", datetime.timedelta(days=5)),
        ("PT6M", datetime.timedelta(minutes=6)),
        (datetime.timedelta(hours=3), datetime.timedelta(hours=3)),
    ],
)
def test_timedelta(param, ref):
    assert as_timedelta(param) == ref


def test_as_dt2str():
    assert dt2str(as_timedelta("PT3H30M10S")) == "0003:30:10"


@pytest.mark.parametrize("param", ["05", "15", "25", "29", "31"])
def test_get_decade(
    param: Union[
        Literal["05"], Literal["15"], Literal["25"], Literal["29"], Literal["31"]
    ],
):
    truth = {"05": "1205", "15": "1215", "25": "1225", "29": "0105", "31": "0105"}
    dt = as_datetime(f"202312{param}T00")
    assert get_decade(dt) == truth[param]


@pytest.mark.parametrize("param", ["PT3H", "PT0H"])
def test_offsetparam(param: Union[Literal["PT3H"], Literal["PT0H"]]):
    truth_bdshift = {"PT3H": 3, "PT0H": 0}
    truth_bdcycle_start = {"PT3H": 0, "PT0H": 3}
    basetime = as_datetime("20181010T21")
    bdcycle = as_timedelta("PT3H")
    shift = as_timedelta(param)
    assert datetime.timedelta(hours=truth_bdshift[param]) == cycle_offset(
        basetime, bdcycle, bdshift=shift
    )
    bdcycle = as_timedelta("PT6H")
    assert datetime.timedelta(hours=truth_bdcycle_start[param]) == cycle_offset(
        basetime, bdcycle, bdcycle_start=shift
    )


@pytest.mark.parametrize(
    ("output_settings", "expanded_output_settings"),
    [
        (
            "PT3H",
            [
                [
                    datetime.timedelta(hours=0),
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=3),
                ]
            ],
        ),
        (
            ["PT0H:PT6H:PT3H"],
            [
                [
                    datetime.timedelta(hours=0),
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=3),
                ]
            ],
        ),
    ],
)
def test_oi2dt_list(
    output_settings: Union[str, List[str]],
    expanded_output_settings: List[List[datetime.timedelta]],
    mocker: MockFixture,
):
    """The that oi2dt_list returns the expected list of timedelta objects.

    Args:
        output_settings (Union[str, List[str]]): The output settings.
        expanded_output_settings (List[List[datetime.timedelta]]):
            The expanded output settings to be return by expand_output_settings.
        mocker (MockFixture): The mocker object used to mock functions.
    """
    forecast_range = "PT6H"
    mocker.patch(
        "tactus.datetime_utils.expand_output_settings",
        return_value=expanded_output_settings,
    )

    assert oi2dt_list(output_settings, forecast_range) == [
        datetime.timedelta(hours=0),
        datetime.timedelta(hours=3),
        datetime.timedelta(hours=6),
    ]


@pytest.mark.parametrize("param", ["05", "30"])
def test_get_decadal_list(param: Union[Literal["05"], Literal["30"]]):
    truth = {
        "05": [datetime.datetime(2018, 12, 5, 0, tzinfo=datetime.timezone.utc)],
        "30": [
            datetime.datetime(2018, 12, 5, 0, tzinfo=datetime.timezone.utc),
            datetime.datetime(2018, 12, 9, 0, tzinfo=datetime.timezone.utc),
            datetime.datetime(2018, 12, 19, 0, tzinfo=datetime.timezone.utc),
            datetime.datetime(2018, 12, 29, 0, tzinfo=datetime.timezone.utc),
        ],
    }
    dt = as_datetime(f"201812{param}T00")
    assert (
        get_decadal_list(
            datetime.datetime(2018, 12, 5, 0, tzinfo=datetime.timezone.utc), dt
        )
        == truth[param]
    )


@pytest.mark.parametrize(
    ("start", "end", "ref"),
    [
        (
            "2023-10-05T00:00:00Z",
            "2024-12-31T00:00:00Z",
            [10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        ),
        ("2023-10-05T00:00:00Z", "2024-03-02T00:00:00Z", [10, 11, 12, 1, 2, 3]),
        ("2023-10-05T00:00:00Z", "2024-03-01T00:00:00Z", [10, 11, 12, 1, 2, 3]),
        ("2023-10-05T00:00:00Z", "2024-02-03T00:00:00Z", [10, 11, 12, 1, 2]),
        ("2023-10-05T00:00:00Z", "2023-10-10T00:00:00Z", [10]),
        ("2023-10-05T00:00:00Z", "2023-10-05T00:00:00Z", [10]),
        ("2023-10-01T00:00:00Z", "2023-10-31T23:59:59Z", [10]),
    ],
)
def test_get_month_list(end: str, start: str, ref: List[int]):
    assert get_month_list(start, end) == ref


@pytest.mark.parametrize(
    ("output_settings", "forecast_range", "expected"),
    [
        ("", "PT6H", []),
        (
            "PT1H",
            "PT6H",
            [
                [
                    datetime.timedelta(hours=0),
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=1),
                ]
            ],
        ),
        (
            ["PT0H:PT6H:PT1H", "PT6H:PT12H:PT2H"],
            "PT12H",
            [
                [
                    datetime.timedelta(hours=0),
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=1),
                ],
                [
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=12),
                    datetime.timedelta(hours=2),
                ],
            ],
        ),
        (
            ("PT0H:PT6H:PT1H", "PT6H:PT12H:PT2H"),
            "PT12H",
            [
                [
                    datetime.timedelta(hours=0),
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=1),
                ],
                [
                    datetime.timedelta(hours=6),
                    datetime.timedelta(hours=12),
                    datetime.timedelta(hours=2),
                ],
            ],
        ),
    ],
)
def test_expand_output_settings(
    output_settings: Union[str, List[str], Union[str]],
    forecast_range: str,
    expected: List[List[datetime.timedelta]],
    mocker: MockFixture,
):
    """Test that expand_output_settings expands the output settings correctly.

    Args:
        output_settings (Union[str, List[str], Union[str]]): The output settings.
        forecast_range (str): The forecast range.
        expected (List[List[datetime.timedelta]]): The expected expanded output settings.
        mocker (MockFixture): The mocker object used to mock functions.
    """
    mocker.patch("tactus.datetime_utils.check_syntax")
    assert expand_output_settings(output_settings, forecast_range) == expected


@pytest.mark.parametrize(
    ("output_settings", "forecast_range", "exception"),
    [
        (["PT0H:PT6H:PT0H"], "PT6H", RuntimeError),
    ],
)
def test_expand_output_settings_exceptions(
    output_settings: List[str],
    forecast_range: Literal["PT6H"],
    exception: type[RuntimeError],
    mocker: MockFixture,
):
    """Test that expand_output_settings raises an exception if the output settings are invalid.

    Args:
        output_settings (List[str]): The output settings.
        forecast_range (Literal["PT6H"]): The forecast range.
        exception (type[RuntimeError]): The expected exception.
        mocker (MockFixture): The mocker object used to mock functions.
    """
    mocker.patch("tactus.datetime_utils.check_syntax")
    with pytest.raises(exception):
        expand_output_settings(output_settings, forecast_range)


@pytest.mark.parametrize(
    ("output_settings", "length", "exception"),
    [
        (["PT0H:PT6H"], 2, SystemExit),
        (["PT0H:PT6H:PT1H"], 1, SystemExit),
    ],
)
def test_check_syntax_exceptions(
    output_settings: List[str],
    length: int,
    exception: type[SystemExit],
):
    """Test that check_syntax raises an exception if the output settings are invalid.

    Args:
        output_settings (List[str]): The output settings.
        length (int): The expected length of the output settings.
        exception (type[SystemExit]): The expected exception.
    """
    with pytest.raises(exception):
        check_syntax(output_settings, length)


@pytest.mark.parametrize(
    ("output_settings", "length"),
    [
        (["PT0H:PT6H"], 1),
        (["PT0H:PT6H:PT1H"], 2),
    ],
)
def test_check_syntax_valid(output_settings: List[str], length: int):
    """Test that check_syntax does not raise an exception if the output settings are valid.

    Args:
        output_settings (List[str]): The output settings.
        length (int): The expected length of the output settings.
    """
    try:
        check_syntax(output_settings, length)
    except SystemExit:
        pytest.fail("check_syntax raised SystemExit unexpectedly!")


def test_since_str():
    """Test that since_str returns the expected string representation of the timestamp."""
    expected_results = {
        0: "Now",
        1: "Now",
        59: "Now",
        60: "1 min ago",
        90: "1 min ago",
        180: "3 min ago",
        3600: "1 hour ago",
        12000: "3 hours ago",
        86399: "23 hours ago",
        86400: "set_dynamically",
        186412: "set_dynamically",
        991261: "set_dynamically",
    }

    for time in expected_results:
        now_date = datetime.datetime(2026, 6, 1, 0, 0, 0)
        creation_date = now_date - datetime.timedelta(seconds=time)
        if expected_results[time] == "set_dynamically":
            expected_results[time] = f"updated on {creation_date}"
        assert since_str(creation_date, now_date) == expected_results[time]
