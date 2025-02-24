#!/usr/bin/env python3
"""Unit tests for datetime_utils.py."""
import datetime
from typing import List, Literal, Union

import pandas as pd
import pytest
from pytest_mock import MockFixture

from deode.datetime_utils import (
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
)


def test_as_datetime():
    dt = as_datetime("20181010T21")
    assert dt == datetime.datetime(2018, 10, 10, 21, tzinfo=datetime.timezone.utc)


def test_as_timedelta():
    assert as_timedelta("PT3H") == datetime.timedelta(hours=3)


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
    truth_bdshift = {"PT3H": -3, "PT0H": 0}
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
        ("PT3H", [[pd.Timedelta(hours=0), pd.Timedelta(hours=6), pd.Timedelta(hours=3)]]),
        (
            ["PT0H:PT6H:PT3H"],
            [[pd.Timedelta(hours=0), pd.Timedelta(hours=6), pd.Timedelta(hours=3)]],
        ),
    ],
)
def test_oi2dt_list(
    output_settings: Union[str, List[str]],
    expanded_output_settings: List[List[pd.Timedelta]],
    mocker: MockFixture,
):
    """The that oi2dt_list returns the expected list of timedelta objects.

    Args:
        output_settings (Union[str, List[str]]): The output settings.
        expanded_output_settings (List[List[pd.Timedelta]]):
            The expanded output settings to be return by expand_output_settings.
        mocker (MockFixture): The mocker object used to mock functions.
    """
    forecast_range = "PT6H"
    mocker.patch(
        "deode.datetime_utils.expand_output_settings",
        return_value=expanded_output_settings,
    )

    assert oi2dt_list(output_settings, forecast_range) == [
        pd.Timedelta(hours=0),
        pd.Timedelta(hours=3),
        pd.Timedelta(hours=6),
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


@pytest.mark.parametrize("param", ["2024-02-03T00:00:00Z", "2023-10-10T00:00:00Z"])
def test_get_month_list(
    param: Union[Literal["2024-02-03T00:00:00Z"], Literal["2023-10-10T00:00:00Z"]],
):
    truth = {"2024-02-03T00:00:00Z": [10, 11, 12, 1, 2], "2023-10-10T00:00:00Z": [10]}

    assert get_month_list("2023-10-02T00:00:00Z", param) == truth[param]


@pytest.mark.parametrize(
    ("output_settings", "forecast_range", "expected"),
    [
        ("", "PT6H", []),
        ("PT1H", "PT6H", [[pd.Timedelta("0h"), pd.Timedelta("6h"), pd.Timedelta("1h")]]),
        (
            ["PT0H:PT6H:PT1H", "PT6H:PT12H:PT2H"],
            "PT12H",
            [
                [pd.Timedelta("0h"), pd.Timedelta("6h"), pd.Timedelta("1h")],
                [pd.Timedelta("6h"), pd.Timedelta("12h"), pd.Timedelta("2h")],
            ],
        ),
        (
            ("PT0H:PT6H:PT1H", "PT6H:PT12H:PT2H"),
            "PT12H",
            [
                [pd.Timedelta("0h"), pd.Timedelta("6h"), pd.Timedelta("1h")],
                [pd.Timedelta("6h"), pd.Timedelta("12h"), pd.Timedelta("2h")],
            ],
        ),
    ],
)
def test_expand_output_settings(
    output_settings: Union[str, List[str], Union[str]],
    forecast_range: str,
    expected: List[List[pd.Timedelta]],
    mocker: MockFixture,
):
    """Test that expand_output_settings expands the output settings correctly.

    Args:
        output_settings (Union[str, List[str], Union[str]]): The output settings.
        forecast_range (str): The forecast range.
        expected (List[List[pd.Timedelta]]): The expected expanded output settings.
        mocker (MockFixture): The mocker object used to mock functions.
    """
    mocker.patch("deode.datetime_utils.check_syntax")
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
    mocker.patch("deode.datetime_utils.check_syntax")
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
