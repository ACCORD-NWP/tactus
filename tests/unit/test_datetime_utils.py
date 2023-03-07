#!/usr/bin/env python3
"""Unit tests for the TimeWindow/TimeWindowContainer objects."""
import pickle

import pandas as pd
import pytest

from deode.datetime_utils import (
    TimeWindow,
    TimeWindowContainer,
    as_datetime,
    as_timedelta,
)


class TestTimeWindow:
    def test_time_window(self):
        mid = as_datetime("20181010T21")
        length = "PT3H"
        length_as_offset = as_timedelta(length)
        tw = TimeWindow(mid, length=length)
        assert isinstance(tw, TimeWindow)
        assert tw.mid == mid
        assert tw.left == mid - 0.5 * length_as_offset
        assert tw.right == mid + 0.5 * length_as_offset
        assert tw.length == length_as_offset

    def test_can_pickle(self, tmp_path):
        time_window = TimeWindow("20180110T12", length="PT3H")
        fname = tmp_path / "time_window.pickle"
        with open(fname, "wb") as f:
            pickle.dump(time_window, f)
        with open(fname, "rb") as f:
            time_window2 = pickle.load(f)
        assert isinstance(time_window2, TimeWindow)
        assert time_window2 == time_window


class TestTimeWindowContainer:
    @pytest.mark.timeout(1)
    def test_time_window_container_from_iterable(self):
        data = pd.date_range("18950101T00", "20000101T00", freq="1T")
        container = TimeWindowContainer(data=data, cycle_length="1H")
        assert isinstance(container[len(container) // 2], TimeWindow)

    def test_time_window_container_from_start_end_and_length(self):
        start = "18950101T00"
        end = "20000101T00"
        cycle_legth = "1H"
        container = TimeWindowContainer.from_start_end_and_length(
            start=start, end=end, cycle_length=cycle_legth
        )
        assert isinstance(container[len(container) // 2], TimeWindow)
