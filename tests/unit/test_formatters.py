"""Unit tests for the formatters module."""

from unittest.mock import patch

import pytest

from deode.formatters import duration_format_validator, duration_slice_format_validator


class TestDurationFormatValidator:
    """Unit tests for the duration_format_validator function."""

    @pytest.mark.parametrize(
        ("duration", "expected"),
        [
            ("P1D", True),  # 1 day
            ("PT1H", True),  # 1 hour
            ("PT1M", True),  # 1 minute
            ("PT1S", True),  # 1 second
            ("P0D", True),  # zero duration
            ("invalid", False),  # invalid format
            ("", False),  # empty string
        ],
    )
    def test_duration_format_validator(self, duration, expected):
        """Test duration_format_validator with various inputs."""
        result = duration_format_validator(duration)
        assert result == expected


class TestDurationSliceFormatValidator:
    """Unit tests for the duration_slice_format_validator function."""

    @patch("deode.formatters.duration_format_validator")
    @pytest.mark.parametrize(
        ("duration", "expected"),
        [
            ("P1D:PT1H", True),  # valid slice
            ("P1D:PT1H:PT1M", True),  # valid slice with step
            ("P1D:invalid", False),  # invalid slice
            ("invalid", False),  # invalid format
            ("", True),  # empty string
        ],
    )
    def test_duration_slice_format_validator(
        self, mock_duration_validator, duration, expected
    ):
        """Test duration_slice_format_validator with various inputs."""
        mock_duration_validator.side_effect = lambda x: x != "invalid"
        result = duration_slice_format_validator(duration)
        assert result == expected

    def test_with_empty_slice(self):
        """Test duration_slice_format_validator with an empty slice."""
        duration = ""
        result = duration_slice_format_validator(duration)
        assert result is False
