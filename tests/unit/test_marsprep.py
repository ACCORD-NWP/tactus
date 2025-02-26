#!/usr/bin/env python3
"""Unit tests for the marsprep."""

from unittest.mock import mock_open, patch

import pytest
import tomlkit

from deode.derived_variables import derived_variables, set_times
from deode.mars_utils import BaseRequest, get_value_from_dict
from deode.tasks.marsprep import Marsprep


@pytest.fixture(scope="module")
def base_parsed_config(default_config):
    """Return a parsed config common to all tasks."""
    config = default_config
    config = config.copy(update=set_times(config))
    config = config.copy(update=derived_variables(config))

    return config


@pytest.fixture(params=["HRES", "atos_bologna_DT"], scope="module")
def parsed_config(request, base_parsed_config, tmp_directory):
    """Return a parsed config common to tasks."""
    config_patch = tomlkit.parse(
        f"""
        [boundaries]
            ifs.selection = "{request.param}"
        [system]
            wrk = "{tmp_directory}"

        """
    )

    config = base_parsed_config.copy(update=config_patch)
    return config


@pytest.fixture()
def marsprep_instance(parsed_config):
    """Create a Marsprep instance using the parsed config."""
    instance = Marsprep(parsed_config)
    return instance


def test_mars_selection(marsprep_instance):
    """Test the mars_selection method with the parsed config."""
    selection = marsprep_instance.mars_selection()

    assert "expver" in selection


def test_update_data_request(marsprep_instance):
    """Test the update_data_request method with the parsed config."""
    param = (
        get_value_from_dict(marsprep_instance.mars["GG"], marsprep_instance.init_date_str)
        + "/"
        + get_value_from_dict(
            marsprep_instance.mars["GG_sea"], marsprep_instance.init_date_str
        )
    )
    base_request = BaseRequest(
        class_=marsprep_instance.mars["class"],
        data_type=marsprep_instance.mars["type_FC"],
        expver=marsprep_instance.mars["expver"],
        levtype="SFC",
        date=marsprep_instance.init_date_str,
        time=marsprep_instance.init_hour_str,
        steps="0/1/2",
        param=param,
        target='"test+[STEP]"',
    )

    marsprep_instance.update_data_request(
        base_request,
        prefetch=True,
        specify_domain=False,
        members=["1", "2"],
    )

    assert "NUMBER" in base_request.request
    assert base_request.request["NUMBER"] == ["1/2"]
    assert "STREAM" in base_request.request
    assert base_request.request["TARGET"] == ['"test_[NUMBER]+[STEP]"']

    param = get_value_from_dict(
        marsprep_instance.mars["SHZ"], marsprep_instance.init_date_str
    )
    request_shz = BaseRequest(
        class_=marsprep_instance.mars["class"],
        data_type=marsprep_instance.mars["GGZ_type"],
        expver=marsprep_instance.mars["expver"],
        levtype="ML",
        date=marsprep_instance.init_date_str,
        time=marsprep_instance.init_hour_str,
        steps="00",
        param=param,
        target="mars_latlonZ",
    )
    marsprep_instance.update_data_request(
        request_shz, prefetch=False, members=[], specify_domain=True, source="test_source"
    )
    assert "NUMBER" not in request_shz.request
    assert request_shz.request["SOURCE"] == ["test_source"]
    assert "GRID" in request_shz.request
    assert "AREA" in request_shz.request


@patch("builtins.open", new_callable=mock_open, read_data=b"mocked binary data")
@patch("os.remove")  # Mock os.remove to prevent actual file deletion
def test_add_additional_data(mock_remove, mock_open, marsprep_instance):
    """Test the add_additional_data method."""
    marsprep_instance.additional_data = {}

    # Run the method that should read the file and add the data to the dictionary
    marsprep_instance.add_additional_data("test_file", "001")

    # Assert that the correct data has been added
    assert "001" in marsprep_instance.additional_data
    assert (
        marsprep_instance.additional_data["001"] == b"mocked binary data"
    )  # Ensure the binary data was added correctly

    # Check that open was called with the correct arguments
    mock_open.assert_called_with("test_file", "rb")

    # Ensure that os.remove was called to remove the file
    mock_remove.assert_called_with("test_file")
