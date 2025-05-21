#!/usr/bin/env python3
"""Unit tests for the marsprep."""


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


@pytest.fixture(
    name="parsed_config_and_selection", params=["HRES", "atos_bologna_DT"], scope="module"
)
def fixture_parsed_config_and_selection(request, base_parsed_config, tmp_directory):
    """Return a parsed config common to tasks."""
    selection = request.param
    config_patch = tomlkit.parse(
        f"""
        [boundaries]
            ifs.selection = "{selection}"
        [system]
            wrk = "{tmp_directory}"

        """
    )

    config = base_parsed_config.copy(update=config_patch)
    return selection, config


@pytest.fixture(name="marsprep_instance")
def fixture_marsprep_instance(parsed_config_and_selection):
    """Create a Marsprep instance using the parsed config."""
    _, config = parsed_config_and_selection
    instance = Marsprep(config)
    return instance


def test_mars_selection(parsed_config_and_selection):
    """Test the mars_selection method with the parsed config."""
    selection_str, config = parsed_config_and_selection
    selection = Marsprep.mars_selection(selection=selection_str, config=config)

    assert "expver" in selection


def test_update_data_request(marsprep_instance: Marsprep):
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
        bdmembers=[1, 2],
    )

    assert "NUMBER" in base_request.request
    assert base_request.request["NUMBER"] == "1/2"
    assert "STREAM" in base_request.request
    assert base_request.request["TARGET"] == '"test_[NUMBER]+[STEP]"'

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
    # make sure not to trigger get_mars_keys (not supported in test)
    marsprep_instance.use_static_sh_oro = False
    marsprep_instance.use_static_gg_oro = False
    marsprep_instance.update_data_request(
        request_shz,
        prefetch=False,
        bdmembers=[],
        specify_domain=True,
        source="test_source",
    )
    assert "NUMBER" not in request_shz.request
    assert request_shz.request["SOURCE"] == "test_source"
    assert "GRID" in request_shz.request
    assert "AREA" in request_shz.request
