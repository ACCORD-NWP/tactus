#!/usr/bin/env python3
"""Unit tests for the marsprep."""

import pytest
import tomlkit

from tactus.derived_variables import derived_variables, set_times
from tactus.mars_utils import (
    BaseRequest,
    compile_target,
    get_value_from_dict,
    mars_selection,
)
from tactus.tasks.marsprep import Marsprep


@pytest.fixture(scope="module")
def base_parsed_config(default_config):
    """Return a parsed config common to all tasks."""
    config = default_config.copy(update=set_times(default_config))
    return config.copy(update=derived_variables(config))


@pytest.fixture(
    name="parsed_config_and_selection",
    # (selection, basetime). HRES from 2028 switches to HRES_GRIB2.
    params=[
        ("HRES", None),
        ("HRES", "2028-01-02T00:00:00Z"),
        ("atos_bologna_DT", None),
    ],
    ids=["HRES", "HRES-2028", "atos_bologna_DT"],
    scope="module",
)
def fixture_parsed_config_and_selection(request, base_parsed_config, tmp_directory):
    """Return a parsed config common to tasks."""
    selection, basetime = request.param
    times = ""
    if basetime is not None:
        times = f"""
        [general.times]
            basetime = "{basetime}"
            end = "{basetime}"
            start = "{basetime}"
            validtime = "{basetime}"
        """
    config_patch = tomlkit.parse(
        f"""
        {times}
        [boundaries]
            ifs.selection = "{selection}"
        [system]
            wrk = "{tmp_directory}"
        [eps.member_settings.boundaries.ifs]
            bdmember = 0

        """
    )

    config = base_parsed_config.copy(update=config_patch)
    return selection, config


@pytest.fixture(name="marsprep_instance")
def fixture_marsprep_instance(parsed_config_and_selection):
    """Create a Marsprep instance using the parsed config."""
    _, config = parsed_config_and_selection
    return Marsprep(config)


def test_mars_selection(parsed_config_and_selection):
    """Test the mars_selection method with the parsed config."""
    selection_str, config = parsed_config_and_selection
    selection = mars_selection(selection=selection_str, config=config)

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

    tag = "test"
    members = [1, 2]

    base_request = BaseRequest(
        class_=marsprep_instance.mars["class"],
        data_type=marsprep_instance.mars["type_FC"],
        expver=marsprep_instance.mars["expver"],
        levtype="SFC",
        date=marsprep_instance.init_date_str,
        time=marsprep_instance.init_hour_str,
        steps="0/1/2",
        param=param,
        target=compile_target(tag, "perturbed_members", members),
    )

    marsprep_instance.update_data_request(
        base_request,
        prefetch=True,
        specify_domain=False,
        bdmember=members,
    )

    assert "NUMBER" in base_request.request
    assert base_request.request["NUMBER"] == "1/2"
    assert "STREAM" in base_request.request
    assert base_request.request["TARGET"] == f'"{tag}_[NUMBER]+[STEP]"'

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
        bdmember=[],
        specify_domain=True,
        source="test_source",
    )
    assert "NUMBER" not in request_shz.request
    assert request_shz.request["SOURCE"] == "test_source"
    assert "GRID" in request_shz.request
    assert "AREA" in request_shz.request


def test_update_data_request_control_stream(marsprep_instance: Marsprep):
    """Test that bdmember=[0] always selects stream_control, without NUMBER.

    Regression test: geopotential retrieval must always use control-member
    semantics, regardless of which member's file it reads from.
    """
    param = get_value_from_dict(
        marsprep_instance.mars["SHZ"], marsprep_instance.init_date_str
    )
    marsprep_instance.mars["stream"] = "ENFO"
    marsprep_instance.mars["stream_control"] = "OPER"
    request = BaseRequest(
        class_=marsprep_instance.mars["class"],
        data_type=marsprep_instance.mars["GGZ_type"],
        expver=marsprep_instance.mars["expver"],
        levtype="ML",
        date=marsprep_instance.init_date_str,
        time=marsprep_instance.init_hour_str,
        steps="00",
        param=param,
        target="mars_latlonZ_0",
    )
    marsprep_instance.use_static_sh_oro = False
    marsprep_instance.use_static_gg_oro = False
    marsprep_instance.update_data_request(
        request,
        prefetch=False,
        bdmember=[0],
        specify_domain=True,
        source="test_source",
    )

    assert "NUMBER" not in request.request
    assert request.request["STREAM"] == marsprep_instance.mars["stream_control"]

    request_perturbed = BaseRequest(
        class_=marsprep_instance.mars["class"],
        data_type=marsprep_instance.mars["GGZ_type"],
        expver=marsprep_instance.mars["expver"],
        levtype="ML",
        date=marsprep_instance.init_date_str,
        time=marsprep_instance.init_hour_str,
        steps="00",
        param=param,
        target="mars_latlonZ_1",
    )
    marsprep_instance.update_data_request(
        request_perturbed,
        prefetch=False,
        bdmember=[1],
        specify_domain=True,
        source="test_source",
    )

    assert request_perturbed.request["NUMBER"] == "1"
    assert request_perturbed.request["STREAM"] != marsprep_instance.mars["stream_control"]


def test_get_lat_lon_data_soil_layer(marsprep_instance: Marsprep, monkeypatch):
    """Soil layer fields (levtype SOL) are added to the SFX lat/lon data if set."""
    requests = []
    monkeypatch.setattr(
        marsprep_instance,
        "_build_and_run_retrieve_request",
        lambda **kwargs: requests.append(kwargs),
    )

    marsprep_instance.get_lat_lon_data([0])

    soil_requests = [r for r in requests if r["target"] == "mars_latlonGG_soil_layer_0"]
    if marsprep_instance.exist_soil_layer:
        assert len(soil_requests) == 1
        request = soil_requests[0]
        assert request["levtype"] == "SOL"
        assert request["param"] == marsprep_instance.param_soil_layer
        assert request["levelist"] == marsprep_instance.soil_layer_levelist
        assert request["specify_domain"]
    else:
        assert soil_requests == []


def test_hres_grib2_sh_lnsp_not_first(base_parsed_config):
    """In GRIB2, lnsp (152) has no pv, and IAL reads pv from the first SH field."""
    selection = mars_selection(selection="HRES_GRIB2", config=base_parsed_config)
    sh_params = str(selection["SH"]).split("/")

    assert "152" in sh_params
    assert sh_params[0] != "152"


def test_hres_selection_by_date(base_parsed_config, tmp_directory):
    """HRES uses the GRIB1 settings before the switch date and GRIB2 after it."""
    for basetime, snow_albedo, soil_layer in [
        ("2027-12-31T18:00:00Z", "32", False),
        ("2028-01-01T00:00:00Z", "228032", True),
    ]:
        config = base_parsed_config.copy(
            update=tomlkit.parse(
                f"""
                [general.times]
                    basetime = "{basetime}"
                    end = "{basetime}"
                    start = "{basetime}"
                    validtime = "{basetime}"
                [boundaries]
                    bdshift = "PT0H"
                    ifs.selection = "HRES"
                [system]
                    wrk = "{tmp_directory}"
                """
            )
        )
        marsprep = Marsprep(config)
        assert marsprep.snow_albedo_param == snow_albedo
        assert marsprep.exist_soil_layer == soil_layer
