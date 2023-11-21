#!/usr/bin/env python3
"""Unit tests for the Marsprep(config)."""

import ast
import contextlib
from pathlib import Path

import pandas as pd
import pytest
import tomlkit

from deode.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from deode.datetime_utils import as_datetime
from deode.geo_utils import Projection, Projstring
from deode.tasks.marsprep import Marsprep

WORKING_DIR = Path.cwd()


@pytest.fixture(scope="module", params=["CY46h1", "CY48t3", "CY48t3_target"])
def base_raw_config(request):
    """Return a raw config common to all tasks."""
    tag_map = {"CY46h1": ""}
    test_map = {"CY46h1": {"general": {"windfarm": True}}}
    tag = tag_map[request.param] if request.param in tag_map else f"_{request.param}"
    config = BasicConfig.from_file(ConfigParserDefaults.DIRECTORY / f"config{tag}.toml")
    try:
        config = config.copy(update=test_map[request.param])
    except KeyError:
        pass
    return config


def get_domain_data(config):
    """Read and return domain data.

    Args:
         config: config method

    Returns:
         String containing the domain info for MARS
    """
    # Get domain specs
    domain_spec = {
        "nlon": config["domain.nimax"],
        "nlat": config["domain.njmax"],
        "latc": config["domain.xlatcen"],
        "lonc": config["domain.xloncen"],
        "lat0": config["domain.xlat0"],
        "lon0": config["domain.xlon0"],
        "gsize": config["domain.xdx"],
    }

    # Get domain properties
    projstring = Projstring()
    projection = Projection(
        projstring.get_projstring(lon0=domain_spec["lon0"], lat0=domain_spec["lat0"])
    )
    domain_properties = projection.get_domain_properties(domain_spec)
    fdomainstr = "/".join(
        [
            str(domain_properties["maxlat"]),
            str(domain_properties["minlon"]),
            str(domain_properties["minlat"]),
            str(domain_properties["maxlon"]),
        ]
    )

    return fdomainstr


@pytest.fixture(params=["HRES", "RD_DEFAULT", "ATOS_RD"], scope="module")
def parsed_config(request, base_raw_config, tmp_path_factory):
    """Return a raw config common to tasks."""
    config = ParsedConfig(
        base_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )

    try:
        basetime = config["general.times.basetime"]
    except KeyError:
        basetime = config["general.times.start"]
    try:
        validtime = config["general.times.validtime"]
    except KeyError:
        validtime = basetime
    config_patch = tomlkit.parse(
        f"""
        [general.times]
            basetime = "{basetime}"
            validtime = "{validtime}"
        [mars]
            expver = "{request.param}"
        [system]
            wrk = "{tmp_path_factory.getbasetemp().as_posix()}"

        """
    )

    config = config.copy(update=config_patch)
    return config


def test_update_data_request(parsed_config):
    "Test update data request"

    config = parsed_config
    truth_selection = config["boundaries.ifs.selection"]
    try:
        mars = config[f"mars.{truth_selection}"]
    except KeyError:
        # This experiment is note defined fallback to RD_DEFAULT
        mars = config[f"mars.RD_DEFAULT"]
        mars["expver"] = truth_selection
        logger.warning("SELECTION={} not defined, using RD_DEFAULT", truth_selection)

    dateframe = Marsprep(config).split_date(
        Marsprep(config).basetime,
        Marsprep(config).strategy,
        int(
            Marsprep(config).forecast_range.days * 24
            + Marsprep(config).forecast_range.seconds / 3600
        ),
        Marsprep(config).bdint,
        int(Marsprep(config).bdshift.seconds / 3600),
    )
    date_str = dateframe.iloc[0].strftime("%Y%m%d")
    hour_str = dateframe.iloc[0].strftime("%H")

    step = int(Marsprep(config).bdint.total_seconds() / 3600)
    str_steps = [
        "{0:02d}".format(
            dateframe.index.tolist()[0]
            + (i * step)
            + Marsprep(config).basetime.hour % Marsprep(config).int_bdcycle
        )
        for i in range(len(dateframe.index.tolist()))
    ]
    truth_levelist = Marsprep(config).check_value(mars["levelist"], date_str)
    area_truth = get_domain_data(config)
    grid_truth = Marsprep(config).check_value(mars["grid"], date_str)
    truth_stream = "SCDA" if hour_str in {"06", "18"} else "OPER"
    truth_class = mars["class"]
    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "SFC",
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": str_steps,
        "PARAM": mars["GG"],
        "TYPE": "FC",
        "TARGET": '"ICMGG+[STEP]"',
        "PROCESS": "LOCAL",
    }
    param = Marsprep(config).mars["GG"]
    req = Marsprep(config).update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps=str_steps,
        prefetch=True,
        levtype="SFC",
        param=param,
        specify_domain=False,
        target='"ICMGG+[STEP]"',
    )
    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "SFC",
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": "00",
        "PARAM": mars["GG_sea"],
        "TYPE": "FC",
        "TARGET": "ICMGG.sea",
        "PROCESS": "LOCAL",
    }

    param = Marsprep(config).mars["GG_sea"]
    req = Marsprep(config).update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps="00",
        prefetch=True,
        levtype="SFC",
        param=param,
        specify_domain=False,
        target="ICMGG.sea",
    )

    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    if mars["expver"] == "0001":
        req_truth = {
            "CLASS": truth_class,
            "EXPVER": mars["expver"],
            "LEVTYPE": "SFC",
            "STREAM": truth_stream,
            "DATE": date_str,
            "TIME": hour_str,
            "STEP": "00",
            "PARAM": mars["GG_soil"],
            "TYPE": "AN",
            "TARGET": "ICMGG.soil",
            "PROCESS": "LOCAL",
        }

        param1 = Marsprep(config).mars["GG_soil"]
        param2 = Marsprep(config).mars["GG1"]

        req = Marsprep(config).update_data_request(
            data_type="analysis",
            date=date_str,
            time=hour_str,
            steps="00",
            prefetch=True,
            levtype="SFC",
            param=param1,
            specify_domain=False,
            target="ICMGG.soil",
        )
        req_dic = {}
        for col in req.columns:
            for _index, row in req.iterrows():
                req_dic[str(col)] = row[col]
        assert req_dic == req_truth

        req_truth = {
            "CLASS": truth_class,
            "EXPVER": mars["expver"],
            "LEVTYPE": "SFC",
            "STREAM": truth_stream,
            "DATE": date_str,
            "TIME": hour_str,
            "STEP": "00",
            "PARAM": mars["GG1"],
            "TYPE": "AN",
            "TARGET": "ICMGG",
            "GRID": "O640",
            "PROCESS": "LOCAL",
        }

        req = Marsprep(config).update_data_request(
            data_type="analysis",
            date=date_str,
            time=hour_str,
            steps="00",
            prefetch=True,
            levtype="SFC",
            param=param2,
            grid=Marsprep(config).check_value(mars["grid_GG1"], date_str),
            specify_domain=False,
            target="ICMGG",
        )
        req_dic = {}
        for col in req.columns:
            for _index, row in req.iterrows():
                req_dic[str(col)] = row[col]
        assert req_dic == req_truth

    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "ML",
        "LEVELIST": truth_levelist,
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": str_steps,
        "PARAM": mars["SH"],
        "TYPE": "FC",
        "TARGET": '"ICMSH+[STEP]"',
        "GRID": "AV",
        "PROCESS": "LOCAL",
    }

    param = Marsprep(config).mars["SH"]
    req = Marsprep(config).update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps=str_steps,
        prefetch=True,
        levtype="ML",
        param=param,
        grid=Marsprep(config).check_value(mars["grid_ML"], date_str),
        specify_domain=False,
        target='"ICMSH+[STEP]"',
    )
    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "ML",
        "LEVELIST": 1,
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": "00",
        "PARAM": mars["SHZ"],
        "TYPE": "AN" if mars["expver"] == "0001" else "FC",
        "TARGET": "ICMSH.Z",
        "GRID": "AV",
        "PROCESS": "LOCAL",
    }
    param = Marsprep(config).mars["SHZ"]
    d_type = Marsprep(config).mars["SHZ_type"]
    req = Marsprep(config).update_data_request(
        data_type=d_type,
        date=date_str,
        time=hour_str,
        steps="00",
        prefetch=True,
        levtype="ML",
        grid=Marsprep(config).check_value(mars["grid_ML"], date_str),
        param=param,
        specify_domain=False,
        target="ICMSH.Z",
    )
    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "ML",
        "LEVELIST": truth_levelist,
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": str_steps,
        "PARAM": mars["UA"],
        "TYPE": "FC",
        "TARGET": '"ICMUA+[STEP]"',
        "GRID": "AV",
        "PROCESS": "LOCAL",
    }
    param = Marsprep(config).mars["UA"]
    req = Marsprep(config).update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps=str_steps,
        prefetch=True,
        levtype="ML",
        param=param,
        grid=Marsprep(config).check_value(mars["grid_ML"], date_str),
        specify_domain=False,
        target='"ICMUA+[STEP]"',
    )
    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    template = Marsprep(config).template
    str_step = "{}".format(str_steps[0])

    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "SFC",
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": str_step,
        "PARAM": mars["GG"],
        "TYPE": "FC",
        "TARGET": '"{}"'.format(template),
        "GRID": grid_truth,
        "AREA": area_truth,
        "PROCESS": "LOCAL",
    }

    param = Marsprep(config).mars["GG"]
    req = Marsprep(config).update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps=str_step,
        prefetch=True,
        levtype="SFC",
        param=param,
        specify_domain=True,
        target='"{}"'.format(template),
    )
    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    req_truth = {
        "CLASS": truth_class,
        "EXPVER": mars["expver"],
        "LEVTYPE": "ML",
        "STREAM": truth_stream,
        "DATE": date_str,
        "TIME": hour_str,
        "STEP": "00",
        "LEVELIST": 1,
        "PARAM": mars["SHZ"],
        "TYPE": "AN" if mars["expver"] == "0001" else "FC",
        "TARGET": '"{}"'.format(template + ".Z"),
        "GRID": grid_truth,
        "AREA": area_truth,
        "PROCESS": "LOCAL",
    }

    param = Marsprep(config).mars["SHZ"]
    d_type = Marsprep(config).mars["SHZ_type"]
    req = Marsprep(config).update_data_request(
        data_type=d_type,
        date=date_str,
        time=hour_str,
        steps="00",
        prefetch=True,
        levtype="ML",
        param=param,
        specify_domain=True,
        target='"{}"'.format(template + ".Z"),
    )

    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    if __name__ == "__main__":
        pytest.main()
