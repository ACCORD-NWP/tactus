#!/usr/bin/env python3
"""Unit tests for the marsprep."""

import pytest
import tomlkit

from deode.config_parser import default_config
from deode.derived_variables import derived_variables, set_times
from deode.geo_utils import Projection, Projstring
from deode.tasks.marsprep import Marsprep


@pytest.fixture(scope="module")
def base_parsed_config():
    """Return a parsed config common to all tasks."""
    config = default_config()
    config = config.copy(update=set_times(config))
    config = config.copy(update=derived_variables(config))

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


@pytest.fixture(params=["HRES", "ATOS_DT"], scope="module")
def parsed_config(request, base_parsed_config, tmp_path_factory):
    """Return a parsed config common to tasks."""
    config_patch = tomlkit.parse(
        f"""
        [boundaries]
            ifs.selection = "{request.param}"
        [system]
            wrk = "{tmp_path_factory.getbasetemp().as_posix()}"

        """
    )

    config = base_parsed_config.copy(update=config_patch)
    return config


def test_update_data_request(parsed_config):
    """Test update data request."""
    config = parsed_config
    marsprep = Marsprep(config)
    mars = marsprep.mars

    dateframe = marsprep.split_date(
        marsprep.basetime,
        int(marsprep.forecast_range.total_seconds() // 3600),
        marsprep.bdint,
    )
    date_str = dateframe.iloc[0].strftime("%Y%m%d")
    hour_str = dateframe.iloc[0].strftime("%H")

    step = int(marsprep.bdint.total_seconds() // 3600)
    str_steps = [
        "{0:02d}".format(
            dateframe.index.tolist()[0]
            + (i * step)
            + marsprep.basetime.hour % marsprep.int_bdcycle
        )
        for i in range(len(dateframe.index.tolist()))
    ]
    truth_levelist = marsprep.check_value(mars["levelist"], date_str)
    area_truth = get_domain_data(config)
    grid_truth = marsprep.check_value(mars["grid"], date_str)
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
    param = marsprep.mars["GG"]
    req = marsprep.update_data_request(
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

    param = marsprep.mars["GG_sea"]
    req = marsprep.update_data_request(
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

        param1 = marsprep.mars["GG_soil"]
        param2 = marsprep.mars["GG1"]

        req = marsprep.update_data_request(
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

        req = marsprep.update_data_request(
            data_type="analysis",
            date=date_str,
            time=hour_str,
            steps="00",
            prefetch=True,
            levtype="SFC",
            param=param2,
            grid=marsprep.check_value(mars["grid_GG1"], date_str),
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

    param = marsprep.mars["SH"]
    req = marsprep.update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps=str_steps,
        prefetch=True,
        levtype="ML",
        param=param,
        grid=marsprep.check_value(mars["grid_ML"], date_str),
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
    param = marsprep.mars["SHZ"]
    d_type = marsprep.mars["SHZ_type"]
    req = marsprep.update_data_request(
        data_type=d_type,
        date=date_str,
        time=hour_str,
        steps="00",
        prefetch=True,
        levtype="ML",
        grid=marsprep.check_value(mars["grid_ML"], date_str),
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
    param = marsprep.mars["UA"]
    req = marsprep.update_data_request(
        data_type="forecast",
        date=date_str,
        time=hour_str,
        steps=str_steps,
        prefetch=True,
        levtype="ML",
        param=param,
        grid=marsprep.check_value(mars["grid_ML"], date_str),
        specify_domain=False,
        target='"ICMUA+[STEP]"',
    )
    req_dic = {}
    for col in req.columns:
        for _index, row in req.iterrows():
            req_dic[str(col)] = row[col]
    assert req_dic == req_truth

    template = marsprep.template
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

    param = marsprep.mars["GG"]
    req = marsprep.update_data_request(
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

    param = marsprep.mars["SHZ"]
    d_type = marsprep.mars["SHZ_type"]
    req = marsprep.update_data_request(
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
