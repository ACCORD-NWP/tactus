#!/usr/bin/env python3
"""Derive runtime variables."""

import shutil
from math import atan, floor, sin

from .datetime_utils import as_datetime, as_timedelta, oi2dt_list
from .fullpos import Fullpos
from .logs import logger
from .os_utils import Search
from .toolbox import Platform


def set_times(config):
    """Set basetime/validtime if not present.

    Args:
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Returns:
        update (dict): Dict of corrected basetime/validtime
    """
    times = {}
    if "basetime" not in config["general"]["times"]:
        times.update({"basetime": config["general"]["times"]["start"]})
        logger.info("Set basetime to {}", times["basetime"])
    if "validtime" not in config["general"]["times"]:
        times.update({"validtime": config["general"]["times"]["start"]})
        logger.info("Set validtime to {}", times["basetime"])

    update = {"general": {"times": times}}
    return update


def check_fullpos_namelist(config, nlgen):
    """Find existing fullpos select files or generate them.

    Args:
        config (deode.ParsedConfig): Configuration
        nlgen (dict): master forecast namelist

    Returns:
        nlgen (dict) : Possibly updated forecast namelist

    """
    platform = Platform(config)
    accept_static_namelists = config["general.accept_static_namelists"]

    generate_namelist = True
    if accept_static_namelists:
        namelists = platform.get_system_value("namelists")
        fullpos_select_files = Search.find_files(
            namelists, prefix="xxt", recursive=False, fullpath=True
        )
        if len(fullpos_select_files) > 0:
            for filename in fullpos_select_files:
                shutil.copy(filename, ".")
                logger.info("Copy fullpos select file {}", filename)

            generate_namelist = False

    if generate_namelist:
        fpdir = config["fullpos.config_path"]
        fpdir = platform.substitute(fpdir)
        fpfiles = config["fullpos.selection"]
        domain = config["domain.name"]
        fullpos = Fullpos(domain, fpdir=fpdir, fpfiles=fpfiles)
        namfpc, selections = fullpos.construct()
        logger.info("Create fullpos selection for {}", list(selections.keys()))
        for head, body in selections.items():
            nlgen.write_namelist(body, head)

        nlgen.update(namfpc, "fullpos")

    return nlgen


def derived_variables(config, processor_layout=None):
    """Derive some variables required in the namelists.

    Args:
        config (deode.ParsedConfig): Configuration
        processor_layout (ProcessorLayout, optional): Processor layout object

    Returns:
        update (dict) : Derived config update

    """
    # Geometry
    truncation = {"linear": 2, "quadratic": 3, "cubic": 4, "custom": None}
    lspsmoro = {"linear": True, "quadratic": False, "cubic": False, "custom": True}

    ndguxg = int(config["domain.nimax"]) + int(config["domain.ilone"])
    ndglg = int(config["domain.njmax"]) + int(config["domain.ilate"])

    gridtype = config["domain.gridtype"]

    if gridtype == "custom":
        truncation[gridtype] = config["domain.custom_truncation"]

    nsmax = floor((ndglg - 2) / truncation[gridtype])
    nmsmax = floor((ndguxg - 2) / truncation[gridtype])

    pi = 4.0 * atan(1.0)
    xrpk = sin(float(config["domain.xlat0"]) * pi / 180.0)

    # Vertical levels
    nrfp3s = list(range(1, int(config["vertical_levels.nlev"]) + 1))

    # Current time
    basetime = as_datetime(config["general.times.basetime"])
    year = basetime.year
    month = basetime.month
    day = basetime.day
    time = basetime.strftime("%H%M")

    # Time ranges
    tstep = int(config["general.tstep"])
    bdint = as_timedelta(config["boundaries.bdint"])
    forecast_range = as_timedelta(config["general.times.forecast_range"])
    cstop = int((forecast_range.days * 24 * 3600 + forecast_range.seconds) / 60)
    radiation_frequency = as_timedelta(config["general.times.radiation_frequency"])
    nradfr = int(radiation_frequency.seconds / tstep)
    logger.info("nradfr:{}", nradfr)
    if cstop % 60 == 0:
        cstop = int(cstop / 60)
        cstop = f"h{cstop}"
    else:
        cstop = f"m{cstop}"

    # Output settings
    namoutput = {
        "history": [1, -1],
        "fullpos": [1, -1],
        "surfex": [1, -1],
        "nrazts": [1, -1],
    }
    oi = config["general.output_settings"]

    forecast_range_org = config["general.times.forecast_range"]
    for x, y in oi.items():
        dtlist = oi2dt_list(y, forecast_range_org)
        output_timesteps = [
            int((dt.days * 24 * 3600 + dt.seconds) / tstep) for dt in dtlist
        ]

        output_timesteps.insert(0, len(output_timesteps))
        namoutput[x] = output_timesteps

    # Update namelist settings
    update = {
        "domain": {
            "ndguxg": ndguxg,
            "ndglg": ndglg,
            "xrpk": xrpk,
            "xtrunc": truncation[gridtype],
            "nsmax": nsmax,
            "nmsmax": nmsmax,
            "lspsmoro": lspsmoro[gridtype],
        },
        "namelist": {
            "nradfr": nradfr,
            "nrazts": namoutput["nrazts"],
            "nhists": namoutput["history"],
            "nposts": namoutput["fullpos"],
            "nsfxhists": namoutput["surfex"],
            "cstop": cstop,
            "tefrcl": bdint.seconds,
            "nrfp3s": nrfp3s,
            "year": year,
            "month": month,
            "day": day,
            "time": int(time),
        },
        "namelist_update": {"master": {}},
    }
    if processor_layout is not None:
        procs = processor_layout.get_proc_dict()
        # Update namelist dicts
        if procs:
            update["namelist"].update(procs)
        update.update(
            {"submission": {"task": {"wrapper": processor_layout.get_wrapper()}}}
        )

    return update
