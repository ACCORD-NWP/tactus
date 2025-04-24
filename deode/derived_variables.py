#!/usr/bin/env python3
"""Derive runtime variables."""

import shutil
from math import atan, floor, sin

from .datetime_utils import as_datetime, as_timedelta
from .fullpos import Fullpos, flatten_list
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
    times = config["general.times"].dict()
    if "basetime" not in times:
        times.update({"basetime": times["start"]})
        logger.debug("Set basetime to {}", times["basetime"])
    if "validtime" not in times:
        try:
            times.update({"validtime": times["basetime"]})
        except KeyError:
            times.update({"validtime": times["start"]})
        logger.debug("Set validtime to {}", times["validtime"])
    if "start" not in times:
        times.update({"start": times["basetime"]})
        logger.debug("Set start to {}", times["start"])
    if "end" not in times:
        times.update({"end": times["basetime"]})
        logger.debug("Set end to {}", times["end"])

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
        _fpdir = config["fullpos.config_path"]
        fpdir = platform.substitute(_fpdir)
        selection = config.get("fullpos.selection", {})
        fplist = [v if isinstance(v, str) else list(v) for v in selection.values()]
        fplist.append(list(config["fullpos.main"]))
        fpfiles = [platform.substitute(x) for x in flatten_list(fplist)]
        _domain = config["fullpos.domain_name"]
        domain = platform.substitute(_domain)
        nrfp3s = list(range(1, int(config["vertical_levels.nlev"]) + 1))
        rules = {
            "${vertical_levels.nlev}": config["vertical_levels.nlev"],
            "${namelist.nrfp3s}": nrfp3s,
        }
        fullpos = Fullpos(domain, fpdir=fpdir, fpfiles=fpfiles, rules=rules)
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

    Raises:
        NotImplementedError: For configurations checking

    """
    # Geometry
    nbzonl = int(config["domain.nbzonl"])
    if nbzonl == -1:
        xdx = int(config["domain.xdx"])
        nbzonl = next(x for x in range(2000) if x * xdx >= 20000)
        nbzonl = int(nbzonl) if ((int(nbzonl) % 2) == 0) else int(nbzonl) + 1
        if int(config["domain.nimax"]) < 250:
            nbzonl = 8

    nbzong = int(config["domain.nbzong"])
    if nbzong == -1:
        xdy = int(config["domain.xdy"])
        nbzong = next(y for y in range(2000) if y * xdy >= 20000)
        nbzong = int(nbzong) if ((int(nbzong) % 2) == 0) else int(nbzong) + 1
        if int(config["domain.njmax"]) < 250:
            nbzong = 8

    ndguxg = int(config["domain.nimax"]) + int(config["domain.ilone"])
    ndglg = int(config["domain.njmax"]) + int(config["domain.ilate"])

    # Calculate spectral truncation
    truncation_map = {"linear": 2, "quadratic": 3, "cubic": 4, "custom": None}
    gridtype = config["domain.gridtype"]

    if gridtype == "custom":
        truncation_map[gridtype] = config["domain.custom_truncation"]

    truncation = truncation_map[gridtype]
    nsmax = floor((ndglg - 2) / truncation)
    nmsmax = floor((ndguxg - 2) / truncation)

    orographic_smoothing_method = config["domain.orographic_smoothing_method"]

    if orographic_smoothing_method == "spectral":
        gridtype_oro = gridtype
        lspsmoro_map = config["domain.spectral_smoothing_by_gridtype"]
        lspsmoro = lspsmoro_map[gridtype]
        nsmax_oro = nsmax
        nmsmax_oro = nmsmax
    elif orographic_smoothing_method == "truncation":
        lspsmoro = False
        gridtype_oro = config["domain.gridtype_oro"]
        if gridtype_oro == "":
            gridtype_oro_map = config["domain.truncation_by_gridtype"]
            gridtype_oro = gridtype_oro_map[gridtype]
            logger.info("gridtype_oro set to {}", gridtype_oro)

        if gridtype_oro == "custom":
            truncation_map[gridtype] = config["domain.custom_truncation_oro"]

        nsmax_oro = floor((ndglg - 2) / truncation_map[gridtype_oro])
        nmsmax_oro = floor((ndguxg - 2) / truncation_map[gridtype_oro])
    else:
        msg = (
            "Orographic smoothing method: "
            f"{orographic_smoothing_method} is not implemented"
        )
        raise NotImplementedError(msg)

    xlat0 = config.get("domain.xlat0", "")
    xlon0 = config.get("domain.xlon0", "")
    if xlat0 == "":
        xlat0 = config.get("domain.xlatcen")
    if xlon0 == "":
        xlon0 = config.get("domain.xloncen")

    pi = 4.0 * atan(1.0)
    xrpk = sin(float(xlat0) * pi / 180.0)

    # Vertical levels
    nrfp3s = list(range(1, int(config["vertical_levels.nlev"]) + 1))

    # Current time
    basetime = as_datetime(config["general.times.basetime"])
    year = basetime.year
    month = basetime.month
    day = basetime.day
    time = basetime.strftime("%H%M")

    # Time ranges
    bdint = as_timedelta(config["boundaries.bdint"])
    forecast_range = as_timedelta(config["general.times.forecast_range"])
    cstop = int((forecast_range.days * 24 * 3600 + forecast_range.seconds) / 60)
    if cstop % 60 == 0:
        cstop = int(cstop / 60)
        cstop = f"h{cstop}"
    else:
        cstop = f"m{cstop}"

    # Wind farm parameterization
    if config["general.windfarm"] and config.get("general.fullpos_windpower", True):
        selection = list(config["fullpos.selection"])
        selection.append("windfarm")

    # Turn boolean to strings and macros
    default_macros = config.get(
        "macros.select.default",
        {"gen_macros": [], "group_macros": [], "os_macros": []},
    ).dict()
    gen_macros = list(default_macros["gen_macros"])

    decades = "one_decade" if config["pgd.one_decade"] else "all_decade"
    gen_macros.append("namelist.decades")

    sg_input = "osm" if config["pgd.use_osm"] else ""
    gen_macros.append("namelist.sg_input")
    default_macros["gen_macros"] = gen_macros

    # Update config and namelist settings
    update = {
        "domain": {
            "gridtype_oro": gridtype_oro,
            "nbzong": nbzong,
            "nbzonl": nbzonl,
            "ndguxg": ndguxg,
            "ndglg": ndglg,
            "xrpk": xrpk,
            "xlat0": xlat0,
            "xlon0": xlon0,
            "xtrunc": truncation,
            "nsmax": nsmax,
            "nmsmax": nmsmax,
            "nsmax_oro": nsmax_oro,
            "nmsmax_oro": nmsmax_oro,
            "lspsmoro": lspsmoro,
        },
        "macros": {"select": {"default": default_macros}},
        "namelist": {
            "cstop": cstop,
            "tefrcl": bdint.seconds,
            "nrfp3s": nrfp3s,
            "year": year,
            "month": month,
            "day": day,
            "time": int(time),
            "sg_input": sg_input,
            "decades": decades,
        },
    }

    # Wind farm parameterization
    if config["general.windfarm"] and config.get("general.fullpos_windpower", True):
        update["fullpos"] = {"selection": {"windfarm": ["windfarm"]}}

    if processor_layout is not None:
        procs = processor_layout.get_proc_dict()
        # Update namelist dicts
        if procs:
            update["namelist"].update(procs)
        update.update(
            {"submission": {"task": {"wrapper": processor_layout.get_wrapper()}}}
        )

    return update
