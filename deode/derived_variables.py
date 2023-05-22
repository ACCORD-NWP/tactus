#!/usr/bin/env python3
"""Derive runtime variables."""

from math import atan, floor, sin

from .datetime_utils import as_datetime, as_timedelta, oi2dt_list


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

    ndguxg = int(config.get_value("domain.nimax")) + int(config.get_value("domain.ilone"))
    ndglg = int(config.get_value("domain.njmax")) + int(config.get_value("domain.ilate"))

    gridtype = config.get_value("domain.gridtype")

    if gridtype == "custom":
        truncation[gridtype] = config.get_value("domain.custom_truncation")

    nsmax = floor((ndglg - 2) / truncation[gridtype])
    nmsmax = floor((ndguxg - 2) / truncation[gridtype])

    pi = 4.0 * atan(1.0)
    xrpk = sin(float(config.get_value("domain.xlat0")) * pi / 180.0)

    # Vertical levels
    nrfp3s = list(range(1, int(config.get_value("vertical_levels.nlev")) + 1))

    # Current time
    basetime = as_datetime(config.get_value("general.times.basetime"))
    year = basetime.year
    month = basetime.month
    day = basetime.day
    time = basetime.strftime("%H%M")

    # Time ranges
    tstep = int(config.get_value("general.tstep"))
    bdint = as_timedelta(config.get_value("general.bdint"))
    forecast_range = as_timedelta(config.get_value("general.forecast_range"))
    cstop = int((forecast_range.days * 24 * 3600 + forecast_range.seconds) / 60)
    if cstop % 60 == 0:
        cstop = int(cstop / 60)
        cstop = f"h{cstop}"
    else:
        cstop = f"m{cstop}"

    # Output settings
    namoutput = {"history": 0, "fullpos": 0, "surfex": 0}
    oi = config.get_value("general.output_settings")

    forecast_range_org = config.get_value("general.forecast_range")
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
        },
        "namelist": {
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
        update.update({"task": {"wrapper": processor_layout.get_wrapper()}})

    return update
