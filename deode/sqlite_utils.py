"""ExtractSQLite."""

import os

from .logs import logger

# For now (on ATOS), only tasks with prgenv/gnu can import eccodes in python
try:
    import eccodes
except (ImportError, OSError):
    logger.warning("eccodes python API could not be imported. Usually OK.")

import datetime
import sqlite3
from contextlib import closing
from copy import deepcopy

import numpy as np
import pyproj


def get_date_info(gid):
    """Forecast time, lead time, accumulation time etc. from GRIB record.

    Args:
        gid: grib handle

    Returns:
        Forecast date/time and lead time as read from the grib record
    """
    keys = [
        "productDefinitionTemplateNumber",
        "dataDate",
        "dataTime",
        "indicatorOfUnitOfTimeRange",
        "forecastTime",
    ]
    info = get_keylist(gid, keys, "long")
    if info["productDefinitionTemplateNumber"] == 8:
        keys2 = ["indicatorOfUnitForTimeRange", "lengthOfTimeRange"]
        info2 = get_keylist(gid, keys2, "long")
        info = info | info2
    return date_from_gribinfo(info)


def date_from_gribinfo(info):
    """Interprete list of date information as given by eccodes.

    Args:
        info: a dict with all necessary grib key values

    Returns:
        forecast date (datetime object) and lead time (in seconds)

    """
    fcdate = datetime.datetime.strptime(
        "{:8}T{:06}".format(info["dataDate"], info["dataTime"]), "%Y%m%dT%H%M%S"
    )
    if info["indicatorOfUnitOfTimeRange"] == 1:
        leadtime = info["forecastTime"] * 3600.0
    elif info["indicatorOfUnitOfTimeRange"] == 2:
        leadtime = info["forecastTime"] * 60.0
    elif info["indicatorOfUnitOfTimeRange"] == 13:
        leadtime = info["forecastTime"]
    else:
        return None

    # At this point, leadtime may be just the START of accumulation (or min/max/mean) time
    if info["productDefinitionTemplateNumber"] == 8:
        if info["indicatorOfUnitForTimeRange"] == 1:
            lt2 = info["lengthOfTimeRange"] * 3600.0
        elif info["indicatorOfUnitForTimeRange"] == 2:
            lt2 = info["lengthOfTimeRange"] * 60.0
        elif info["indicatorOfUnitForTimeRange"] == 13:
            lt2 = info["lengthOfTimeRange"]
        else:
            return None
        leadtime = leadtime + lt2

    return (fcdate, leadtime)


def get_proj4(gid):
    """Read all projection details and return proj4 string.

    Args:
        gid: GRIB handle

    Returns:
        projection function corresponding to proj4 string
    """
    # NOTE: for now, we assume standard ACCORD Earth shape etc.
    proj4 = {"R": 6371229}
    gridtype = get_keylist(gid, ["gridType"], "string")["gridType"]
    if gridtype in ["lambert", "lambert_lam"]:
        pkeys = ["Latin1InDegrees", "Latin2InDegrees", "LoVInDegrees"]
        p4 = get_keylist(gid, pkeys, "double")

        proj4["proj"] = "lcc"
        proj4["lon_0"] = p4["LoVInDegrees"]
        proj4["lat_1"] = p4["Latin1InDegrees"]
        proj4["lat_2"] = p4["Latin2InDegrees"]

    elif gridtype == "polar_steoreographic":
        pkeys = ["LoVInDegrees"]
        p4 = get_keylist(gid, pkeys, "double")
        proj4["proj"] = "stere"
        proj4["lon_0"] = p4["LoVInDegrees"]
        proj4["lat_0"] = 90.0  # NOTE: assuming Northern hemisphere!
    elif gridtype == "regular_ll":
        proj4["proj"] = "latlong"  # FIXME
    #  [rotated] mercator?
    else:
        return None
    # FIXME: calling this seems to triger a deprecation warning in Numpy!
    return proj4


def get_gridinfo(gid):
    """Read all grid details and return all necessary data.

    Args:
        gid: GRIB handle

    Returns:
        a dictionary with the main grid characteristics
    """
    # NOTE: is it OK to assume SW is the first point?
    gridtype = get_keylist(gid, ["gridType"], "string")["gridType"]
    if gridtype in ["lambert", "lambert_lam"]:
        gkeys = [
            "Nx",
            "Ny",
            "DxInMetres",
            "DyInMetres",
            "iScansPositively",
            "jScansPositively",
            "latitudeOfFirstGridPointInDegrees",
            "longitudeOfFirstGridPointInDegrees",
            "latitudeOfLastGridPointInDegrees",
            "longitudeOfLastGridPointInDegrees",
        ]
    else:
        return None
    gg = get_keylist(gid, gkeys, "double")

    return {
        "nlon": int(gg["Nx"]),
        "nlat": int(gg["Ny"]),
        "dx": gg["DxInMetres"],
        "dy": gg["DyInMetres"],
        "lon0": gg["longitudeOfFirstGridPointInDegrees"],
        "lat0": gg["latitudeOfFirstGridPointInDegrees"],
    }


def get_grid_limits(gid):
    """Find bounding box (lat/lon) of a grid."""
    lons, lats = get_gridpoints(gid)

    minlat = np.floor(np.min(lats)) - 1
    minlon = np.floor(np.min(lons)) - 1
    maxlat = np.ceil(np.max(lats)) + 1
    maxlon = np.ceil(np.max(lons)) + 1

    minlat = np.max([minlat, -90])
    minlon = np.max([minlon, -180])
    maxlat = np.min([maxlat, 90])
    maxlon = np.min([maxlon, 180])

    return minlon, maxlon, minlat, maxlat


def proj4_to_string(proj4):
    """Transfrom a proj4 from dictionary to string.

    Args:
        proj4: a dictionary

    Returns:
        a single string
    """
    result = " ".join([f"+{p}={proj4[p]}" for p in proj4])
    return result


def get_keylist(gid, keylist, ktype="string"):
    """Get list of grib keys. Return "" or None for keys that are not found.

    Args:
        gid: GRIB handle
        keylist: list of key names
        ktype: required output type ("string", "double" or "long")

    Returns:
        A dictionary with all key values. Keys that are not present in the grib record
        return None.
    """
    if ktype == "string":
        func = eccodes.codes_get_string
        miss = ""
    elif ktype == "long":
        func = eccodes.codes_get_long
        miss = None
    elif ktype == "double":
        func = eccodes.codes_get_double
        miss = None

    ginfo = {}
    for kk in keylist:
        try:
            val = func(gid, kk)
        except eccodes.KeyValueNotFoundError:
            val = miss
        ginfo[kk] = val
    return ginfo


def param_match(gid, parameter_list):
    """Check whether a grib record is in the list of required parameters.

    TODO: can we re-organise the code to avoid getting the same key multiple times?
      But I suspect the impact is minimal

    Args:
        gid: grib handle
        parameter_list: list of parameter descriptors

    Returns:
        Parameter descriptor (from the list) that matches the current grib handle,
        or None.
    """
    for param in parameter_list:
        # param['grib_id'] is a dictionary of keys|values that describe the parameter
        # so you need to check all of these to know if the grib record matches
        plist = param["grib_id"]
        if isinstance(plist, dict):
            plist = [plist]

        for par in plist:
            keylist = list(par.keys())
            ginfo = get_keylist(gid, keylist, "string")
            # we make a deepcopy, because we may have to modify some key values...
            if match_keys(par, ginfo):
                pmatch = deepcopy(param)
                # make sure we have a single grib_id in the pmatch!
                pmatch["grib_id"] = deepcopy(par)
                pmatch["units"] = get_keylist(gid, ["parameterUnits"], "string")[
                    "parameterUnits"
                ]
                # make sure level information is explicitly passed!
                # e.g. for "2t" it may not be in the grib_id list.
                level_type = get_keylist(gid, ["typeOfLevel"], "string")["typeOfLevel"]

                pmatch["typeOfLevel"] = level_type
                pmatch["level"] = int(get_keylist(gid, ["level"], "long")["level"])
                if level_type == "isobaricInhPa":
                    pmatch["level_name"] = "p"
                elif level_type == "heightAboveGround":
                    pmatch["level_name"] = "z"
                elif level_type == "hybridLevel":
                    pmatch["level_name"] = "ml"
                elif level_type == "surface":
                    pmatch["level_name"] = None  #  'level' #  "sfc"
                else:
                    pmatch["level_name"] = None  # "level" #  "xx"

                return pmatch

    # no match was found
    return None


def sqlite_name(param, fcdate, sqlite_template):
    """Create the full name of the SQLite file from template and date.

    Args:
        param: parameter descriptor
        fcdate: forecast date
        sqlite_template: template for SQLite file name

    Returns:
        full name of SQLite file
    """
    result = sqlite_template
    result = result.replace("@PP@", param["harp_param"])
    result = result.replace("@YYYY@", fcdate.strftime("%Y"))
    result = result.replace("@MM@", fcdate.strftime("%m"))
    result = result.replace("@DD@", fcdate.strftime("%d"))
    result = result.replace("@HH@", fcdate.strftime("%H"))
    return result


def points_restrict(gid, plist):
    """Restrict the station list to points inside the current domain.

    NOTE: * eccodes returns distances in kilometer
          * store the list for use in next run? run this function seperately?

    Args:
        gid: grib handle
        plist: list of stations

    Returns:
        reduced station list that contains only stations inside the domain
    """
    # 1. Get the bounding box lat/lon values
    #    that is a fast way to eliminate most outside points

    minlon, maxlon, minlat, maxlat = get_grid_limits(gid)

    # reduce the table to the bounding box
    # Make a copy! The original retains old row numbers and becomes hard to manage.
    p1 = plist[
        (plist["lat"] >= minlat)
        & (plist["lat"] <= maxlat)
        & (plist["lon"] >= minlon)
        & (plist["lon"] <= maxlon)
    ].copy()

    # 2. Now use grid index (requires projection)
    #    to decide which stations are inside the grid
    #    NOTE: we could skip step 1 and project all stations...
    gridinfo = get_gridinfo(gid)
    nlon = gridinfo["nlon"]
    nlat = gridinfo["nlat"]

    lon = p1["lon"].tolist()
    lat = p1["lat"].tolist()

    i, j = get_gridindex(lon, lat, gid)
    p2 = p1[(i > 0) & (i < nlon - 1) & (j > 0) & (j < nlat - 1)].copy()
    return p2


def get_gridpoints(gid):
    """Get all lat/lon co-ordinates of the grid points.

    Args:
        gid: GRIB handle
    Returns:
        Numpy arrays with all lat/lon values.
    """
    gridinfo = get_gridinfo(gid)
    p4 = get_proj4(gid)
    proj = pyproj.Proj(p4)
    nlon = int(gridinfo["nlon"])
    nlat = int(gridinfo["nlat"])
    dx = gridinfo["dx"]
    dy = gridinfo["dy"]

    x0, y0 = proj(gridinfo["lon0"], gridinfo["lat0"])

    xxx = np.empty(nlon)
    yyy = np.empty(nlat)
    for i in range(nlon):
        xxx[i] = x0 + (float(i) * dx)
    for j in range(nlat):
        yyy[j] = y0 + (float(j) * dy)

    x_v, y_v = np.meshgrid(xxx, yyy)
    lons, lats = proj(x_v, y_v, inverse=True)
    return lons, lats


def get_gridindex(lon, lat, gid):
    """Convert lat/lon values to (zero-offset) grid indices.

    The SW corner has index (0,0). Points are projected on the grid
    and a (non-integer) index is calculated.

    Args:
        lon: single value or list of longitude values
        lat: single value of list of latitude values
        gid: grib handle

    Returns:
        two vectors with (non-integer) index values.
    """
    gridinfo = get_gridinfo(gid)
    p4 = get_proj4(gid)
    proj = pyproj.Proj(p4)
    x0, y0 = proj(gridinfo["lon0"], gridinfo["lat0"])
    dx = gridinfo["dx"]
    dy = gridinfo["dy"]

    x, y = proj(np.array(lon), np.array(lat))
    i = (x - x0) / dx
    j = (y - y0) / dy
    return i, j


def train_weights(station_list, gid, lsm=False):
    """Train weights for bilinear and nearest neighbour interpolation.

    We train two kinds of interpolation at once.
    NOTE: Land/Sea Mask is not yet implemented.

    Args:
        station_list: pandas table
        gid: grib handle
        lsm: use Land/Sea mask (ignored for now)

    Returns:
        interpolation weights (nearest neighbour & bilinear)
    """
    # TODO: land/sea mask for T2m...
    if lsm:
        logger.warning("SQLITE: ignoring land/sea mask!")

    lat = np.array(station_list["lat"].tolist())
    lon = np.array(station_list["lon"].tolist())
    nstations = len(lon)
    i, j = get_gridindex(lon, lat, gid)

    # eccodes python interface only gives us distances
    # so we need to do some math for bilinear weights
    nearestweights = []
    bilinweights = []
    # assuming the 4 closest points are exactly what we need (OK if dx=dy)
    # NOTE: this assumes dx == dy !!!
    # otherwise, you have to check whether 2nd point is along X or Y axis from first
    # probably easy, just look at the index
    ic = np.round(i).astype("i4")
    jc = np.round(j).astype("i4")
    i0 = np.floor(i).astype("i4")
    i1 = i0 + 1  # np.ceil(i)
    j0 = np.floor(j).astype("i4")
    j1 = j0 + 1  # np.ceil(j)
    di = i - i0
    dj = j - j0
    w1 = (1 - di) * (1 - dj)
    w2 = (1 - di) * dj
    w3 = di * (1 - dj)
    w4 = di * dj
    for pp in range(nstations):
        nearestweights.append([[[ic[pp], jc[pp]], 1.0]])
        bilinweights.append(
            [
                [[i0[pp], j0[pp]], w1[pp]],
                [[i0[pp], j1[pp]], w2[pp]],
                [[i1[pp], j0[pp]], w3[pp]],
                [[i1[pp], j1[pp]], w4[pp]],
            ]
        )

    weights = {"bilin": bilinweights, "nearest": nearestweights}
    return weights


def interp_from_weights(gid, weights, method):
    """Interpolate a GRIB field to points using the given weights.

    Args:
        gid: grib handle
        weights: interpolation weights
        method: interpolation method (bilin, nearest)

    Returns:
        interpolated values
    """
    # NOTE: this assumes all records in a file use the same grid representation

    data = get_grid_values(gid)
    interp = [sum([data[x[0][0], x[0][1]] * x[1] for x in w]) for w in weights[method]]
    return interp


def get_grid_values(gid):
    """Decode a GRIB2 field.

    Args:
      gid: A grib handle

    Returns:
      Numpy array of decoded field.
    """
    gkeys = [
        "Nx",
        "Ny",
        "iScansNegatively",
        "jScansPositively",
        "jPointsAreConsecutive",
        "alternativeRowScanning",
    ]
    ginfo = get_keylist(gid, gkeys, "long")
    nx = ginfo["Nx"]
    ny = ginfo["Ny"]

    # data given by column (C) or by row (Fortran)?
    order = "C" if ginfo["jPointsAreConsecutive"] else "F"
    data = eccodes.codes_get_values(gid).reshape(nx, ny, order=order)
    if ginfo["iScansNegatively"]:
        data[range(nx), :] = data[range(nx)[::-1], :]
    if not ginfo["jScansPositively"]:
        data[:, range(ny)] = data[:, range(ny)[::-1]]

    return data


def combine_fields(param):
    """Combine multiple decoded fields into one parameter.

    Args:
        param: full parameter descriptor

    Returns:
        a new data matrix that combines the input fields according to parameter descriptor
    """
    nfields = len(param["data"])
    logger.debug("COMBINE {} fields", nfields)
    if any(dd is None for dd in param["data"]):
        # there are missing fields
        logger.warning(
            "COMBINE : there are missing data fields for parameter {}...",
            param["harp_param"],
        )
        return None

    npoints = len(param["data"][0])
    logger.debug("COMBINE {} points", npoints)
    result = np.zeros(npoints)

    if param["function"] == "vector_norm":
        logger.debug("WIND SPEED")
        for ff in range(nfields):
            result += param["data"][ff] * param["data"][ff]
        return np.sqrt(result)

    if param["function"] == "sum":
        logger.debug("SUM")
        for ff in range(nfields):
            result += param["data"][ff]
        return result

    if param["function"] == "vector_angle":
        # FIXME
        param["units"] = "deg"
        return None

    return None


def parse_parameter_list(param_list):
    """Parse a (json) structure of required parameters.

    Handle single vs combined fields, model levels etc.

    Args:
        param_list: the list pf paramers (from json file)

    Returns:
        param_id_list: the complete list of fields that should be decoded
        param_combine: details for combined fields
    """
    param_sgl_list = []
    param_cmb_list = []

    for param in param_list:
        if isinstance(param["grib_id"], dict):
            # a single field is decoded for this parameter
            if "level" in param["grib_id"] and isinstance(
                param["grib_id"]["level"], list
            ):
                # expand to multiple fields
                for lev in param["grib_id"]["level"]:
                    pid = deepcopy(param)
                    pid["grib_id"]["level"] = str(lev)
                    param_sgl_list.append(pid)
            else:
                pid = deepcopy(param)
                # single field parameters are not cached for now
                # so "data" entry is not needed
                param_sgl_list.append(pid)

        else:
            nfields = len(param["grib_id"])
            if "common" not in param:
                # the most simple combine case: no common keys
                pid = deepcopy(param)
                pid["data"] = [None] * nfields
                param_cmb_list.append(pid)

            elif "level" in param["common"] and isinstance(
                param["common"]["level"], list
            ):
                # there is a common level key, so multiple entries
                for lev in param["common"]["level"]:
                    pid = deepcopy(param)
                    pid["common"]["level"] = str(lev)
                    for f in range(len(pid["grib_id"])):
                        pid["grib_id"][f].update(pid["common"])
                    pid.pop("common")
                    pid["data"] = [None] * nfields
                    param_cmb_list.append(pid)

            else:
                # there are common keys, but no level expansion
                pid = deepcopy(param)
                for f in range(len(pid["grib_id"])):
                    pid["grib_id"][f].update(pid["common"])
                pid.pop("common")
                pid["data"] = [None] * nfields
                param_cmb_list.append(pid)
    return param_sgl_list, param_cmb_list


def match_keys(p1, p2):
    """Match two sets of grib key values.

    Note that a key missing in p1 may be None in p2.

    Args:
      p1: a dictionary with GRIB2 key values
      p2: a dictionary with GRIB2 key values

    Returns:
      True if all (not None) values are equal. False otherwise.
    """
    for k in list(set(list(p1.keys()) + list(p2.keys()))):
        if k not in p1:
            if p2[k] is not None:
                return False
        elif k not in p2:
            if p1[k] is not None:
                return False
        elif p1[k] != p2[k]:
            return False
    return True


def cache_field(param, data, param_cmb_list):
    """Check if a decoded field needs to be cached for later parameters.

    Args:
      param: parameter description of the current data
      data: decode (and interpolated) data
      param_cmb_list: a list (may be MODIFIED!)

    Returns:
      a count of the number of matching parameters (usually 0 or 1)
    """
    count = 0
    for cmb in param_cmb_list:
        nfields = len(cmb["grib_id"])
        for ff in range(nfields):
            if match_keys(cmb["grib_id"][ff], param["grib_id"]):
                logger.debug("Caching output for {}", param["harp_param"])
                cmb["data"][ff] = np.array(data)
                # FIXME: we assume the unit is the same for all constituents and result
                #        this is NOT the case for e.g. wind direction
                #        probably should be fixed in the final combination function
                cmb["units"] = param["units"]
                cmb["level"] = param["level"]
                cmb["level_name"] = param["level_name"]
                count += 1
                continue
    return count


def parse_grib_file(
    infile,
    param_list,
    station_list,
    sqlite_template,
    weights=None,
    model_name="TEST",
):
    """Read a GRIB2 file and extract all required data points to SQLite.

    Args:
      infile: input grib2 file
      param_list: list of parameters
      station_list: pandas table with all stations
      sqlite_template: template for sqlite output files
      weights: interpolation weights (if None, they are calculated)
      model_name: model name (string) used in the SQLite file name and data columns

    Returns:
        Total number of GRIB records and number of matching parameters found.
    """
    # split into "combined" and "direct" parameters
    param_sgl_list, param_cmb_list = parse_parameter_list(param_list)
    param_cmb_cache = [None] * len(param_cmb_list)
    if len(param_cmb_list) > 0:
        for pp in range(len(param_cmb_cache)):
            param_cmb_cache[pp] = {}

    logger.info(
        "SQLITE: {} single and {} combined parameters.",
        len(param_sgl_list),
        len(param_cmb_list),
    )

    fcdate = None
    leadtime = None
    gi = 0
    gt = 0
    with open(infile, "rb") as gfile:
        while True:
            # loop over all grib records in the file
            gid = eccodes.codes_grib_new_from_file(gfile)
            if gid is None:
                # We've reached the last grib record in the file
                break
            gt += 1
            param = param_match(gid, param_sgl_list)
            if param is not None:
                direct = True
            else:
                # the record is not in our "direct" parameter list
                direct = False
                param = param_match(gid, param_cmb_list)
                if param is None:
                    continue
                logger.debug("SQLITE: COMBI FOUND", param["harp_param"])

            # we have a matching parameter
            gi += 1
            logger.info("SQLITE: found parameter {}", param["harp_param"])
            # NOTE: actually, we only need to get fcdate & lead time once
            #       and we could even consider those to be known
            fcdate, leadtime = get_date_info(gid)
            logger.debug(
                "SQLITE: gt={} gi={} fcdate={}, leadtime={}, direct={}",
                gt,
                gi,
                fcdate,
                leadtime,
                direct,
            )

            if weights is None:
                # We assume that station list and weights are the same for all files
                # so we only "train" once
                # First reduce the station list to points inside the domain
                station_list = points_restrict(gid, station_list)
                logger.info(
                    "SQLITE: selected {} stations inside domain.", station_list.shape[0]
                )
                # create a list of interpolation weights
                logger.info("SQLITE: training interpolation weights.")
                weights = train_weights(station_list, gid, lsm=False)

            # by default, we do bilinear interpolation
            method = param["method"] if "method" in param else "bilin"
            # add columns to data table
            data_vector = interp_from_weights(gid, weights, method)

            # cache this data vector if necessary
            cache_field(param, data_vector, param_cmb_list)

            # if this is a "direct" field, create a table and write to SQLite
            if direct:
                data = create_table(
                    data_vector, station_list, param, fcdate, leadtime, model_name
                )
                sqlite_file = sqlite_name(param, fcdate, sqlite_template)
                write_to_sqlite(data, sqlite_file, param, model_name)

        # at this point, all grib records have been parsed
        # make sure to release the grib handle
        if gid is not None:
            gid.release()

    # OK, we have parsed the whole file and written all "direct" parameters
    # So now we still need to check all the "combined" ones
    # NOTE: this should only be run if we found some parameters in the first loop
    #       but checking whether the combined field is None is enough
    logger.debug("SQLITE: checking cached combined fields")
    for param in param_cmb_list:
        data_vector = combine_fields(param)
        # only write to SQLite if ALL components were found!
        if data_vector is not None:
            logger.debug("SQLITE: writing combined field")
            data = create_table(
                data_vector, station_list, param, fcdate, leadtime, model_name
            )
            sqlite_file = sqlite_name(param, fcdate, sqlite_template)
            write_to_sqlite(data, sqlite_file, param, model_name)
    # Return: total count and # of matchign param
    return gt, gi


def create_table(data_vector, station_list, param, fcdate, leadtime, model_name):
    """Put a data vector (interpolated field) to pandas table.

    Args:
      data_vector: numpy vector with interpolated values
      station_list: pandas table with station list
      param: parameter desctiptor
      fcdate: forecast date (datetime object)
      leadtime: lead time (int)
      model_name: model name (string)

    Returns:
      pandas table
    """
    prim_keys = ["SID", "lon", "lat"]

    # For T2m (and MAXT2m etc.) we want station elevation for height correction
    if (
        "T2m" in param["harp_param"]
        and "elev" in station_list.columns.to_numpy().tolist()
    ):
        prim_keys.append("elev")
    data = station_list[prim_keys].copy()
    data[model_name] = data_vector

    # prepare SQLITE output
    fcd = int(fcdate.timestamp())
    vad = int(fcdate.timestamp() + leadtime)

    data["fcst_dttm"] = fcd
    # NOTE: currently FCTABLE still expects leadtime in hours!
    data["lead_time"] = leadtime / 3600.0
    data["valid_dttm"] = vad
    data["parameter"] = param["harp_param"]
    data["units"] = param["units"]
    logger.debug("SQLITE: writing parameter ", param["harp_param"])
    # model_elevation only relevant for T2m
    # NOTE: we can not yet read model elevation from clim file!
    # otherwise remove column
    if "T2m" in param["harp_param"]:
        if "model_elevation" not in data.columns.to_numpy().tolist():
            data["model_elevation"] = 0
        if "elev" not in data.columns.to_numpy().tolist():
            data["elev"] = 0
    else:
        if "model_elevation" in data.columns.to_numpy().tolist():
            data = data.drop("model_elevation", axis=1)
        if "elev" in data.columns.to_numpy().tolist():
            data = data.drop("elev", axis=1)

    if param["level"] is not None and param["level_name"] is not None:  #  "level"]):
        data[param["level_name"]] = int(param["level"])
    return data


def write_to_sqlite(data, sqlite_file, param, model_name):
    """Write a data table to SQLite.

    Args:
        data: a data table
        sqlite_file: file name
        param: parameter descriptor
        model_name: model name used in

    """
    logger.info("Writing to {}", sqlite_file)
    if os.path.isfile(sqlite_file):
        con = sqlite3.connect(sqlite_file)
    else:
        sqlite_path = os.path.dirname(sqlite_file)
        if not os.path.isdir(sqlite_path):
            logger.info("SQLITE: Creating directory {}.", sqlite_path)
            os.makedirs(sqlite_path)
        # if the SQLite file doesn't exist yet: create the SQLite table
        logger.info("SQLITE: Creating sqlite file {}.", sqlite_file)
        con = db_create(sqlite_file, param, model_name)
    # NOTE: we need to cast to int (or float), because "numpy.int64" will not work in SQL
    fcd = int(data.iloc[0]["fcst_dttm"])
    # NOTE: we now take leadtime directly from the data table
    #       this should keep it OK if we switch to sub-hourly.
    leadtime = int(data.iloc[0]["lead_time"])
    logger.debug("leadtime: {}", leadtime)
    db_cleanup(param, fcd, leadtime, con)

    # now write to SQLite
    data.to_sql("FC", con, if_exists="append", index=False)
    con.commit()
    con.close()


def db_cleanup(param, fcd, leadtime, con):
    """Delete any prior version of the date (it's a primary key).

    Args:
        param: parameter description
        fcd: forecast date (string or int)
        leadtime: lead time (int)
        con: database connection
    """
    logger.debug("Cleanup: ldt={}", leadtime)
    cleanup = "DELETE from FC WHERE fcst_dttm=? AND lead_time=?"
    cur = con.cursor()
    # NOTE: variables must be cast to int (or float),
    # because "numpy.int64" will not work in SQL
    # FIXME: better look whether the table has a "level" column!

    cn1 = cur.execute("select name from PRAGMA_TABLE_INFO('FC')").fetchall()
    colnames = [x[0] for x in cn1]

    if param["level_name"] in colnames:
        lname = param["level_name"]
        parlev = int(param["level"])
        cleanup = cleanup + f" AND {lname}=?"
        cur.execute(
            cleanup,
            (
                float(fcd),
                float(leadtime),
                int(parlev),
            ),
        )
    else:
        cur.execute(
            cleanup,
            (
                float(fcd),
                float(leadtime),
            ),
        )
    con.commit()


def db_create(sqlite_file, param, model_name):
    """Create new SQLite file with FC table.

    The table is designed to be compatible with harp, so uses the same
    "unique index" approach.

    Args:
      sqlite_file: SQLite file name
      param: parameter descriptor
      model_name: model name used in SQLite

    Returns:
      data base connection
    """
    primary_keys, all_keys = fctable_definition(param, model_name)
    fc_def = (
        "CREATE table if not exists FC ("
        + ",".join(f"{p[0]} {p[1]} " for p in all_keys.items())
        + ")"
    )
    pk_def = (
        "CREATE unique INDEX IF NOT EXISTS "
        + "index_"
        + "_".join(primary_keys.keys())
        + " ON FC("
        + ",".join(primary_keys.keys())
        + ")"
    )

    con = sqlite3.connect(sqlite_file)
    with con, closing(con.cursor()) as cur:
        cur.execute(fc_def)
        cur.execute(pk_def)
    return con


def fctable_definition(param, model):
    """Create the SQL command for FCtable definition.

    Args:
        param: the parameter descriptor
        model: model name to be used

    Returns:
        SQL commands for creating the FCTABLE table
    """
    primary_keys = {"fcst_dttm": "DOUBLE", "lead_time": "DOUBLE", "SID": "INT"}
    # if there is a vertical level column, that is also a primary key

    if param["level_name"] in ["z", "p", "h"]:
        primary_keys[param["level_name"]] = "INT"

    other_keys = {
        "lat": "DOUBLE",
        "lon": "DOUBLE",
        "valid_dttm": "INT",
        "parameter": "TEXT",
        "units": "TEXT",
        model: "DOUBLE",
    }
    # for T2m correction
    if "T2m" in param["harp_param"]:
        other_keys["elev"] = "DOUBLE"
        other_keys["model_elevation"] = "DOUBLE"

    all_keys = {**primary_keys, **other_keys}
    return (primary_keys, all_keys)
