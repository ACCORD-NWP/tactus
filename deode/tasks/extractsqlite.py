"""ExtractSQLite."""

import os

from ..datetime_utils import as_datetime, oi2dt_list
from ..logs import logger
from .base import Task

# For now (on ATOS), only tasks with prgenv/gnu can import eccodes in python
try:
    import eccodes
except (ImportError, OSError):
    logger.warning("eccodes python API could not be imported. Usually OK.")

import datetime
import json
import math
import sqlite3
from contextlib import closing
from copy import deepcopy

import pandas


class ExtractSQLite(Task):
    """Extract sqlite point files."""

    def __init__(self, config):
        """Construct ExtractSQLite object.

        Args:
            config (deode.ParsedConfig): Configuration

        Raises:
            FileNotFoundError: Required file not fount
        """
        Task.__init__(self, config, __name__)

        self.archive = self.platform.get_system_value("archive")

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]
        self.infile_dt = self.config["general.output_settings.fullpos"]
        self.infile_template = self.config["file_templates.fullpos.archive"]

        self.sqlite_path = self.platform.substitute(
            self.config["task.extractsqlite.sqlite_path"]
        )
        self.sqlite_template = self.platform.substitute(
            self.config["task.extractsqlite.sqlite_template"]
        )
        self.model_name = self.platform.substitute(
            self.config["task.extractsqlite.sqlite_model_name"]
        )
        stationfile = self.platform.substitute(
            self.config["task.extractsqlite.station_list"]
        )
        if not os.path.isfile(stationfile):
            raise FileNotFoundError(f" missing {stationfile}")
        logger.info("Station list: {}", stationfile)
        self.station_list = pandas.read_csv(stationfile, skipinitialspace=True)
        paramfile = self.platform.substitute(
            self.config["task.extractsqlite.parameter_list"]
        )
        if not os.path.isfile(paramfile):
            raise FileNotFoundError(f" missing {paramfile}")
        logger.info("Parameter list: {}", paramfile)
        with open(paramfile) as pf:
            self.parameter_list = json.load(pf)
            pf.close()
        kl = [list(p["grib_id"].keys()) for p in self.parameter_list]
        keyset = set(sum(kl, []))
        self.keylist = [*list(keyset), "parameterUnits"]
        self.weights = None
        self.output_settings = self.config["general.output_settings"]

    def get_date_info(self, gid):
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
        info = self.get_keylist(gid, keys, "long")
        if info["productDefinitionTemplateNumber"] == 8:
            keys2 = ["indicatorOfUnitForTimeRange", "lengthOfTimeRange"]
            info2 = self.get_keylist(gid, keys2, "long")
            info = info | info2
        return self.date_from_gribinfo(info)

    def date_from_gribinfo(self, info):
        """Interprete list of date information as given by eccodes."""
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
            logger.debug("Leadtime must be in 'h', 'm' or 's'")
            exit(1)

        # At this point, leadtime may be just the START of accumulation (or min/max/mean) time
        if info["productDefinitionTemplateNumber"] == 8:
            if info["indicatorOfUnitForTimeRange"] == 1:
                lt2 = info["lengthOfTimeRange"] * 3600.0
            elif info["indicatorOfUnitForTimeRange"] == 2:
                lt2 = info["lengthOfTimeRange"] * 60.0
            elif info["indicatorOfUnitForTimeRange"] == 13:
                lt2 = info["lengthOfTimeRange"]
            else:
                logger.debug("Leadtime must be in 'h', 'm' or 's'")
                exit(1)
            leadtime = leadtime + lt2

        return (fcdate, leadtime)

    def fctable_definition(self, param, model):
        """
        Create the SQL command for FCtable definition.

        Args:
            param: the parameter descriptor
            model: model name to be used

        Returns:
            SQL commands for creating the FCTABLE table
        """
        logger.info("SQLITE TABLE DEFINITION")
        primary_keys = {"fcst_dttm": "DOUBLE", "lead_time": "DOUBLE", "SID": "INT"}
        # if there is a vertical level column, that is also a primary key
        if "level" in param["grib_id"].keys():
            primary_keys[param["level_name"]] = "INT"

        other_keys = {
            "lat": "DOUBLE",
            "lon": "DOUBLE",
            "fcst_dttm": "INT",
            "valid_dttm": "INT",
            "lead_time": "DOUBLE",
            "parameter": "TEXT",
            "units": "TEXT",
            "model_elevation": "DOUBLE",
            param["harp_param"]: "DOUBLE",
            model: "DOUBLE",
        }

        all_keys = {**primary_keys, **other_keys}
        return (primary_keys, all_keys)

    def get_keylist(self, gid, keylist, ktype="string"):
        """Get list of grib keys. Return "" or None for keys that are not found."""
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
                logger.debug("Key {} not found", kk)
                val = miss
            ginfo[kk] = val
        return ginfo

    def param_match(self, gid):
        """
        Check whether a grib record is in the list of required parameters.

        TODO: can we re-organise the code to avoid getting the same key multiple times?
          But I suspect the impact is minimal

        Args:
            gid: grib handle

        Returns:
            Parameter descriptor (from the list) that matches the current grib handle, or None.
        """
        for param in self.parameter_list:
            # param['grib_id'] is a dictionary of keys|values that describe the parameter
            # so you need to check all of these to know if the grib record matches
            ok = True
            keylist = [*list(param["grib_id"].keys()), "parameterUnits"]
            ginfo = self.get_keylist(gid, keylist, "string")
            # we make a deepcopy, because we may have to modify some key values...
            pmatch = deepcopy(param)
            for pl in param["grib_id"].keys():
                if type(param["grib_id"][pl]) is str:
                    if ginfo[pl] != param["grib_id"][pl]:
                        ok = False
                        break
                elif type(param["grib_id"][pl]) is list:
                    # This should only happen for the "level" key
                    # check a list of level values
                    if int(ginfo[pl]) not in param["grib_id"][pl]:
                        ok = False
                        break
                    # now modify the parameter to pass the level value back
                    pmatch["grib_id"][pl] = ginfo[pl]
                    if pl == "level":
                        logger.debug("SQLITE: level {}", ginfo[pl])
                        if param["grib_id"]["typeOfLevel"] == "isobaricInhPa":
                            pmatch["level_name"] = "p"
                        elif param["grib_id"]["typeOfLevel"] == "heightAboveGround":
                            pmatch["level_name"] = "z"
                        elif param["grib_id"]["typeOfLevel"] == "hybridLevel":
                            pmatch["level_name"] = "ml"
                        else:
                            pmatch["level_name"] = "xx"

            if ok:
                # all keys match, so we have found a suitable grib record
                pmatch["units"] = ginfo["parameterUnits"]
                return pmatch

        # no match was found
        return None

    def sqlite_name(self, param, fcdate):
        """
        Create the full name of the SQLite file from template and date.

        Args:
            param: parameter descriptor
            fcdate: forecast date

        Returns:
            full name of SQLite file
        """
        result = self.sqlite_template
        result = result.replace("@PP@", param["harp_param"])
        result = result.replace("@YYYY@", fcdate.strftime("%Y"))
        result = result.replace("@MM@", fcdate.strftime("%m"))
        result = result.replace("@DD@", fcdate.strftime("%d"))
        result = result.replace("@HH@", fcdate.strftime("%H"))
        return result

    def points_restrict(self, gid, plist):
        """
        Restrict the station list to points inside the current domain.

        NOTE: * eccodes returns distances in kilometer
              * store the list for use in next run? run this function seperately?

        Args:
            gid: grib handle
            plist: list of stations

        Returns:
            reduced station list that contains only stations inside the domain
        """
        logger.info("SQLITE: Filtering station list")
        # 1. Get the bounding box lat/lon values
        #    that is a fast way to eliminate most outside points
        iterid = eccodes.codes_grib_iterator_new(gid, 0)
        minlat = 200
        maxlat = -200
        minlon = 200
        maxlon = -200
        while 1:
            nextpoint = eccodes.codes_grib_iterator_next(iterid)
            if not nextpoint:
                break
            if nextpoint[0] > maxlat:
                maxlat = nextpoint[0]
            elif nextpoint[0] < minlat:
                minlat = nextpoint[0]
            if nextpoint[1] > maxlon:
                maxlon = nextpoint[1]
            elif nextpoint[1] < minlon:
                minlon = nextpoint[1]

        npoints = plist.shape[0]
        # reduce the table to the bounding box
        # Make a copy! The original retains old row numbers and becomes hard to manage.
        plist = plist[
            (plist["lat"] >= minlat)
            & (plist["lat"] <= maxlat)
            & (plist["lon"] >= minlon)
            & (plist["lon"] <= maxlon)
        ].copy()

        # 2. Now use distance to closest grid point to eliminate the last few exterior points
        #    NOTE: ideally, we would look at the 4 points and decide if a point is really inside
        #          but just checking that the closest point is within 1 grid distance is OK
        #    NOTE: we assume that dx == dy !
        dxy = self.get_keylist(gid, ["DxInMetres", "DyInMetres"], "double")
        dx = dxy["DxInMetres"] / 1000.0
        dy = dxy["DyInMetres"] / 1000.0

        # NOT USED dmax = math.sqrt(dx * dx + dy * dy)
        lon = plist["lon"].tolist()
        lat = plist["lat"].tolist()
        npoints = len(lon)
        drop = [False] * npoints
        for pp in range(npoints):
            n1 = eccodes.codes_grib_find_nearest(
                gid, inlat=lat[pp], inlon=lon[pp], is_lsm=False, npoints=1
            )
            drop[pp] = any(x.distance > dx for x in n1)  # maybe use dmax in stead?

        plist["drop"] = drop
        plist[plist["drop"] == False].copy()
        return plist

    def train_weights(self, gid, lsm=False):
        """Train weights for bilinear and nearest neighbour interpolation.

        We train two kinds of interpolation at once.

        Args:
            gid: grib handle
            lsm: use Land/Sea mask (ignored for now)

        Returns:
            interpolation weights (nearest neighbour, bilinear)
        """
        # TODO: land/sea mask for T2m...
        logger.info("SQLITE: Training interpolation weights")
        dxy = self.get_keylist(gid, ["DxInMetres", "DyInMetres"], "double")
        dx = dxy["DxInMetres"] / 1000.0
        dy = dxy["DyInMetres"] / 1000.0

        lat = self.station_list["lat"].tolist()
        lon = self.station_list["lon"].tolist()
        nstations = len(lon)

        weights = {}

        # eccodes python interface only gives us distances
        # so we need to do some math for bilinear weights
        nearestweights = []
        bilinweights = []
        for pp in range(nstations):
            # assuming the 4 closest points are exactly what we need (OK if dx=dy)
            # NOTE: this assumes dx == dy !!!
            # otherwise, you have to check whether 2nd point is along X or Y axis from first
            # probably easy, just look at the index
            n4 = eccodes.codes_grib_find_nearest(
                gid, inlat=lat[pp], inlon=lon[pp], is_lsm=lsm, npoints=4
            )

            nearestweights.append([[n4[0].index, 1.0]])
            dist = [x.distance for x in n4]
            t1 = (dist[1] * dist[1] - dist[0] * dist[0]) / (dx * dx)
            t2 = (dist[2] * dist[2] - dist[0] * dist[0]) / (dx * dx)
            bilinweights.append(
                [
                    [n4[0].index, (1 + t1) * (1 + t2) / 4.0],
                    [n4[1].index, (1 - t1) * (1 + t2) / 4.0],
                    [n4[2].index, (1 + t1) * (1 - t2) / 4.0],
                    [n4[3].index, (1 - t1) * (1 - t2) / 4.0],
                ]
            )

        weights["bilin"] = bilinweights
        weights["nearest"] = nearestweights
        return weights

    def interp_from_weights(self, gid, method):
        """Interpolate a GRIB field to points using the given weights.

        Args:
            gid: grib handle
            method: interpolation method (bilin, nearest)

        Returns:
            interpolated values
        """
        logger.info("SQLITE: Interpolation using weights")
        # NOTE: this assumes all records in a file use the same grid representation
        data = eccodes.codes_get_values(gid)
        interp = [sum([data[x[0]] * x[1] for x in w]) for w in self.weights[method]]
        return interp

    def combine_fields(self, data, param):
        """Combine multiple decoded fields into one parameter.

        Args:
            data: list of data fields
            param: full parameter descriptor

        Returns:
            a new data matrix that combines the input fields according to parameter descriptor
        """
        if param["function"] == "norm":
            return math.sqrt(data[0] * data[0] + data[1] * data[1])
        elif param["function"] == "sum":
            return data[0] + data[1]
        else:
            return None

    def db_cleanup(self, param, fcd, leadtime, con):
        """Delete any prior version of the date (it's a primary key).

        Args:
            param: parameter description
            fcd: forecast date (string or int)
            leadtime: lead time (int)
            con: database connection
        """
        cleanup = "DELETE from FC WHERE fcst_dttm=? AND lead_time=?"

        cur = con.cursor()
        if "level" in param["grib_id"].keys():
            lname = param["level_name"]
            parlev = int(param["grib_id"]["level"])
            cleanup = cleanup + f" AND {lname} = ?"
            logger.info(cleanup, fcd, leadtime / 3600.0, str(parlev))
            cur.execute(cleanup, [fcd, leadtime / 3600.0, parlev])
        else:
            logger.info(cleanup, fcd, leadtime / 3600.0)
            cur.execute(cleanup, [fcd, leadtime / 3600.0])

    def db_create(self, sqlite_file, param):
        """Create new SQLite file with FC table.

        The table is designed to be compatible with harp, so uses the same
        "unique index" approach.

        Args:
          sqlite_file: SQLite file name
          param: parameter descriptor

        Returns:
          data base connection
        """
        if not os.path.isdir(self.sqlite_path):
            logger.info("SQLITE: Creating directory {}.", self.sqlite_path)
            os.makedirs(self.sqlite_path)
        logger.info("SQLITE: Creating SQLite file {}.", sqlite_file)
        primary_keys, all_keys = self.fctable_definition(param, self.model_name)
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
        logger.info("SQLITE: {}", fc_def)
        logger.info("SQLITE: {}", pk_def)

        index_name = "index_" + "_".join(primary_keys.keys())
        con = sqlite3.connect(sqlite_file)
        with con:
            with closing(con.cursor()) as cur:
                cur.execute(fc_def)
                cur.execute(pk_def)
        return con

    def parse_file(self, gfile):
        """Read a GRIB2 file and extract all required data points to SQLite.

        Args:
          gfile: input grib2 file connection

        Returns:
          A (pretty useless) count of parsed grib records.
        """
        gi = 0
        while True:
            # loop over all grib records in the file
            gid = eccodes.codes_grib_new_from_file(gfile)
            if gid is None:
                # We've reached the last grib record in the file
                break
            gi += 1
            param = self.param_match(gid)
            if param is None:
                # the record is not in our parameter list
                continue

            # we have a matching parameter
            logger.info("Parameter {} matches.", gi)
            fcdate, leadtime = self.get_date_info(gid)
            sqlite_file = self.sqlite_path + self.sqlite_name(param, fcdate)

            # TODO: what if the parameter requires multiple fields? e.g. wind speed from u,v

            # if the SQLite file doesn't exist yet: create the SQLite table
            if os.path.isfile(sqlite_file):
                logger.info("Writing to {}.", sqlite_file)
                con = sqlite3.connect(sqlite_file)
            else:
                con = self.db_create(sqlite_file, param)

            if self.weights is None:
                # We assume that station list and weights are the same for all files
                # so we only "train" once
                # First reduce the station list to points inside the domain
                point_list = self.points_restrict(gid, self.station_list)
                self.station_list = point_list
                # create a list of interpolation weights
                self.weights = self.train_weights(gid, lsm=False)

            # FIXME: for 2TM : elevation correction ???
            data = self.station_list[["SID", "lat", "lon"]].copy()

            # by default, we do bilinear interpolation
            if "method" in param.keys():
                method = param["method"]
            else:
                method = "bilin"

            data[self.model_name] = self.interp_from_weights(gid, method)
            fcd = int(fcdate.timestamp())
            vad = int(fcdate.timestamp() + leadtime)
            data["fcst_dttm"] = fcd
            # NOTE: currently FCTABLE still expects leadtime in hours!
            data["lead_time"] = leadtime / 3600.0
            data["valid_dttm"] = vad
            data["parameter"] = param["harp_param"]
            data["units"] = param["units"]
            # data['model_elevation'] =
            if "level" in param["grib_id"].keys():
                lname = param["level_name"]
                parlev = int(param["grib_id"]["level"])
                data[lname] = parlev

            self.db_cleanup(param, fcd, leadtime, con)

            # now write to SQLite
            data.to_sql("FC", con, if_exists="append", index=False)
            con.commit()
            con.close()

        if gid is not None:
            gid.release()
        return gi

    def execute(self):
        """Execute ExtractSQLite on all output files."""
        dt_list = oi2dt_list(self.infile_dt, self.forecast_range)

        for dt in dt_list:
            infile = self.platform.substitute(
                self.archive + self.infile_template, validtime=self.basetime + dt
            )
            if not os.path.isfile(infile):
                raise FileNotFoundError(f" missing {infile}")
            logger.info("SQLITE EXTRACTION: {}", infile)
            gfile = open(infile, "rb")

            self.parse_file(gfile)
            gfile.close()
