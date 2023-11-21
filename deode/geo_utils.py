"""Utilities for simple geographic tasks."""
import numpy as np
import pyproj


class Projstring:
    """Proj4 string class."""

    def __init__(self):
        """Construct proj4 string."""
        self.earth_radius = 6371000.0

    def get_projstring(self, lon0=0.0, lat0=-90.0) -> str:
        """Get proj4 string.

        Args:
            lon0 (float, optional): Central longitude. Defaults to 0.0.
            lat0 (float, optional): Central latitude. Defaults to -90.0.

        Returns:
            str: Proj4 string
        """
        if lat0 == -90.0:
            proj_string = (
                f"+proj=stere +lat_0={lat0!s} +lon_0={lon0!s} " f"+lat_ts={lat0!s}"
            )
        else:
            proj_string = (
                f"+proj=lcc +lat_0={lat0!s} +lon_0={lon0!s} "
                f"+lat_1={lat0!s} +lat_2={lat0!s} "
                f"+units=m +no_defs +R={self.earth_radius!s}"
            )

        return proj_string


class Projection:
    """Projection class."""

    def __init__(self, proj4str):
        """Construct projection.

        Args:
            proj4str (str): Proj4 string
        """
        self.proj4str = proj4str
        self.proj = pyproj.CRS.from_string(proj4str)
        self.wgs84 = pyproj.CRS.from_string("EPSG:4326")

    def check_key(self, key: str, config: dict) -> bool:
        """Check if key is in config.

        Args:
            key (str): Key to check
            config (dict): Configuration

        Returns:
            bool: True if key is in config

        Raises:
            ValueError: If key is not in config
        """
        if key in config:
            return True
        else:
            raise ValueError("{} not in dictionary. Check config file".format(key))

    def get_domain_properties(self, domain_spec: dict) -> dict:
        """Get domain properties.

        Args:
            domain_spec (dict): Domain specification

        Returns:
            dict: Domain properties
        """
        self.check_key("lonc", domain_spec)
        self.check_key("latc", domain_spec)
        self.check_key("nlon", domain_spec)
        self.check_key("nlat", domain_spec)
        self.check_key("gsize", domain_spec)

        lonc = domain_spec["lonc"]
        latc = domain_spec["latc"]
        nlon = domain_spec["nlon"]
        nlat = domain_spec["nlat"]
        gsize = domain_spec["gsize"]

        xloncen, xlatcen = pyproj.Transformer.from_crs(
            self.wgs84, self.proj, always_xy=True
        ).transform(lonc, latc)

        x_0 = float(xloncen) - (0.5 * ((float(nlon) - 1.0) * gsize))
        y_0 = float(xlatcen) - (0.5 * ((float(nlat) - 1.0) * gsize))

        xxx = np.empty([nlon])
        yyy = np.empty([nlat])
        for i in range(0, nlon):
            xxx[i] = x_0 + (float(i) * gsize)
        for j in range(0, nlat):
            yyy[j] = y_0 + (float(j) * gsize)

        x_v, y_v = np.meshgrid(xxx, yyy)
        lons, lats = pyproj.Transformer.from_crs(
            self.proj, self.wgs84, always_xy=True
        ).transform(x_v, y_v)

        minlat = np.floor(np.min(lats)) - 1
        minlon = np.floor(np.min(lons)) - 1
        maxlat = np.ceil(np.max(lats)) + 1
        maxlon = np.ceil(np.max(lons)) + 1

        minlat = np.max([minlat, -90])
        minlon = np.max([minlon, -180])
        maxlat = np.min([maxlat, 90])
        maxlon = np.min([maxlon, 180])

        domain_properties = {
            "minlat": minlat,
            "minlon": minlon,
            "maxlat": maxlat,
            "maxlon": maxlon,
        }

        return domain_properties
