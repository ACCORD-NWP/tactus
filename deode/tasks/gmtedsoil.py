"""GMTED and SOILGRID."""

import os
import sys

from ..geo_utils import Projection, Projstring
from ..os_utils import Search
from .base import Task


def _import_gdal():
    """Return imported gdal from osgeo. Utility func useful for debugging and testing."""
    try:
        from osgeo import gdal

        return gdal
    except ImportError as error:
        msg = "Cannot use the installed gdal library, or there is no gdal library installed. "
        msg += "If you have not installed it, you may want to try running"
        msg += " 'pip install pygdal==\"`gdal-config --version`.*\"' "
        msg += "or, if you use conda,"
        msg += " 'conda install -c conda-forge gdal'."
        raise ImportError(msg) from error


class Gmted(Task):
    """GMTED."""

    def __init__(self, config):
        """Init Gmted.

        Args:
            config (Config): Config object
        """
        self.domain = self.get_domain_properties(config)

        Task.__init__(self, config, "Gmted")

        self.gmted2010_path = self.fmanager.platform.get_platform_value(
            "gmted2010_data_path"
        )

    def get_domain_properties(self, config) -> dict:
        """Get domain properties.

        Args:
            config (Config): Config object

        Returns:
            dict: Domain properties
        """
        domain = {
            "nlon": config["domain.njmax"],
            "nlat": config["domain.nimax"],
            "latc": config["domain.xlatcen"],
            "lonc": config["domain.xloncen"],
            "lat0": config["domain.xlat0"],
            "lon0": config["domain.xlon0"],
            "gsize": config["domain.xdx"],
        }

        return domain

    def gmted_header_coordinates(
        self, east: float, west: float, south: float, north: float
    ) -> tuple:
        """Get GMTED header coordinates.

        Args:
            east (float): East
            west (float): West
            south (float): South
            north (float): North

        Returns:
            tuple: Header coordinates
        """
        longitude_bin_size = 30

        # GMTED2010 Latitudes
        gmted2010_input_lats = []
        i = 0
        for lat in range(70, -90, -20):
            if north > lat:
                gmtedlat = (
                    "{:02d}N".format(lat) if lat >= 0 else "{:02d}S".format(-1 * lat)
                )
                gmted2010_input_lats.append(gmtedlat)
                i += 1
            if south >= lat:
                break

        hdr_south = lat
        hdr_north = lat + i * 20

        # GMTED2010 Longitudes
        gmted2010_input_lons = []
        i = 0
        for lon in range(-180, 150, longitude_bin_size):
            if west < lon:
                rel_lon = lon - longitude_bin_size
                gmtedlon = (
                    "{:03d}E".format(rel_lon)
                    if rel_lon >= 0
                    else "{:03d}W".format(-1 * rel_lon)
                )
                gmted2010_input_lons.append(gmtedlon)
                i += 1
            if east <= lon:
                break

        hdr_east = lon
        hdr_west = lon - i * longitude_bin_size

        return (
            hdr_east,
            hdr_west,
            hdr_south,
            hdr_north,
            gmted2010_input_lats,
            gmted2010_input_lons,
        )

    def define_gmted_input(self, domain_properties: dict) -> tuple:
        """Define GMTED input files.

        Args:
            domain_properties (dict): Domain properties

        Returns:
            tuple: GMTED input files
        """
        west = domain_properties["minlon"]
        east = domain_properties["maxlon"]
        south = domain_properties["minlat"]
        north = domain_properties["maxlat"]

        (
            hdr_east,
            hdr_west,
            hdr_south,
            hdr_north,
            gmted2010_input_lats,
            gmted2010_input_lons,
        ) = self.gmted_header_coordinates(east, west, south, north)

        tif_files = []

        for lat in gmted2010_input_lats:
            for lon in gmted2010_input_lons:
                tif_file = f"{self.gmted2010_path}/{lat}{lon}_20101117_gmted_mea075.tif"
                tif_files.append(tif_file)

        for tif_file in tif_files:
            if not os.path.isfile(tif_file):
                self.logger.error("GMTED file %s not found", tif_file)
                sys.exit(1)

        return tif_files, hdr_east, hdr_west, hdr_south, hdr_north

    @staticmethod
    def tif2bin(gd, bin_file) -> None:
        """Convert tif file to binary file used by surfex.

        Args:
            gd: gdal dataset
            bin_file (str): Binary file
        """
        band = gd.GetRasterBand(1)

        f = open(bin_file, "wb")
        for iy in range(gd.RasterYSize):
            data = band.ReadAsArray(0, iy, gd.RasterXSize, 1)
            sel = data == -32768
            data[sel] = 0
            data.byteswap().astype("int16").tofile(f)
        f.close()

    @staticmethod
    def write_gmted_header_file(
        header_file, hdr_north, hdr_south, hdr_west, hdr_east, hdr_rows, hdr_cols
    ) -> None:
        """Write header file.

        Args:
            header_file (str): Header file
            hdr_north (float): North
            hdr_south (float): South
            hdr_west (float): West
            hdr_east (float): East
            hdr_rows (int): Number of rows
            hdr_cols (int): Number of columns
        """
        with open(header_file, mode="w", encoding="utf8") as f:
            f.write("PROCESSED GMTED2010, orography model, resolution 250m\n")
            f.write("nodata: -9999\n")
            f.write(f"north: {hdr_north:.2f}\n")
            f.write(f"south: {hdr_south:.2f}\n")
            f.write(f"west: {hdr_west:.2f}\n")
            f.write(f"east: {hdr_east:.2f}\n")
            f.write(f"rows: {int(hdr_rows):d}\n")
            f.write(f"cols: {int(hdr_cols):d}\n")
            f.write("recordtype: integer 16 bytes\n")

    def execute(self):
        """Run task.

        Define run sequence.
        """
        climdir = self.platform.get_system_value("climdir")
        os.makedirs(climdir, exist_ok=True)

        projstr = Projstring().get_projstring(
            lon0=self.domain["lon0"], lat0=self.domain["lat0"]
        )
        proj = Projection(projstr)
        domain_properties = proj.get_domain_properties(self.domain)

        tif_files, hdr_east, hdr_west, hdr_south, hdr_north = self.define_gmted_input(
            domain_properties
        )

        # Output merged GMTED file to working directory as file gmted_mea075.tif
        gdal = _import_gdal()
        gd = gdal.Warp(
            "gmted_mea075.tif",
            tif_files,
            format="GTiff",
            options=["COMPRESS=LZW", "TILED=YES"],
        )

        Gmted.tif2bin(gd, "gmted_mea075.bin")
        os.rename("gmted_mea075.bin", f"{climdir}/gmted2010.dir")

        # Get number of rows and columns
        hdr_rows = gd.RasterYSize
        hdr_cols = gd.RasterXSize

        gd = None

        # Create the header file
        header_file = f"{climdir}/gmted2010.hdr"
        self.logger.debug("Write header file %s", header_file)
        Gmted.write_gmted_header_file(
            header_file, hdr_north, hdr_south, hdr_west, hdr_east, hdr_rows, hdr_cols
        )


class Soil(Task):
    """Prepare soil data task for PGD."""

    def __init__(self, config):
        """Construct soil data object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        self.domain = self.get_domain_properties(config)

        Task.__init__(self, config, "Soil")
        self.logger.debug("Constructed Soil task")

    def get_domain_properties(self, config) -> dict:
        """Get domain properties.

        Args:
            config (deode.ParsedConfig): Configuration

        Returns:
            dict: Domain properties
        """
        domain = {
            "nlon": config["domain.njmax"],
            "nlat": config["domain.nimax"],
            "latc": config["domain.xlatcen"],
            "lonc": config["domain.xloncen"],
            "lat0": config["domain.xlat0"],
            "lon0": config["domain.xlon0"],
            "gsize": config["domain.xdx"],
        }

        return domain

    @staticmethod
    def check_domain_validity(domain_properties: dict) -> None:
        """Check if domain is valid.

        Args:
            domain_properties (dict): Dict with domain properties

        Raises:
            ValueError: If domain is outside soilgrid data area
        """
        # Area available from soilgrid data
        glo_north = 84.0
        glo_south = -56.0
        glo_east = 180.0
        glo_west = -180.0

        is_outside = (
            True
            if (
                domain_properties["minlon"] < glo_west
                or domain_properties["minlon"] > glo_east
                or domain_properties["maxlon"] < glo_west
                or domain_properties["maxlon"] > glo_east
                or domain_properties["minlat"] < glo_south
                or domain_properties["minlat"] > glo_north
                or domain_properties["maxlat"] < glo_south
                or domain_properties["maxlat"] > glo_north
            )
            else False
        )

        if is_outside:
            raise ValueError("Domain outside soilgrid data area")

    @staticmethod
    def coordinates_for_cutting_dataset(
        domain_properties: dict, halo: float = 5.0
    ) -> tuple:
        """Get coordinates for cutting dataset.

        Args:
            domain_properties (dict): Dict with domain properties
            halo (float): Halo. Defaults to 5.0.

        Returns:
            tuple: Coordinates for cutting dataset
        """
        cut_west = domain_properties["minlon"] - halo
        cut_east = domain_properties["maxlon"] + halo
        cut_south = domain_properties["minlat"] - halo
        cut_north = domain_properties["maxlat"] + halo

        return cut_west, cut_east, cut_south, cut_north

    @staticmethod
    def write_soil_header_file(
        header_file,
        soiltype,
        hdr_north,
        hdr_south,
        hdr_west,
        hdr_east,
        hdr_rows,
        hdr_cols,
        nodata=0,
        bits=8,
        write_fact=False,
        fact=10,
    ) -> None:
        """Write header file.

        Args:
            header_file (str): Header file
            soiltype (str): Soil type
            hdr_north (float): North
            hdr_south (float): South
            hdr_west (float): West
            hdr_east (float): East
            hdr_rows (int): Number of rows
            hdr_cols (int): Number of columns
            nodata (int): No data value. Defaults to 0.
            bits (int): Number of bits. Defaults to 8.
            write_fact (bool): Write factor. Defaults to False.
            fact (int): Factor. Defaults to 10
        """
        with open(header_file, mode="w", encoding="utf8") as f:
            f.write(f"{soiltype} cut from global soilgrids of 250m resolution\n")
            f.write(f"nodata: {nodata:d}\n")
            f.write(f"north: {float(hdr_north):.6f}\n")
            f.write(f"south: {float(hdr_south):.6f}\n")
            f.write(f"west: {float(hdr_west):.6f}\n")
            f.write(f"east: {float(hdr_east):.6f}\n")
            f.write(f"rows: {int(hdr_rows):d}\n")
            f.write(f"cols: {int(hdr_cols):d}\n")
            # TODO Check if factor can be float
            if write_fact:
                f.write(f"fact: {fact:d}\n")
            f.write(f"recordtype: integer {bits:d} bits\n")

    def execute(self):
        """Run task.

        Define run sequence.

        Raises:
            FileNotFoundError: If no tif files are found.
        """
        self.logger.debug("Running soil task")

        soilgrid_path = self.fmanager.platform.get_platform_value("SOILGRID_DATA_PATH")

        soilgrid_tifs = Search.find_files(soilgrid_path, postfix=".tif", fullpath=True)

        if len(soilgrid_tifs) == 0:
            raise FileNotFoundError(f"No soilgrid tifs found under {soilgrid_path}")

        # symlink with filemanager from toolbox
        for soilgrid_tif in soilgrid_tifs:
            soilgrid_tif_basename = os.path.basename(soilgrid_tif)
            self.fmanager.input(
                soilgrid_tif,
                soilgrid_tif_basename,
                check_archive=False,
                provider_id="symlink",
            )

        projstr = Projstring().get_projstring(
            lon0=self.domain["lon0"], lat0=self.domain["lat0"]
        )
        proj = Projection(projstr)
        domain_properties = proj.get_domain_properties(self.domain)
        self.check_domain_validity(domain_properties)

        # Get coordinates for cutting dataset
        cut_lon, cut_east, cut_south, cut_north = self.coordinates_for_cutting_dataset(
            domain_properties
        )

        # Cut soilgrid tifs
        soilgrid_tif_subarea_files = []
        find_size_and_corners = True
        gdal = _import_gdal()
        for soilgrid_tif in soilgrid_tifs:
            soilgrid_tif_basename = os.path.basename(soilgrid_tif)
            soilgrid_tif_subarea = soilgrid_tif_basename.replace(".tif", "_subarea.tif")
            soilgrid_tif_subarea_files.append(soilgrid_tif_subarea)
            ds = gdal.Open(soilgrid_tif_basename)
            ds = gdal.Translate(
                soilgrid_tif_subarea,
                ds,
                projWin=[cut_lon, cut_north, cut_east, cut_south],
            )
            if find_size_and_corners:
                # Get number of rows and columns
                hdr_rows = ds.RasterYSize
                hdr_cols = ds.RasterXSize
                # Get corners
                gt = ds.GetGeoTransform()
                hdr_west = gt[0]
                hdr_north = gt[3]
                hdr_east = hdr_west + hdr_cols * gt[1]
                hdr_south = hdr_north + hdr_rows * gt[5]
                find_size_and_corners = False
            ds = None

        climdir = self.platform.get_system_value("climdir")
        os.makedirs(climdir, exist_ok=True)
        for subarea_file in soilgrid_tif_subarea_files:
            if subarea_file.startswith("SNDPPT"):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate(
                    climdir + "/SAND_SOILGRID.dir",
                    ds,
                    format="EHdr",
                    outputType=gdal.GDT_Byte,
                )
                ds = None
            elif subarea_file.startswith("CLYPPT"):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate(
                    climdir + "/CLAY_SOILGRID.dir",
                    ds,
                    format="EHdr",
                    outputType=gdal.GDT_Byte,
                )
                ds = None
            elif subarea_file.startswith("SOC_TOP"):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate(
                    climdir + "/soc_top.dir", ds, format="EHdr", outputType=gdal.GDT_Int16
                )
                ds = None
            elif subarea_file.startswith("SOC_SUB"):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate(
                    climdir + "/soc_sub.dir", ds, format="EHdr", outputType=gdal.GDT_Int16
                )
                ds = None
            else:
                self.logger.warning("Unknown soilgrid tif file: %s", subarea_file)

        # Compose headers in surfex/pgd format
        self.write_soil_header_file(
            climdir + "/CLAY_SOILGRID.hdr",
            "Clay",
            hdr_north,
            hdr_south,
            hdr_west,
            hdr_east,
            hdr_rows,
            hdr_cols,
            nodata=0,
            bits=8,
            write_fact=False,
        )
        self.write_soil_header_file(
            climdir + "/SAND_SOILGRID.hdr",
            "Sand",
            hdr_north,
            hdr_south,
            hdr_west,
            hdr_east,
            hdr_rows,
            hdr_cols,
            nodata=0,
            bits=8,
            write_fact=False,
        )
        self.write_soil_header_file(
            climdir + "/soc_top.hdr",
            "soc_top",
            hdr_north,
            hdr_south,
            hdr_west,
            hdr_east,
            hdr_rows,
            hdr_cols,
            nodata=-9999,
            bits=16,
            write_fact=True,
        )
        self.write_soil_header_file(
            climdir + "/soc_sub.hdr",
            "soc_sub",
            hdr_north,
            hdr_south,
            hdr_west,
            hdr_east,
            hdr_rows,
            hdr_cols,
            nodata=-9999,
            bits=16,
            write_fact=True,
        )

        self.logger.debug("Finished soil task")
