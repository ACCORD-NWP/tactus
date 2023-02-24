"""GMTED and SOILGRID."""

from .base import Task
import os
import sys

try:
    from osgeo import gdal
except ImportError:
    raise ImportError('gdal python module not found. Suggestion: pip install pygdal=="`gdal-config --version`.*"')

from ..os_utils import Search
from ..geo_utils import Projection, Projstring


class Gmted(Task):
    """GMTED."""

    def __init__(self, config):
        """Init Gmted.

        Args:
            config (Config): Config object
        """
        self.domain = self.get_domain_properties(config)

        Task.__init__(self, config, __name__)

        self.gmted2010_path = self.fmanager.platform.get_platform_value("gmted2010_data_path")

    def get_domain_properties(self, config) -> dict:
        """Get domain properties.

        Args:
            config (Config): Config object

        Returns:
            dict: Domain properties
        """
        domain = {"nlon": config.get_value('domain.njmax'),
                  "nlat": config.get_value('domain.nimax'),
                  "latc": config.get_value('domain.xlatcen'),
                  "lonc": config.get_value('domain.xloncen'),
                  "lat0": config.get_value('domain.xlat0'),
                  "lon0": config.get_value('domain.xlon0'),
                  "gsize": config.get_value('domain.xdx')}

        return domain

    def gmted_header_coordinates(self, east : float, west : float, south : float, north : float) -> tuple:
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
                gmtedlat = '{:02d}N'.format(lat) if lat >= 0 else '{:02d}S'.format(-1 * lat)
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
                gmtedlon = '{:03d}E'.format(rel_lon) if rel_lon >= 0 else '{:03d}W'.format(-1 * lon)
                gmted2010_input_lons.append(gmtedlon)
                i += 1
            if east <= lon:
                break

        hdr_east = lon
        hdr_west = lon - i * longitude_bin_size

        return hdr_east, hdr_west, hdr_south, hdr_north, gmted2010_input_lats, gmted2010_input_lons

    def define_gmted_input(self, domain_properties : dict) -> tuple:
        """Define GMTED input files.

        Args:
            domain_properties (dict): Domain properties

        Returns:
            tuple: GMTED input files
        """
        east = domain_properties['minlon']
        west = domain_properties['maxlon']
        south = domain_properties['minlat']
        north = domain_properties['maxlat']

        hdr_east, hdr_west, hdr_south, hdr_north, gmted2010_input_lats, gmted2010_input_lons = self.gmted_header_coordinates(
            east, west, south, north)

        tif_files = []

        for lat in gmted2010_input_lats:
            for lon in gmted2010_input_lons:
                tif_file = '{}/{}{}_20101117_gmted_mea075.tif'.format(self.gmted2010_path, lat, lon)
                tif_files.append(tif_file)

        for tif_file in tif_files:
            if not os.path.isfile(tif_file):
                self.logger.error('GMTED file %s not found', tif_file)
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

        f = open(bin_file, 'wb')
        for iy in range(gd.RasterYSize):
            data = band.ReadAsArray(0, iy, gd.RasterXSize, 1)
            sel = (data == -32768)
            data[sel] = 0
            data.byteswap().astype('int16').tofile(f)
        f.close()

    @staticmethod
    def write_gmted_header_file(header_file, hdr_north, hdr_south,
                                hdr_west, hdr_east, hdr_rows, hdr_cols) -> None:
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
        with open(header_file, 'w') as f:
            f.write('PROCESSED GMTED2010, orography model, resolution 250m\n')
            f.write('nodata: -9999\n')
            f.write('north: {}.\n'.format(hdr_north))
            f.write('south: {}.\n'.format(hdr_south))
            f.write('west: {}.\n'.format(hdr_west))
            f.write('east: {}.\n'.format(hdr_east))
            f.write('rows: {}\n'.format(hdr_rows))
            f.write('cols: {}\n'.format(hdr_cols))
            f.write('recordtype: integer 16 bytes\n')

        return

    def execute(self):
        """Run task.

        Define run sequence.
        """
        projstr = Projstring().get_projstring(lon0=self.domain['lon0'], lat0=self.domain['lat0'])
        proj = Projection(projstr)
        domain_properties = proj.get_domain_properties(self.domain)

        tif_files, hdr_east, hdr_west, hdr_south, hdr_north = self.define_gmted_input(domain_properties)

        # Output merged GMTED file to working directory as file gmted_mea075.tif
        gd = gdal.Warp("gmted_mea075.tif", tif_files, format="GTiff",
                       options=["COMPRESS=LZW", "TILED=YES"])

        Gmted.tif2bin(gd, "gmted_mea075.bin")

        # Get number of rows and columns
        hdr_rows = gd.RasterYSize
        hdr_cols = gd.RasterXSize

        gd = None

        # Create the header file
        Gmted.write_gmted_header_file("gmted2010.hdr", hdr_north, hdr_south,
                                      hdr_west, hdr_east, hdr_rows, hdr_cols)


class Soil(Task):
    """Prepare soil data task for PGD."""

    def __init__(self, config):
        """Construct soil data object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        self.domain = self.get_domain_properties(config)

        Task.__init__(self, config, __name__)
        self.logger.debug("Constructed Soil task")

    def get_domain_properties(self, config) -> dict:
        """Get domain properties.

        Args:
            config (deode.ParsedConfig): Configuration

        Returns:
            dict: Domain properties
        """
        domain = {"nlon": config.get_value('domain.njmax'),
                  "nlat": config.get_value('domain.nimax'),
                  "latc": config.get_value('domain.xlatcen'),
                  "lonc": config.get_value('domain.xloncen'),
                  "lat0": config.get_value('domain.xlat0'),
                  "lon0": config.get_value('domain.xlon0'),
                  "gsize": config.get_value('domain.xdx')}

        return domain

    @staticmethod
    def check_domain_validity(domain_properties : dict) -> None:
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

        is_outside = True if (domain_properties['minlon'] < glo_west or domain_properties['minlon'] > glo_east
                              or domain_properties['maxlon'] < glo_west or domain_properties['maxlon'] > glo_east
                              or domain_properties['minlat'] < glo_south or domain_properties['minlat'] > glo_north
                              or domain_properties['maxlat'] < glo_south or domain_properties['maxlat'] > glo_north) else False

        if is_outside:
            raise ValueError('Domain outside soilgrid data area')

    @staticmethod
    def coordinates_for_cutting_dataset(domain_properties : dict, halo : float = 5.0) -> tuple:
        """Get coordinates for cutting dataset.

        Args:
            domain_properties (dict): Dict with domain properties
            halo (float): Halo. Defaults to 5.0.

        Returns:
            tuple: Coordinates for cutting dataset
        """
        cut_west = domain_properties['minlon'] - halo
        cut_east = domain_properties['maxlon'] + halo
        cut_south = domain_properties['minlat'] - halo
        cut_north = domain_properties['maxlat'] + halo

        return cut_west, cut_east, cut_south, cut_north

    @staticmethod
    def write_soil_header_file(header_file, soiltype, hdr_north, hdr_south,
                               hdr_west, hdr_east, hdr_rows, hdr_cols,
                               nodata=0, bits=8, write_fact=False) -> None:
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
        """
        with open(header_file, 'w') as f:
            f.write('{} cut from global soilgrids of 250m resolution\n'.format(soiltype))
            f.write('nodata: {}\n'.format(nodata))
            f.write('north: {}.\n'.format(hdr_north))
            f.write('south: {}.\n'.format(hdr_south))
            f.write('west: {}.\n'.format(hdr_west))
            f.write('east: {}.\n'.format(hdr_east))
            f.write('rows: {}\n'.format(hdr_rows))
            f.write('cols: {}\n'.format(hdr_cols))
            if write_fact:
                f.write('fact: 10\n')
            f.write('recordtype: integer {} bits\n'.format(bits))

        return

    def execute(self):
        """Run task.

        Define run sequence.
        """
        self.logger.debug('Running soil task')

        soilgrid_path = self.fmanager.platform.get_platform_value("SOILGRID_DATA_PATH")

        soilgrid_tifs = Search.find_files(soilgrid_path, postfix='.tif', fullpath=True)

        if len(soilgrid_tifs) == 0:
            self.logger.error('No soilgrid tifs found in %s') % self.soilgrid_path
            sys.exit(1)

        # symlink with filemanager from toolbox
        for soilgrid_tif in soilgrid_tifs:
            soilgrid_tif_basename = os.path.basename(soilgrid_tif)
            self.fmanager.input(soilgrid_tif, soilgrid_tif_basename, check_archive=False, provider_id="symlink")

        projstr = Projstring().get_projstring(lon0=self.domain['lon0'], lat0=self.domain['lat0'])
        proj = Projection(projstr)
        domain_properties = proj.get_domain_properties(self.domain)
        self.check_domain_validity(domain_properties)

        # Get coordinates for cutting dataset
        cut_lon, cut_east, cut_south, cut_north = self.coordinates_for_cutting_dataset(domain_properties)

        # Cut soilgrid tifs
        soilgrid_tif_subarea_files = []
        find_size_and_corners = True
        for soilgrid_tif in soilgrid_tifs:
            soilgrid_tif_basename = os.path.basename(soilgrid_tif)
            soilgrid_tif_subarea = soilgrid_tif_basename.replace('.tif', '_subarea.tif')
            soilgrid_tif_subarea_files.append(soilgrid_tif_subarea)
            ds = gdal.Open(soilgrid_tif_basename)
            ds = gdal.Translate(soilgrid_tif_subarea, ds, projWin=[cut_lon, cut_north, cut_east, cut_south])
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

        # change format from tif to binary for SURFEX
        # TODO: Can this use Bollis tif2bin method?
        hdr_north_str = "{:06.3f}N".format(hdr_north) if hdr_north > 0 else "{:06.3f}S".format(-hdr_north)
        hdr_south_str = "{:06.3f}N".format(hdr_south) if hdr_south > 0 else "{:06.3f}S".format(-hdr_south)
        hdr_east_str = "{:07.3f}E".format(hdr_east) if hdr_east > 0 else "{:07.3f}W".format(-hdr_east)
        hdr_west_str = "{:07.3f}E".format(hdr_west) if hdr_west > 0 else "{:07.3f}W".format(-hdr_west)

        outname_append = "{}_{}_{}_{}.dir".format(hdr_west_str, hdr_north_str, hdr_east_str, hdr_south_str)

        for subarea_file in soilgrid_tif_subarea_files:
            if subarea_file.startswith('SNDPPT'):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate("SAND_{}.dir".format(outname_append), ds, format='EHdr', outputType=gdal.GDT_Byte)
                ds = None
            elif subarea_file.startswith('CLYPPT'):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate("CLAY_{}.dir".format(outname_append), ds, format='EHdr', outputType=gdal.GDT_Byte)
                ds = None
            elif subarea_file.startswith('SOC_TOP'):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate("soc_top.dir", ds, format='EHdr', outputType=gdal.GDT_Int16)
                ds = None
            elif subarea_file.startswith('SOC_SUB'):
                ds = gdal.Open(subarea_file)
                ds = gdal.Translate("soc_sub.dir", ds, format='EHdr', outputType=gdal.GDT_Int16)
                ds = None
            else:
                self.logger.warning('Unknown soilgrid tif file: %s', subarea_file)

        # Compose headers in surfex/pgd format
        self.write_soil_header_file("CLAY_{}.hdr".format(outname_append), 'Clay',
                                    hdr_north, hdr_south, hdr_west, hdr_east,
                                    hdr_rows, hdr_cols, nodata=0, bits=8,
                                    write_fact=False)
        self.write_soil_header_file("SAND_{}.hdr".format(outname_append), 'Sand',
                                    hdr_north, hdr_south, hdr_west, hdr_east,
                                    hdr_rows, hdr_cols, nodata=0, bits=8,
                                    write_fact=False)
        self.write_soil_header_file("soc_top.hdr", 'soc_top', hdr_north, hdr_south,
                                    hdr_west, hdr_east, hdr_rows, hdr_cols,
                                    nodata=-9999, bits=16, write_fact=True)
        self.write_soil_header_file("soc_sub.hdr", 'soc_sub', hdr_north, hdr_south,
                                    hdr_west, hdr_east, hdr_rows, hdr_cols,
                                    nodata=-9999, bits=16, write_fact=True)

        self.logger.debug('Finished soil task')
