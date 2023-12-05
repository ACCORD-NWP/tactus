# create a mock "eccodes" module
# this must be in conftest.py to make sure it is read first
import itertools
import sys

import numpy as np


class MockObject(object):
    # use this to return objects with attributes
    pass


def mock_codes_get_string(msgid, key):
    # in reality msgid should be a grib handle
    # in the mocker, it will be a dict
    if key in msgid.keys():
        return str(msgid[key])
    else:
        return "ok"


def mock_codes_get_long(msgid, key):
    if key in msgid.keys():
        return int(msgid[key])
    else:
        return 0


def mock_codes_get_double(msgid, key):
    if key in msgid.keys():
        return float(msgid[key])
    else:
        return 0.0


def mock_codes_grib_new_from_file(fileobj):
    # NOTE: we return a dict with test values
    # but the tests also need to stop
    # fileobj should be a file with 1 byte or so
    testgid = {
        "shortName": "t",
        "productDefinitionTemplateNumber": 8,
        "indicatorOfUnitOfTimeRange": 13,
        "indicatorOfUnitForTimeRange": 13,
        "Nx": 2,
        "Ny": 2,
        "level": 0,
        "typeOfLevel": "isobaricInhPa",
        "dataDate": "20230915",
        "dataTime": "000000",
        "forecastTime": 0,
        "lengthOfTimeRange": 0,
        "gridType": "lambert",
        "Latin1InDegrees": 50.0,
        "Latin2InDegrees": 50.0,
        "LoVInDegrees": 0.0,
        "DxInMetres": 1,
        "DyInMetres": 1,
    }
    if fileobj.read(1):
        return testgid
    else:
        return None


def mock_codes_get_values(gribid):
    nx = int(gribid["Nx"])
    ny = int(gribid["Ny"])
    return np.array([0] * nx * ny)


class MockKeyValueNotFoundError(Exception):
    pass


mock_eccodes = type(sys)("eccodes")
mock_eccodes.codes_get_string = mock_codes_get_string
mock_eccodes.codes_get_long = mock_codes_get_long
mock_eccodes.codes_get_double = mock_codes_get_double
mock_eccodes.codes_grib_new_from_file = mock_codes_grib_new_from_file
mock_eccodes.codes_get_values = mock_codes_get_values
mock_eccodes.KeyValueNotFoundError = MockKeyValueNotFoundError

sys.modules["eccodes"] = mock_eccodes
