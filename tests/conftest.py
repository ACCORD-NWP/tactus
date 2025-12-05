# create a mock "eccodes" module
# this must be in conftest.py to make sure it is read first
import sys

import numpy as np
import pytest

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.host_actions import DeodeHost


class MockObject(object):
    # use this to return objects with attributes
    pass


def mock_codes_get_string(msgid, key):
    # in reality msgid should be a grib handle
    # in the mocker, it will be a dict
    if key in msgid:
        return str(msgid[key])
    return "ok"


def mock_codes_get_long(msgid, key):
    if key in msgid:
        return int(msgid[key])

    return 0


def mock_codes_get_double(msgid, key):
    if key in msgid:
        return float(msgid[key])

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
    return None


def mock_codes_get_values(gribid):
    nx = int(gribid["Nx"])
    ny = int(gribid["Ny"])
    return np.array([0] * nx * ny)


def mock_codes_release(msgid):
    kl = list(msgid.keys())
    for kk in kl:
        del msgid[kk]


class MockKeyValueNotFoundError(Exception):
    pass


mock_eccodes = type(sys)("eccodes")
mock_eccodes.codes_get_string = mock_codes_get_string
mock_eccodes.codes_get_long = mock_codes_get_long
mock_eccodes.codes_get_double = mock_codes_get_double
mock_eccodes.codes_grib_new_from_file = mock_codes_grib_new_from_file
mock_eccodes.codes_get_values = mock_codes_get_values
mock_eccodes.codes_release = mock_codes_release
mock_eccodes.KeyValueNotFoundError = MockKeyValueNotFoundError

sys.modules["eccodes"] = mock_eccodes


@pytest.fixture(scope="module")
def tmp_directory(tmp_path_factory):
    """Return a temp directory valid for this module."""
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def default_config(tmp_directory):
    """Return a parsed config to be used for unit tests."""
    deode_host = DeodeHost().detect_deode_host(use_default=False)
    if deode_host is None:
        deode_host = "pytest"

    config = ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        host=deode_host,
    )
    if deode_host == "pytest":
        config = config.copy(update={"platform": {"scratch": str(tmp_directory)}})

    return config
