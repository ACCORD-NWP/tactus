# create a mock "eccodes" module
# this must be in conftest.py to make sure it is read first
import sys
from unittest.mock import Mock

import numpy as np
import pytest

from tactus.config_parser import ConfigParserDefaults, ParsedConfig
from tactus.host_actions import TactusHost


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


def mock_codes_count_in_file(fileobj):
    if fileobj.read(1):
        return 1

    return 0


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

# mocked functions for epygram
mock_eccodes.codes_count_in_file = Mock(name="codes_count_in_file")
mock_eccodes.codes_any_new_from_file = Mock(name="codes_any_new_from_file")
mock_eccodes.codes_set = Mock(name="codes_set")
mock_eccodes.codes_set_missing = Mock(name="codes_set_missing")
mock_eccodes.codes_clone = Mock(name="codes_clone")
mock_eccodes.codes_get = Mock(name="codes_get")
mock_eccodes.codes_get_double_array = Mock(name="codes_get_double_array")
mock_eccodes.codes_grib_new_from_samples = Mock(name="codes_grib_new_from_samples")
mock_eccodes.codes_set_array = Mock(name="codes_set_array")
mock_eccodes.codes_set_values = Mock(name="codes_set_values")
mock_eccodes.codes_keys_iterator_new = Mock(name="codes_keys_iterator_new")
mock_eccodes.codes_keys_iterator_next = Mock(name="codes_keys_iterator_next")
mock_eccodes.codes_keys_iterator_get_name = Mock(name="codes_keys_iterator_get_name")
mock_eccodes.codes_keys_iterator_delete = Mock(name="codes_keys_iterator_delete")
mock_eccodes.codes_write = Mock(name="codes_write")
mock_eccodes.codes_index_new_from_file = Mock(name="codes_index_new_from_file")
mock_eccodes.codes_index_select = Mock(name="codes_index_select")
mock_eccodes.codes_new_from_index = Mock(name="codes_new_from_index")
mock_eccodes.codes_index_release = Mock(name="codes_index_release")


# mocked exception class
class MockCodesInternalError(Exception):
    pass


mock_eccodes.CodesInternalError = MockCodesInternalError


# version attribute
mock_eccodes.__version__ = "mock-0.0.0"

sys.modules["eccodes"] = mock_eccodes


@pytest.fixture(scope="module")
def tmp_directory(tmp_path_factory, request):
    """Return a temp directory valid for this module."""
    return tmp_path_factory.mktemp(request.module.__name__, numbered=False).as_posix()


@pytest.fixture(scope="module")
def default_config(tmp_directory):
    """Return a parsed config to be used for unit tests."""
    tactus_host = TactusHost().detect_tactus_host(use_default=False)
    if tactus_host is None:
        tactus_host = "pytest"

    config = ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        host=tactus_host,
    )
    if tactus_host == "pytest":
        config = config.copy(update={"platform": {"scratch": str(tmp_directory)}})

    return config
