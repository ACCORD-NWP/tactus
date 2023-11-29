# create a mock "eccodes" module
# this must be in conftest.py to make sure it is read first
import itertools
import sys


class MockObject(object):
    # use this to return objects with attributes
    pass


def mock_getstring(_msgid, key):
    if key == "level":
        return "0"
    return "ok"


def mock_getlong(_msgid, key):
    if key == "productDefinitionTemplateNumber":
        return 8
    if key == "indicatorOfUnitOfTimeRange":
        return 13
    if key == "indicatorOfUnitForTimeRange":
        return 13
    return 0


def mock_getdouble(_msgid, _key):
    return 1.0


def mock_gethandle(fileobj):
    # NOTE: this mocker expects an intertools.count object!
    x = fileobj.__next__()
    if x < 3:
        return x
    return None


def mock_iterator(*_args, **_kwargs):
    return itertools.count(1)


def mock_iterator_next(gribid):
    # NOTE: this mocker expects an intertools.count object!
    x = gribid.__next__()
    if x < 3:
        return [0.0, 0.0]
    return None


def mock_nearest(*_args, **kwargs):
    x = MockObject()
    x.distance = 1.0
    x.index = 0
    return [x] * kwargs["npoints"]


def mock_values(*_args, **_kwargs):
    return [0, 0]


mock_eccodes = type(sys)("eccodes")
mock_eccodes.codes_get_string = mock_getstring
mock_eccodes.codes_get_long = mock_getlong
mock_eccodes.codes_get_double = mock_getdouble
mock_eccodes.codes_grib_new_from_file = mock_gethandle
mock_eccodes.codes_grib_iterator_new = mock_iterator
mock_eccodes.codes_grib_iterator_next = mock_iterator_next
mock_eccodes.codes_grib_find_nearest = mock_nearest
mock_eccodes.codes_get_values = mock_values

sys.modules["eccodes"] = mock_eccodes
