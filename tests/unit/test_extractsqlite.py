"""unit tests for extractsqlite."""
import datetime
import itertools

import pandas
import pytest

from deode.tasks.extractsqlite import ExtractSQLite


class MockObject(object):
    pass


@pytest.fixture(scope="session")
def tmp_sqlite_path(tmp_path_factory):
    return tmp_path_factory.getbasetemp() / "sqlite"


class TestExtractSQLite:
    """Test extractsqlite in parts."""

    sqlite_template = "FC_@PP@_@YYYY@@MM@_@HH@.sqlite"
    fcdate = datetime.datetime.strptime("20230915T00", "%Y%m%dT%H")

    get_keylist = ExtractSQLite.get_keylist

    def get_nearest(self, gid, inlat, inlon, is_lsm, npoints):  # noqa ARG002
        x = MockObject()
        x.distance = 0
        x.index = 0
        return [x] * npoints

    param_match = ExtractSQLite.param_match
    get_date_info = ExtractSQLite.get_date_info
    sqlite_name = ExtractSQLite.sqlite_name
    db_create = ExtractSQLite.db_create
    db_cleanup = ExtractSQLite.db_cleanup
    fctable_definition = ExtractSQLite.fctable_definition
    interp_from_weights = ExtractSQLite.interp_from_weights
    model_name = "DEODE"

    def date_from_gribinfo(*args, **kwargs):  # noqa ARG002
        fcdate = datetime.datetime.strptime("20230915T00", "%Y%m%dT%H")
        leadtime = 1
        return fcdate, leadtime

    param1 = {
        "harp_param": "T",
        "method": "bilin",
        "grib_id": {
            "shortName": "t",
            "productDefinitionTemplateNumber": "8",
            "typeOfLevel": "isobaricInhPa",
            "level": 50,
        },
        "level_name": "p",
    }
    parameter_list = [
        {
            "harp_param": "T",
            "method": "bilin",
            "grib_id": {
                "shortName": "ok",
                "productDefinitionTemplateNumber": "ok",
                "typeOfLevel": "ok",
                "level": [0, 20],
            },
            "level_name": "p",
        }
    ]
    station_list = pandas.DataFrame({"lat": [0], "lon": [0], "SID": ["OK"]})
    weights = {"nearest": [[[0, 0.0] * 4]], "bilin": [[[0, 0.0] * 4]]}

    def test_date(self):
        ginfo = {
            "productDefinitionTemplateNumber": 8,
            "indicatorOfUnitOfTimeRange": 13,
            "dataDate": 20230915,
            "dataTime": 0,
            "forecastTime": 7200,
            "indicatorOfUnitForTimeRange": 13,
            "lengthOfTimeRange": 3600,
        }

        fcdate, leadtime = ExtractSQLite.date_from_gribinfo(None, ginfo)
        assert leadtime == 10800.0

    def test_fctable(self):
        primary_keys, all_keys = ExtractSQLite.fctable_definition(
            self, self.param1, model="DEOD"
        )
        sqlite_name = ExtractSQLite.sqlite_name(self, self.param1, self.fcdate)
        assert primary_keys["SID"] == "INT"
        assert all_keys["DEOD"] == "DOUBLE"
        assert sqlite_name == "FC_T_202309_00.sqlite"

    def test_param(self):
        assert ExtractSQLite.param_match(self, "mock_handle")["grib_id"]["level"] == "0"

    def test_restrict(self):
        x = ExtractSQLite.points_restrict(self, "mock_handle", self.station_list)
        assert x is not None

    def test_interp(self):
        x = ExtractSQLite.interp_from_weights(self, "mock_handle", "nearest")
        assert x[0] == 0

    def test_train(self):
        x = ExtractSQLite.train_weights(self, "mock_handle", lsm=False)
        assert x["bilin"][0][0][0] == 0

    def test_parse(self, tmp_sqlite_path):
        self.sqlite_path = str(tmp_sqlite_path)
        x = ExtractSQLite.parse_file(self, itertools.count(1))
        assert x == 2
