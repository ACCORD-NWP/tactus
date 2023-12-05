"""unit tests for extractsqlite"""
import datetime
import itertools
import os
import sys

import pandas
import pytest

import deode.sqlite_utils as sqlite
from deode.tasks.extractsqlite import ExtractSQLite


class MockObject(object):
    pass


@pytest.fixture(scope="session")
def tmp_sqlite_path(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


class TestExtractSQLite:
    """Test extractsqlite in parts"""

    sqlite_template = "FC_@PP@_@YYYY@@MM@_@HH@.sqlite"
    fcdate = datetime.datetime.strptime("20230915T00", "%Y%m%dT%H")

    model_name = "DEODE"
    weights = None
    basetime = datetime.datetime.strptime("20230915T00", "%Y%m%dT%H")
    infile_template = "mock_gribfile"
    forecast_range = "PT1H"
    infile_dt = "PT1H"

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
                "shortName": "t",
                "productDefinitionTemplateNumber": "8",
                "typeOfLevel": "isobaricInhPa",
                "level": [0, 50],
            },
            "level_name": "p",
        },
        {
            "harp_param": "S",
            "method": "bilin",
            "grib_id": [{"shortName": "u"}, {"shortName": "v"}],
            "common": {
                "productDefinitionTemplateNumber": "8",
                "typeOfLevel": "isobaricInhPa",
                "level": 0,
            },
        },
    ]
    station_list = pandas.DataFrame({"lat": [0], "lon": [0], "SID": ["OK"]})
    weights = {"nearest": [[[[0, 0], 0.0] * 4]], "bilin": [[[[0, 0], 0.0] * 4]]}

    mockgrib = {
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
        "forecastTime": 7200,
        "lengthOfTimeRange": 0,
        "gridType": "lambert",
        "Latin1InDegrees": 50.0,
        "Latin2InDegrees": 50.0,
        "LoVInDegrees": 0.0,
        "DxInMetres": 1,
        "DyInMetres": 1,
    }
    p4 = {"R": 6371229, "proj": "lcc", "lon_0": 0, "lat_1": 50, "lat_2": 50}

    def test_date(self):
        fcdate, leadtime = sqlite.get_date_info(self.mockgrib)
        assert leadtime == 7200

    def test_restrict(self):
        x = sqlite.points_restrict(self.mockgrib, self.station_list)
        assert x is not None

    def test_interp(self):
        x = sqlite.interp_from_weights(self.mockgrib, self.weights, "bilin")
        assert x[0] == 0

    def test_train(self):
        x = sqlite.train_weights(self.station_list, self.mockgrib, lsm=False)
        assert x["bilin"][0][0][0][0] == 0.0

    def test_parse(self, tmp_sqlite_path):
        # create a mockgrib file with 2 bytes
        infile = tmp_sqlite_path + "/mock_gribfile"
        inf = open(infile, "w")
        inf.write("12")
        inf.close()

        nt, ni = sqlite.parse_grib_file(
            infile=infile,
            param_list=self.parameter_list,
            station_list=self.station_list,
            sqlite_template=tmp_sqlite_path + "/" + self.sqlite_template,
            model_name="TEST",
            weights=self.weights,
        )
        assert nt == 2
        assert ni == 2
        # check that sqlite file was created
        assert os.path.isfile(tmp_sqlite_path + "/FC_T_202309_00.sqlite")
