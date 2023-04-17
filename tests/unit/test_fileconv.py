"""Unit tests for fileconv."""
from deode.datetime_utils import as_timedelta
from deode.tasks.creategrib import Fileconv


class TestFileconv:
    """Test the Fileconv class."""

    def test_find_files(self, tmp_path_factory):
        """Test that the return type of get_projstring is a string."""
        test_dir = f"{tmp_path_factory.getbasetemp().as_posix()}"
        test_file = f"{test_dir}/ICMSHHARM+0025:30:30.sfx"
        with open(test_file, "w", encoding="utf8"):
            filetest = Fileconv("2023-02-19T00:00:00Z", src_dir=test_dir)
            print(filetest.files)
        assert list(filetest.files.keys())[0] == as_timedelta("PT1D1H30M30S")

    def test_list_files(self):
        """Test that the return type of get_projstring is a string."""
        filetest = Fileconv(
            "2023-02-19T00:00:00Z", forecast_range="PT2D", output_interval="PT24H"
        )
        assert len(filetest.files) == 3
