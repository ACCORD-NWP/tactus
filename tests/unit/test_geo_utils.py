"""Unit tests for geo_utils."""
import pytest

from tactus.geo_utils import Projection, Projstring


class TestProjstring:
    """Test the Projstring class."""

    def test_get_projstring_return_type(self):
        """Test that the return type of get_projstring is a string."""
        projstring = Projstring()
        assert isinstance(projstring.get_projstring(), str)

    def test_get_projstring_return_value(self):
        """Test that the return value of get_projstring is a valid proj4 string."""
        projstring = Projstring()
        assert (
            projstring.get_projstring()
            == "+proj=stere +lat_0=-90.0 +lon_0=0.0 +lat_ts=-90.0"
        )
        assert (
            projstring.get_projstring(lon0=10.0, lat0=90.0)
            == "+proj=lcc +lat_0=90.0 +lon_0=10.0 +lat_1=90.0 +lat_2=90.0 +units=m +no_defs +R=6371000.0"
        )

    def test_projstring_init(self):
        """Test that the Projstring class can be initialized."""
        projstring = Projstring()
        assert projstring.earth_radius == 6371000.0


class TestProjection:
    """Test the Projection class."""

    def test_projection_init(self):
        """Test that the Projection class can be initialized."""
        projstring = Projstring()
        projection = Projection(projstring.get_projstring())
        assert projection.proj4str == "+proj=stere +lat_0=-90.0 +lon_0=0.0 +lat_ts=-90.0"

    def test_check_key(self):
        """Test that check_key returns True if the key is in the config."""
        projstring = Projstring()
        projection = Projection(projstring.get_projstring())
        config = {"key": "value"}
        assert projection.check_key("key", config)

    def test_check_key_raises_error(self):
        """Test that check_key raises an error if the key is not in the config."""
        projstring = Projstring()
        projection = Projection(projstring.get_projstring())
        config = {"key": "value"}
        with pytest.raises(
            ValueError, match="not_key not in dictionary. Check config file"
        ):
            projection.check_key("not_key", config)

    def test_domain_properties(self):
        """Test that domain properties are returned correctly."""
        domain_spec = {
            "lonc": 0.0,
            "latc": 50.0,
            "nlon": 200,
            "nlat": 200,
            "gsize": 1000.0,
        }
        projstring = Projstring()
        projection = Projection(projstring.get_projstring())
        domain_properties = projection.get_domain_properties(domain_spec)

        assert domain_properties["minlon"] == -2.0
        assert domain_properties["maxlon"] == 2.0
        assert domain_properties["minlat"] == 48.0
        assert domain_properties["maxlat"] == 52.0
