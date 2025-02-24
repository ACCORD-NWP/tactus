import pytest

from deode.config_parser import ParsedConfig
from deode.experiment import EPSExp


@pytest.fixture(name="eps_config")
def fixture_eps_config(default_config: ParsedConfig):
    """Fixture that adds EPS-specific configuration to the parsed configuration."""
    return default_config.copy(
        update={
            "eps": {
                "general": {
                    "control_member": 0,
                    "members": "0:3,5,7",
                    "run_continously": False,
                },
                "member_settings": {
                    "boundaries": {
                        "ifs": {
                            "selection": "IFSENS",
                        }
                    },
                    "general": {
                        "csc": ["AROME", "ALARO", "HARMONIE-AROME"],
                        "realization": "-1",
                        "output_settings": {
                            "fullpos": {"1": "PT15M", "5:": "PT3H"},
                        },
                    },
                    "namelist_update": {
                        "master": {
                            "forecast": {
                                "namspp": {
                                    "lspp": [True, False],
                                }
                            }
                        }
                    },
                },
            }
        }
    )


class TestEPSExp:
    """Integration tests for the EPSExp class."""

    def test_instanciation(self, default_config: ParsedConfig):
        """Test instanciation of the EPSExp class."""
        EPSExp(default_config)

    def test_setup_exp(self, eps_config: ParsedConfig):
        """Test the setup_exp method of the EPSExp class."""
        # Setup the eps experiment
        eps_exp = EPSExp(eps_config)
        eps_exp.setup_exp()

        # Assert that member string is expanded to a list of integers
        expected_members = (0, 1, 2, 5, 7)
        assert eps_exp.config["eps.general.members"] == expected_members

        # Assert that the fullpos setting of deviating members are correctly set
        fullpos_deviating_members = (1, 5, 7)
        for member in fullpos_deviating_members:
            assert "fullpos" in eps_exp.config[f"eps.mbr{member}.general.output_settings"]

        no_fullpos_members = set(expected_members) - set(fullpos_deviating_members)
        for member in no_fullpos_members:
            assert "output_settings" not in eps_exp.config[f"eps.mbr{member}.general"]

        # Assert that the namelist update of deviating members are correctly set
        for member in expected_members:
            assert (
                "lspp"
                in eps_exp.config[
                    f"eps.mbr{member}.namelist_update.master.forecast.namspp"
                ]
            )

        # Assert that the csc update of deviating members are correctly set
        for member in expected_members:
            assert "csc" in eps_exp.config[f"eps.mbr{member}.general"]
