from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
import tomlkit

from tactus.config_parser import ConfigPaths, ParsedConfig
from tactus.experiment import EPSExp


@pytest.fixture(name="eps_config")
def fixture_eps_config(default_config: ParsedConfig):
    """Fixture that adds EPS-specific configuration to the parsed configuration."""
    return default_config.copy(
        update={
            "eps": {
                "general": {
                    "members": "0:3,5,7",
                },
                "member_settings": {
                    "boundaries": {
                        "ifs": {
                            "selection": "IFSENS",
                        }
                    },
                    "general": {
                        "csc": ["AROME", "ALARO", "HARMONIE-AROME"],
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


@pytest.fixture(name="module_scope_tmp_path", scope="module")
def fixture_module_scope_tmp_path(tmp_path_factory):
    """Fixture that provides a temporary path with module scope."""
    # Create a temporary directory with module scope
    tmp_path = tmp_path_factory.mktemp("module_scope_tmp")
    return tmp_path


@pytest.fixture(name="modifications")
def fixture_modifications(module_scope_tmp_path: Path):
    """Fixture that creates modification files in the temporary directory."""
    mods = [
        "modifications/mod0.toml",
        "modifications/mod1.toml",
        "modifications/mod2.toml",
        "modifications/mod5.toml",
        "modifications/mod7.toml",
    ]
    for mod in mods:
        mod_path = Path(mod)
        # Add content to the modification file
        modification = {
            "general": {
                "case": f"test_case_{mod_path.stem}",
            }
        }
        (module_scope_tmp_path / mod_path.parent).mkdir(parents=True, exist_ok=True)
        with open(module_scope_tmp_path / mod, "w", encoding="UTF-8") as file_:
            tomlkit.dump(modification, file_)

    return mods


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
            assert (
                "fullpos"
                in eps_exp.config[f"eps.members.{member}.general.output_settings"]
            )

        no_fullpos_members = set(expected_members) - set(fullpos_deviating_members)
        for member in no_fullpos_members:
            assert (
                "output_settings" not in eps_exp.config[f"eps.members.{member}.general"]
            )

        # Assert that the namelist update of deviating members are correctly set
        for member in expected_members:
            assert (
                "lspp"
                in eps_exp.config[
                    f"eps.members.{member}.namelist_update.master.forecast.namspp"
                ]
            )

        # Assert that the csc update of deviating members are correctly set
        for member in expected_members:
            assert "csc" in eps_exp.config[f"eps.members.{member}.general"]

    @patch.object(ConfigPaths, "CONFIG_DATA_SEARCHPATHS")
    def test_setup_exp_with_modifications(
        self,
        mock_config_data_searchpaths: MagicMock,
        eps_config: ParsedConfig,
        modifications: list[str],
        module_scope_tmp_path: Path,
    ) -> None:
        """Test the setup_exp method of the EPSExp class with modifications."""
        # Add modifications to the configuration
        eps_config = eps_config.copy(
            update={"eps": {"member_settings": {"modifications": {"mod": modifications}}}}
        )
        # Mock the CONFIG_DATA_SEARCHPATHS to include the temporary path
        mock_config_data_searchpaths.copy.return_value = [
            str(module_scope_tmp_path),
        ]
        # Setup the eps experiment
        eps_exp = EPSExp(eps_config)
        eps_exp.setup_exp()

        # Assert that the modifications are correctly resolved
        for mod, member in zip(modifications, eps_exp.config["eps.general.members"]):
            case_name = f"test_case_{Path(mod).stem}"
            assert case_name == eps_exp.config[f"eps.members.{member}.general.case"]
