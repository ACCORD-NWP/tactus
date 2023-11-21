#!/usr/bin/env python3
"""Unit tests for the fullpos."""

from pathlib import Path

import pytest
import tomlkit

from deode.config_parser import BasicConfig, ConfigParserDefaults, ParsedConfig
from deode.derived_variables import set_times
from deode.fullpos import Fullpos, InvalidSelectionCombinationError
from deode.toolbox import Platform

WORKING_DIR = Path.cwd()


def load():
    """Test load of the yml files."""
    raw_config = BasicConfig.from_file(ConfigParserDefaults.PACKAGE_CONFIG_PATH)
    config = ParsedConfig(
        raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )
    config_patch = tomlkit.parse(
        f"""
        [platform]
            deode_home = "{WORKING_DIR}"
        """
    )
    config = config.copy(update=config_patch)
    config = config.copy(update=set_times(config))
    platform = Platform(config)

    fpdir = platform.substitute(config["fullpos.config_path"])

    return Fullpos(
        "test",
        fpdir=fpdir,
        fpfiles=[
            "rules",
            "namfpc_header",
            "aq_selection",
            "master_selection",
            "master_selection",
        ],
    )


class TestFullpos:
    """Test Fullpos."""

    def test_merge_dict(self):
        """Test merge of two dictionaries."""
        d1 = {0: [0, 1, 2], 1: {0: [0]}, 2: 0}
        d2 = {0: [3, 4], 1: {0: [1], 1: 0}, 3: [0, 1]}
        dict_ref = {0: [0, 1, 2, 3, 4], 1: {0: [0, 1], 1: 0}, 2: 0, 3: [0, 1]}
        dict_merged = Fullpos("test").merge_dict(d1, d2)
        assert dict_ref == dict_merged

    def test_fullpos(self):
        """Test fullpos namelist generation for master."""
        fullpos_config = {
            "selection": {
                "xxtddddhhmm": {
                    "NAMFPPHY": {
                        "CLCFU": ["SURFACCGRAUPEL"],
                        "CLXFU": ["CLSTEMPERATURE"],
                        "CLPHY": ["SURFTEMPERATURE"],
                    },
                    "NAMFPDYS": {"TEST": {"CL3DF": ["TEMPERATURE"], "NRFP3S": [65]}},
                    "NAMFPDYP": {"CL3DF": ["ABS_VORTICITY"], "RFP3P": [80000.0]},
                }
            },
            "LEVEL_MAP": {
                "NAMFPDYI": "RFP3I",
                "NAMFPDYV": "RFP3PV",
                "NAMFPDYH": "RFP3H",
                "NAMFPDYP": "RFP3P",
                "NAMFPDYS": "NRFP3S",
                "NAMFPDYT": "RFP3TH",
                "NAMFPDYF": "RFP3F",
            },
            "PARAM_MAP": {
                "NAMFPDY2": {"CL2DF": "CFP2DF"},
                "NAMFPDYV": {"CL3DF": "CFP3DF"},
                "NAMFPDYI": {"CL3DF": "CFP3DF"},
                "NAMFPDYP": {"CL3DF": "CFP3DF"},
                "NAMFPDYS": {"CL3DF": "CFP3DF"},
                "NAMFPDYF": {"CL3DF": "CFP3DF"},
                "NAMFPDYT": {"CL3DF": "CFP3DF"},
                "NAMFPDYH": {"CL3DF": "CFP3DF"},
                "NAMFPPHY": {"CLCFU": "CFPCFU", "CLXFU": "CFPXFU", "CLPHY": "CFPPHY"},
            },
            "NAMFPC": {
                "NFPGRIB": 141,
                "CFPDOM": "${domain.name}",
                "LFPCAPEX": True,
                "LFPMOIS": False,
                "LISOT_ABOVEG": True,
                "L_READ_MODEL_DATE": True,
                "NFPCAPE": 5,
                "RFPCD2": 5,
                "RFPCSAB": 50,
                "RFPVCAP": 7000,
                "RENTRA": 0.0001,
            },
        }

        ref_namfpc = {
            "NAMFPC": {
                "NFPGRIB": 141,
                "CFPDOM": "${domain.name}",
                "LFPCAPEX": True,
                "LFPMOIS": False,
                "LISOT_ABOVEG": True,
                "L_READ_MODEL_DATE": True,
                "NFPCAPE": 5,
                "RFPCD2": 5,
                "RFPCSAB": 50,
                "RFPVCAP": 7000,
                "RENTRA": 0.0001,
                "RFP3P": [80000.0],
                "NRFP3S": [65],
                "CFP3DF": ["ABS_VORTICITY", "TEMPERATURE"],
                "CFPCFU": ["SURFACCGRAUPEL"],
                "CFPXFU": ["CLSTEMPERATURE"],
                "CFPPHY": ["SURFTEMPERATURE"],
            }
        }

        ref_xxtddddhhmm = {
            "NAMFPPHY": {
                "CLCFU": ["SURFACCGRAUPEL"],
                "CLDCFU": ["test"],
                "CLXFU": ["CLSTEMPERATURE"],
                "CLDXFU": ["test"],
                "CLPHY": ["SURFTEMPERATURE"],
                "CLDPHY": ["test"],
            },
            "NAMFPDYS": {
                "CL3DF(1)": "TEMPERATURE",
                "IL3DF(1,1)": 1,
                "CLD3DF(1,1)": "test",
            },
            "NAMFPDYP": {
                "CL3DF(1)": "ABS_VORTICITY",
                "IL3DF(1,1)": 1,
                "CLD3DF(1,1)": "test",
            },
        }

        namfpc, selection = Fullpos("test", fpdict=fullpos_config).construct()
        assert selection["xxtddddhhmm"] == ref_xxtddddhhmm
        assert namfpc == ref_namfpc

    def test_exception(self):
        """Test fullpos namelist generation for master."""
        fullpos_config = {
            "selection": {
                "xxtddddhhmm": {
                    "NAMFPDYS": {"CL3DF": ["TEMPERATURE"], "NRFP3S": [65], "TEST": 0},
                },
            },
            "NAMFPC": {},
            "LEVEL_MAP": {},
            "PARAM_MAP": {},
        }

        with pytest.raises(InvalidSelectionCombinationError):
            namfpc, selection = Fullpos("test", fpdict=fullpos_config).construct()

    def test_load(self):
        """Test load of the yml files."""
        fp = load()
        assert isinstance(fp.nldict, dict)

    def test_update(self):
        """Test update of the settings."""
        fp = load()
        fp.update_selection(additions_list=["master_selection"], additions_dict={})


if __name__ == "__main__":
    pytest.main()
