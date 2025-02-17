#!/usr/bin/env python3
"""Unit tests for the fullpos."""

import pytest
import tomlkit

from deode import GeneralConstants
from deode.derived_variables import set_times
from deode.fullpos import Fullpos, InvalidSelectionCombinationError, flatten_list
from deode.toolbox import Platform


@pytest.fixture()
def load(default_config):
    """Test load of the yml files."""
    config = default_config
    config_patch = tomlkit.parse(
        f"""
        [platform]
            deode_home = "{GeneralConstants.PACKAGE_DIRECTORY}"
        """
    )
    config = config.copy(update=config_patch)
    config = config.copy(update=set_times(config))
    platform = Platform(config)

    fpdir = platform.substitute(config["fullpos.config_path"])
    _fpfiles = ["master_selection_AROME"]
    _fpfiles.append(list(config["fullpos.main"]))
    fpfiles = flatten_list(_fpfiles)

    nrfp3s = list(range(1, int(config["vertical_levels.nlev"]) + 1))
    rules = {
        "${vertical_levels.nlev}": config["vertical_levels.nlev"],
        "${namelist.nrfp3s}": nrfp3s,
    }
    fullpos = Fullpos("test", fpdir=fpdir, fpfiles=fpfiles, rules=rules)

    return fullpos


class TestFullpos:
    """Test Fullpos."""

    def test_merge_dict(self):
        """Test merge of two dictionaries."""
        d1 = {0: [0, 1, 2], 1: {0: [0]}, 2: 0}
        d2 = {0: [3, 4], 1: {0: [1], 1: 0}, 3: [0, 1]}
        dict_ref = {0: [0, 1, 2, 3, 4], 1: {0: [0, 1], 1: 0}, 2: 0, 3: [0, 1]}
        dict_merged = Fullpos("test").merge_dict(d1, d2)
        assert dict_ref == dict_merged

    def test_invalid_merge_dict(self):
        """Test merge of two dictionaries."""
        d1 = {0: {0: 0}}
        d2 = {0: "x"}
        with pytest.raises(RuntimeError):
            Fullpos("test").merge_dict(d1, d2)

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
                    "NAMFPDYS": {
                        "TEST": {
                            "CL3DF": ["TEMPERATURE", "FOO"],
                            "NRFP3S": ["test_compare"],
                        },
                        "TEST2": {"CL3DF": ["TEMPERATURE"], "NRFP3S": [0]},
                    },
                    "NAMFPDYP": {"CL3DF": ["ABS_VORTICITY"], "RFP3P": [80000.0]},
                },
                "xxtddddhh00": {
                    "NAMFPDYS": {"CL3DF": ["TEMPERATURE"], "NRFP3S": ["test_compare"]},
                },
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
                "NRFP3S": [0, 65],
                "CFP3DF": ["ABS_VORTICITY", "FOO", "TEMPERATURE"],
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
                "CL3DF(2)": "FOO",
                "IL3DF(1,1)": 1,
                "IL3DF(2,1)": 2,
                "IL3DF(1,2)": 2,
                "CLD3DF(1,1)": "test",
                "CLD3DF(2,1)": "test",
                "CLD3DF(1,2)": "test",
            },
            "NAMFPDYP": {
                "CL3DF(1)": "ABS_VORTICITY",
                "IL3DF(1,1)": 1,
                "CLD3DF(1,1)": "test",
            },
            "NAMFPDYF": {},
            "NAMFPDYH": {},
            "NAMFPDYI": {},
            "NAMFPDYT": {},
            "NAMFPDY2": {},
            "NAMFPDYV": {},
        }

        ref_xxtddddhh00 = {
            "NAMFPDYS": {
                "CL3DF(1)": "TEMPERATURE",
                "IL3DF(1,1)": 2,
                "CLD3DF(1,1)": "test",
            },
            "NAMFPPHY": {},
            "NAMFPDYP": {},
            "NAMFPDYF": {},
            "NAMFPDYH": {},
            "NAMFPDYI": {},
            "NAMFPDYT": {},
            "NAMFPDY2": {},
            "NAMFPDYV": {},
        }

        rules = {"test_compare": 65}
        fullpos = Fullpos("test", fpdict=fullpos_config, rules=rules)
        namfpc, selection = fullpos.construct()
        assert selection["xxtddddhhmm"] == ref_xxtddddhhmm
        assert selection["xxtddddhh00"] == ref_xxtddddhh00
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

    def test_invalid_substitution(self):
        fullpos_config = {
            "selection": {
                "xxtddddhhmm": {
                    "NAMFPDYS": {
                        "TEST": {"CL3DF": ["TEMPERATURE"], "NRFP3S": "test_compare"}
                    },
                }
            },
            "LEVEL_MAP": {
                "NAMFPDYS": "NRFP3S",
            },
            "PARAM_MAP": {
                "NAMFPDYS": {"CL3DF": "CFP3DF"},
            },
            "NAMFPC": {},
        }

        rules = {"test_compare": 65}
        with pytest.raises(RuntimeError):
            namfpc, selection = Fullpos(
                "test", fpdict=fullpos_config, rules=rules
            ).construct()

    def test_load(self, load):
        """Test load of the yml files."""
        fp = load
        assert isinstance(fp.nldict, dict)

    def test_update(self, load):
        """Test update of the settings."""
        fp = load
        fp.update_selection(additions_list=["master_selection_AROME"], additions_dict={})

    def test_non_instant(self, load):
        """Test the check of non instant fields."""
        fp = load
        namfpc, selection = fp.construct()
        with pytest.raises(RuntimeError):
            fp.check_non_instant_fields(selection, "xxtddddhh00")


if __name__ == "__main__":
    pytest.main()
