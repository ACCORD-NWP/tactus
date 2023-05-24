#!/usr/bin/env python3
"""Unit tests for the fullpos."""

import pytest

from deode.fullpos import Fullpos


class TestFullpos:
    # pylint: disable=no-self-use
    """Test Fullpos."""

    def test_fullpos(self):
        """Test fullpos namelist generation for master."""
        ref_xxtddddhhmm = {
            "NAMFPPHY": {
                "CLCFU": ["SURFACCGRAUPEL"],
                "CLDCFU": ["test"],
                "CLXFU": ["CLSTEMPERATURE"],
                "CLDXFU": ["test"],
                "CLPHY": ["SURFTEMPERATURE"],
                "CLDPHY": ["test"],
            },
            "NAMFPDY2": {"CL2DF": ["SURFPRESSION"], "CLD2DF": ["test"]},
            "NAMFPDYS": {
                "CL3DF(1)": "TEMPERATURE",
                "IL3DF(1,1)": 1,
                "CLD3DF(1,1)": "test",
            },
        }

        ref_namfpc = {
            "NAMFPC": {
                "NFPGRIB": 141,
                "NRFP3S": [65],
                "CFP2DF": ["SURFPRESSION"],
                "CFP3DF": ["TEMPERATURE"],
                "CFPCFU": ["SURFACCGRAUPEL"],
                "CFPXFU": ["CLSTEMPERATURE"],
                "CFPPHY": ["SURFTEMPERATURE"],
            }
        }

        fullpos_config = {
            "LEVEL_MAP": {"NAMFPDYS": "NRFP3S"},
            "PARAM_MAP": {
                "NAMFPDY2": {"CL2DF": "CFP2DF"},
                "NAMFPDYS": {"CL3DF": "CFP3DF"},
                "NAMFPPHY": {"CLCFU": "CFPCFU", "CLXFU": "CFPXFU", "CLPHY": "CFPPHY"},
            },
            "NAMFPC": {
                "NFPGRIB": 141,
            },
            "selection": {
                "xxtddddhhmm": {
                    "NAMFPPHY": {
                        "CLCFU": ["SURFACCGRAUPEL"],
                        "CLXFU": ["CLSTEMPERATURE"],
                        "CLPHY": ["SURFTEMPERATURE"],
                    },
                    "NAMFPDY2": {"CL2DF": ["SURFPRESSION"]},
                    "NAMFPDYS": {"CL3DF": ["TEMPERATURE"], "NRFP3S": [65]},
                },
            },
        }
        namfpc, selection = Fullpos("test", fullpos_config=fullpos_config).construct()
        assert selection["xxtddddhhmm"] == ref_xxtddddhhmm
        assert namfpc == ref_namfpc


if __name__ == "__main__":
    pytest.main()
