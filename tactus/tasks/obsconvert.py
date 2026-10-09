"""Obsconvert task — ODB subbase creation via the OBSCONVERT binary."""

from .obs_ingestion import OdbIngestionTask


class Obsconvert(OdbIngestionTask):
    """Run OBSCONVERT for one observation type to produce an ECMA ODB subbase."""

    _BINARY_NAME = "obsconvert.x"
    _PARAM_CFG_NAME = "param_obsconvert.cfg"
    _LOG_TAG = "Obsconvert"
    _NLGEN_KEY = "obsconvert"
