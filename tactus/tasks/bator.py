"""Bator task — ODB subbase creation via the BATOR binary."""

from .obs_ingestion import OdbIngestionTask


class Bator(OdbIngestionTask):
    """Run BATOR for one observation type to produce an ECMA ODB subbase."""

    _BINARY_NAME = "bator.x"
    _PARAM_CFG_NAME = "param_bator.cfg"
    _LOG_TAG = "Bator"
    _NLGEN_KEY = "bator"
