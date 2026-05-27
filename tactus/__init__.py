#!/usr/bin/env python3
"""Package to run the Destination Earth on Demand Extremes system."""

from importlib.metadata import version
from pathlib import Path

from .aux_types import QuasiConstant
import os
import sys

class GeneralConstants(QuasiConstant):
    """General package-related constants."""

    PACKAGE_NAME = __name__
    VERSION = version(__name__)
    PACKAGE_DIRECTORY = Path(__file__).parent
    PACKAGE_FILE = os.path.basename(sys.argv[0] if sys.argv else __file__)
    

