#!/usr/bin/env python3
"""Common definitions."""
from importlib.metadata import version

try:
    __version__ = version(__name__)
except ModuleNotFoundError:
    __version__ = "?"
PACKAGE_NAME = __name__
