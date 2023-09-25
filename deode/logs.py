#!/usr/bin/env python3
"""Logging-related classes, functions and definitions."""
import os
import pprint
import sys
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from loguru import logger

from . import PACKAGE_NAME

GLOBAL_LOGLEVEL = os.environ.get("DEODE_LOGLEVEL", os.environ.get("LOGURU_LEVEL", "INFO"))
DEFAULT_LOGDIR = Path().home() / ".logs" / PACKAGE_NAME
DEFAULT_LOGFILE_RETENTION_TIME = "1 week"
DEFAULT_LOG_SINKS = {
    "console": sys.stderr,
}


@dataclass
class LogFormatter:
    """Helper class to setup logging without poluting the module's main scope."""

    datetime: str = "<green>{time:YYYY-MM-DD HH:mm:ss}</green>"
    level: str = "<level>{level: <8}</level>"
    code_location: str = (
        "<cyan>@{name}</cyan>:<cyan>{function}</cyan> "
        + "<cyan><{file.path}</cyan>:<cyan>{line}>:</cyan>"
    )
    message: str = "<level>{message}</level>"

    def format_string(self, loglevel: str):
        """Return the appropriate fmt string according to log level and fmt opts."""
        rtn = f"{self.datetime} | {self.level} | "

        loglevel = logger.level(loglevel.upper())
        if loglevel.no < 20:  # More detail than just "INFO"
            rtn = f"{self.code_location}\n{rtn}"

        rtn += f"{self.message}"
        return rtn


class LoggerHandlers(Sequence):
    """Helper class to configure logger handlers when using `loguru.logger.configure`."""

    def __init__(self, default_level: str = GLOBAL_LOGLEVEL, **sinks):
        """Initialise instance with default loglevel and sinks."""
        self.default_level = default_level.upper()
        self.handlers = {}
        for name, sink in {**DEFAULT_LOG_SINKS.copy(), **sinks}.items():
            self.add(name=name, sink=sink)

    def add(self, name, sink, **configs):
        """Add handler to instance."""
        configs["level"] = configs.pop("level", self.default_level).upper()
        configs["format"] = configs.pop(
            "format", LogFormatter().format_string(configs["level"])
        )

        try:
            configs["sink"] = Path(sink)
            configs["retention"] = configs.get(
                "retention", DEFAULT_LOGFILE_RETENTION_TIME
            )
        except TypeError:
            configs["sink"] = sink

        self.handlers[name] = configs

    def __repr__(self):
        return pprint.pformat(self.handlers)

    # Implement abstract methods
    def __getitem__(self, item):
        return tuple(self.handlers.values())[item]

    def __len__(self):
        return len(self.handlers)


logger.configure(handlers=LoggerHandlers())
# Disable logger by defalt in case the project is used as a library. Leave it for the user
# to enable it if they so wish.
logger.disable(PACKAGE_NAME)
