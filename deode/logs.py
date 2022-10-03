#!/usr/bin/env python3
"""Logging-related classes, functions and definitions."""
import logging
from collections import namedtuple

# Define aliases to ANSI escape sequences to set text color in log
LogColor = namedtuple(
    typename="LogColor",
    field_names=("reset", "yellow", "red", "green", "cyan"),
)

logcolor = LogColor(
    reset="\u001b[0m",
    yellow="\u001b[33m",
    red="\u001b[31m",
    green="\u001b[32m",
    cyan="\u001b[36m",
)


class CustomFormatter(logging.Formatter):
    """Logging Formatter to add colors and count warning / errors."""

    # Adapted from: <https://stackoverflow.com/q/14844970/3644857>

    FORMATS = {
        "DEFAULT": "%(asctime)s: %(message)s",
        logging.CRITICAL: (
            logcolor.red
            + "%(asctime)s %(levelname)s(%(module)s: %(lineno)d): %(message)s"
            + logcolor.reset
        ),
        logging.ERROR: logcolor.red
        + "%(asctime)s %(levelname)s: "
        + logcolor.reset
        + "%(message)s",
        logging.WARNING: logcolor.yellow
        + "%(asctime)s %(levelname)s: "
        + logcolor.reset
        + "%(message)s",
        logging.DEBUG: (
            logcolor.green
            + "%(asctime)s %(levelname)s(%(module)s: %(lineno)d): "
            + logcolor.reset
            + "%(message)s"
        ),
    }

    def format(self, record):  # noqa: A003 (class attribute shadowing builtin)
        """Return a formatter.format for "record"."""
        log_fmt = self.FORMATS.get(record.levelno, self.FORMATS["DEFAULT"])
        formatter = logging.Formatter(log_fmt, datefmt="%Y-%m-%d %H:%M:%S")
        return formatter.format(record)


def get_logger(name, loglevel="INFO"):
    """Get logger with name "name" and loglevel "loglevel"."""
    logger = logging.getLogger(name)
    logger_handler = logging.StreamHandler()
    logger_handler.setFormatter(CustomFormatter())
    logging.basicConfig(
        level=logging.getLevelName(loglevel.upper()),
        handlers=[logger_handler],
    )
    return logger
