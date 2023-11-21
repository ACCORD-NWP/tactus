#!/usr/bin/env python3
"""Test eventual aspects from logger that are not touched in other parts of the code."""
from deode import GeneralConstants
from deode.logs import LoggerHandlers, logger


def test_add_logger_handlers(tmpdir_factory):
    logger_handlers = LoggerHandlers()
    logdir = tmpdir_factory.mktemp("logs")
    logger_handlers.add(
        name="logfile",
        sink=logdir / f"{GeneralConstants.PACKAGE_NAME}_{{time}}.log",
        level="debug",
    )
    logger.configure(handlers=logger_handlers)
    logger.info("foo")
    assert str(logger_handlers) == logger_handlers.__repr__()
    assert "retention" in logger_handlers.handlers["logfile"]
    assert len(logger_handlers) == 2
