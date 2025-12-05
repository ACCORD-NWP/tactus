#!/usr/bin/env python3
"""Test eventual aspects from logger that are not touched in other parts of the code."""
from deode import GeneralConstants
from deode.logs import InterceptHandler, LoggerHandlers, builtin_logging, logger


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


def test_logger_intercept(tmpdir_factory):
    """Test to log from builtin logger and capture it with loguru logger."""
    logger_handlers = LoggerHandlers()
    logdir = tmpdir_factory.mktemp("logs")
    logger_handlers.add(
        name="logfile",
        sink=logdir / f"{GeneralConstants.PACKAGE_NAME}_intercept.log",
        level="debug",
    )
    logger.configure(handlers=logger_handlers)

    builtin_logging.basicConfig(handlers=[InterceptHandler()], level=0, force=True)
    builtin_logging.info("foo:%s", "bar")

    with open(f"{logdir}/{GeneralConstants.PACKAGE_NAME}_intercept.log") as logfile:
        assert "foo:bar" in logfile.read()
