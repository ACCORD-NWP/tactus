#!/usr/bin/env python3
"""Program's entry point."""
import contextlib
import time

import humanize

from . import PACKAGE_NAME, __version__
from .argparse_wrapper import get_parsed_args
from .config_parser import MAIN_CONFIG_JSON_SCHEMA, ParsedConfig
from .logs import LoggerHandlers, logger

# Enable logger, with our own configs, if the project is being used as an application.
logger.enable(PACKAGE_NAME)


def main(argv=None):
    """Program's main routine."""
    t_start = time.time()
    logger.info("Initialising {} v{}", PACKAGE_NAME, __version__)
    args = get_parsed_args(program_name=PACKAGE_NAME, argv=argv)
    config = ParsedConfig.from_file(args.config_file, json_schema=MAIN_CONFIG_JSON_SCHEMA)
    with contextlib.suppress(KeyError):
        # Reset default loglevel if specified in the config
        logger.configure(
            handlers=LoggerHandlers(default_level=config["general.loglevel"])
        )
    args.run_command(args=args, config=config)

    elapsed = time.time() - t_start
    if elapsed >= 60:
        logger.info(
            "Leaving {}. Total runtime: {}s (~{}).",
            PACKAGE_NAME,
            elapsed,
            humanize.precisedelta(elapsed),
        )
    else:
        logger.info("Leaving {}. Total runtime: {:.2f}s.", PACKAGE_NAME, elapsed)


if __name__ == "__main__":
    main()
