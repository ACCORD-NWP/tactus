#!/usr/bin/env python3
"""Program's entry point."""
import time

import humanize

from . import PACKAGE_NAME, __version__
from .argparse_wrapper import get_parsed_args
from .config_parser import ParsedConfig
from .logs import get_logger


def main(argv=None):
    """Program's main routine."""
    t_start = time.time()
    args = get_parsed_args(program_name=PACKAGE_NAME, argv=argv)
    logger = get_logger(PACKAGE_NAME, args.loglevel)
    logger.info("Initialising %s v%s", PACKAGE_NAME, __version__)

    config = ParsedConfig.from_file(args.config_file)
    args.run_command(args=args, config=config)

    elapsed = time.time() - t_start
    if elapsed >= 60:
        logger.info(
            "Leaving %s. Total runtime: %.2fs (~%s).",
            PACKAGE_NAME,
            elapsed,
            humanize.precisedelta(elapsed),
        )
    else:
        logger.info("Leaving %s. Total runtime: %.2fs.", PACKAGE_NAME, elapsed)
