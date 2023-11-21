#!/usr/bin/env python3
"""Program's entry point."""
import contextlib

from . import GeneralConstants
from .argparse_wrapper import get_parsed_args
from .config_parser import ConfigParserDefaults, ParsedConfig
from .logs import LoggerHandlers, log_elapsed_time, logger

# Enable logger if the project is being used as an application
logger.enable(GeneralConstants.PACKAGE_NAME)


@log_elapsed_time()
def main(argv=None):
    """Program's main routine."""
    args = get_parsed_args(argv=argv)
    config = ParsedConfig.from_file(
        args.config_file, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )
    with contextlib.suppress(KeyError):
        # Reset default loglevel if specified in the config
        logger.configure(
            handlers=LoggerHandlers(default_level=config["general.loglevel"])
        )

    args.run_command(args=args, config=config)


if __name__ == "__main__":
    main()
