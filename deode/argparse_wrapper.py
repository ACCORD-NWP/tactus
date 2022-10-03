#!/usr/bin/env python3
"""Wrappers for argparse functionality."""
import argparse
import sys
from pathlib import Path

from . import __version__
from .commands_functions import show_config, run_forecast
from .config_parser import get_default_config_path


def get_parsed_args(program_name="program", argv=None):
    """Get parsed command line arguments.

    Args:
        program_name (str): The name of the program.
        argv (list): A list of passed command line args.

    Returns:
        argparse.Namespace: Parsed command line arguments.

    """
    if argv is None:
        argv = sys.argv[1:]

    ##########################################
    # Define main parser and general options #
    ##########################################
    parser = argparse.ArgumentParser(
        prog=program_name, formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s v" + __version__
    )
    parser.add_argument(
        "-config_file",
        metavar="CONFIG_FILE_PATH",
        default=get_default_config_path(),
        type=Path,
        help=(
            "Path to the config file. The default is whichever of the "
            + "following is first encountered: "
            + "(i) The value of the 'DEODE_CONFIG_PATH' envvar or "
            + "(ii) './config.toml'. If both (i) and (ii) are missing, "
            + "then the default will become "
            + "'"
            + str(Path("$HOME/.deode/config.toml"))
            + "'"
        ),
    )
    parser.add_argument(
        "-loglevel",
        default="info",
        choices=["critical", "error", "warning", "info", "debug", "notset"],
        help="What type of info should be printed to the log",
    )

    # Configure the main parser to handle the commands
    subparsers = parser.add_subparsers(
        title="commands",
        required=True,
        dest="command",
        description=(
            "Valid commands for {0} (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ).format(program_name),
        help="command description",
    )

    ###############################################
    # Configure parser for the "forecast" command #
    ###############################################
    # Configure the main parser to handle the commands
    parser_forecast = subparsers.add_parser(
        "forecast",
        help="Runs the forecast task.",
    )
    parser_forecast.add_argument(
        "--an-option", "-opt", help="An option", default="Some default"
    )
    parser_forecast.set_defaults(run_command=run_forecast)

    ###########################################
    # Configure parser for the "show" command #
    ###########################################
    # Configure the main parser to handle the commands
    parser_show = subparsers.add_parser(
        "show",
        help="Display results from output files, as well as configs",
    )
    show_command_subparsers = parser_show.add_subparsers(
        title="show",
        dest="show_what",
        required=True,
        description=(
            "Valid commands below (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ),
        help="command description",
    )

    # show config
    parser_show_config = show_command_subparsers.add_parser(
        "config", help="Print configs in use and exit"
    )
    parser_show_config.add_argument(
        "section", help="The config section (optional)", default="", nargs="?"
    )
    parser_show_config.add_argument(
        "--format", "-fmt", help="Output format", choices=["toml", "json"], default="toml"
    )
    parser_show_config.add_argument(
        "--no-defaults",
        "--nodefs",
        action="store_true",
        help="Don't show defaults: Show only what is written in the config file.",
    )
    parser_show_config.set_defaults(run_command=show_config)

    return parser.parse_args(argv)
