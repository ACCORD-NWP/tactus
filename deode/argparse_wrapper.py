#!/usr/bin/env python3
"""Wrappers for argparse functionality."""
import argparse
import sys
from pathlib import Path

from . import PACKAGE_NAME, __version__
from .commands_functions import (
    doc_config,
    run_task,
    show_config,
    show_namelist,
    start_suite,
)
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
            + str(Path(f"$HOME/.{PACKAGE_NAME}/config.toml"))
            + "'"
        ),
    )
    parser.add_argument(
        "-loglevel",
        default="info",
        choices=["critical", "error", "warning", "info", "debug", "notset"],
        help="What type of info should be printed to the log",
    )

    parser.add_argument(
        "-deode_home",
        default=None,
        help="Specify deode_home to override automatic detection",
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
    parser_run = subparsers.add_parser(
        "run",
        help="Runs a task.",
    )
    parser_run.add_argument("--task", "-t", dest="task", help="Task name", required=True)
    parser_run.add_argument(
        "--template", dest="template_job", help="Template", required=True
    )
    parser_run.add_argument("--job", dest="task_job", help="Task job file", required=True)
    parser_run.add_argument(
        "--output", "-o", dest="output", help="Task output file", required=True
    )
    parser_run.add_argument("--troika", dest="troika", default="troika", required=False)
    parser_run.add_argument(
        "--troika_config",
        dest="troika_config",
        default="/opt/troika/etc/troika.yml",
        required=False,
    )
    parser_run.set_defaults(run_command=run_task)

    ##########################################
    # Configure parser for the "start" command #
    ##########################################
    # Configure the main parser to handle the commands
    parser_start = subparsers.add_parser(
        "start",
        help="Start various tasks and exit.",
    )
    start_command_subparsers = parser_start.add_subparsers(
        title="start",
        dest="start_what",
        required=True,
        description=(
            "Valid commands below (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ),
        help="command description",
    )

    # suite
    parser_start_suite = start_command_subparsers.add_parser(
        "suite", help="Start the suite"
    )
    parser_start_suite.add_argument(
        "--ecf_host",
        "-host",
        type=str,
        dest="ecf_host",
        help="Ecflow host",
        required=False,
        default=None,
    )
    parser_start_suite.add_argument(
        "--ecf_port",
        "-port",
        type=int,
        dest="ecf_port",
        help="Ecflow port",
        required=False,
        default=None,
    )
    parser_start_suite.add_argument(
        "--start_command",
        type=str,
        dest="start_command",
        help="Start command for server",
        required=False,
        default=None,
    )
    parser_start_suite.add_argument(
        "--joboutdir", "-j", dest="joboutdir", help="Job out directory", required=True
    )
    parser_start_suite.add_argument(
        "--ecf_files",
        "-f",
        dest="ecf_files",
        help="Ecflow container directory",
        required=True,
    )
    parser_start_suite.add_argument(
        "--begin", "-b", dest="begin", help="Begin suite", default=True, required=False
    )
    parser_start_suite.set_defaults(run_command=start_suite)

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
        "--format",
        "-fmt",
        help="Output format",
        choices=["toml", "json", "yaml"],
        default="toml",
    )
    parser_show_config.add_argument(
        "--show-metadata",
        "--metadata",
        action="store_true",
        help="Include the [metadata] section in the output.",
    )
    parser_show_config.set_defaults(run_command=show_config)

    # show namelist
    parser_show_namelist = show_command_subparsers.add_parser(
        "namelist", help="Print namelist in use and exit"
    )

    parser_show_namelist.add_argument(
        "-t",
        type=str,
        dest="namelist_type",
        help="Namelist target, master or surfex",
        choices=["master", "surfex"],
        required=True,
        default=None,
    )

    parser_show_namelist.add_argument(
        "-n",
        type=str,
        dest="namelist",
        help="Namelist to show, type anything to print available options",
        required=True,
        default=None,
    )

    parser_show_namelist.add_argument(
        "-o",
        type=str,
        dest="namelist_name",
        help="Optional namelist name",
        required=False,
        default=None,
    )

    parser_show_namelist.add_argument(
        "--no-substitute",
        "-b",
        action="store_false",
        default=True,
        help="Do not substitute config values in the written namelist",
    )

    parser_show_namelist.set_defaults(run_command=show_namelist)

    parser_doc = subparsers.add_parser(
        "doc",
        help="Print documentation style output",
    )
    doc_command_subparsers = parser_doc.add_subparsers(
        title="doc",
        dest="doc_what",
        required=True,
        description=(
            "Valid commands below (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ),
        help="command description",
    )

    # doc config
    parser_doc_config = doc_command_subparsers.add_parser(
        "config", help="Print a merge of config and json schema in .md style"
    )

    parser_doc_config.set_defaults(run_command=doc_config)

    return parser.parse_args(argv)
