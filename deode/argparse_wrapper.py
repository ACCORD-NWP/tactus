#!/usr/bin/env python3
"""Wrappers for argparse functionality."""
import argparse
import sys
from pathlib import Path

from . import __version__
from .commands_functions import (
    doc_config,
    run_task,
    show_config,
    show_config_schema,
    show_namelist,
    start_suite,
)
from .config_parser import PACKAGE_CONFIG_PATH, get_default_config_path


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

    ######################################################################################
    # Command line args that will be common to main_parser and possibly other subparsers.#
    #                                                                                    #
    # You should add `parents=[common_parser]` to your subparser definition if you want  #
    # these options to apply there too.                                                  #
    ######################################################################################
    common_parser = argparse.ArgumentParser(add_help=False)

    common_parser.add_argument(
        "-deode_home",
        default=None,
        help="Specify deode_home to override automatic detection",
    )
    common_parser.add_argument(
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
            + f"{PACKAGE_CONFIG_PATH}"
            + "'"
        ),
    )
    common_parser.add_argument(
        "-loglevel",
        default="info",
        choices=["critical", "error", "warning", "info", "debug", "notset"],
        help="What type of info should be printed to the log",
    )

    ##########################################
    # Define main parser and general options #
    ##########################################
    main_parser = argparse.ArgumentParser(
        prog=program_name, formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    main_parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s v" + __version__
    )

    # Configure the main parser to handle the commands
    subparsers = main_parser.add_subparsers(
        title="commands",
        required=True,
        dest="command",
        description=(
            f"Valid commands for {program_name} (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ),
        help="command description",
    )

    ##########################################
    # Configure parser for the "run" command #
    ##########################################
    parser_run = subparsers.add_parser(
        "run", help="Runs a task.", parents=[common_parser]
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

    ############################################
    # Configure parser for the "start" command #
    ############################################
    parser_start = subparsers.add_parser("start", help="Start various tasks and exit.")
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
        "suite", help="Start the suite", parents=[common_parser]
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
    parser_show = subparsers.add_parser(
        "show", help="Display results from output files, as well as configs"
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
        "config", help="Print configs in use and exit", parents=[common_parser]
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
    parser_show_config.set_defaults(run_command=show_config)

    # show config-schema
    parser_show_config_schema = show_command_subparsers.add_parser(
        "config-schema",
        help="Print JSON schema used for validation of configs and exit",
        parents=[common_parser],
    )
    parser_show_config_schema.add_argument(
        "section", help="The config section (optional)", default="", nargs="?"
    )
    parser_show_config_schema.set_defaults(run_command=show_config_schema)

    # show namelist
    parser_show_namelist = show_command_subparsers.add_parser(
        "namelist", help="Print namelist in use and exit", parents=[common_parser]
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
        "config",
        help="Print a merge of config and json schema in .md style",
        parents=[common_parser],
    )

    parser_doc_config.set_defaults(run_command=doc_config)

    return main_parser.parse_args(argv)
