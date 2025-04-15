#!/usr/bin/env python3
"""Wrappers for argparse functionality."""
import argparse
import sys
from pathlib import Path

from . import GeneralConstants
from .commands_functions import (
    create_exp,
    doc_config,
    namelist_convert,
    namelist_format,
    namelist_integrate,
    run_task,
    show_config,
    show_config_schema,
    show_host,
    show_namelist,
    show_paths,
    start_suite,
)
from .config_parser import ConfigParserDefaults
from .namelist import NamelistConverter


def get_parsed_args(program_name=GeneralConstants.PACKAGE_NAME, argv=None):
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
        "--deode-home",
        default=None,
        help="Specify deode_home to override automatic detection",
    )
    common_parser.add_argument(
        "--config-file",
        "-c",
        metavar="CONFIG_FILE_PATH",
        default=ConfigParserDefaults.CONFIG_PATH,
        type=Path,
        help=(
            "Path to the config file. The default is whichever of the "
            + "following is first encountered: "
            + "(i) The value of the 'DEODE_CONFIG_PATH' envvar or "
            + "(ii) './config.toml'. If both (i) and (ii) are missing, "
            + "then the default will become "
            + "'"
            + f"{ConfigParserDefaults.PACKAGE_CONFIG_PATH}"
            + "'"
        ),
    )
    common_parser.add_argument(
        "--host-file",
        dest="host_file",
        help="Config file for host recognition rules",
        required=False,
        default=None,
    )
    common_parser.add_argument(
        "--config-data-dir",
        nargs="+",
        type=str,
        help="Search path(s) for config directory.",
        required=False,
        default=None,
    )

    ##########################################
    # Define main parser and general options #
    ##########################################
    main_parser = argparse.ArgumentParser(
        prog=program_name,
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        fromfile_prefix_chars="?",
    )

    main_parser.add_argument(
        "--version",
        "-v",
        action="version",
        version="%(prog)s v" + GeneralConstants.VERSION,
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
    parser_run.add_argument("--task", "-t", help="Task name", required=True)
    parser_run.add_argument(
        "--template-job",
        help="Template",
        required=False,
        default=f"{GeneralConstants.PACKAGE_DIRECTORY}/templates/stand_alone.py",
    )
    parser_run.add_argument(
        "--job", dest="task_job", help="Task job file", required=False
    )
    parser_run.add_argument("--output", "-o", help="Task output file", required=False)
    parser_run.add_argument("--troika", default="troika")
    parser_run.add_argument("--troika-config", default="/opt/troika/etc/troika.yml")
    parser_run.set_defaults(run_command=run_task)

    ##########################################
    # Configure parser for the "case" command #
    ##########################################
    parser_case = subparsers.add_parser(
        "case",
        help="Create a config file to run an experiment case",
        parents=[common_parser],
    )
    parser_case.add_argument(
        "--output",
        "-o",
        dest="output_file",
        help=(
            "Output config file, if not given the name will be the same as the case. "
            + "If the name does not end with '.toml' it's assumed to be a directory "
            + "and the file name will be the same as the case."
        ),
        default=None,
        required=False,
    )
    parser_case.add_argument(
        "--case-name", dest="case", help="Case name", required=False, default=None
    )
    parser_case.add_argument(
        "config_mods",
        help="Path to configuration modifications",
        nargs="*",
        type=Path,
        default=None,
    )
    parser_case.add_argument(
        "--expand-config",
        "-e",
        action="store_true",
        default=False,
        help="Expand macros in config",
        required=False,
    )
    parser_case.add_argument(
        "--start-suite",
        "-s",
        action="store_true",
        default=False,
        help="Start suite as well",
        required=False,
    )
    add_keep_def_file(
        parser_case, help_message="Keep suite definition file in case of submission"
    )
    parser_case.set_defaults(run_command=create_exp)

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
        "--start-command", type=str, help="Start command for server", default=None
    )
    parser_start_suite.add_argument(
        "--def-file",
        "-f",
        help="Suite definition file",
        default="",
    )
    add_keep_def_file(parser_start_suite)
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

    # show host
    parser_show_host = show_command_subparsers.add_parser(
        "host", help="Print current and available hosts", parents=[common_parser]
    )
    parser_show_host.set_defaults(run_command=show_host)

    # show namelist
    parser_show_namelist = show_command_subparsers.add_parser(
        "namelist", help="Print namelist in use and exit", parents=[common_parser]
    )
    add_namelist_args(parser_show_namelist)

    # show paths
    parser_show_paths = show_command_subparsers.add_parser(
        "paths", help="Print paths in use and exit", parents=[common_parser]
    )
    parser_show_paths.set_defaults(run_command=show_paths)

    ###########################################
    # Configure parser for the "doc" command #
    ###########################################
    parser_doc = subparsers.add_parser("doc", help="Print documentation style output")
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
        help="Print documentation for the config's json schema in markdown style",
        parents=[common_parser],
    )

    parser_doc_config.set_defaults(run_command=doc_config)

    # namelist subparser
    parser_namelist = subparsers.add_parser(
        "namelist",
        help="Namelist show (output), integrate (input), "
        + "convert (input, output), format (input, output)",
    )
    namelist_command_subparsers = parser_namelist.add_subparsers(
        title="namelist",
        dest="namelist_what",
        required=True,
        description=(
            "Valid commands below (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ),
        help="command description",
    )

    # show namelist
    parser_namelist_show = namelist_command_subparsers.add_parser(
        "show", help="Print namelist in use and exit", parents=[common_parser]
    )
    add_namelist_args(parser_namelist_show)

    # namelist integrate
    parser_namelist_integrate = namelist_command_subparsers.add_parser(
        "integrate",
        help="Read fortran [+yaml] namelist(s) and output as yaml dict(s)",
        parents=[common_parser],
    )
    parser_namelist_integrate.add_argument(
        "-n",
        "--namelist",
        nargs="+",
        type=str,
        help="Fortran namelist input file(s)",
        required=True,
        default=None,
    )
    parser_namelist_integrate.add_argument(
        "-o",
        "--output",
        type=str,
        help="Output file (yaml format)",
        required=True,
        default=None,
    )
    parser_namelist_integrate.add_argument(
        "-t",
        "--tag",
        type=str,
        help="Tag used as base for comparisons",
        required=False,
        default=None,
    )
    parser_namelist_integrate.add_argument(
        "-y",
        "--yaml",
        type=str,
        help="Input yaml file (from earlier run)",
        required=False,
        default=None,
    )
    parser_namelist_integrate.set_defaults(run_command=namelist_integrate)

    # namelist convert
    parser_namelist_convert = namelist_command_subparsers.add_parser(
        "convert",
        help="Convert a namelist (ftn or yml) to a new Cycle",
        parents=[common_parser],
    )
    parser_namelist_convert.add_argument(
        "-n",
        "--namelist",
        type=str,
        help="Input namelist definition filename",
        required=True,
        default=None,
    )
    parser_namelist_convert.add_argument(
        "-o",
        "--output",
        type=str,
        help="Output namelist definition filename",
        required=True,
        default=None,
    )
    parser_namelist_convert.add_argument(
        "--from-cycle",
        type=str,
        help="Cycle of input namelist",
        choices=NamelistConverter.get_known_cycles(),
        required=True,
        default=None,
    )

    parser_namelist_convert.add_argument(
        "--to-cycle",
        type=str,
        help="Cycle of output namelist",
        choices=NamelistConverter.get_known_cycles(),
        required=True,
        default=None,
    )

    parser_namelist_convert.add_argument(
        "--format", "-fmt", help="Input format", choices=["yaml", "ftn"], default="yaml"
    )
    parser_namelist_convert.set_defaults(run_command=namelist_convert)

    # namelist format
    parser_namelist_format = namelist_command_subparsers.add_parser(
        "format",
        help="Format a namelist (ftn or yml) ",
        parents=[common_parser],
    )
    parser_namelist_format.add_argument(
        "-n",
        "--namelist",
        type=str,
        help="Input namelist definition filename",
        required=True,
        default=None,
    )
    parser_namelist_format.add_argument(
        "-o",
        "--output",
        type=str,
        help="Output namelist definition filename",
        required=True,
        default=None,
    )

    parser_namelist_format.add_argument(
        "--format", "-fmt", help="Input format", choices=["yaml", "ftn"], default="yaml"
    )
    parser_namelist_format.set_defaults(run_command=namelist_format)

    return main_parser.parse_args(argv)


def add_namelist_args(parser_object):
    """Add namelist args.

    Args:
        parser_object (args oject): args object to update

    Returns:
        parser_object (args oject): updated args object

    """
    parser_object.add_argument(
        "--namelist-type",
        "-t",
        type=str,
        help="Namelist target, master or surfex",
        choices=["master", "surfex"],
        required=True,
        default=None,
    )
    parser_object.add_argument(
        "--namelist",
        "-n",
        type=str,
        help="Namelist to show, type anything to print available options",
        required=True,
        default=None,
    )
    parser_object.add_argument(
        "--optional-namelist-name",
        "-o",
        type=str,
        dest="namelist_name",
        help="Optional namelist name",
        default=None,
    )
    parser_object.add_argument(
        "--substitute",
        "-s",
        action="store_true",
        default=False,
        help=(
            "Substitute config values in the written namelist. "
            + "Note that this does not handle task submission "
            + "dependent settings such as NPROC."
        ),
    )
    parser_object.set_defaults(run_command=show_namelist)

    return parser_object


def add_keep_def_file(
    parser_object, help_message="Keep suite definition file after submission"
):
    """Add object args.

    Args:
        parser_object (args oject): args object to update
        help_message (str): Help text

    """
    parser_object.add_argument(
        "--keep-def-file",
        "-k",
        help=help_message,
        action="store_true",
        default=False,
        required=False,
    )
