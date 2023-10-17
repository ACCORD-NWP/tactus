#!/usr/bin/env python3
"""Wrappers for argparse functionality."""
import argparse
import sys
from pathlib import Path

from . import GeneralConstants
from .commands_functions import (
    doc_config,
    namelist_integrate,
    run_task,
    show_config,
    show_config_schema,
    show_namelist,
    start_suite,
    toml_formatter,
)
from .config_parser import ConfigParserDefaults


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

    ##########################################
    # Define main parser and general options #
    ##########################################
    main_parser = argparse.ArgumentParser(
        prog=program_name, formatter_class=argparse.ArgumentDefaultsHelpFormatter
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
    parser_run.add_argument("--template-job", help="Template", required=True)
    parser_run.add_argument("--job", dest="task_job", help="Task job file", required=True)
    parser_run.add_argument("--output", "-o", help="Task output file", required=True)
    parser_run.add_argument("--troika", default="troika")
    parser_run.add_argument("--troika-config", default="/opt/troika/etc/troika.yml")
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
        "--ecf-host", "-host", type=str, help="Ecflow host", default=None
    )
    parser_start_suite.add_argument(
        "--ecf-port", "-port", type=int, help="Ecflow port", default=None
    )
    parser_start_suite.add_argument(
        "--start-command", type=str, help="Start command for server", default=None
    )
    parser_start_suite.add_argument(
        "--joboutdir", "-j", help="Job out directory", required=True
    )
    parser_start_suite.add_argument(
        "--ecf-files", "-f", help="Ecflow container directory", required=True
    )
    parser_start_suite.add_argument(
        "--ecf_home", "-eh", help="Ecflow home directory locally on server", required=False, default=None
    )
    parser_start_suite.add_argument("--begin", "-b", help="Begin suite", default=True)
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
        "--namelist-type",
        "-t",
        type=str,
        help="Namelist target, master or surfex",
        choices=["master", "surfex"],
        required=True,
        default=None,
    )
    parser_show_namelist.add_argument(
        "--namelist",
        "-n",
        type=str,
        help="Namelist to show, type anything to print available options",
        required=True,
        default=None,
    )
    parser_show_namelist.add_argument(
        "--optional-namelist-name",
        "-o",
        type=str,
        dest="namelist_name",
        help="Optional namelist name",
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
        "namelist", help="Namelist show (output) or integrate (input)"
    )
    namelist_command_subparsers = parser_namelist.add_subparsers(
        title="integrate",
        dest="namelist_what",
        required=True,
        description=(
            "Valid commands below (note that commands also accept their "
            + "own arguments, in particular [-h]):"
        ),
        help="command description",
    )

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

    #####################################################
    # Configure parser for the "toml-formatter" command #
    #####################################################
    parser_toml_formatter = subparsers.add_parser(
        "toml-formatter",
        parents=[common_parser],
        help="Helper to format/standardise TOML files. "
        + "Return error code 1 if any file needs to be formatted.",
    )

    parser_toml_formatter.add_argument(
        "file_paths",
        help="Path(s) to the TOML files to be formatted. If a directory is passed, "
        + "then the code will descent recursively into it looking for TOML files.",
        type=lambda x: Path(x).expanduser().resolve(),
        nargs="+",
    )
    parser_toml_formatter.add_argument(
        "--show-formatted",
        help="Whether to show the formatted file contents for ill-formated files."
        + "If omitted, oly the diff will be shown.",
        action="store_true",
    )
    parser_toml_formatter.add_argument(
        "--fix-inplace",
        help="Modify the file(s) in-place to apply the suggested formatting.",
        action="store_true",
    )
    parser_toml_formatter.add_argument(
        "--include-hidden",
        help="Include hidden files in the recursive search.",
        action="store_true",
    )
    parser_toml_formatter.set_defaults(run_command=toml_formatter)

    return main_parser.parse_args(argv)
