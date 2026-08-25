#!/usr/bin/env python3
"""Wrappers for argparse functionality."""

import argparse
from pathlib import Path

from . import GeneralConstants
from .commands_functions import (
    create_compile_exp,
    create_exp,
    doc_config,
    namelist_convert,
    namelist_format,
    namelist_integrate,
    remove_cases,
    replace_node,
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
from .test_runner import run_test


def get_common_parser(config_file_required=False):
    """Build and return the common argument parser shared by all subcommands.

    Args:
        config_file_required (bool): Whether the config file argument is required.

    Returns:
        argparse.ArgumentParser: Parser with common arguments (config-file,
            host-file, etc.).

    """
    common_parser = argparse.ArgumentParser(add_help=False)

    common_parser.add_argument(
        "--tactus-home",
        default=None,
        help="Specify tactus_home to override automatic detection",
    )
    if config_file_required:
        common_parser.add_argument(
            "--config-file",
            "-c",
            metavar="CONFIG_FILE_PATH",
            required=True,
            type=Path,
            help=("Path to the config file."),
        )
    else:
        common_parser.add_argument(
            "--config-file",
            "-c",
            metavar="CONFIG_FILE_PATH",
            default=ConfigParserDefaults.CONFIG_PATH,
            type=Path,
            help=(
                "Path to the config file. The default is whichever of the "
                + "following is first encountered: "
                + "(i) The value of the 'TACTUS_CONFIG_PATH' envvar or "
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
    return common_parser


def get_args_parser(program_name=GeneralConstants.PACKAGE_NAME):
    """Build and return the argument parser.

    Args:
        program_name (str): The name of the program.

    Returns:
        argparse.ArgumentParser: The configured argument parser.

    """
    common_parser = get_common_parser(config_file_required=False)
    common_parser_config_file_required = get_common_parser(config_file_required=True)

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
    parser_run.add_argument("--task", "-t", type=str, help="Task name", required=True)
    parser_run.add_argument(
        "--template-job",
        help="Template",
        required=False,
        type=Path,
        default=GeneralConstants.PACKAGE_DIRECTORY / "templates/stand_alone.py",
    )
    parser_run.add_argument(
        "--job", dest="task_job", type=Path, help="Task job file", required=False
    )
    parser_run.add_argument(
        "--output", "-o", type=Path, help="Task output file", required=False
    )
    parser_run.add_argument("--troika", type=str, default="troika")
    parser_run.add_argument(
        "--troika-config", type=str, default="/opt/troika/etc/troika.yml"
    )
    parser_run.add_argument(
        "--create-only",
        action="store_true",
        help="Just create the job, do not submit it.",
        required=False,
        default=False,
    )
    parser_run.set_defaults(run_command=run_task)

    ##########################################
    # Configure parser for the "remove" command #
    ##########################################
    parser_remove = subparsers.add_parser(
        "remove",
        help="Remove a case from all locations",
        parents=[common_parser],
    )
    parser_remove.add_argument(
        "--execute-removal",
        help="Execute the actual removal of data",
        action="store_true",
        default=False,
    )
    parser_remove.add_argument(
        "--force-remove",
        "-f",
        help="Remove suites, and possibly their data, even if not completed",
        action="store_true",
        default=False,
    )
    parser_remove.add_argument(
        "config_files",
        help="Config files for cases to remove",
        nargs="*",
        type=Path,
        default=None,
    )

    parser_remove.set_defaults(run_command=remove_cases)

    ##########################################
    # Configure parser for the "case" command #
    ##########################################
    parser_case = subparsers.add_parser(
        "case",
        help="Create a config file to run an experiment case",
        parents=[common_parser_config_file_required],
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
    add_expand_config(parser_case)
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
        "suite", help="Start the suite", parents=[common_parser_config_file_required]
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
    # Configure parser for the "compile" command #
    ###########################################
    parser_compile = subparsers.add_parser(
        "compile",
        help="Start a compilation suite",
        parents=[common_parser],
    )
    parser_compile.add_argument(
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

    parser_compile.add_argument(
        "--dry-run",
        "-d",
        action="store_false",
        dest="start_suite",
        help="Start suite as well",
        required=False,
    )
    parser_compile.add_argument(
        "--case-name", dest="case", help="Case name", required=False, default=None
    )
    parser_compile.add_argument(
        "--ial-tag",
        dest="ial_tag",
        help="IAL git tag/branch, if not given default in config will be used",
        required=False,
    )
    parser_compile.add_argument(
        "--ial-repo",
        dest="ial_repo",
        help="IAL repository to use, if not given default in config will be used",
        required=False,
    )
    add_keep_def_file(
        parser_compile, help_message="Keep suite definition file in case of submission"
    )
    add_expand_config(parser_compile)
    parser_compile.set_defaults(run_command=create_compile_exp)

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
    add_expand_config(parser_show_config)
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

    ##########################################
    # Configure parser for the "test" command #
    ##########################################
    parser_test = subparsers.add_parser(
        "test",
        help="Run integration test cases via the test runner",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser_test.add_argument(
        "--config-file",
        "-c",
        dest="config_file",
        help="Test runner config file. A summary of tests results will be displayed "
        + "if only this option is given",
        required=False,
        default=None,
    )
    parser_test.add_argument(
        "--list",
        "-l",
        action="store_true",
        default=False,
        help="List selected cases",
    )
    parser_test.add_argument(
        "--dry",
        "-d",
        action="store_true",
        default=False,
        help="Prepare only, do not execute actions",
    )
    parser_test.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        default=False,
        help="Increase verbosity",
    )
    parser_test.add_argument(
        "--prepare-binaries",
        "-p",
        action="store_true",
        default=False,
        help="Prepare binaries from an IAL hash",
    )
    parser_test.add_argument(
        "-m",
        action="store_true",
        dest="configure",
        default=False,
        help="Create config files",
    )
    parser_test.add_argument(
        "-r",
        action="store_true",
        dest="run",
        default=False,
        help="Launch the tests",
    )
    parser_test.add_argument(
        "--generate-references",
        "-g",
        action="store_true",
        dest="generate_refs",
        help="Generate references outputs.",
        required=False,
        default=False,
    )

    parser_test.set_defaults(run_command=run_test, standalone_command=True)

    # Configure parser for the "replace" command #
    ##########################################
    parser_replace = subparsers.add_parser(
        "replace", help="Replaces a task/family/suite.", parents=[common_parser]
    )
    parser_replace.add_argument(
        "--ecf-node",
        type=str,
        help="Ecflow node name (ECF_NAME)",
        dest="node_path",
        required=True,
    )
    parser_replace.add_argument(
        "--def-file",
        "-f",
        help="Suite definition file",
        default="",
    )
    add_keep_def_file(parser_replace)
    parser_replace.set_defaults(run_command=replace_node)

    return main_parser


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
        help="Namelist target: master, surfex or gl",
        choices=["master", "surfex", "gl"],
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


def add_expand_config(parser_object):
    """Add object args.

    Args:
        parser_object (args oject): args object to update

    """
    parser_object.add_argument(
        "--expand-config",
        "-e",
        action="store_true",
        default=False,
        help="Expand macros in config",
        required=False,
    )
