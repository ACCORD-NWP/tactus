#!/usr/bin/env python3
"""Implement the package's commands."""

import argparse
import contextlib
import datetime
import os
import sys
from functools import partial
from pathlib import Path
from typing import List, Optional

from toml_formatter.formatter import FormattedToml

from . import GeneralConstants
from .cleaning import CleanTactus
from .config_parser import BasicConfig, ConfigParserDefaults, ConfigPaths, ParsedConfig
from .derived_variables import check_fullpos_namelist, derived_variables, set_times
from .experiment import case_setup
from .host_actions import TactusHost, set_tactus_home
from .logs import logger
from .namelist import (
    NamelistComparator,
    NamelistConverter,
    NamelistGenerator,
    NamelistIntegrator,
)
from .scheduler import (
    EcflowEnvironmentFromConfig,
    EcflowServerFromConfig,
    TroikaConfigurationFromConfig,
)
from .submission import NoSchedulerSubmission, TaskSettings
from .tasks.discover_task import create_task_index
from .toolbox import Platform


class RunTaskNamespace(argparse.Namespace):
    """Namespace for the 'run' command."""

    task: str
    tactus_home: str
    task_job: Path
    output: Path
    template_job: Path
    troika: str
    members: Optional[List[int]] = None


def run_task(args: RunTaskNamespace, config: ParsedConfig):
    """Implement the 'run' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger.info("Prepare {}...", args.task)

    tactus_home = set_tactus_home(config, args.tactus_home)

    cwd = Path.cwd()

    # note: Path.cwd() is already resolved, so no need to resolve in this case
    task_job = (
        cwd / Path(f"{args.task}.job") if not args.task_job else args.task_job.resolve()
    )
    output = cwd / Path(f"{args.task}.log") if not args.output else args.output.resolve()
    template_job = args.template_job.resolve()

    config = config.copy(update={"platform": {"tactus_home": tactus_home}})
    config = config.copy(update=set_times(config))

    submission_defs = TaskSettings(config)
    sub = NoSchedulerSubmission(submission_defs)

    if not args.create_only:
        create_task_index(config)

    sub.submit(
        task=args.task,
        config=config,
        template_job=template_job,
        task_job=task_job,
        output=output,
        troika=args.troika,
        create_only=args.create_only,
    )
    logger.info("Task {} submitted.", args.task)


def create_exp(args, config):
    """Implement the 'case' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    known_hosts_file = args.host_file
    if known_hosts_file is None:
        known_hosts_file = ConfigPaths.path_from_subpath("known_hosts.yml")

    host = TactusHost(known_hosts_file=known_hosts_file).detect_tactus_host()
    output_file = args.output_file
    case = args.case
    mod_files = args.config_mods

    if mod_files is None:
        mod_files = []
    output_file = case_setup(
        config,
        output_file,
        mod_files,
        case=case,
        host=host,
        expand_config=args.expand_config,
    )

    if args.start_suite:
        config = ParsedConfig.from_file(
            output_file, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        args.start_command = None
        args.config_file = output_file
        args.def_file = ""
        start_suite(args, config)


def start_suite(args, config):
    """Implement the 'start suite' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Raises:
        SystemExit: If error occurs while transferring files.
    """
    # Is this needed???
    tactus_home = set_tactus_home(config, args.tactus_home)
    config = config.copy(update={"platform": {"tactus_home": tactus_home}})
    config = config.copy(update=set_times(config))

    # Setup ecflow server and environment from config
    server = EcflowServerFromConfig(config, start_command=args.start_command)
    ecflow_env = EcflowEnvironmentFromConfig(config)

    # Display settings
    ecflow_env.display_properties()

    # Create a troika object and possibly substitute
    troika = TroikaConfigurationFromConfig(config)
    platf = Platform(config)
    troika.substitute_troika_config(platf)

    # Copy files to remote server if needed
    ecflow_env.copy_to_remote(server, troika=troika)

    # Check arguments
    def_file = args.def_file
    if args.def_file is None:
        def_file = f"{ecflow_env.suite_name}.def"
    elif os.path.exists(def_file):
        args.keep_def_file = True
    else:
        def_file = args.def_file
    # Save definition file
    if def_file is not None:
        ecflow_env.suite_def_obj.save_as_defs(def_file)

    # Create the task index to ensure all tasks are registered before starting the suite
    create_task_index(config)
    # Start the suite (and server if needed)
    server.start_suite(ecflow_env.suite_name, def_file)
    logger.info("Done with suite.")

    if not args.keep_def_file:
        os.remove(def_file)


#########################################
# Code related to the "show *" commands #
#########################################
def doc_config(args, config: ParsedConfig):
    """Implement the 'doc_config' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (ParsedConfig): Parsed config file contents.

    """
    now = datetime.datetime.now().isoformat(timespec="seconds")
    sys.stdout.write(f"""The following section was automatically generated running
        `tactus doc config` on {now}.\n\n""")
    sys.stdout.write(config.json_schema.get_markdown_doc() + "\n")


def show_config(args, config):
    """Implement the 'show_config' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger.info("Printing requested configs...")

    pyproject_toml = GeneralConstants.PACKAGE_DIRECTORY.parent / "pyproject.toml"

    pkg_configs = (
        BasicConfig.from_file(pyproject_toml)
        if os.path.isfile(pyproject_toml)
        else BasicConfig({})
    )

    toml_formatting_function = partial(
        FormattedToml.from_string,
        formatter_options=pkg_configs.get("tool.toml-formatter", {}),
    )

    if args.expand_config:
        tactus_home = set_tactus_home(config)
        config = config.copy(update={"platform": {"tactus_home": tactus_home}})
        config = config.expand_macros(True)

    try:
        dumps = config.dumps(
            section=args.section,
            style=args.format,
            toml_formatting_function=toml_formatting_function,
        )
    except KeyError:
        logger.error('Error retrieving config data for config section "{}"', args.section)
    else:
        sys.stdout.write(str(dumps) + "\n")


def show_config_schema(args, config):
    """Implement the `show config-schema` command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger.info("Printing JSON schema used in the validation of the configs...")
    sys.stdout.write(str(config.json_schema) + "\n")


def show_host(args, config):
    """Implement the `show host` command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    tactus_host = TactusHost()
    logger.info("Current host: {}", tactus_host.detect_tactus_host())
    logger.info("Known hosts (host, recognition method):")
    for host, pattern in tactus_host.known_hosts.items():
        logger.info("{:>16}   {}", host, pattern)


def remove_cases(args, config):  # ARG001
    """Remove output from cases."""
    if len(args.config_files) == 0:
        logger.info("No files no removal")
        return False

    # Fetch the remove config
    cleaning_config = config.get_as_dict("remove")
    defaults = cleaning_config.get("defaults")
    cleaning_config.pop("defaults")

    # Loop over all given config files
    for filename in args.config_files:
        if not os.path.isfile(filename):
            logger.warning("Cannot find {}", filename)
            continue

        logger.info("Read config from: {}", filename)
        case_config = ParsedConfig.from_file(filename, json_schema={})
        case_config = case_config.copy(update=set_times(case_config))
        case_config = case_config.copy(update={"remove": cleaning_config})
        platform = Platform(case_config)

        # Loop over the different section in the remove config
        for section, original_settings in cleaning_config.items():
            settings = dict(original_settings)
            if section != "main" and not case_config.get(
                f"impact.{section}.active", False
            ):
                continue

            logger.info("Remove for section:{}", section)

            suite_name = settings.get("suite_name")
            remove_from_scheduler = settings.get("remove_from_scheduler", False)
            remove_not_completed_suites = (
                settings.get("remove_not_completed_suites", False) or args.force_remove
            )

            if suite_name is not None:
                suite_name = platform.substitute(suite_name)

                server = EcflowServerFromConfig(case_config)
                server.ecf_client.sync_local()

                this_suite = None
                for suite in server.ecf_client.get_defs().suites:
                    if suite.name() == suite_name:
                        this_suite = suite
                        break

                if not remove_not_completed_suites:
                    if this_suite is None:
                        logger.warning("No suite found for {}", suite_name)
                        continue
                    if not server.suite_is_complete(this_suite):
                        logger.info(
                            "Suite is not completed, do not remove anything from {}.",
                            suite_name,
                        )
                        continue

            for key in (
                "suite_name",
                "remove_from_scheduler",
                "remove_not_completed_suites",
            ):
                with contextlib.suppress(KeyError):
                    settings.pop(key)

            cleaner = CleanTactus(case_config, defaults)
            dry_run = not args.execute_removal
            cleaner.prep_cleaning(settings, dry_run=dry_run)
            cleaner.clean()
            if suite_name is not None and remove_from_scheduler:
                if dry_run:
                    logger.info(" would have removed suite {}", suite_name)
                    for directory in server.get_ecf_vars(this_suite):
                        if os.path.isdir(directory):
                            logger.info(
                                " would have removed ecflow directory {}", directory
                            )

                else:
                    try:
                        server.remove_suites([suite_name], check_if_complete=False)
                    except (ModuleNotFoundError, UnboundLocalError):
                        logger.warning(
                            "ecflow or config not found, suite {} not removed",
                            suite_name,
                        )
            if dry_run:
                logger.info(
                    "\n\nRerun with '--execute-removal' to do the actual removal\n"
                )

    return True


#########################################
#########################################


def show_namelist(args, config):
    """Implement the 'show_namelist' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    tactus_home = set_tactus_home(config, args.tactus_home)
    config = config.copy(update={"platform": {"tactus_home": tactus_home}})
    config = config.copy(update=set_times(config))
    config = config.copy(update=derived_variables(config))

    nlgen = NamelistGenerator(config, args.namelist_type, substitute=args.substitute)
    nlgen.load(args.namelist)

    if "forecast" in args.namelist and args.namelist_type == "master":
        nlgen = check_fullpos_namelist(config, nlgen)
    nlres = nlgen.assemble_namelist(args.namelist)
    if args.namelist_name is not None:
        namelist_name = args.namelist_name
    else:
        namelist_name = f"namelist_{args.namelist_type}_{args.namelist}"
    nlgen.write_namelist(nlres, namelist_name)
    logger.info("Printing namelist in use to file {}", namelist_name)


def show_paths(args, config):
    """Implement the 'show_paths' command."""
    tactus_host = TactusHost()
    ConfigPaths.print(args.config_file, tactus_host.detect_tactus_host())


def namelist_integrate(args, config):
    """Implement the 'namelist integrate' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Raises:
        SystemExit   # noqa: DAR401

    """
    logger.info("Integrating namelist(s) ...")

    nlcomp = NamelistComparator(config)
    nlint = NamelistIntegrator(config)
    # Read all input namelist files and convert to yaml dicts
    nml_in = {}
    nltags = []
    for nlfile in args.namelist:
        nlpath = Path(nlfile)
        ltag = nlpath.name.replace(".", "_")
        nltags.append(ltag)
        msg = f"Reading {nlfile}"
        logger.info(msg)
        nml_in[ltag] = nlint.ftn2dict(nlpath)

    # Start with empty output namelist set
    nml = {}
    if args.tag:
        tag = args.tag
        if tag in nltags:
            # Use given tag as base for comparisons, then
            nml[tag] = nml_in[tag]
    else:
        tag = "00_common"
    if args.yaml:
        if not args.tag:
            raise SystemExit(
                "With -y given, you must also specify with -t which tag to use as basis!"
            )
        # Read yaml to use as basis for comparisons
        nml = NamelistIntegrator.yml2dict(Path(args.yaml))
        if tag not in nml:
            raise SystemExit(f"Tag {tag} was not found in input yaml file {args.yaml}!")

        if tag in nltags:
            raise SystemExit(f"Tag {tag} found in both yaml and namelist input, abort!")
    elif not nml:
        # Construct basis as intersection of all input files
        for ltag in nltags:
            if not nml:
                nml[tag] = nml_in[ltag]
            elif ltag != tag:
                nml[tag] = nlcomp.compare_dicts(nml[tag], nml_in[ltag], "intersection")

    # Now, whether yaml input or not, nml[tag] should contain the common settings
    # Loop over input namelists to produce diffs
    for ltag in nltags:
        if ltag != tag:
            nml[ltag] = nlcomp.compare_dicts(nml[tag], nml_in[ltag], "diff")

    # Write output yaml
    NamelistIntegrator.dict2yml(nml, Path(args.output))


def namelist_convert(args, config: ParsedConfig):
    """Implement the 'namelist convert' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    # Configuration
    # Check that parameters are present
    for parameter, parameter_name in zip(
        [args.from_cycle, args.to_cycle, args.namelist, args.output],
        ["from_cycle", "to_cycle", "namelist", "output"],
    ):
        if not parameter:
            raise SystemExit(f"Please provide parameter {parameter_name}")

    # Convert namelists
    logger.info(f"Convert namelist from cycle {args.from_cycle} to cycle {args.to_cycle}")

    if args.format == "yaml":
        NamelistConverter.convert_yml(
            args.namelist, args.output, args.from_cycle, args.to_cycle
        )
    elif args.format == "ftn":
        NamelistConverter.convert_ftn(
            args.namelist, args.output, args.from_cycle, args.to_cycle
        )
    else:
        raise SystemExit(f"Format {args.format} not handled")


def namelist_format(args, config: ParsedConfig):
    """Implement the 'namelist format' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    # Configuration
    # Check that parameters are present
    for parameter, parameter_name in zip(
        [args.namelist, args.output],
        ["namelist", "output"],
    ):
        if not parameter:
            raise SystemExit(f"Please provide parameter {parameter_name}")

    # Convert namelists
    logger.info("Format namelist")
    if args.format == "yaml":
        NamelistConverter.convert_yml(args.namelist, args.output, None, None)
    elif args.format == "ftn":
        NamelistConverter.convert_ftn(args.namelist, args.output, None, None)
    else:
        raise SystemExit(f"Format {args.format} not handled")


def replace_node(args, config):
    """Implement the 'replace' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    # Is this needed???
    tactus_home = set_tactus_home(config, args.tactus_home)
    config = config.copy(update={"platform": {"tactus_home": tactus_home}})
    config = config.copy(update=set_times(config))

    # Setup ecflow server and environment from config
    server = EcflowServerFromConfig(config)
    ecflow_env = EcflowEnvironmentFromConfig(config)
    # Display settings
    ecflow_env.display_properties()

    # Create a troika object and possibly substitute
    troika = TroikaConfigurationFromConfig(config)
    platf = Platform(config)
    troika.substitute_troika_config(platf)

    # Copy files to remote server if needed
    ecflow_env.copy_to_remote(server, troika=troika)

    # Check arguments
    def_file = args.def_file
    node_path = args.node_path
    if args.def_file is None:
        def_file = f"{ecflow_env.suite_name}.def"
    elif os.path.exists(def_file):
        args.keep_def_file = True
    else:
        def_file = args.def_file
    # Save definition file
    if def_file is not None:
        ecflow_env.suite_def_obj.save_as_defs(def_file)

    # Replace node
    logger.info("Replace node {} from def file: {}", node_path, def_file)
    server.replace_node(node_path, def_file)
    logger.info("Replaced node {}", node_path)

    if not args.keep_def_file:
        os.remove(def_file)
