#!/usr/bin/env python3
"""Implement the package's commands."""
import os
from pathlib import Path

from .config_doc import DocConfig
from .derived_variables import derived_variables
from .fullpos import Fullpos
from .logs import get_logger
from .namelist import NamelistGenerator
from .scheduler import EcflowServer
from .submission import NoSchedulerSubmission, TaskSettings
from .suites import SuiteDefinition
from .toolbox import Platform


def set_deode_home(args, config):
    """Set deode_home in various ways.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Returns:
        deode_home
    """
    try:
        deode_home_from_config = config.get_value("plaform.deode_home")
    except AttributeError:
        deode_home_from_config = "set-by-the-system"
    if args.deode_home is not None:
        deode_home = args.deode_home
    elif deode_home_from_config != "set-by-the-system":
        deode_home = deode_home_from_config
    else:
        deode_home = os.environ.get("PWD")
        if deode_home is None:
            deode_home = f"{os.path.dirname(__file__)}/.."

    return deode_home


def run_task(args, config):
    """Implement the 'run' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Running %s...", args.task)

    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    submission_defs = TaskSettings(config)
    sub = NoSchedulerSubmission(submission_defs)
    sub.submit(
        args.task, config, args.template_job, args.task_job, args.output, args.troika
    )
    logger.info("Done with task %s", args.task)


def start_suite(args, config):
    """Implement the 'start suite' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Starting suite...")

    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    server = EcflowServer(
        args.ecf_host, ecf_port=args.ecf_port, start_command=args.start_command
    )

    suite_name = config.get_value("general.case")
    submission_defs = TaskSettings(config)
    defs = SuiteDefinition(
        suite_name,
        args.joboutdir,
        args.ecf_files,
        config,
        submission_defs,
        args.loglevel,
    )
    def_file = f"{suite_name}.def"
    defs.save_as_defs(def_file)

    server.start_suite(suite_name, def_file, begin=args.begin)
    logger.info("Done with suite.")


#########################################
# Code related to the "show *" commands #
#########################################
def doc_config(args, config):
    """Implement the 'doc_config' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    schema_file = (
        Path(__file__).parent / "data" / "config_file_schemas" / "main_config_schema.json"
    )

    DocConfig(config.dict(), schema_file).print_doc()


def show_config(args, config):
    """Implement the 'show_config' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Printing configs in use...")
    print(
        config.dumps(
            section=args.section, style=args.format, include_metadata=args.show_metadata
        )
    )


def show_namelist(args, config):
    """Implement the 'show_namelist' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Printing namelist in use...")

    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    config = config.copy(update=derived_variables(config))

    nlgen = NamelistGenerator(config, args.namelist_type, substitute=args.no_substitute)
    nlgen.load(args.namelist)
    update = config.namelist_update.dict()
    if args.namelist_type in update:
        nlgen.update(update[args.namelist_type], args.namelist_type)
    if args.namelist == "forecast" and args.namelist_type == "master":
        fullpos_config = Platform(config).get_system_value("fullpos_config_file")
        domain = config.get_value("domain.name")
        namfpc, selections = Fullpos(domain, nlfile=fullpos_config).construct()
        nlgen.update(namfpc, "fullpos")
        for head, body in selections.items():
            nlgen.write_namelist(body, head)
    nlres = nlgen.assemble_namelist(args.namelist)
    if args.namelist_name is not None:
        namelist_name = args.namelist_name
    else:
        namelist_name = f"namelist_{args.namelist_type}_{args.namelist}"
    nlgen.write_namelist(nlres, namelist_name)
