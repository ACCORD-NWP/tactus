#!/usr/bin/env python3
"""Implement the package's commands."""

import os
from .logs import get_logger
from .scheduler import EcflowServer
from .submission import NoSchedulerSubmission, TaskSettings
from .suites import SuiteDefinition


def run_task(args, config):
    """Implement the 'run' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Running %s...", args.task)

    deode_home = f"{os.path.dirname(__file__)}/.."
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    submission_defs = TaskSettings(config)
    sub = NoSchedulerSubmission(submission_defs)
    sub.submit(
        args.task,
        config,
        args.template_job,
        args.task_job,
        args.output,
        args.troika
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

    deode_home = f"{os.path.dirname(__file__)}/.."
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
