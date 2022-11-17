#!/usr/bin/env python3
"""Implement the package's commands."""

from .logs import get_logger
from .scheduler import EcflowServer
from .submission import NoSchedulerSubmission, TaskSettingsJson
from .suites import SuiteDefinition


def run_task(args, config):
    """Implement the 'run' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Running %s...", args.task)
    submission_defs = TaskSettingsJson(args.submission_file)
    sub = NoSchedulerSubmission(submission_defs)
    sub.submit(
        args.task,
        args.config_file,
        args.template_job,
        args.task_job,
        args.output,
        args.troika,
        args.troika_config,
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

    server = EcflowServer(
        args.ecf_host, ecf_port=args.ecf_port, start_command=args.start_command
    )

    submission_defs = TaskSettingsJson(args.submission_file)
    defs = SuiteDefinition(
        args.suite_name,
        args.joboutdir,
        args.ecf_files,
        args.config_file,
        submission_defs,
        args.loglevel,
    )
    def_file = f"{args.suite_name}.def"
    defs.save_as_defs(def_file)

    server.start_suite(args.suite_name, def_file, begin=args.begin)
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
            section=args.section,
            style=args.format,
            exclude_unset=args.no_defaults,
            include_metadata=args.show_metadata,
        )
    )
