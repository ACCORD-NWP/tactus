#!/usr/bin/env python3
"""Implement the package's commands."""
import json
import os

from .logs import get_logger
from .scheduler import EcflowServer, EcflowTask
from .submission import (
    EcflowSubmitTask,
    KillException,
    StatusException,
    SubmitException,
    TaskSettings,
    get_submission_object,
)
from .suites import SuiteDefinition


def run_forecast(args, config):
    """Implement the 'forecast' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Running forecast...")
    logger.info("Done with forecast.")


def start_suite(args, config):
    """Implement the 'start suite' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Starting suite...")

    server = EcflowServer(args.ecf_host, args.ecf_port, logfile=args.logfile)
    defs = SuiteDefinition(
        args.suite_name,
        args.joboutdir,
        args.ecf_files,
        args.submission_file,
        server.logfile,
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
            section=args.section, style=args.format, exclude_unset=args.no_defaults
        )
    )


def run_submit_ecflow_task(args, config):
    """Submit task.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Raise:
        exc (deode.SubmitException): If submission was not successful
    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Submit task")
    ecf_name = args.ecf_name
    ecf_tryno = args.ecf_tryno
    ecf_pass = args.ecf_pass
    ecf_rid = args.ecf_rid
    joboutdir = args.joboutdir
    if isinstance(joboutdir, str):
        joboutdir = {"0": joboutdir}
    with open(args.submission_file, mode="r", encoding="utf-8") as file_handler:
        submit_defs = json.load(file_handler)
    logfile = args.logfile

    if args.ecf_host is not None and args.ecf_port is not None:
        server = EcflowServer(args.ecf_host, args.ecf_port, logfile=logfile)
    else:
        server = None

    submission_id = None
    if ecf_rid is not None:
        if ecf_rid == "":
            ecf_rid = os.getpid()
    else:
        ecf_rid = os.getpid()

    try:
        task = EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
        sub = EcflowSubmitTask(task, submit_defs, server, joboutdir)
        sub.submit()
    except SubmitException as exc:
        raise exc
    logger.info("Done with submission.")


def run_status_ecflow_task(args, config):
    """Status task.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Raise:
        exc (deode.SubmitException): If submission was not successful
    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Status task")
    ecf_name = args.ecf_name
    ecf_tryno = args.ecf_tryno
    ecf_pass = args.ecf_pass
    ecf_rid = args.ecf_rid
    submission_id = args.submission_id
    joboutdir = args.joboutdir
    if isinstance(joboutdir, str):
        joboutdir = {"0": joboutdir}
    with open(args.submission_file, mode="r", encoding="utf-8") as file_handler:
        submit_defs = json.load(file_handler)
    logfile = args.logfile

    if args.ecf_host is not None and args.ecf_port is not None:
        server = EcflowServer(args.ecf_host, args.ecf_port, logfile=logfile)
    else:
        server = None

    task = EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    task_settings = TaskSettings(task, submit_defs, joboutdir)

    sub = get_submission_object(task, task_settings, server)
    sub.status()
    logger.info("Done with status.")


def run_kill_ecflow_task(args, config):
    """Kill task.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Raise:
        exc (deode.SubmitException): If submission was not successful
    """
    logger = get_logger(__name__, args.loglevel)
    logger.info("Status task")
    ecf_name = args.ecf_name
    ecf_tryno = args.ecf_tryno
    ecf_pass = args.ecf_pass
    ecf_rid = args.ecf_rid
    submission_id = args.submission_id
    joboutdir = args.joboutdir
    if isinstance(joboutdir, str):
        joboutdir = {"0": joboutdir}
    with open(args.submission_file, mode="r", encoding="utf-8") as file_handler:
        submit_defs = json.load(file_handler)
    logfile = args.logfile

    if args.ecf_host is not None and args.ecf_port is not None:
        server = EcflowServer(args.ecf_host, args.ecf_port, logfile=logfile)
    else:
        server = None

    task = EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    task_settings = TaskSettings(task, submit_defs, joboutdir)

    sub = get_submission_object(task, task_settings, server)
    sub.kill()
    server.force_aborted(task)
    logger.info("Done with Kill.")
