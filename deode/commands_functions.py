#!/usr/bin/env python3
"""Implement the package's commands."""
import datetime
import os
import sys
from functools import partial
from pathlib import Path

from toml_formatter.formatter import FormattedToml
from troika.connections.ssh import SSHConnection

from . import GeneralConstants
from .config_parser import BasicConfig, ParsedConfig
from .derived_variables import check_fullpos_namelist, derived_variables, set_times
from .logs import logger
from .namelist import NamelistComparator, NamelistGenerator, NamelistIntegrator
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
        deode_home_from_config = config["platform.deode_home"]
    except KeyError:
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
    logger.info("Running {}...", args.task)

    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})
    config = config.copy(update=set_times(config))

    submission_defs = TaskSettings(config)
    sub = NoSchedulerSubmission(submission_defs)
    sub.submit(
        args.task, config, args.template_job, args.task_job, args.output, args.troika
    )
    logger.info("Done with task {}", args.task)


def start_suite(args, config):
    """Implement the 'start suite' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})
    config = config.copy(update=set_times(config))
    platform = Platform(config)
    update = {
        "scheduler": {
            "ecfvars": {
                "ecf_jobout": platform.substitute(config["scheduler.ecfvars.ecf_jobout"]),
                "ecf_files": platform.substitute(config["scheduler.ecfvars.ecf_files"]),
                "ecf_files_remotely": platform.substitute(
                    config["scheduler.ecfvars.ecf_files_remotely"]
                ),
                "ecf_host": platform.substitute(config["scheduler.ecfvars.ecf_host"]),
            },
        },
    }
    config = config.copy(update=update)

    logger.info("Starting suite...")
    logger.info("Settings and paths loaded: ")
    logger.info("Config file: {}", args.config_file)
    logger.info("Ecflow settings: ")

    # Check if user passed flags, and reiterate ecf vars to them
    logger.info(
        "ecf_jobout: {}", config["scheduler.ecfvars.ecf_jobout"]
    ) if args.joboutdir is None else logger.info(
        "ecf_jobout: {}", args.joboutdir
    )
    logger.info(
        "ecf_files: {}", config["scheduler.ecfvars.ecf_files"]
    ) if args.ecf_files is None else logger.info(
        "ecf_files: {}", args.ecf_files
    )
    logger.info(
        "ecf_files_remotely: {}", config["scheduler.ecfvars.ecf_files_remotely"]
    ) if args.ecf_files_remotely is None else logger.info(
        "ecf_files_remotely: {}", args.ecf_files_remotely
    )
    logger.info(
        "ecf_home: {}", config["scheduler.ecfvars.ecf_home"]
    ) if args.ecf_home is not None else logger.info(
        "ecf_home: {}", args.ecf_home
    )

    server = EcflowServer(config, start_command=args.start_command)

    suite_name = config["general.case"]
    suite_name = Platform(config).substitute(suite_name)

    # Set ecf_home. If diff than joboutdir => we will copy it to server
    ecf_home = args.joboutdir if args.ecf_home is None else args.ecf_home
    ecf_files_local = args.ecf_files
    ecf_files_remotely = (
        args.ecf_files if args.ecf_files_remotely is None else args.ecf_files_remotely
    )

    remote_user = args.remote_user
    logger.debug("ECF_HOME={}", ecf_home)
    troika_config_file = config["troika.config_file"]
    troika_config_file = Platform(config).substitute(troika_config_file)
    local_troika_config_file = troika_config_file
    if ecf_home != args.joboutdir:
        local_troika_config_file = f"{ecf_files_remotely}/troika.yml"
    logger.debug(
        "troika config used: {} local file={}",
        troika_config_file,
        local_troika_config_file,
    )
    config = config.copy(update={"troika": {"config_file": local_troika_config_file}})

    server = EcflowServer(config, start_command=args.start_command)
    submission_defs = TaskSettings(config)
    defs = SuiteDefinition(suite_name, config, submission_defs)
    def_file = f"{suite_name}.def"
    defs.save_as_defs(def_file)

    # Copy troika and containers
    if ecf_home != args.joboutdir:
        logger.info(
            "Copy ecflow files to host={} and directory={} remote_user={}",
            args.ecf_host,
            ecf_files_remotely,
            remote_user,
        )
        cfg = {"host": args.ecf_host}
        ssh = SSHConnection(cfg, remote_user)
        for root, __, files in os.walk(f"{ecf_files_local}/{suite_name}"):
            for file in files:
                src = f"{root}/{file}"
                rpath = root.replace(f"{ecf_files_local}", "")
                ssh.execute(["mkdir", "-p", f"{ecf_files_remotely}/{rpath}"])
                dst = f"{ecf_files_remotely}/{rpath}/{file}"
                logger.info("Copy src={} to dst={}", src, dst)
                ssh.sendfile(src, dst)
            ssh.sendfile(troika_config_file, local_troika_config_file)

    server.start_suite(suite_name, def_file, begin=args.begin)
    logger.info("Done with suite.")


#########################################
# Code related to the "show *" commands #
#########################################
def doc_config(args, config: ParsedConfig):  # noqa ARG001
    """Implement the 'doc_config' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (ParsedConfig): Parsed config file contents.

    """
    now = datetime.datetime.now().isoformat(timespec="seconds")
    sys.stdout.write(
        f"This was automatically generated running `deode doc config` on {now}.\n\n"
    )
    sys.stdout.write(config.json_schema.get_markdown_doc() + "\n")


def show_config(args, config):
    """Implement the 'show_config' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger.info("Printing requested configs...")

    pkg_configs = BasicConfig.from_file(
        GeneralConstants.PACKAGE_DIRECTORY.parent / "pyproject.toml"
    )

    toml_formatting_function = partial(
        FormattedToml.from_string,
        formatter_options=pkg_configs.get("tool.toml-formatter", {}),
    )

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


def show_config_schema(args, config):  # noqa ARG001
    """Implement the `show config-schema` command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    logger.info("Printing JSON schema used in the validation of the configs...")
    sys.stdout.write(str(config.json_schema) + "\n")


def show_namelist(args, config):
    """Implement the 'show_namelist' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})
    config = config.copy(update=set_times(config))
    config = config.copy(update=derived_variables(config))

    nlgen = NamelistGenerator(config, args.namelist_type, substitute=args.no_substitute)
    nlgen.load(args.namelist)
    update = config["namelist_update"]
    if args.namelist_type in update:
        nlgen.update(update[args.namelist_type], args.namelist_type)
    if "forecast" in args.namelist and args.namelist_type == "master":
        nlgen = check_fullpos_namelist(config, nlgen)
    nlres = nlgen.assemble_namelist(args.namelist)
    if args.namelist_name is not None:
        namelist_name = args.namelist_name
    else:
        namelist_name = f"namelist_{args.namelist_type}_{args.namelist}"
    nlgen.write_namelist(nlres, namelist_name)
    logger.info("Printing namelist in use to file {}", namelist_name)


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
        nml = nlint.yml2dict(Path(args.yaml))
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
    nlint.dict2yml(nml, Path(args.output))
