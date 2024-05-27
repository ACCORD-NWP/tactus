#!/usr/bin/env python3
"""Implement the package's commands."""
import datetime
import os
import subprocess
import sys

from functools import partial
from pathlib import Path

import yaml
from toml_formatter.formatter import FormattedToml
from troika.connections.ssh import SSHConnection

from . import GeneralConstants
from .config_parser import BasicConfig, ParsedConfig
from .derived_variables import check_fullpos_namelist, derived_variables, set_times
from .experiment import case_setup
from .host_actions import DeodeHost
from .logs import logger
from .namelist import NamelistComparator, NamelistGenerator, NamelistIntegrator, NamelistConverter
from .scheduler import EcflowServer
from .submission import NoSchedulerSubmission, TaskSettings
from .suites.discover_suite import get_suite
from .toolbox import Platform


def ssh_cmd(host, user, cmd):
    """SSH to remote server and execute basic commands.

    Args:
        host: Host name or server IP address
        user: Username (string) to login to server with
        cmd: Command to be executed

    Returns:
        Message to notify if failed and why or successfully completed.
    """
    try:
        ssh_command = f'ssh {user}@{host} "{cmd}"'
        subprocess.run(ssh_command, shell=True, check=True)  # noqa
        logger.info("SSH command executed succesfully.")
        return True
    except subprocess.CalledProcessError as e:
        logger.info(f"Error executing SSH command: {e}")
        return False
    except Exception as e:  # noqa
        logger.info(f"Error: {e}")
        return False


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
    logger.info("Prepare {}...", args.task)

    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})
    config = config.copy(update=set_times(config))

    submission_defs = TaskSettings(config)
    sub = NoSchedulerSubmission(submission_defs)
    sub.submit(
            args.task, config, args.template_job, args.task_job, args.output, args.troika
            )
    logger.info("Done with task {}", args.task)


def create_exp(args, config):
    """Implement the 'case' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})
    config_dir = args.config_dir
    known_hosts = args.host_file
    if known_hosts is None:
        known_hosts = f"{deode_home}/deode/data/config_files/known_hosts.yml"
    host = DeodeHost(known_hosts=known_hosts)
    output_file = args.output_file
    case = args.case
    mod_files = args.config_mods
    if mod_files is None:
        mod_files = []
    case_setup(
            config,
            output_file,
            mod_files,
            case=case,
            host=host,
            config_dir=config_dir,
            )


def start_suite(args, config):
    """Implement the 'start suite' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    Raises:
        SystemExit: If error occurs while transferring files.
    """
    deode_home = set_deode_home(args, config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})
    config = config.copy(update=set_times(config))
    platform = Platform(config)
    update = {
            "scheduler": {
                "ecfvars": {
                    "case_prefix": platform.substitute(
                        config["scheduler.ecfvars.case_prefix"]
                        ),
                    "ecf_out": platform.substitute(config["scheduler.ecfvars.ecf_out"]),
                    "ecf_jobout": platform.substitute(config["scheduler.ecfvars.ecf_jobout"]),
                    "ecf_files": platform.substitute(config["scheduler.ecfvars.ecf_files"]),
                    "ecf_files_remotely": platform.substitute(
                        config["scheduler.ecfvars.ecf_files_remotely"]
                        ),
                    "ecf_home": platform.substitute(config["scheduler.ecfvars.ecf_home"]),
                    "ecf_host": platform.substitute(config["scheduler.ecfvars.ecf_host"]),
                    "ecf_remoteuser": platform.substitute(
                        config["scheduler.ecfvars.ecf_remoteuser"]
                        ),
                    },
                },
            }
    config = config.copy(update=update)

    logger.info("Starting suite...")
    logger.info("Config file: {}", args.config_file)
    logger.info("Ecflow settings: ")

    # Assign Ecfvars
    joboutdir = config["scheduler.ecfvars.ecf_jobout"]
    ecf_files = config["scheduler.ecfvars.ecf_files"]
    ecf_files_remotely = config["scheduler.ecfvars.ecf_files_remotely"]
    ecf_home = config["scheduler.ecfvars.ecf_home"]
    ecf_host = config["scheduler.ecfvars.ecf_host"]
    ecf_remoteuser = config["scheduler.ecfvars.ecf_remoteuser"]
    try:
        suite_def = config["suite_control.suite_definition"]
    except KeyError:
        suite_def = "DeodeSuiteDefinition"

    server = EcflowServer(config, start_command=args.start_command)

    suite_name = config["general.case"]
    suite_name = Platform(config).substitute(suite_name)
    ecf_files_local = ecf_files

    troika_config_file = Platform(config).substitute(config["troika.config_file"])
    if ecf_home != joboutdir:
        remote_troika_config_file = os.path.join(
                ecf_files_remotely, suite_name, os.path.basename(troika_config_file)
                )
    else:
        remote_troika_config_file = troika_config_file

    config = config.copy(
            update={
                "general": {"case": suite_name},
                "troika": {"config_file": remote_troika_config_file},
                }
            )

    server = EcflowServer(config, start_command=args.start_command)
    defs = get_suite(suite_def, config)
    def_file = f"{suite_name}.def"
    defs.save_as_defs(def_file)

    # Clean, then copy troika and containers
    srv = f"{ecf_remoteuser}@{ecf_host}"
    src = f"{ecf_files_local}/{suite_name}"
    dst = f"{srv}:{ecf_files_remotely}/"

    if ecf_files_local != ecf_files_remotely:
        logger.info("--- SSL protocol for remote Ecflow server detected ---")
        logger.info("--- Copying job files to remote server ---")
        logger.info("Copy ecflow files from : {} to: {}", src, dst)

        # Clean command
        del_cmd = f"rm -rf {ecf_files_remotely}/{suite_name}"

        # Copy command
        copy_cmd = [
                "rsync",
                "-az",
                src,
                dst,
                ]

        # Try cleaning and copying commands. If it fails, then stop with message
        if ssh_cmd(ecf_host, ecf_remoteuser, del_cmd):
            logger.info("SSH command successful.")
        else:
            logger.info("Failed to execute SSH command.")

        try:
            subprocess.run(copy_cmd, check=True)  # noqa
            logger.info("Files transferred successfully.")
        except subprocess.CalledProcessError as e:
            logger.info(f"Error occurred: {e}")
            raise SystemExit("Copying ecf files to ecflow server FAILED.") from e

        # Read and parse the troika config file
        cfg = {"host": ecf_host}
        ssh = SSHConnection(cfg, ecf_remoteuser)

        temp_troika_config_file = f"parsed_{os.path.basename(troika_config_file)}"
        with open(troika_config_file, "rb") as infile:
            troika_input = yaml.safe_load(infile)
        troika_output = platform.sub_str_dict(troika_input)
        with open(temp_troika_config_file, mode="w", encoding="utf8") as outfile:
            yaml.dump(troika_output, outfile, encoding="utf-8")

        # Use ssh for single files (troika)
        try:
            ssh.sendfile(temp_troika_config_file, remote_troika_config_file)
            logger.info("Troika file transferred successfully.")
        except subprocess.CalledProcessError as e:
            logger.info(f"Error occurred transferring Troika: {e}")
            raise SystemExit("Copying {temp_troika_config_file} FAILED.") from e
        logger.info("--- File copying to Ecflow server DONE ---")

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

def namelist_convert(args, config):
    """Implement the 'namelist convert' command.

    Args:
        args (argparse.Namespace): Parsed command line arguments.
        config (.config_parser.ParsedConfig): Parsed config file contents.

    """
    
    #Configuration 
    #Check that parameters are present
    for parameter, parameter_name in zip([args.from_cycle, args.to_cycle, args.namelist,args.output],
                                         ["from_cycle", "to_cycle", "namelist", "output"]):
        if not parameter: 
            raise SystemExit("Please provide parameter {parameter_name}")
   
    #Convert namelists     
    logger.info(f'Convert namelist from cycle {args.from_cycle} to cycle {args.to_cycle}')        
    if args.format == "yaml":
        NamelistConverter.convert_yml(args.namelist, args.output, args.from_cycle, args.to_cycle)
    elif args.format == "ftn":
        NamelistConverter.convert_ftn(args.namelist, args.output, args.from_cycle, args.to_cycle)
    else:
        raise SystemExit(f"Format {args.format} not handled")