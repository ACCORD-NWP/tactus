"""Default ecflow container."""

import os
from typing import Dict

import ecflow as ecf

from deode.config_parser import ConfigParserDefaults, GeneralConstants, ParsedConfig
from deode.derived_variables import derived_variables
from deode.eps.eps_setup import get_member_config
from deode.host_actions import DeodeHost
from deode.logs import LogDefaults, LoggerHandlers, logger
from deode.scheduler import EcflowClient, EcflowServer, EcflowTask
from deode.submission import ProcessorLayout
from deode.tasks.discover_task import get_task

logger.enable(GeneralConstants.PACKAGE_NAME)


def query_ecflow_variable(client: ecf.Client, ecf_name: str, variable: str):
    """Query ecflow variable from client.

    Args:
        client (ecf.Client): ecflow client.
        ecf_name (str): ecflow name, i.e. path to ecFlow node.
        variable (str): variable to query.

    Returns:
        str or None: variable value if variable exists on client.
    """
    try:
        return client.query("variable", ecf_name, variable)
    except RuntimeError:
        logger.warning("Could not find variable {} in ecflow", variable)
        return None


def parse_ecflow_vars() -> Dict[str, str]:
    """Parse the ecflow variables."""
    ecf_host = os.environ["ECF_HOST"]
    ecf_port = os.environ["ECF_PORT"]
    ecf_name = os.environ["ECF_NAME"]
    client = ecf.Client(f"{ecf_host}:{ecf_port}")

    ecflow_vars = {
        "ECF_HOST": ecf_host,
        "ECF_PORT": ecf_port,
        "ECF_NAME": ecf_name,
        "ECF_PASS": query_ecflow_variable(client, ecf_name, "ECF_PASS"),
        "ECF_TRYNO": query_ecflow_variable(client, ecf_name, "ECF_TRYNO"),
        "ECF_RID": query_ecflow_variable(client, ecf_name, "ECF_RID"),
        "ECF_TIMEOUT": query_ecflow_variable(client, ecf_name, "ECF_TIMEOUT"),
        "BASETIME": query_ecflow_variable(client, ecf_name, "BASETIME"),
        "VALIDTIME": query_ecflow_variable(client, ecf_name, "VALIDTIME"),
        "LOGLEVEL": query_ecflow_variable(client, ecf_name, "LOGLEVEL"),
        "ARGS": query_ecflow_variable(client, ecf_name, "ARGS"),
        "WRAPPER": query_ecflow_variable(client, ecf_name, "WRAPPER"),
        "NPROC": query_ecflow_variable(client, ecf_name, "NPROC"),
        "NPROC_IO": query_ecflow_variable(client, ecf_name, "NPROC_IO"),
        "NPROCX": query_ecflow_variable(client, ecf_name, "NPROCX"),
        "NPROCY": query_ecflow_variable(client, ecf_name, "NPROCY"),
        "CONFIG": query_ecflow_variable(client, ecf_name, "CONFIG"),
        "DEODE_HOME": query_ecflow_variable(client, ecf_name, "DEODE_HOME"),
        "KEEP_WORKDIRS": query_ecflow_variable(client, ecf_name, "KEEP_WORKDIRS"),
        "MEMBER": query_ecflow_variable(client, ecf_name, "MEMBER"),
    }

    return ecflow_vars


def default_main(kwargs: dict):
    """Ecflow container default method."""
    config_file = kwargs.get("CONFIG")
    deode_host = DeodeHost().detect_deode_host()
    config = ParsedConfig.from_file(
        config_file,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        host=deode_host,
    )

    # Reset loglevel according to (in order of priority):
    #     (a) Configs in ECFLOW UI
    #     (b) What was originally set in the config file
    #     (c) The default `LogDefaults.level` if none of the above is found.
    loglevel = kwargs.get("LOGLEVEL", config.get("loglevel", LogDefaults.LEVEL)).upper()
    logger.configure(handlers=LoggerHandlers(default_level=loglevel))
    logger.info("Read config from {}", config_file)

    args = kwargs.get("ARGS")
    args_dict = {}
    if args != "":
        for arg in args.split(";"):
            parts = arg.split("=")
            if len(parts) == 2:
                args_dict.update({parts[0]: parts[1]})
            else:
                logger.debug("Could not convert ARGS:{} to dict, skip it", arg)

    # Update config based on ecflow settings
    config = config.copy(
        update={
            "submission": {"task": {"wrapper": kwargs.get("WRAPPER")}},
            "task": {"args": args_dict},
            "general": {
                "times": {
                    "validtime": kwargs.get("VALIDTIME"),
                    "basetime": kwargs.get("BASETIME"),
                },
                "keep_workdirs": bool(int(kwargs.get("KEEP_WORKDIRS"))),
                "loglevel": loglevel,
            },
            "platform": {"deode_home": kwargs.get("DEODE_HOME")},
        }
    )

    ecf_name = kwargs.get("ECF_NAME")
    ecf_pass = kwargs.get("ECF_PASS")
    ecf_tryno = kwargs.get("ECF_TRYNO")
    ecf_rid = kwargs.get("ECF_RID")
    ecf_timeout = kwargs.get("ECF_TIMEOUT")
    task = EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, ecf_timeout=ecf_timeout)

    # Get member number
    member = kwargs.get("MEMBER")
    # If member is not an integer, skip EPS setup
    try:
        member = int(member)
    except TypeError:
        logger.debug("MEMBER is not an integer, skipping eps setup for task {}", task)
    else:
        # Update config based on member
        config = get_member_config(config, member=member)

    # TODO Add wrapper
    server = EcflowServer(config)
    # This will also handle call to sys.exit(), i.e. Client.__exit__ will still be called.
    with EcflowClient(server, task):
        processor_layout = ProcessorLayout(kwargs)
        update = derived_variables(config, processor_layout=processor_layout)
        config = config.copy(update=update)

        # TODO Add wrapper to config
        logger.info("Running task {}", task.ecf_name)
        get_task(task.ecf_task, config).run()
        logger.info("Finished task {}", task.ecf_name)


if __name__ == "__main__":
    logger.info("Running {} v{}", GeneralConstants.PACKAGE_NAME, GeneralConstants.VERSION)
    # Get ecflow variables
    kwargs_main = parse_ecflow_vars()
    default_main(kwargs_main)
