"""Default ecflow container."""
from deode.config_parser import ParsedConfig
from deode.derived_variables import derived_variables
from deode.logs import get_logger_from_config
from deode.scheduler import EcflowClient, EcflowServer, EcflowTask
from deode.submission import ProcessorLayout
from deode.tasks.discover_task import get_task

# @ENV_SUB@


def parse_ecflow_vars():
    """Parse the ecflow variables."""
    print("%ECF_KILL_CMD%")
    print("%ECF_STATUS_CMD%")
    return {
        "ECF_HOST": "%ECF_HOST%",
        "ECF_PORT": "%ECF_PORT%",
        "ECF_NAME": "%ECF_NAME%",
        "ECF_PASS": "%ECF_PASS%",
        "ECF_TRYNO": "%ECF_TRYNO%",
        "ECF_RID": "%ECF_RID%",
        "ECF_TIMEOUT": "%ECF_TIMEOUT%",
        "BASETIME": "%BASETIME%",
        "VALIDTIME": "%VALIDTIME%",
        "LOGLEVEL": "%LOGLEVEL%",
        "ARGS": "%ARGS%",
        "WRAPPER": "%WRAPPER%",
        "NPROC": "%NPROC%",
        "NPROC_IO": "%NPROC_IO%",
        "NPROCX": "%NPROCX%",
        "NPROCY": "%NPROCY%",
        "CONFIG": "%CONFIG%",
        "DEODE_HOME": "%DEODE_HOME%",
        "KEEP_WORKDIRS": "%KEEP_WORKDIRS%",
    }


"""
%nopp"
"""


def default_main(**kwargs):
    """Ecflow container default method."""
    config = kwargs.get("CONFIG")
    config = ParsedConfig.from_file(config)
    logger = get_logger_from_config(config)

    args = kwargs.get("ARGS")
    args_dict = {}
    if args != "":
        for arg in args.split(";"):
            parts = arg.split("=")
            if len(parts) == 2:
                args_dict.update({parts[0]: parts[1]})
            else:
                logger.warning("Could not convert ARGS:%s to dict, skip it", arg)

    # How to update config based on ecflow settings when config is assumed to be immutable
    # config["general"].update({"loglevel": loglevel})  # noqa
    config = config.copy(
        update={
            "task": {"args": args_dict, "wrapper": kwargs.get("WRAPPER")},
            "general": {
                "loglevel": kwargs.get("LOGLEVEL"),
                "times": {
                    "validtime": kwargs.get("VALIDTIME"),
                    "basetime": kwargs.get("BASETIME"),
                },
                "keep_workdirs": bool(int(kwargs.get("KEEP_WORKDIRS"))),
            },
        }
    )
    logger = get_logger_from_config(config)

    # TODO Add wrapper  # noqa
    ecf_host = kwargs.get("ECF_HOST")
    ecf_port = kwargs.get("ECF_PORT")
    server = EcflowServer(ecf_host, ecf_port)

    ecf_name = kwargs.get("ECF_NAME")
    ecf_pass = kwargs.get("ECF_PASS")
    ecf_tryno = kwargs.get("ECF_TRYNO")
    ecf_rid = kwargs.get("ECF_RID")
    ecf_timeout = kwargs.get("ECF_TIMEOUT")
    task = EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, ecf_timeout=ecf_timeout)

    # This will also handle call to sys.exit(), i.e. Client.__exit__ will still be called.
    with EcflowClient(server, task):

        processor_layout = ProcessorLayout(kwargs)
        update = derived_variables(config, processor_layout=processor_layout)
        config = config.copy(update=update)

        # TODO Add wrapper to config
        logger.info("Running task %s", task.ecf_name)
        get_task(task.ecf_task, config).run()
        logger.info("Finished task %s", task.ecf_name)


if __name__ == "__main__":
    # Get ecflow variables
    kwargs_main = parse_ecflow_vars()
    default_main(**kwargs_main)

"""    # noqa
%end"  # noqa
"""  # noqa
