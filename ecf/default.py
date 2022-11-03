"""Default ecflow container."""
from deode.discover_task import get_task
from deode.logs import get_logger
from deode.config_parser import ParsedConfig
from deode.scheduler import EcflowClient, EcflowServer, EcflowTask

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
        "LOGLEVEL": "%LOGLEVEL%",
        "CONFIG": "%CONFIG%",
    }


"""
%nopp"
"""


def default_main(**kwargs):
    """Ecflow container default method."""
    loglevel = kwargs.get("LOGLEVEL")
    logger = get_logger(__name__, loglevel)

    config = kwargs.get("CONFIG")
    #TODO Add wrapper
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
        config = ParsedConfig.from_file(config)
        #TODO Add wrapper to config
        logger.info("Running task %s", task.ecf_name)
        get_task(task.ecf_task, config).run()
        logger.info("Finished task %s", task.ecf_name)


if __name__ == "__main__":
    # Get ecflow variables
    kwargs_main = parse_ecflow_vars()
    default_main(**kwargs_main)

"""
%end"
"""
