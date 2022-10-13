"""Default ecflow container."""
import time

from deode.logs import get_logger
from deode.scheduler import EcflowClient, EcflowServer, EcflowTask


def parse_ecflow_vars():
    """Parse the ecflow variables."""
    print("%ECF_KILL_CMD%")
    print("%ECF_STATUS_CMD%")
    return {
        "HOST": "@HOST_TO_BE_SUBSTITUTED@",
        "WRAPPER": "@WRAPPER_TO_BE_SUBSTITUTED@",
        "ECF_HOST": "%ECF_HOST%",
        "ECF_PORT": "%ECF_PORT%",
        "ECF_NAME": "%ECF_NAME%",
        "ECF_PASS": "%ECF_PASS%",
        "ECF_TRYNO": "%ECF_TRYNO%",
        "ECF_RID": "%ECF_RID%",
        "SUBMISSION_ID": "%SUBMISSION_ID%",
        "SUBMISSION_FILE": "%SUBMISSION_FILE%",
        "SERVER_LOGFILE": "%SERVER_LOGFILE%",
        "LOGLEVEL": "%LOGLEVEL%",
    }


"""
%nopp"
"""


def default_main(**kwargs):
    """Ecflow container default method."""
    loglevel = kwargs.get("LOGLEVEL")
    logger = get_logger(__name__, loglevel)

    ecf_host = kwargs.get("ECF_HOST")
    ecf_port = kwargs.get("ECF_PORT")
    server_logfile = kwargs.get("SERVER_LOGFILE")
    server = EcflowServer(ecf_host, ecf_port, server_logfile)

    ecf_name = kwargs.get("ECF_NAME")
    ecf_pass = kwargs.get("ECF_PASS")
    ecf_tryno = kwargs.get("ECF_TRYNO")
    ecf_rid = kwargs.get("ECF_RID")
    submission_id = kwargs.get("SUBMISSION_ID")
    task = EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)

    # This will also handle call to sys.exit(), i.e. Client.__exit__ will still be called.
    with EcflowClient(server, task):

        logger.info("Running task %s", ecf_name)


if __name__ == "__main__":
    # Get ecflow variables
    kwargs_main = parse_ecflow_vars()
    default_main(**kwargs_main)

"""
%end"
"""
