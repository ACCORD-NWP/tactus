#!/usr/bin/env python3
"""This script checks the status of ecflow suite.

test_worfklow_scripts/get_suite_status.py ECF_HOST ECF_PORT SUITE_NAME
input parameters:
ECF_HOST - ecflow server hostname,
ECF_PORT - port of the ecflow server,
SUITE_NAME - the name of the suite which status is checked

The suite is taken by its name from the ecflow server defs and
it is queried for its status.
The status check lasts maximum 3600 second / 1 hour.
Status check is done each 60 s / 1 minute

returns the exit code: 0 - suite status is "complete",
1 - status is aborted or the check time has lasted
"""

import sys
import time

import ecflow

ecf_host = sys.argv[1]
ecf_port = sys.argv[2]
suite_name = sys.argv[3]

try:
    ci = ecflow.Client(
        ecf_host, ecf_port
    )  # use default host(ECF_HOST) &amp; port(ECF_PORT)

    # Suite status
    COMPLETE_STATUS = "complete"
    ABORTED_STATUS = "aborted"

    DELAY = 60  # seconds
    TIMEOUT = 3600
    total_time = 0  # initialisation

    suite_state = ""
    while suite_state != COMPLETE_STATUS:
        sys.stdout.write("waiting time is {}\n".format(total_time))
        ci.sync_local()
        defs = ci.get_defs()
        suite = defs.find_suite(suite_name)

        suite_state = str(suite.get_state())
        sys.stdout.write("suite state={}\n".format(suite_state))
        if suite_state == ABORTED_STATUS:
            log_message = "suite {} is aborted\n".format(suite_name)
            sys.stdout.write(log_message)
            sys.exit(1)
        if total_time < TIMEOUT:
            time.sleep(DELAY)
            total_time = total_time + DELAY
        else:
            sys.stdout.write("Timeout is reached. Exiting with an error.\n")
            sys.exit(1)
except RuntimeError as e:
    sys.stdout.write(str(e))

exit_code = 1
if suite_state == COMPLETE_STATUS:
    exit_code = 0
# when suite is completed exit_code=0 otherwise exit_code=1
sys.stdout.write("exit code={}\n".format(exit_code))
sys.exit(exit_code)
