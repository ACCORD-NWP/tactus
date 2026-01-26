#!/usr/bin/env python3
"""This script takes one commnad line input parameter - tactus configuration file.

poetry run test_worfklow_scripts/export_case_variables.py config_file

Takes ECF_HOST, ECF_PORT and SUITE_NAME values  from the configuration file
and prepares export bash commnad, which is printed as a string output
"""

import sys

from tactus.config_parser import ConfigParserDefaults, ParsedConfig
from tactus.derived_variables import set_times
from tactus.host_actions import DeodeHost, SelectHost
from tactus.toolbox import Platform

config_file = sys.argv[1]

tactus_host = DeodeHost().detect_tactus_host()

config = ParsedConfig.from_file(
    config_file, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA, host=tactus_host
)

ecflow_port = Platform(config).substitute(config["scheduler.ecfvars.ecf_port"])
ecf_user = Platform(config).substitute(config["scheduler.ecfvars.ecf_user"])
ecflow_host = Platform(config).substitute(config.get("scheduler.ecfvars.ecf_host"))
ecflow_host = Platform(config).evaluate(ecflow_host, object_=SelectHost)


config = config.copy(update=set_times(config))
suite_name = config["general.case"]

suite_name = Platform(config).substitute(suite_name)

# this writes the export clause to the standart output so the command will
# be assigned to bash variable.
sys.stdout.write(
    "export ECF_HOST={} ECF_PORT={}  SUITE_NAME={} ".format(
        ecflow_host, ecflow_port, suite_name
    )
)
