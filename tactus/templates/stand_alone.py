"""NoSchedulerTemplate."""

import os

from tactus.config_parser import ConfigParserDefaults, GeneralConstants, ParsedConfig
from tactus.derived_variables import derived_variables, set_times
from tactus.eps.eps_setup import get_member_config
from tactus.host_actions import DeodeHost
from tactus.logs import logger  # Use deode's own configs for logger
from tactus.submission import ProcessorLayout, TaskSettings
from tactus.tasks.discover_task import get_task

logger.enable("deode")


def default_main(task: str, config_file: str, deode_home: str):
    """Execute default main.

    Args:
        task (str): Task name
        config_file (str): Config file
        deode_home(str): Deode home path
    """
    deode_host = DeodeHost().detect_deode_host()
    logger.info("Read config from {}", config_file)
    config = ParsedConfig.from_file(
        config_file,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        host=deode_host,
    )
    # Get eps member specific config if a member is specified
    try:
        member = int(config["general.member"])
    except (TypeError, ValueError):
        logger.debug("MEMBER is not an integer, skipping eps setup for task {}", task)
    else:
        # Update config based on member
        logger.info("Setup EPS")
        config = get_member_config(config, member=member)

    config = config.copy(update=set_times(config))
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    task_settings = TaskSettings(config).get_task_settings(task)
    processor_layout = ProcessorLayout(task_settings)
    update = derived_variables(config, processor_layout=processor_layout)
    config = config.copy(update=update)

    common_log_string = f"task {task}" + f" for member {member}" if member else ""
    logger.info("Running {}", common_log_string)
    get_task(task, config).run()
    logger.info("Finished {}", common_log_string)


if __name__ == "__main__":
    logger.info("Running {} v{}", GeneralConstants.PACKAGE_NAME, GeneralConstants.VERSION)
    default_main(
        task=os.environ["STAND_ALONE_TASK_NAME"],
        config_file=os.environ["STAND_ALONE_TASK_CONFIG"],
        deode_home=os.environ["STAND_ALONE_DEODE_HOME"],
    )
