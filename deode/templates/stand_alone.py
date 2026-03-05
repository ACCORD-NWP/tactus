"""NoSchedulerTemplate."""

import os

from deode.config_parser import ConfigParserDefaults, GeneralConstants, ParsedConfig
from deode.derived_variables import derived_variables, set_times
from deode.eps.eps_setup import get_member_config
from deode.host_actions import DeodeHost
from deode.logs import logger  # Use deode's own configs for logger
from deode.submission import ProcessorLayout, TaskSettings
from deode.tasks.discover_task import get_task

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
    member_info = ""
    if config.get("general.use_member_stand_alone", True):
        try:
            member = int(config["general.member"])
        except (TypeError, ValueError):
            logger.debug("MEMBER is not an integer, skipping eps setup for task {}", task)
        else:
            # Update config based on member
            logger.info("Setup EPS")
            config = get_member_config(config, member=member)
            member_info = f" for member {member}"

    config = config.copy(update=set_times(config))
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    task_settings = TaskSettings(config).get_task_settings(task)
    processor_layout = ProcessorLayout(task_settings)
    update = derived_variables(config, processor_layout=processor_layout)
    config = config.copy(update=update)

    logger.info("Running {}{}", task, member_info)
    get_task(task, config).run()
    logger.info("Finished {}{}", task, member_info)


if __name__ == "__main__":
    logger.info("Running {} v{}", GeneralConstants.PACKAGE_NAME, GeneralConstants.VERSION)
    default_main(
        task=os.environ["STAND_ALONE_TASK_NAME"],
        config_file=os.environ["STAND_ALONE_TASK_CONFIG"],
        deode_home=os.environ["STAND_ALONE_DEODE_HOME"],
    )
