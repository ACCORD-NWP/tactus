"""NoSchedulerTemplate."""

import os
from typing import Optional

from deode.config_parser import ConfigParserDefaults, GeneralConstants, ParsedConfig
from deode.derived_variables import derived_variables, set_times
from deode.eps.eps_setup import get_member_config
from deode.host_actions import DeodeHost
from deode.logs import logger  # Use deode's own configs for logger
from deode.submission import ProcessorLayout, TaskSettings
from deode.tasks.discover_task import get_task

logger.enable("deode")


def default_main(
    task: str, config_file: str, deode_home: str, member: Optional[int] = None
):
    """Execute default main.

    Args:
        task (str): Task name
        config_file (str): Config file
        deode_home(str): Deode home path
        member (Optional[int], optional): Member number for which to run the
            standalone task. Defaults to None.
    """
    deode_host = DeodeHost().detect_deode_host()
    logger.info("Read config from {}", config_file)
    config = ParsedConfig.from_file(
        config_file,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        host=deode_host,
    )
    if member is not None:
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
    member_ = os.environ.get("STAND_ALONE_MEMBER")
    default_main(
        task=os.environ["STAND_ALONE_TASK_NAME"],
        config_file=os.environ["STAND_ALONE_TASK_CONFIG"],
        deode_home=os.environ["STAND_ALONE_DEODE_HOME"],
        member=int(member_) if member_ is not None else None,
    )
