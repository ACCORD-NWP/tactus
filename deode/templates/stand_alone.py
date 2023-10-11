"""NoSchedulerTemplate."""

from os import environ  # noqa

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.derived_variables import derived_variables
from deode.logs import logger  # Use deode's own configs for logger
from deode.submission import ProcessorLayout, TaskSettings
from deode.tasks.discover_task import get_task

# @ENV_SUB@

logger.enable("deode")


def default_main(task, config, deode_home):
    """Execute default main.

    Args:
        task (str): Task name
        config (str): Config file
        deode_home(str): Deode home path
    """
    config = ParsedConfig.from_file(
        config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    task_settings = TaskSettings(config).get_task_settings(task)
    processor_layout = ProcessorLayout(task_settings)
    update = derived_variables(config, processor_layout=processor_layout)
    config = config.copy(update=update)

    logger.info("Running task {}", task)
    get_task(task, config).run()
    logger.info("Finished task {}", task)


if __name__ == "__main__":
    TASK_NAME = "@STAND_ALONE_TASK_NAME@"
    CONFIG = "@STAND_ALONE_TASK_CONFIG@"
    DEODE_HOME = "@STAND_ALONE_DEODE_HOME@"
    default_main(TASK_NAME, CONFIG, DEODE_HOME)
