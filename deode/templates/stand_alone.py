"""NoSchedulerTemplate."""

from os import environ  # noqa

from deode.config_parser import ParsedConfig
from deode.derived_variables import derived_variables
from deode.logs import get_logger_from_config
from deode.submission import ProcessorLayout, TaskSettings
from deode.tasks.discover_task import get_task

# @ENV_SUB@


def default_main(task, config, deode_home):
    """Execute default main.

    Args:
        task (str): Task name
        config (str): Config file
        deode_home(str): Deode home path
    """
    config = ParsedConfig.from_file(config)
    config = config.copy(update={"platform": {"deode_home": deode_home}})

    task_settings = TaskSettings(config).get_task_settings(task)
    processor_layout = ProcessorLayout(task_settings)
    update = derived_variables(config, processor_layout=processor_layout)
    config = config.copy(update=update)

    logger = get_logger_from_config(config)
    logger.info("Running task %s", task)
    get_task(task, config).run()
    logger.info("Finished task %s", task)


if __name__ == "__main__":
    TASK_NAME = "@STAND_ALONE_TASK_NAME@"
    CONFIG = "@STAND_ALONE_TASK_CONFIG@"
    DEODE_HOME = "@STAND_ALONE_DEODE_HOME@"
    default_main(TASK_NAME, CONFIG, DEODE_HOME)
