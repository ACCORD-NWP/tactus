"""NoSchedulerTemplate."""

from deode.config_parser import ParsedConfig
from deode.discover_task import get_task
from deode.logs import get_logger_from_config
from os import environ  # noqa
# @ENV_SUB@


def default_main(task, config, deode_home):
    """Execute default main.

    Args:
        task (str): Task name
        config (str): Config file
        deode_home(str): Deode home path
    """
    config = ParsedConfig.from_file(config)
    config = config.copy(
        update={
            "platform": {
                "deode_home": deode_home
            }
        }
    )
    logger = get_logger_from_config(config)
    logger.info("Running task %s", task)
    get_task(task, config).run()
    logger.info("Finished task %s", task)


if __name__ == "__main__":
    TASK_NAME = "@STAND_ALONE_TASK_NAME@"
    CONFIG = "@STAND_ALONE_TASK_CONFIG@"
    DEODE_HOME = "@STAND_ALONE_DEODE_HOME@"
    default_main(TASK_NAME, CONFIG, DEODE_HOME)
