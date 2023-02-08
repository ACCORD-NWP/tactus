"""NoSchedulerTemplate."""

from deode.config_parser import ParsedConfig
from deode.discover_task import get_task
from deode.logs import get_logger_from_config

# @ENV_SUB@


def default_main(task, config):
    """Execute default main.

    Args:
        task (str): Task name
        config (str): Config file
    """
    config = ParsedConfig.from_file(config)
    logger = get_logger_from_config(config)
    logger.info("Running task %s", task)
    get_task(task, config).run()
    logger.info("Finished task %s", task)


if __name__ == "__main__":
    TASK_NAME = "@STAND_ALONE_TASK_NAME@"
    CONFIG = "@STAND_ALONE_TASK_CONFIG@"
    default_main(TASK_NAME, CONFIG)
