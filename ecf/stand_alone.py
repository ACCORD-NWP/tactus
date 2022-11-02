"""NoSchedulerTemplate."""

from deode.logs import get_logger
from deode.discover_task import get_task
#@ENV_SUB@


def default_main(task, config, loglevel):
    """_summary_.

    Args:
        task (_type_): _description_
        config (_type_): _description_
        loglevel (_type_): _description_
    """
    logger = get_logger(__name__, loglevel)
    logger.info("Running task %s", task)
    get_task(task, config).run()
    logger.info("Finished task %s", task)


if __name__ == "__main__":
    task_name = "@STAND_ALONE_TASK_NAME@"
    loglevel = "@STAND_ALONE_TASK_LOGLEVEL@"
    config = "@STAND_ALONE_TASK_CONFIG@"
    default_main(task_name, config, loglevel)
