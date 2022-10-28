#!/home/sbu/.cache/pypoetry/virtualenvs/deode-lpWJ2U07-py3.8/bin/python3
#SBATCH --output=/home/sbu/projects/Deode-Prototype/forecast.log
"""Default ecflow container."""
import time

from deode.logs import get_logger
#@ENV_SUB@

"""
%nopp"
"""


def default_main(task, loglevel):

    logger = get_logger(__name__, loglevel)
    logger.info("Running task %s", task)


if __name__ == "__main__":
    # Get ecflow variables
    default_main("Forecast", "DEBUG")
"""
%end"
"""
