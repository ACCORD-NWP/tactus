"""Serial task."""

from ..logs import logger
from .base import Task


class Serial(Task):
    """Serial task."""

    def __init__(self, config):
        """Construct serial task.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        logger.info("Construct serial task")
        Task.__init__(self, config, __class__.__name__)

    def execute(self):
        """Execute task."""
        logger.info("Run serial")
        logger.info("Config: {}", self.config)
