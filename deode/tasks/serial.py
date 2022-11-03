"""Serial task."""

from .base import Task


class Serial(Task):
    """Serial task."""

    def __init__(self, config):
        """Construct serial task.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        print("Construct serial task")
        Task.__init__(self, config, __name__)

    def execute(self):
        """Execute task."""
        print("Run serial")
        print("Config: ", self.config)
