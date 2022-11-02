"""Forecast."""

from .base import Task


class Forecast(Task):
    """Site where jobs are run directly."""

    def __init__(self, config):
        """_summary_.

        Args:
            config (_type_): _description_
        """
        print("Construct forecast task")
        Task.__init__(self, config)

    def run(self):
        """Run the forecast task."""
        print("Run forecast")
        print("Config: ", self.config)
