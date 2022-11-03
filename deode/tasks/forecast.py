"""Forecast."""

from .base import BinaryTask


class Forecast(BinaryTask):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.
        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Task name
        """
        print("Construct forecast task")
        BinaryTask.__init__(self, config, __name__)
