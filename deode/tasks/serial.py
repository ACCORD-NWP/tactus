"""Serial."""

from .base import Task


class Serial(Task):
    """Site where jobs are run directly"""

    def __init__(self, config):
        """_summary_

        Args:
            config (_type_): _description_
        """
        print("Construct serial task")
        Task.__init__(self, config)

    def run(self):
        print("Run serial")
        print("Config: ", self.config)
