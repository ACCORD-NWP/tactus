"""Base site class."""


class Task(object):
    """Base Task class."""

    def __init__(self, config):
        """Construct base task.

        Args:
            config (_type_): _description_
        """
        self.config = config
        print("Base task")

    def run(self):
        """Run the task."""
        pass
