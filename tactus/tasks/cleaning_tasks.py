"""Clean tactus file systems."""

from tactus.cleaning import CleanTactus
from tactus.tasks.base import Task


class Cleaning(Task):
    """Interface class to the cleaning."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)
        cleaning_type = config["task.args.cleaning_type"]
        self.name = cleaning_type
        defaults = self.config.get("cleaning.defaults")
        choices = self.config.get(f"cleaning.{cleaning_type}").dict()
        self.cleaner = CleanTactus(self.config, defaults)
        self.cleaner.prep_cleaning(choices)

    def execute(self):
        """Run the cleaning."""
        self.cleaner.clean()
