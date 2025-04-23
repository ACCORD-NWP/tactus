"""Clean deode file systems."""

from pathlib import Path

from deode.cleaning import CleanDeode
from deode.config_parser import ParsedConfig
from deode.os_utils import deodemakedirs
from deode.tasks.base import Task


class Cleaning(Task):
    """Interface class to the cleaning."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Name of task
        """
        Task.__init__(self, config, __class__.__name__)

    def prep_clean_task(self, cleaning_type):
        """Setup clean task.

        Args:
            cleaning_type (str): Cleaning config section identifier

        """
        defaults = self.config.get("cleaning.defaults")
        choices = self.config.get(f"cleaning.{cleaning_type}").dict()
        self.cleaner = CleanDeode(self.config, defaults)
        self.cleaner.prep_cleaning(choices)

    def execute(self):
        """Run the cleaning."""
        self.cleaner.clean()


class PreCleaning(Cleaning):
    """Preparatory cleaning task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Cleaning.__init__(self, config)
        self.name = "PreCleaning"
        self.prep_clean_task(self.name)

        # Create (parsed) config file
        parsed_config = config.dict()
        parsed_config["general"]["times"].pop("basetime")
        parsed_config["general"]["times"].pop("validtime")

        parsed_config = ParsedConfig(parsed_config, json_schema={}).expand_macros(True)
        archive_root = Path(parsed_config["platform.archive_root"])
        deodemakedirs(archive_root, unixgroup=parsed_config["platform.unix_group"])
        parsed_config.save_as(archive_root / "parsed_config.toml")
        config.save_as(archive_root / "config.toml")


class CycleCleaning(Cleaning):
    """Cycle cleaning task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Cleaning.__init__(self, config)
        self.name = "CycleCleaning"
        self.prep_clean_task(self.name)


class PostMortem(Cleaning):
    """Final cleaning task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Cleaning.__init__(self, config)
        self.name = "PostMortem"
        self.prep_clean_task(self.name)
