"""Preparatory task for a deode suite."""

from pathlib import Path

from deode.config_parser import ParsedConfig
from deode.logs import logger
from deode.os_utils import deodemakedirs
from deode.tasks.cleaning_tasks import Cleaning


class PrepRun(Cleaning):
    """Preparatory run/cleaning task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Cleaning.__init__(self, config)
        self.name = "PrepRun"
        self.prep_clean_task(self.name)

        # Archive the used config file
        archive_root = Path(self.platform.get_platform_value("archive_root"))
        deodemakedirs(
            archive_root, unixgroup=self.platform.get_platform_value("unix_group")
        )
        archive_config = archive_root / "config.toml"
        config.save_as(archive_config)
        logger.info("Stored used config as: {}", archive_config)

        # Create and archive expanded config file
        expanded_config = self.config.dict()
        expanded_config["general"]["times"].pop("basetime")
        expanded_config["general"]["times"].pop("validtime")
        expanded_config = ParsedConfig(expanded_config, json_schema={}).expand_macros(
            True
        )
        archive_expanded_config = archive_root / "expanded_config.toml"
        expanded_config.save_as(archive_expanded_config)
        logger.info("Stored used expanded config as: {}", archive_expanded_config)
