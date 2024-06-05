"""Clean deode file systems."""

import os

from ..datetime_utils import as_datetime, as_timedelta
from ..logs import logger
from ..os_utils import Search
from .base import Task


class CleanDeode(Task):
    """Clean data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

    def trigger(self, trigger):
        """Return trigger."""
        if isinstance(trigger, bool):
            return trigger
        return self.config[trigger]

    def set_defaults(self, choice):
        """Set default values."""
        x = choice.copy()
        for k, v in self.defaults.items():
            if k not in choice:
                x[k] = v

        return x

    def clean(self, choice):
        """Send files to the file manager.

        Args:
            choice (dict): Full path on the input archive

        """
        logger.info("Choice:{}", choice)

        basetime = self.basetime - as_timedelta(choice["cleaning_delay"])
        endtime = self.basetime - as_timedelta(choice["cleaning_max_delay"])
        dry_run = choice["dry_run"]

        logger.info("basetime: {}", basetime)
        logger.info("endtime: {}", endtime)

        while basetime >= endtime:
            logger.info("basetime: {}", basetime)

            inp = self.platform.substitute(choice["path"], basetime=basetime)
            pattern = (
                [choice["include"]]
                if isinstance(choice["include"], str)
                else choice["include"]
            )

            logger.info("cleaning file pattern: {}", pattern)
            logger.info("Treat path:{}", inp)
            for ptrn in pattern:
                files = Search.find_files(inp, recursive=True, pattern=ptrn)

                for filename in files:
                    if dry_run:
                        logger.info("Dry Run: Would have removed file: {}", filename)
                    else:
                        os.remove(filename)
                        logger.info("Removed file: {}", filename)

            basetime -= as_timedelta(choice["step"])


class Cleaning(CleanDeode):
    """Archving task for time dependent data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanDeode.__init__(self, config)

        self.choices = self.config.get("cleaning").dict()
        self.choices.pop("defaults")
        self.defaults = self.config.get("cleaning.defaults")
        self.basetime = as_datetime(self.config["general.times.basetime"])

        logger.info("Cleaning Basetime: {}", self.basetime)
        logger.info("Cleaning defaults: {}", self.defaults)

    def execute(self):
        """Run task.

        Define run sequence.

        """
        for name, choice in self.choices.items():
            if self.trigger(choice["active"]):
                logger.info("\n")
                logger.info("Handle {}", name)
                self.clean(self.set_defaults(choice))
