"""Clean deode file systems."""

import os
import re

from .datetime_utils import as_datetime, as_timedelta
from .logs import logger
from .os_utils import Search, remove_empty_dirs
from .toolbox import Platform


class CleanDeode:
    """Clean data."""

    def __init__(self, config, defaults=None, basetime=None):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            defaults (dict): Default cleaning settings
            basetime (dateTime object): Reference time

        """
        self.defaults = {} if defaults is None else defaults
        self.basetime = (
            as_datetime(config["general.times.basetime"])
            if basetime is None
            else basetime
        )
        self.cycle_length = as_timedelta(config["general.times.cycle_length"])
        self.platform = Platform(config)

    def _set_defaults(self, choice):
        """Copy default settings.

        Args:
            choice (dict): Dict with cleaning settings

        Returns:
            x (dict): Updated cleaning dict including default settings

        """
        x = choice.copy()
        for k, v in self.defaults.items():
            if k not in choice:
                x[k] = v

        return x

    def _check_delay(self, delay, tag):
        """Check if that a value is a multiple of cycle_length.

        Args:
            delay (timeDelta object): Timedelta to check
            tag (str): Name for error message

        Raises:
            RuntimeError: If not divisible by cycle_length

        """
        if delay % self.cycle_length != as_timedelta("PT0H"):
            logger.error(f"{tag} must be a multiple of cycle_length")
            logger.error(f"{tag}:{delay}")
            logger.error(f"cycle_length:{self.cycle_length}")
            raise RuntimeError

    def prep_cleaning(self, choices, basetime=None):
        """Prepare tasks for cleaning.

        Args:
            choices (dict): Dict with cleaning settings
            basetime (dateTime object): Reference time

        Raises:
            RuntimeError: If erroneous choices

        """
        if basetime is not None:
            self.basetime = basetime

        self.clean_tasks = []
        for _choice in choices.values():
            choice = self._set_defaults(_choice)
            if choice["active"]:
                # Check consistency of settings

                if "cleaning_delay" in choice and "ncycles_delay" in choice:
                    raise RuntimeError("Define either ncycles_delay or cleaning_delay")

                if "cleaning_delay" not in choice:
                    choice["cleaning_delay"] = choice["ncycles_delay"] * self.cycle_length
                else:
                    choice["cleaning_delay"] = as_timedelta(choice["cleaning_delay"])
                self._check_delay(choice["cleaning_delay"], "cleaning_delay")

                if "cleaning_max_delay" not in choice:
                    choice["cleaning_max_delay"] = choice["cleaning_delay"]
                else:
                    choice["cleaning_max_delay"] = as_timedelta(
                        choice["cleaning_max_delay"]
                    )
                    self._check_delay(choice["cleaning_max_delay"], "cleaning_max_delay")

                if "step" not in choice:
                    choice["step"] = self.cycle_length
                else:
                    choice["step"] = as_timedelta(choice["step"])
                    self._check_delay(choice["step"], "step")

                self.clean_tasks.append(choice)

    def clean(self):
        """Perform the cleaning."""
        for choice in self.clean_tasks:
            logger.info("Cleaning choice:{}", choice)

            basetime = self.basetime - choice["cleaning_delay"]
            endtime = self.basetime - choice["cleaning_max_delay"]
            dry_run = choice["dry_run"]

            while basetime >= endtime:
                inp = self.platform.substitute(choice["path"], basetime=basetime)
                pattern = (
                    [choice["include"]]
                    if isinstance(choice["include"], str)
                    else choice["include"]
                )

                do_exclude = "exclude" in choice
                if do_exclude:
                    exclude = re.compile(choice["exclude"])

                logger.info("Clean path:{}", inp)
                for ptrn in pattern:
                    files = Search.find_files(inp, recursive=True, pattern=ptrn)
                    for filename in files:
                        if do_exclude and exclude.match(filename):
                            logger.info("Exclude:{}", filename)
                            continue
                        if dry_run:
                            logger.info("Would have removed:{}", filename)
                        else:
                            logger.info("Remove :{}", filename)
                            os.remove(filename)

                # Remove possible empty directories
                remove_empty_dirs(inp, dry_run=dry_run)

                basetime -= choice["step"]
