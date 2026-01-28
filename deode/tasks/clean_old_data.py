"""Clean old data."""

import os
import shutil
from argparse import Namespace
from datetime import datetime

from ..commands_functions import remove_cases
from ..datetime_utils import as_timedelta
from ..logs import logger
from ..scheduler import EcflowServer
from .base import Task


class CleanOldData(Task):
    """Interface class to the cleaning of the old data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Name of task
        """
        Task.__init__(self, config, __class__.__name__)
        self.now = datetime.now()
        self.force_delete_time = self.now - as_timedelta(
            config["clean_old_data.force_delete_period"]
        )
        self.scratch = (
            self.platform.get_platform_value("scratch")
            + config["clean_old_data.scratch_ext"]
        )
        self.ecf_jobout = self.platform.get_value("scheduler.ecfvars.ecf_jobout")
        self.ecf_files = self.platform.get_value("scheduler.ecfvars.ecf_files")

    def cutoff(self, delay):
        """Calculates the time to determine the old data.

        Args:
            delay (int): delay in days

        Returns:
            cutoff (int): time in seconds
        """
        cutoff = self.now - delay
        return cutoff.timestamp()

    def find_top_empty_directories(self, path):
        """Find highest-level directories whose subtree contains only empty directories.

        Args:
            path (str): Root directory to search.

        Returns:
            List[str]: Full paths to the highest-level empty directory trees.
        """
        if not path:
            return []
        empty_tree_dirs = set()

        for root, dirs, files in os.walk(path, topdown=False):
            if not files and (
                not dirs or all(os.path.join(root, d) in empty_tree_dirs for d in dirs)
            ):
                empty_tree_dirs.add(root)

        result = []
        for d in empty_tree_dirs:
            parent = os.path.dirname(d)
            if parent not in empty_tree_dirs:
                result.append(d)

        return sorted(result)

    def remove_list(self, dir_list, files=False):
        """Remove directories/files from the list.

        Args:
            dir_list    (list): Dictionary of directories/files
            files     (boolean): If in list are files, Default False
        """
        for dir_file in dir_list:
            if files:
                os.remove(dir_file)
            else:
                shutil.rmtree(dir_file)
            logger.info("Removing {}", dir_file)


class CleanCases(CleanOldData):
    """Clean old data from scratch."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanCases"
        self.delay = as_timedelta(config["clean_old_data.cases_period"])
        self.cutoff_time = self.cutoff(self.delay)
        ignore = list(config["clean_old_data.ignore"])
        self.dw_case_prefixes = list(config["clean_old_data.dw_case_prefixes"])
        self.ignore_suite = [*ignore]
        logger.info("Ignore: {}", ignore)

    def execute(self):
        """Run clean cases."""
        # Get suites
        server = EcflowServer(self.config)
        suites = server.get_suites_from_server(ignore=self.ignore_suite, complete=True)
        case_configs = []

        for suite in suites:
            finish_time = server.suite_finish_time(
                suite, self.force_delete_time, last_task_name="PostMortem"
            )
            if finish_time == self.force_delete_time.timestamp():
                logger.info("There are no ecf files for suite {}", suite.name())
            if finish_time < self.cutoff_time:
                # get config file of suite
                logger.info("suite {}", suite.name())
                if suite.name().startswith(tuple(self.dw_case_prefixes)):
                    try:
                        case_config = server.get_config_of_suite(suite)
                        case_configs.append(case_config)
                    except FileNotFoundError:
                        logger.warning("Config for suite {} does not exist", suite.name())
                else:
                    logger.warning("{} is not DW case", suite.name())

        args = Namespace(
            config_files=case_configs,
            dry_run=self.config["remove.defaults.dry_run"],
            force_remove=False,
            execute_removal=self.config["remove.defaults.execute_removal"],
        )

        remove_cases(args, self.config)


class CleanEmptyDirectories(CleanOldData):
    """Clean empty dirctories."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanEmpryDirectories"

        self.ehype_work = self.platform.get_value("impact.ehype.communicate.work_root")

    def execute(self):
        """Run clean empty directories."""
        self.remove_list(self.find_top_empty_directories(self.scratch))
        self.remove_list(self.find_top_empty_directories(self.ecf_jobout))
        self.remove_list(self.find_top_empty_directories(self.ecf_files))
        try:
            self.remove_list(self.find_top_empty_directories(self.ehype_work))
        except KeyError:
            logger.warning("Switch of ehype cleaning as config is incomplete")
