"""Clean old data."""

import os
import shutil
from datetime import datetime

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

    def cutoff(self, delay):
        """Calculates the time to determine the old data.

        Args:
            delay (int): delay in days

        Returns:
            cutoff (int): time in seconds
        """
        cutoff = self.now - delay
        return cutoff.timestamp()

    def get_old(self, path, cutoff_time, return_files=False, ignore=None):
        """Get directories which are older cutoff.

        Args:
            path (str): Path to start of searching old directories
            cutoff_time (int): Directories or files older then cutoff
            return_files (boolean): return files to clean, default False
            ignore (list): List of directories to ignore
        Returns:
            dic_to_remove (dict): Dictionary of directories for remove
        """
        dic_to_remove = {}
        for root, _dirs, files in os.walk(path):
            dirs = _dirs if ignore is None else [x for x in _dirs if x not in ignore]
            if not return_files:
                for dir_name in dirs:
                    dir_path = os.path.join(root, dir_name)
                    dir_mtime = os.path.getmtime(dir_path)
                    logger.debug(
                        "path: {}, time: {}, cutoff: {}", dir_path, dir_mtime, cutoff_time
                    )
                    if dir_mtime < cutoff_time:
                        dic_to_remove[dir_name] = dir_path
                del _dirs[:]
            else:
                for filename in files:
                    file_path = os.path.join(root, filename)

                    file_mtime = os.path.getmtime(file_path)
                    logger.debug(
                        "path: {}, time: {}, cutoff: {}",
                        file_path,
                        file_mtime,
                        cutoff_time,
                    )
                    if file_mtime < cutoff_time:
                        dic_to_remove[filename] = file_path
        return dic_to_remove

    def remove_dic(self, dic, files=False):
        """Remove directories/files from the dictionary.

        Args:
            dic    (dictionary): Dictionary of directories/files
            files     (boolean): If in dic are files, Default False
        """
        for dir_file in dic.values():
            if files:
                os.remove(dir_file)
            else:
                shutil.rmtree(dir_file)
            logger.info("Removing {}", dir_file)


class CleanScratchData(CleanOldData):
    """Clean old data from scratch."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanScratchData"
        self.delay = as_timedelta(config["clean_old_data.scratch_data_period"])
        self.scratch = self.platform.get_platform_value("scratch")
        self.cutoff_time = self.cutoff(self.delay)
        ignore = list(config["clean_old_data.ignore"])
        logger.info("Ignore: {}", ignore)
        self.ignore_dir = ["IFS", "Clean_old_data", *ignore]

    def execute(self):
        """Run clean data from scratch."""
        dict_to_remove = self.get_old(
            self.scratch, self.cutoff_time, ignore=self.ignore_dir
        )
        self.remove_dic(dict_to_remove)


class CleanSuites(CleanOldData):
    """Clean old suites."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanSuites"
        self.ecf_jobout = self.platform.get_value("scheduler.ecfvars.ecf_jobout")
        self.ecf_files = self.platform.get_value("scheduler.ecfvars.ecf_files")

        self.delay = as_timedelta(config["clean_old_data.suites_period"])
        self.cutoff_time = self.cutoff(self.delay)
        ignore = list(config["clean_old_data.ignore"])
        logger.info("Ignore: {}", ignore)
        self.ignore_suite = ["IFS", "Clean_old_data", *ignore]

    def execute(self):
        """Run clean suites."""
        logger.info("Delay: {}, Cutoff:{}", self.delay, self.cutoff_time)
        dic_to_remove_files = self.get_old(
            self.ecf_files, self.cutoff_time, ignore=self.ignore_suite
        )
        dic_to_remove_jobout = self.get_old(
            self.ecf_jobout, self.cutoff_time, ignore=self.ignore_suite
        )

        self.remove_dic(dic_to_remove_files)
        self.remove_dic(dic_to_remove_jobout)

        suites = set(list(dic_to_remove_files) + list(dic_to_remove_jobout))
        EcflowServer(self.config).remove_suites(suites)


class CleanIFSData(CleanOldData):
    """Clean IFS data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanIFSData"
        self.marsdir = self.platform.get_platform_value("reference_data") + "/IFS"
        self.delay = as_timedelta(config["clean_old_data.IFS_period"])
        self.cutoff_time = self.cutoff(self.delay)

    def execute(self):
        """Run clean IFS data."""
        dict_to_remove = self.get_old(self.marsdir, self.cutoff_time, return_files=True)
        self.remove_dic(dict_to_remove, files=True)
