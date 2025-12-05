"""Clean old data."""

import glob
import os
import re
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

    def get_old(self, path, format_, cutoff_time, ignore=None):
        """Get directories which are older than cutoff.

        Args:
            path (str): Path to start of searching old directories
            format_ (str): regex for path
            cutoff_time (int): Directories or files older then cutoff
            ignore (list): List of directories to ignore
        Returns:
            list_to_remove (list): List of directories for remove
        """
        list_to_remove = []
        glob_str = "*/".join(["*"] * (format_.count("/") - format_.count("^/")))
        logger.info(glob_str)
        logger.info(path)
        glob_pattern = os.path.join(path, glob_str)
        initial_matches = glob.glob(glob_pattern)
        logger.info("initial: {}", initial_matches)
        pattern = r"^" + re.escape(path) + r"{}".format(format_)
        pattern_no_whitespace = re.sub(r"\s+", "", pattern)
        logger.info("patteren: {}", pattern_no_whitespace)
        pattern_compiled = re.compile(pattern_no_whitespace)
        matched_directories = [d for d in initial_matches if pattern_compiled.match(d)]
        logger.info("Matched: {}", matched_directories)
        for dir_path in matched_directories:
            dir_mtime = os.path.getmtime(dir_path)
            logger.debug(
                "path: {}, time: {}, cutoff: {}",
                dir_path,
                dir_mtime,
                cutoff_time,
            )
            if dir_mtime < cutoff_time:
                if ignore is not None:
                    if os.path.basename(dir_path) not in ignore:
                        list_to_remove.append(dir_path)
                else:
                    list_to_remove.append(dir_path)
        return list_to_remove

    def find_empty_directories(self, path):
        """Return a list of full paths to empty directories under the given path.

        Args:
            path (str): Root directory to search.

        Returns:
            List[str]: Full paths of empty directories.
        """
        empty_dirs = set()

        for root, dirs, files in os.walk(path, topdown=False):
            if not files and (
                not dirs or all(os.path.join(root, d) in empty_dirs for d in dirs)
            ):
                empty_dirs.add(root)

        return sorted(empty_dirs, key=lambda x: x.count(os.sep), reverse=True)

    def remove_list(self, dir_list, files=False):
        """Remove directories/files from the list.

        Args:
            dir_list    (list): Dictionary of directories/files
            files     (boolean): If in list are files, Default False
        """
        logger.info(dir_list)
        for dir_file in dir_list:
            if files:
                os.remove(dir_file)
            else:
                shutil.rmtree(dir_file)
            logger.info("Removing {}", dir_file)


class CleanEhypeData(CleanOldData):
    """Clean old data from scratch."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanEhypeData"
        self.delay = as_timedelta(config["clean_old_data.ehype_data_period"])
        self.active = True
        try:
            self.ehype_scratch = self.platform.get_value(
                "impact.ehype.communicate.run_root"
            )
            self.ehype_work = self.platform.get_value(
                "impact.ehype.communicate.work_root"
            )
            self.ehype_form = config["clean_old_data.ehype_format"]
            self.ehype_form_run = config["clean_old_data.ehype_format_run"]
            self.ehype_form_perm = config["clean_old_data.ehype_format_perm"]
        except KeyError:
            logger.warning("Switch of ehype cleaning as config is incomplete")
            self.active = False
        self.cutoff_time = self.cutoff(self.delay)
        ignore = list(config["clean_old_data.ignore"])
        logger.info("Ignore: {}", ignore)
        self.ignore_dir = [*ignore]

    def execute(self):
        """Run clean data from scratch."""
        if self.active:
            list_to_remove_scratch = self.get_old(
                self.ehype_scratch,
                self.ehype_form,
                self.cutoff_time,
                ignore=self.ignore_dir,
            )
            self.remove_list(list_to_remove_scratch)

            list_to_remove_run = self.get_old(
                self.ehype_scratch,
                self.ehype_form_run,
                self.cutoff_time,
                ignore=self.ignore_dir,
            )
            self.remove_list(list_to_remove_run)
            self.remove_list(self.find_empty_directories(self.ehype_scratch))

            list_to_remove_work = self.get_old(
                self.ehype_work,
                self.ehype_form_perm,
                self.cutoff_time,
                ignore=self.ignore_dir,
            )
            self.remove_list(list_to_remove_work)
            self.remove_list(self.find_empty_directories(self.ehype_work))


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
        logger.info(self.platform.get_platform_value("scratch"))
        self.scratch = (
            self.platform.get_platform_value("scratch")
            + config["clean_old_data.scratch_ext"]
        )
        self.cutoff_time = self.cutoff(self.delay)
        self.scratch_format = config["clean_old_data.scratch_format"]
        ignore = list(config["clean_old_data.ignore"])
        logger.info("Ignore: {}", ignore)
        self.ignore_dir = ["IFS", "Clean_old_data", *ignore]

    def execute(self):
        """Run clean data from scratch."""
        list_to_remove = self.get_old(
            self.scratch,
            self.scratch_format,
            self.cutoff_time,
            ignore=self.ignore_dir,
        )
        self.remove_list(list_to_remove)
        self.remove_list(self.find_empty_directories(self.scratch))


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
        self.suite_form = config["clean_old_data.suite_format"]

        self.delay = as_timedelta(config["clean_old_data.suites_period"])
        self.cutoff_time = self.cutoff(self.delay)
        ignore = list(config["clean_old_data.ignore"])
        logger.info("Ignore: {}", ignore)
        self.ignore_suite = ["IFS", "Clean_Old_Data", "DE_NWP", *ignore]

    def execute(self):
        """Run clean suites."""
        logger.info("Delay: {}, Cutoff:{}", self.delay, self.cutoff_time)
        list_to_remove_files = self.get_old(
            self.ecf_files, self.suite_form, self.cutoff_time, ignore=self.ignore_suite
        )
        list_to_remove_jobout = self.get_old(
            self.ecf_jobout, self.suite_form, self.cutoff_time, ignore=self.ignore_suite
        )

        self.remove_list(list_to_remove_files)

        self.remove_list(list_to_remove_jobout)

        self.remove_list(self.find_empty_directories(self.ecf_files))
        self.remove_list(self.find_empty_directories(self.ecf_jobout))
        suites = [
            os.path.basename(x)
            for x in set(list(list_to_remove_files) + list(list_to_remove_jobout))
        ]

        server = EcflowServer(self.config)
        server.remove_suites(suites, check_if_complete=True)


class CleanIFSData(CleanOldData):
    """Clean IFS data."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        CleanOldData.__init__(self, config)
        self.name = "CleanIFSData"
        self.marsdir = (
            self.platform.get_platform_value("scratch")
            + config["clean_old_data.scratch_ext"]
        )
        self.ifs_form = config["clean_old_data.ifs_format"]
        self.delay = as_timedelta(config["clean_old_data.IFS_period"])
        self.cutoff_time = self.cutoff(self.delay)

    def execute(self):
        """Run clean IFS data."""
        list_to_remove = self.get_old(self.marsdir, self.ifs_form, self.cutoff_time)
        self.remove_list(list_to_remove, files=True)
        self.remove_list(self.find_empty_directories(self.marsdir))
