"""CollectLogs."""


import os
import tarfile

from ..logs import logger
from ..os_utils import Search, deodemakedirs
from .base import Task


class CollectLogs(Task):
    """Collect task and scheduler logfiles and store them as tarfiles."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.archive = self.platform.get_system_value("archive")
        self.logs = self.platform.get_system_value("logs")
        self.wrk = self.platform.get_system_value("wrk")
        self.joboutdir = self.config["task.args.joboutdir"]
        self.tarname = self.config["task.args.tarname"]
        self.task_logs = self.platform.substitute(self.config["task.args.task_logs"])
        self.parent = os.path.dirname(self.joboutdir)
        self.target = os.path.basename(self.joboutdir)
        if self.target == "":
            self.target = "."
        self.tarfile = f"{self.wrk}/{self.tarname}.tar.gz"

    def scan_logs(self, tarlog, parent, target, pattern="", exclude=""):
        """Search for files matching a pattern and add them to a tar file.

        Args:
            tarlog (tarfile object) : Tar file used
            parent (str) : Top search directory
            target (str) : Main search directory
            pattern (str) : Optional search pattern
            exclude (str) : Optional string for files to be excluded

        Raises:
            FileNotFoundError: If parent directory does not exist

        """
        logger.info("Searching for logs under parent={} target={}", parent, target)
        if os.path.exists(parent):
            os.chdir(parent)
            files = Search.find_files(target, pattern=pattern, fullpath=True)
            if exclude != "":
                files = [x for x in files if exclude not in x]
            for f in files:
                tarlog.add(f)
        else:
            raise FileNotFoundError
        logger.info(" found {} files \n {}", len(files), files)

    def execute(self):
        """Execute collect logs ."""
        deodemakedirs(self.logs, unixgroup=self.unix_group)

        # Create the tarfile
        logger.info("Create {}", self.tarfile)
        tarlog = tarfile.open(self.tarfile, "w:gz")

        # Scheduler logs
        self.scan_logs(
            tarlog,
            self.parent,
            self.target,
            pattern=r"(.*)\.(\d){1,}",
            exclude="CollectLogs",
        )
        # Task logs
        task_logs = f"{self.task_logs}/logs"
        try:
            self.scan_logs(tarlog, task_logs, ".")
        except FileNotFoundError:
            logger.warning("task logs directory {} not found", task_logs)

        tarlog.close()
        self.fmanager.output(self.tarfile, self.logs)
