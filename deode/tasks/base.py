"""Base site class."""

import atexit
import os
import shutil
import socket

from deode.logs import get_logger_from_config
from deode.tasks.batch import BatchJob
from deode.tasks.data import InputData, OutputData
from deode.toolbox import FileManager


class Task(object):
    """Base Task class."""

    def __init__(self, config, name):
        """Construct base task.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Task name

        Raises:
            Exception: "You must set wrk"

        """
        self.logger = get_logger_from_config(config)
        self.config = config
        if "." in name:
            name = name.split(".")[-1]
        self.name = name
        self.fmanager = FileManager(self.config)
        self.platform = self.fmanager.platform

        wrk = self.platform.get_system_value("wrk")
        if wrk is None:
            raise Exception("You must set wrk")
        self.wrk = self.platform.substitute(wrk)
        wdir = f"{self.wrk}/{socket.gethostname()}{str(os.getpid())}"
        self.wdir = wdir
        self.logger.info("Task running in %s", self.wdir)
        self.logger.info("Base task info")
        self.logger.warning("Base task warning")
        self.logger.debug("Base task debug")

    def create_wrkdir(self):
        """Create a cycle working directory."""
        os.makedirs(self.wrk, exist_ok=True)

    def create_wdir(self):
        """Create task working directory."""
        os.makedirs(self.wdir, exist_ok=True)

    def change_to_wdir(self):
        """Change to task working dir."""
        os.chdir(self.wdir)

    def remove_wdir(self):
        """Remove working directory."""
        os.chdir(self.wrk)
        shutil.rmtree(self.wdir)
        self.logger.debug("Remove %s", self.wdir)

    def rename_failed(self):
        """Rename failed working directory."""
        fdir = f"{self.wrk}/Failed_{self.name}"
        if os.path.isdir(self.wdir):
            if os.path.exists(fdir):
                self.logger.debug("%s exists. Remove it", fdir)
                shutil.rmtree(fdir)
            shutil.move(self.wdir, fdir)
            self.logger.info("Renamed %s to %s", self.wdir, fdir)

    def execute(self):
        """Do nothing for base execute task."""
        self.logger.debug("Using empty base class execute")

    def prep(self):
        """Do default preparation before execution.

        E.g. clean

        """
        self.logger.debug("Base class prep")
        self.create_wdir()
        self.change_to_wdir()
        atexit.register(self.rename_failed)

    def post(self):
        """Do default postfix.

        E.g. clean

        """
        self.logger.debug("Base class post")
        # Clean workdir
        self.remove_wdir()

    def run(self):
        """Run task.

        Define run sequence.

        """
        self.prep()
        self.execute()
        self.post()

    def get_task_setting(self, setting):
        """Get task setting.

        Args:
            setting (str): Setting to find in task.{self.name}

        Returns:
            value : Found setting

        """
        try:
            value = self.config.get_value(f"task.{self.name}.{setting}")
        except AttributeError:
            self.logger.warning("Setting %s not found!", setting)
            return None

        self.logger.debug("Setting = %s value =%s", setting, value)
        return value


class BinaryTask(Task):
    """Base Task class."""

    def __init__(self, config, name):
        """Construct base task.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Task name
        """
        Task.__init__(self, config, name)
        self.logger.debug("Binary task %s", name)
        try :
            wrapper = self.get_task_setting("wrapper")
        except AttributeError:
            wrapper = ""

        self.batch = BatchJob(os.environ, wrapper)

    def prep(self):
        """Prepare run."""
        Task.prep(self)
        self.logger.debug("Prepping binary task")
        input_data = self.get_task_setting("input_data")
        InputData(input_data).prepare_input()

    def execute(self):
        """Execute binary task."""
        self.logger.debug("Executing binary task")
        cmd = self.get_task_setting("command")
        self.batch.run(cmd)

    def post(self):
        """Post run."""
        self.logger.debug("Post binary task")
        output_data = self.get_task_setting("output_data")
        OutputData(output_data).archive_files()
        Task.post(self)


class UnitTest(Task):
    """Base Task class."""

    def __init__(self, config):
        """Construct test task.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)
