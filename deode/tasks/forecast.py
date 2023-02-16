"""Forecast."""

from deode.tasks.batch import BatchJob
from .base import Task
import os
import glob


class Forecast(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)
        self.logger.debug("Construct forecast task")

    def execute(self):
        """Execute forecast."""
        directory_path = self.config.get_value("system.archive")
        file_list = glob.glob(directory_path + "*")
        for file_path in file_list:
            file_name = file_path.split("/")[-1]
            self.fmanager.input(file_path, file_name, provider_id="copy")
        self.fmanager.input("@NML@", "fort.4", provider_id="copy")
        self.fmanager.input("@BINDIR@/MASTERODB", "MASTERODB")
        binary = self.config.get_value("task.forecast.command")
        batch = BatchJob(os.environ, "")
        batch.run(binary)
        for i in range(7):
            self.fmanager.output(f"ICMSH@CNMEXP@+000{i}", f"@HOME@/output/ICMSH@CNMEXP@+000{i}")
        self.fmanager.output("NODE.001_01", "@HOME@/output/NODE.001_01")


class PgdInput(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


class Pgd(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


class PrepareCycle(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


class E927(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)


class FirstGuess(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)
