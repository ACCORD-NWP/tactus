"""Base site class."""

import os

from deode.tasks.batch import BatchJob
from deode.tasks.data import InputData, OutputData


class Task(object):
    """Base Task class."""

    def __init__(self, config, name):
        """Construct base task.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Task name

        """
        self.config = config
        if "." in name:
            name = name.split(".")[-1]
        self.name = name
        print("Base task")

    def execute(self):
        """Do nothing for base execute task."""
        print("Using empty base class execute")

    def prep(self):
        """Do default preparation before execution.

        E.g. clean

        """
        print("Base class prep")

    def post(self):
        """Do default postfix.

        E.g. clean

        """
        print("Base class post")

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
            print(f"Setting {setting} not found!")
            return None

        print("Setting =", setting, " value =", value)
        return value


class BinaryTask(Task):
    """Base Task class."""

    def __init__(self, config, name):
        """Construct base task.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Task name
        """
        print(f"Binary task {name}")
        Task.__init__(self, config, name)
        wrapper = self.get_task_setting("wrapper")
        self.batch = BatchJob(os.environ, wrapper)

    def prep(self):
        """Prepare run."""
        Task.prep(self)
        print("Prepping binary task")
        input_data = self.get_task_setting("input_data")
        InputData(input_data).prepare_input()

    def execute(self):
        """Execute binary task."""
        print("Executing binary task")
        cmd = self.get_task_setting("command")
        self.batch.run(cmd)

    def post(self):
        """Post run."""
        print("Post binary task")
        output_data = self.get_task_setting("output_data")
        OutputData(output_data).archive_files()
        Task.post(self)
