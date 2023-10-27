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
        wrapper = self.config.get_task_config(self.name, "wrapper")
        # TODO
        wrapper = "time"
        self.batch = BatchJob(os.environ, wrapper)

    def prep(self):
        """Prepare run."""
        Task.prep(self)
        print("Prepping binary task")
        input_data = self.config.get_task_config(self.name, "input_data")
        # TODO
        input_data = {"input_file": "/dev/null"}
        InputData(input_data).prepare_input()

    def execute(self):
        """Execute binary task."""
        print("Executing binary task")
        cmd = self.config.get_task_config(self.name, "command")
        # TODO
        cmd = "echo Hello world && touch output"
        self.batch.run(cmd)

    def post(self):
        """Post run."""
        print("Post binary task")
        output_data = self.config.get_task_config(self.name, "output_data")
        # TODO
        output_data = {"output": "archived_file"}
        OutputData(output_data).archive_files()
        Task.post(self)
