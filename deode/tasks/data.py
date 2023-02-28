"""Handle input/output data from tasks."""
import logging
import os
import subprocess  # noqa
from abc import ABC, abstractmethod
from ..toolbox import ArchiveError


class InputDataToBinaries(ABC):
    """Abstract input data."""

    @abstractmethod
    def __init__(self):
        """Construct."""

    @abstractmethod
    def prepare_input(self):
        """Prepare input.

        Returns:
            NotImplementedError : NotImplementedError

        """
        return NotImplementedError


class OutputDataFromBinaries(ABC):
    """Abstract output data."""

    @abstractmethod
    def __init__(self):
        """Construct."""

    @abstractmethod
    def archive_files(self):
        """Archive files.

        Returns:
            NotImplementedError: Not implemented.
        """
        return NotImplementedError


class OutputData(OutputDataFromBinaries):
    """Output data."""

    def __init__(self, data):
        """Output data from dict.

        Args:
            data (dict): Output data.

        """
        OutputDataFromBinaries.__init__(self)
        self.data = data

    def archive_files(self):
        """Archive files.

        Raises:
            Exception: Error archiving files.
        """
        for output_file, target in self.data.items():

            logging.info("%s -> %s", output_file, target)
            command = "mv"
            if isinstance(target, dict):
                for key in target:
                    logging.debug("%s %s %s", output_file, key, target[key])
                    command = target[key]
                    target = key

            cmd = command + " " + output_file + " " + target
            try:
                logging.info(cmd)
                subprocess.check_call(cmd, shell=True)  # noqa
            except (IOError, subprocess.CalledProcessError) as error:
                logging.error("%s failed", cmd)
                raise ArchiveError(cmd + " failed") from error


class InputData(InputDataToBinaries):
    """Input data object."""

    def __init__(self, data):
        """Construct input data.

        Args:
            data (dict): Input data.
        """
        InputDataToBinaries.__init__(self)
        self.data = data

    def prepare_input(self):
        """Prepare input."""
        for target, input_file in self.data.items():

            logging.info("%s -> %s", target, input_file)
            logging.debug(os.path.realpath(target))
            command = None
            if isinstance(input_file, dict):
                for key in input_file:
                    logging.debug(key)
                    logging.debug(input_file[key])
                    command = str(input_file[key])
                    input_file = str(key)
                    command = command.replace("@INPUT@", input_file)
                    command = command.replace("@TARGET@", target)

            if os.path.realpath(target) == os.path.realpath(input_file):
                logging.info("Target and input file is the same file")
            else:
                if command is None:
                    cmd = "ln -sf " + input_file + " " + target
                else:
                    cmd = command
                try:
                    logging.info(cmd)
                    subprocess.check_call(cmd, shell=True)  # noqa
                except IOError:
                    raise (cmd + " failed") from IOError

    def add_data(self, data):
        """Add data.

        Args:
            data (_type_): _description_
        """
        for key in data:
            value = data[key]
            self.data.update({key: value})
