"""Batch process."""
import subprocess
import sys

from ..logs import logger


class BatchJob(object):
    """Batch job."""

    def __init__(self, rte, wrapper=""):
        """Construct batch job.

        Args:
            rte (dict): Run time environment.
            wrapper (str, optional): Wrapper around command. Defaults to "".

        """
        self.rte = rte
        self.wrapper = wrapper
        logger.debug("Constructed BatchJob")

    def run(self, cmd):
        """Run command.

        Args:
            cmd (str): Command to run.

        Raises:
            TypeError: If the provided command is not a string
            CalledProcessError: Execution error
        """
        if not isinstance(cmd, str):
            raise TypeError(f"Command must be a string. Got {type(cmd)} instead.")
        cmd = self.wrapper + " " + cmd

        if "OMP_NUM_THREADS" in self.rte:
            logger.info("BATCH: {}", self.rte["OMP_NUM_THREADS"])
        logger.info("Batch running {}", cmd)

        process = subprocess.Popen(
            cmd,
            shell=True,  # noqa
            env=self.rte,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            universal_newlines=True,
            bufsize=1,
            errors="replace",
        )
        # Poll process for new output until finished
        while True:
            nextline = process.stdout.readline()
            if nextline == "" and process.poll() is not None:
                break
            sys.stdout.write(nextline)
            sys.stdout.flush()

        return_code = process.wait()
        if return_code != 0:
            raise subprocess.CalledProcessError(return_code, cmd)
