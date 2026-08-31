"""Batch process."""

import io
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

    def run(self, cmd, logfile=None):
        """Run command.

        Args:
            cmd (str): Command to run.
            logfile: Optional (io.IOBase): Object to tee stdout into in addition
                to sys.stdout (e.g. open("oops.log", "a")).

        Raises:
            TypeError: Checks if cmd/logfile is/isnot of type str
            ValueError: If logfile is not a open file handle
            CalledProcessError: Execution error
        """
        if not isinstance(cmd, str):
            raise TypeError(f"Command must be a string. Got {type(cmd)} instead.")

        if logfile is not None and not isinstance(logfile, io.IOBase):
            raise TypeError(
                "logfile must be a io.IOBase file object.Got {type(logfile)} instead."
            )

        if isinstance(logfile, io.IOBase) and logfile.closed:
            raise ValueError(f"{logfile.name} is not open")

        cmd = self.wrapper + " " + cmd

        if "OMP_NUM_THREADS" in self.rte:
            logger.info("BATCH: {}", self.rte["OMP_NUM_THREADS"])
        logger.info("Batch running {}", cmd)

        process = subprocess.Popen(
            cmd,
            shell=True,
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
            if not nextline and process.poll() is not None:
                break
            sys.stdout.write(nextline)
            sys.stdout.flush()
            if logfile is not None:
                logfile.write(nextline)
                logfile.flush()

        return_code = process.wait()
        if return_code != 0:
            raise subprocess.CalledProcessError(return_code, cmd)
