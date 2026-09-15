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
            logfile (io.IOBase, str, optional): Object to tee stdout into in addition
                     to sys.stdout (e.g. open("oops.log", "a")). If a string is given
                     it will be used as a file name.

        Raises:
            TypeError: Checks if cmd is of type str
            ValueError: If logfile is not a open file handle
            CalledProcessError: Execution error
        """
        if not isinstance(cmd, str):
            raise TypeError(f"Command must be a string. Got {type(cmd)} instead.")

        close_log = False
        log_handle = None
        if isinstance(logfile, io.IOBase):
            log_handle = logfile
            if log_handle.closed:
                raise ValueError(f"{log_handle.name} is not open")
        elif isinstance(logfile, str):
            log_handle = open(logfile, "a")
            close_log = True

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
            if log_handle is not None:
                log_handle.write(nextline)
                log_handle.flush()

        return_code = process.wait()
        if return_code != 0:
            raise subprocess.CalledProcessError(return_code, cmd)

        if close_log:
            log_handle.close()
