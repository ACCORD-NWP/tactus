"""Batch process."""
import logging
import subprocess
import sys


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
        logging.debug("Constructed BatchJob")

    def run(self, cmd):
        """Run command.

        Args:
            cmd (str): Command to run.

        Raises:
            Exception: No command
            CalledProcessError: Execution error
        """
        if cmd is None:
            raise Exception("No command provided!")
        cmd = self.wrapper + " " + cmd

        if "OMP_NUM_THREADS" in self.rte:
            logging.info("BATCH: %s", self.rte["OMP_NUM_THREADS"])
        logging.info("Batch running %s", cmd)

        process = subprocess.Popen(
            cmd,
            shell=True,  # noqa
            env=self.rte,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            universal_newlines=True,
            bufsize=1,
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
