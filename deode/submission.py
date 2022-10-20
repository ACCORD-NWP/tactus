"""Job submission setup."""
import os
import shutil
import subprocess
from abc import ABC, abstractmethod
from cmath import log


class EcflowSubmitTask(object):
    """Submit class for ecflow."""

    def __init__(
        self,
        task,
        env_submit,
        server,
        joboutdir,
        stream=None,
        dbfile=None,
        interpreter="#!/usr/bin/env python3",
        ensmbr=None,
        submit_exceptions=None,
        coldstart=False,
        env_file=None,
    ):
        """Construct a ecflow submission task.

        Args:
            task (scheduler.EcflowTask): Task
            env_submit (_type_): _description_
            server (scheduler.Server): Server
            joboutdir (_type_): _description_
            stream (int, optional): Stream. Defaults to None.
            dbfile (str, optional):  Data base for monitoring. Defaults to None.
            interpreter (str, optional): Python interpreter. Defaults to "#!/usr/bin/env python3".
            ensmbr (str, optional): Ensemble member. Defaults to None.
            submit_exceptions (dict, optional): Task submission exceptions. Defaults to None.
            coldstart (bool, optional): Cold start. Defaults to False.
            env_file (str, optional): Environment file. Defaults to None.

        """
        self.task = task
        self.env_file = env_file
        self.ecflow_server = server
        self.coldstart = coldstart

        self.ensmbr = ensmbr
        if self.ensmbr is not None:
            if ensmbr < 0:
                self.ensmbr = None

        self.db_file = dbfile
        self.stream = stream
        self.complete = False
        self.debug = True

        # Parse Env_submit
        self.task_settings = TaskSettings(
            self.task,
            env_submit,
            joboutdir,
            interpreter=interpreter,
            submit_exceptions=submit_exceptions,
            coldstart=False,
        )
        self.sub = get_submission_object(
            self.task, self.task_settings, self.ecflow_server, db_file=self.db_file
        )

    def write_header(self, file_handler):
        """Write header to file handler.

        Args:
            file_handler (_type_): _description_

        Returns:
            _type_: _description_
        """
        file_handler.write(self.task_settings.interpreter + "\n")
        if self.task_settings.header is not None:
            file_handler.write("\n# Batch commands\n")
            # Loop twice, first comments (likely to be batch commands)
            for setting, value in self.task_settings.header.items():
                value = str(value)
                if value.find("#") >= 0:
                    # print(str(self.header[setting]))
                    file_handler.write(str(self.task_settings.header[setting]) + "\n")

            # Host environment
            if self.env_file is not None:
                file_handler.write(
                    "\n# Host specific environment settings in python syntax:\n"
                )
                with open(self.env_file, mode="r", encoding="utf-8") as fh_env:
                    for line in fh_env.readlines():
                        file_handler.write(line)
                fh_env.close()

            file_handler.write("\n# Task specific settings:\n")
            for setting, value in self.task_settings.header.items():
                value = str(value)
                if value.find("#") < 0:
                    # print(str(self.header[setting]))
                    file_handler.write(str(value) + "\n")

            wrapper = ""
            if self.task_settings.wrapper is not None:
                wrapper = str(self.task_settings.wrapper)

            file_handler.write("\n#Python script:\n")
            return wrapper, self.task_settings.host

    def write_trailer(self, file_handler):
        """Write trailer in job file.

        Args:
            file_handler (_type_): _description_

        """
        if self.task_settings.trailer is not None:
            for value in self.task_settings.trailer.values():
                file_handler.write(str(value) + "\n")

    def write_job(self):
        """Write job file."""
        logging.info("Job file: %s", self.task_settings.ecf_job)
        ecf_job = self.task_settings.ecf_job
        fname = ecf_job + ".tmp"
        shutil.move(ecf_job, fname)
        with open(ecf_job, mode="w", encoding="utf-8") as file_handler:
            wrapper, host = self.write_header(file_handler)
            with open(fname, mode="r", encoding="utf-8") as job_fh:
                for line in job_fh.readlines():
                    line = line.replace("@WRAPPER_TO_BE_SUBSTITUTED@", wrapper)
                    line = line.replace("@HOST_TO_BE_SUBSTITUTED@", host)
                    file_handler.write(line)
            self.write_trailer(file_handler)
        os.system("chmod u+x " + ecf_job)

    def submit(self):
        """Sumit task."""
        try:
            logging.debug("submit start")
            if "OUTPUT" not in self.task_settings.header:
                self.task_settings.header.update({"OUTPUT": self.sub.set_output()})
            if "NAME" not in self.task_settings.header:
                self.task_settings.header.update({"NAME": self.sub.set_job_name()})
            logging.debug("write")
            self.write_job()

            # if self.ecflow_server is None:
            #    raise Exception("You must set server to submit!")

            if self.complete:
                if self.ecflow_server is not None:
                    logging.debug("force_complete")
                    self.ecflow_server.force_complete(self.task)
            else:
                logging.debug("sub.set_submit_cmd")
                self.sub.set_submit_cmd()
                logging.debug("submit_job")
                self.sub.submit_job()
                logging.debug("set_jobid")
                self.sub.set_jobid()
                logging.debug("job_id")
                self.task.submission_id = self.sub.job_id
                if self.ecflow_server is not None:
                    logging.debug("update_submission_id")
                    self.ecflow_server.update_submission_id(self.task)

        except RuntimeError:
            # Supposed to handle abort self unless killed
            pass
        except Exception as error:
            msg = f"Submission failed {str(error)}"
            raise SubmitException(msg, self.task, self.task_settings) from error


class TaskSettings(object):
    """Set the task specific setttings."""

    def __init__(
        self,
        task,
        submission_defs,
        joboutdirs,
        submit_exceptions=None,
        interpreter="#!/usr/bin/env python3",
        complete=False,
        coldstart=False,
    ):
        """Construct the task specific settings.

        Args:
            task (scheduler.EcflowTask): Task
            submission_defs (dict): Submission definitions
            joboutdirs (_type_): _description_
            submit_exceptions (_type_, optional): _description_. Defaults to None.
            interpreter (str, optional): Python interpreter. Defaults to "#!/usr/bin/env python3".
            complete (bool, optional): Set to complete. Defaults to False.
            coldstart (bool, optional): Coldstart. Defaults to False.

        Raises:
            Exception: _description_
            Exception: _description_
            Exception: _description_

        """
        self.task = task
        self.submission_defs = submission_defs
        self.header = {}
        self.trailer = {}
        self.wrapper = None
        self.submit_type = "background"
        self.interpreter = interpreter
        self.complete = complete
        self.remote_submit_cmd = None
        self.remote_status_cmd = None
        self.remote_kill_cmd = None
        self.coldstart = coldstart
        self.host = None
        self.submit_variables = None

        if submit_exceptions is not None:
            self.check_exceptions(submit_exceptions)
        self.task_settings = self.parse_submission_defs()
        self.process_settings()

        if self.host is None:
            raise Exception("Host number is mandatory!")

        if "0" in joboutdirs:
            joboutdir = joboutdirs["0"]
        else:
            raise Exception("No joboutdir defined for HOST 0")

        print("Task HOST is ", self.host)
        print(__file__)
        if self.host in joboutdirs:
            joboutdir_at_host = joboutdirs[self.host]
        else:
            raise Exception("No joboutdir found for host " + self.host)

        self.joboutdir = joboutdir
        self.joboutdir_at_host = joboutdir_at_host
        self.ecf_job = self.task.create_ecf_job(joboutdir)
        self.ecf_job_at_host = self.task.create_ecf_job(joboutdir_at_host)
        self.ecf_jobout = self.task.create_ecf_jobout(joboutdir)
        self.ecf_jobout_at_host = self.task.create_ecf_jobout(joboutdir_at_host)

    def check_exceptions(self, submit_exceptions):
        """Possibility to create submission exceptions.

        Args:
            submit_exceptions (_type_): _description_

        """
        if submit_exceptions is not None:
            for state in submit_exceptions:
                if state == "complete":
                    if "task" in submit_exceptions[state]:
                        for task in submit_exceptions[state]["task"]:
                            if task == self.task.ecf_task:
                    if "family" in submit_exceptions[state]:
                        for family in submit_exceptions[state]["family"]:
                            for ecf_family in self.task.ecf_families:
                                if family == ecf_family:
                                    if submit_exceptions[state]["family"][family] == \
                                            "is_coldstart":
                                        if self.coldstart:
                                            self.complete = \
                                                f"Family {family} complete due to " \
                                                            "cold start"

    def process_settings(self):
        """Process the settings."""
        for key, value in self.task_settings.items():
            # print("key=", key, " value=", value)
            if key == "TRAILER":
                self.trailer.update({key: value})
            else:
                if key == "SUBMIT_TYPE":
                    if value != "":
                        self.submit_type = value
                elif key == "SSH":
                    self.remote_submit_cmd = value
                    self.remote_status_cmd = value
                    self.remote_kill_cmd = value
                elif key == "INTERPRETER":
                    self.interpreter = value
                elif key == "SUBMIT_VARIABLES":
                    self.submit_variables = value
                elif key == "WRAPPER":
                    self.wrapper = value
                elif key == "HOST":
                    self.host = str(value)
                    if self.host != "0" and self.host != "1":
                        raise Exception("Expected a single or dual-host system. HOST=",
                                        self.host)
                else:
                    self.header.update({key: value})

    def parse_submission_defs(self):
        """Parse the submssion definitions."""
        ecf_task = self.task.ecf_task
        task_settings = {}
        # print("parse", ecf_task)
        # print(self.submission_defs)
        all_defs = self.submission_defs
        print(all_defs)
        submit_types = all_defs["submit_types"]
        default_submit_type = all_defs["default_submit_type"]
        task_submit_type = None
        for s_t in submit_types:
            if s_t in all_defs and "tasks" in all_defs[s_t]:
                for tname in all_defs[s_t]["tasks"]:
                    if tname == ecf_task:
                        task_submit_type = s_t
        if task_submit_type is None:
            task_submit_type = default_submit_type

        if task_submit_type in all_defs:
            for setting in all_defs[task_submit_type]:
                if setting != "tasks":
                    task_settings.update({setting: all_defs[task_submit_type][setting]})

        ex = "task_exceptions"
        if ex in all_defs:
            for tname in all_defs[ex]:
                if tname == ecf_task:
                    for setting in all_defs[ex][tname]:
                        task_settings.update({setting: all_defs[ex][tname][setting]})

        # print(task_settings)
        return task_settings


class SubmitException(Exception):
    """Submit exception."""

    def __init__(self, msg, task, task_settings):
        """Construct SubmitException.

        Args:
            msg (str): Exception message.
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
        """
        Exception().__init__()
        logfile = task.create_submission_log(task_settings.joboutdir)
        with open(logfile, mode="a", encoding="utf-8") as file_handler:
            file_handler.write(msg)
            file_handler.flush()
        print(msg)
        exit(0)


class KillException(Exception):
    """Kill exception."""

    def __init__(self, msg, task, task_settings):
        """Construct KillException.

        Args:
            msg (str): Exception message.
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
        """
        Exception().__init__()
        logfile = task.create_kill_log(task_settings.joboutdir)
        with open(logfile, mode="a", encoding="utf-8") as file_handler:
            file_handler.write(msg)
            file_handler.flush()
        print(msg)
        exit(0)


class StatusException(Exception):
    """Status exception."""

    def __init__(self, msg, task, task_settings):
        """Construct status exception.

        Args:
            msg (str): Exception message.
            task (scheduler.EcflowTask): task.
            task_settings (TaskSettings): Task settings

        """
        Exception().__init__()
        # joboutdir = task.joboutdir
        logfile = task.create_status_log(task_settings.joboutdir)
        with open(logfile, mode="a", encoding="utf-8") as file_handler:
            file_handler.write(msg)
            file_handler.flush()
        print(msg)
        exit(0)


def get_submission_object(task, task_settings, server, db_file=None):
    """Get the submission object constructed from a submit type.

    Args:
        task (scheduler.EcflowTask): Task.
        task_settings (TaskSettings): Task settings
        server (scheduler.Server): Server.
        db_file (str, optional): Data base for monitoring.. Defaults to None.

    Raises:
        NotImplementedError: _description_

    Returns:
        SubmissionBaseClass: Return a submission object.

    """
    submit_type = task_settings.submit_type
    logging.info("Submit type: %s", submit_type)
    if submit_type.lower() == "pbs":
        sub = PBSSubmission(task, task_settings, server, db_file=db_file)
    elif submit_type.lower() == "slurm":
        sub = SlurmSubmission(task, task_settings, server, db_file=db_file)
    elif submit_type.lower() == "grid_engine":
        sub = GridEngineSubmission(task, task_settings, server, db_file=db_file)
    elif submit_type == "background":
        sub = BackgroundSubmission(task, task_settings, server, db_file=db_file)
    else:
        raise NotImplementedError
    return sub


class SubmissionBaseClass(ABC):
    """An abstract class for submssion to be implemented by all children.

    Args:
        ABC (_type_): _description_
    """

    def __init__(
        self,
        task,
        task_settings,
        server,
        db_file=None,
        remote_submit_cmd=None,
        remote_kill_cmd=None,
        remote_status_cmd=None,
    ):
        """Construct the base class.

        Args:
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
            server (scheduler.Server): Server.
            db_file (str, optional): Data base for monitoring. Defaults to None.
            remote_submit_cmd (str, optional): Remote submit command. Defaults to None.
            remote_kill_cmd (str, optional): Remote kill command. Defaults to None.
            remote_status_cmd (str, optional): Remote status command. Defaults to None.

        """
        self.task = task
        self.task_settings = task_settings
        self.server = server
        self.db_file = db_file
        self.process = None
        self.job_id = None
        if task.submission_id is not None:
            self.job_id = task.submission_id
        self.submit_cmd = None
        self.kill_job_cmd = None
        self.job_status_cmd = None

        self.remote_submit_cmd = remote_submit_cmd
        self.remote_kill_cmd = remote_kill_cmd
        self.remote_status_cmd = remote_status_cmd

    def update_db(self, job_id):
        """Update the data base.

        Args:
            job_id (str): Job identifier.
        """
        if self.db_file is not None:
            with open(self.db_file, mode="a", encoding="utf-8") as file_handler:
                file_handler.write(job_id + "\n")

    def clear_db(self):
        """Remove database."""
        if self.db_file is not None:
            if os.path.exists(self.db_file):
                os.unlink(self.db_file)

    @abstractmethod
    def set_submit_cmd(self):
        """Set submit command.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError

    @abstractmethod
    def set_jobid(self):
        """Set job id.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError

    @abstractmethod
    def get_logfile(self):
        """Set logfile.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError

    def submit_job(self):
        """Submit job.

        Raises:
            RuntimeError: _description_
        """
        logging.info("SUBMIT COMMAND: %s", self.submit_cmd)
        if self.submit_cmd is not None:
            cmd = self.set_remote_cmd(self.submit_cmd, self.remote_submit_cmd)
            logfile = self.get_logfile()
            logging.info(cmd)
            if logfile is None:
                subfile = self.task.create_submission_log(self.task_settings.joboutdir)
                with open(subfile, mode="w", encoding="utf-8") as subfile:
                    if self.server is not None:
                        self.server.update_log("ECF_JOB_CMD: " + cmd)
                    process = subprocess.Popen(
                        cmd, stdout=subfile, stderr=subfile, shell=True
                    )
                    process.wait()
                ret = process.returncode
                if ret != 0:
                    raise RuntimeError(
                        "Submit command failed with error code " + str(ret)
                    )
            else:
                if self.server is not None:
                    self.server.update_log("ECF_JOB_CMD: " + cmd)
                with open(logfile, mode="w", encoding="utf-8") as logfile:
                    self.process = subprocess.Popen(
                        cmd, stdout=logfile, stderr=logfile, shell=True
                    )

            logging.debug(self.submit_cmd)
            self.job_id = self.set_jobid()
            logging.debug(self.job_id)
            if self.db_file is not None:
                SubmissionBaseClass.update_db(self, self.job_id)

    def kill_job(self):
        """Kill task.

        Raises:
            RuntimeError: _description_
        """
        if self.kill_job_cmd is not None:
            killfile = self.task.create_kill_log(self.task_settings.joboutdir)
            # print(self.kill_job_cmd)
            cmd = self.set_remote_cmd(self.kill_job_cmd, self.remote_kill_cmd)
            print(cmd)
            print(killfile)
            with open(killfile, mode="w", encoding="utf-8") as kill_file:
                kill_file.write(
                    f"Kill job {self.task_settings.ecf_job_at_host} with command:\n"
                )
                kill_file.write(f"{cmd}\n")
                kill_file.flush()

            stdout = open(killfile, mode="a", encoding="utf-8")
            process = subprocess.Popen(cmd, stdout=stdout, stderr=stdout, shell=True)
            process.wait()
            ret = process.returncode
            if ret != 0:
                raise RuntimeError("Kill command failed with error code " + str(ret))

            logfile = self.task_settings.ecf_jobout
            if os.path.exists(logfile):
                mode = "a"
            else:
                mode = "w"
            log_handler = open(logfile, mode=mode, encoding="utf-8")
            log_handler.write("\n\n*** KILLED BY ECF_kill ****")
            log_handler.flush()
            log_handler.close()

    @abstractmethod
    def set_job_status(
        self,
    ):
        """Set job status.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError

    def status(self):
        """General status method.

        Raises:
            StatusException: _description_
            StatusException: _description_

        """
        if self.job_id is None:
            StatusException("No job ID was provided!", self.task, self.task_settings)
        try:
            self.set_job_status()
        except Exception as error:
            raise StatusException(
                "Setting of status command failed " + repr(error),
                self.task,
                self.task_settings,
            ) from error
        if self.job_status_cmd is None:
            raise StatusException(
                "No status command set for " + self.task_settings.submit_type,
                self.task,
                self.task_settings,
            )
        try:
            self.job_status()
        except Exception as error:
            raise StatusException(
                "Status command failed " + repr(error), self.task, self.task_settings
            ) from error

    def job_status(self):
        """General job status method.

        Raises:
            RuntimeError: _description_
        """
        logging.info(self.job_status_cmd)
        if self.job_status_cmd is not None:
            statusfile = self.task.create_status_log(self.task_settings.joboutdir)
            cmd = self.set_remote_cmd(self.job_status_cmd, self.remote_status_cmd)
            print(cmd)
            print(statusfile)
            with open(statusfile, mode="w", encoding="utf-8") as stdout:
                process = subprocess.Popen(cmd, stdout=stdout, stderr=stdout, shell=True)
                process.wait()
            ret = process.returncode
            if ret != 0:
                raise RuntimeError("Status command failed with error code " + str(ret))

    def kill(self):
        """General kill method.

        Raises:
            KillException: _description_
            KillException: _description_
            KillException: _description_
            KillException: _description_
        """
        if self.job_id is None:
            raise KillException("No job ID was provided!", self.task, self.task_settings)
        try:
            self.set_kill_cmd()
        except Exception as error:
            raise KillException(
                "Setting of kill command failed " + repr(error),
                self.task,
                self.task_settings,
            ) from error
        if self.kill_job_cmd is None:
            raise KillException(
                "No kill command set for " + self.task_settings.submit_type,
                self.task,
                self.task_settings,
            )
        try:
            self.kill_job()
        except Exception as error:
            raise KillException(
                "Kill failed " + repr(error), self.task, self.task_settings
            ) from error

    @abstractmethod
    def set_kill_cmd(self):
        """Set kill command.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError

    @staticmethod
    def set_remote_cmd(cmd, remote_cmd):
        """Set remote command.

        Pre-pending a remote command statement (e.g. ssh)

        """
        if remote_cmd is not None:
            cmd = remote_cmd + ' "' + str(cmd) + '"'
        return cmd

    @abstractmethod
    def set_output(self):
        """Set output.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError

    @abstractmethod
    def set_job_name(self):
        """Set job name.

        Must be implemented.

        Raises:
            NotImplementedError: If not implemented.

        """
        raise NotImplementedError


class BackgroundSubmission(SubmissionBaseClass):
    """Backgrpund submission.

    A subprocess on the host system.

    Args:
        SubmissionBaseClass (_type_): _description_

    """

    def __init__(self, task, task_settings, server, db_file=None):
        """Construct BackgroundSubmission.

        Args:
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
            server (scheduler.Server): Server.
            db_file (str, optional):  Data base for monitoring. Defaults to None.
        """
        SubmissionBaseClass.__init__(self, task, task_settings, server, db_file=db_file)

    def set_submit_cmd(self):
        """Set submit command."""
        ecf_job = self.task_settings.ecf_job
        submit_vars = ""
        if self.task_settings.submit_variables is not None:
            for key, val in self.task_settings.submit_variables.items():
                submit_vars = f"export {key}={val}; {submit_vars}"

        ecf_job = f"{submit_vars} {ecf_job}"
        print(ecf_job)
        self.submit_cmd = self.set_remote_cmd(ecf_job, self.remote_submit_cmd)

    def set_jobid(self):
        """Set job id."""
        return str(self.process.pid)

    def get_logfile(self):
        """Get the logfile."""
        ecf_jobout = self.task_settings.ecf_jobout
        return ecf_jobout

    def set_kill_cmd(self):
        """Set the kill command."""
        if self.job_id is not None:
            cmd = "kill -9 " + str(self.job_id)
            self.kill_job_cmd = self.set_remote_cmd(cmd, self.remote_kill_cmd)

    def set_job_status(self):
        """Set the job status."""
        logging.debug("set_job_status %s cmd: %s", self.job_id, self.job_status_cmd)
        if self.job_id is not None:
            self.job_status_cmd = "ps -auxq " + str(self.job_id)
        print(self.job_status_cmd)

    def set_output(self):
        """Set output."""
        string = "# Background jobs use standard output/error\n"
        return string

    def set_job_name(self):
        """Set job name."""
        string = "# Background jobs get job name from process name\n"
        return string


class BatchSubmission(SubmissionBaseClass):
    """A general batch system class for a task using a job scheduler system.

    Args:
        SubmissionBaseClass (_type_): _description_
    """

    def __init__(
        self,
        task,
        task_settings,
        server,
        db_file=None,
        sub=None,
        stat=None,
        kill=None,
        prefix="#",
    ):
        """Construct the BatchSubmission object.

        Args:
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
            server (scheduler.Server): Server.
            db_file (str, optional):  Data base for monitoring. Defaults to None.
            sub (str, optional): Submission command. Defaults to None.
            stat (str, optional): Status command. Defaults to None.
            kill (str, optional): Kill command. Defaults to None.
            prefix (str, optional): Batch prefix. Defaults to "#".

        """
        SubmissionBaseClass.__init__(self, task, task_settings, server, db_file=db_file)
        name = self.task.ecf_name.split("/")
        self.batch_sub = sub
        self.batch_stat = stat
        self.batch_kill = kill
        self.batch_prefix = prefix
        self.name = name[-1]

    def set_submit_cmd(self):
        """Set submit command.

        Args:
            remote_cmd (_type_, optional): _description_. Defaults to None.
        """
        submit_vars = ""
        if self.task_settings.submit_variables is not None:
            for key, val in self.task_settings.submit_variables.items():
                submit_vars = f"-v {key}={val} {submit_vars}"

        cmd = f"{self.batch_sub} {submit_vars} {self.task_settings.ecf_job_at_host}"
        cmd = self.set_remote_cmd(cmd, self.task_settings.remote_submit_cmd)
        self.submit_cmd = cmd

    def set_jobid(self):
        """Set job id."""
        raise NotImplementedError

    def get_logfile(self):
        """Get the logfile."""
        return None

    def set_kill_cmd(self):
        """Set kill command."""
        if self.job_id is not None:
            self.kill_job_cmd = self.batch_kill + " " + str(self.job_id)

    def set_job_status(self):
        """Set job status."""
        if self.job_id is not None:
            self.job_status_cmd = self.batch_stat + " " + str(self.job_id)

    def set_output(self):
        """Set output."""
        logfile = self.task_settings.ecf_jobout_at_host
        string = self.batch_prefix + " -o " + logfile + "\n"
        string += self.batch_prefix + " -e " + logfile + "\n"
        string += self.batch_prefix + " -j oe\n"
        return string

    def set_job_name(self):
        """Set job name."""
        string = self.batch_prefix + " -N " + self.name + "\n"
        return string


class PBSSubmission(BatchSubmission):
    """Job submission on a PBS job scheduler.

    Args:
        BatchSubmission (_type_): _description_
    """

    def __init__(
        self,
        task,
        task_settings,
        server,
        sub="qsub",
        stat="qstat -j",
        kill="qdel",
        prefix="#PBS",
        db_file=None,
    ):
        """Construct the PBS job submission object.

        Args:
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
            server (scheduler.Server): Server.
            sub (str, optional): Submission command. Defaults to "qsub".
            stat (str, optional): Status command. Defaults to "qstat -j".
            kill (str, optional): Kill command. Defaults to "qdel".
            prefix (str, optional): PBS prefix. Defaults to "#PBS".
            db_file (str, optional):  Data base for monitoring. Defaults to None.

        """
        BatchSubmission.__init__(
            self,
            task,
            task_settings,
            server,
            db_file=db_file,
            sub=sub,
            stat=stat,
            kill=kill,
            prefix=prefix,
        )
        self.name = self.name[0:15]

    def set_jobid(self):
        """Set job id."""
        logfile = self.task.create_submission_log(self.task_settings.joboutdir)
        with open(logfile, mode="r", encoding="utf-8") as file_handler:
            lines = file_handler.readlines()

        answer = None
        for line in lines:
            answer = line

        # expected_len = 7
        expected_len = 1
        answer = answer.replace("\n", "")
        words = answer.split(" ")
        if len(words) == expected_len:
            # Set job id as the second element in answer
            self.job_id = str(words[0])
            # self.job_id = str(words[2])
        else:
            raise Exception(
                "Expected " + str(expected_len) + " in output. Got " + str(len(words))
            )

    def set_job_name(self):
        """Set job name."""
        string = self.batch_prefix + " -N " + self.name + "\n"
        return string


class SlurmSubmission(BatchSubmission):
    """General slurm submssion class.

    Args:
        BatchSubmission (_type_): _description_
    """

    def __init__(
        self,
        task,
        task_settings,
        server,
        sub="sbatch",
        stat="squeue -j",
        kill="scancel",
        prefix="#SBATCH",
        db_file=None,
    ):
        """Construct SlurmSubmission.

        Args:
            task (scheduler.EcflowTask): Task.
            task_settings (TaskSettings): Task settings.
            server (scheduler.Server): Server.
            sub (str, optional): Submission command. Defaults to "sbatch".
            stat (str, optional): Status command. Defaults to "squeue -j".
            kill (str, optional): Kill command. Defaults to "scancel".
            prefix (str, optional): Slurm prefix. Defaults to "#SBATCH".
            db_file (_type_, optional):  Data base for monitoring. Defaults to None.

        """
        BatchSubmission.__init__(
            self,
            task,
            task_settings,
            server,
            db_file=db_file,
            sub=sub,
            stat=stat,
            kill=kill,
            prefix=prefix,
        )
        name = self.task.ecf_name.split("/")
        self.name = name[-1]

    def set_output(self):
        """Set output."""
        logfile = self.task_settings.ecf_jobout_at_host
        string = self.batch_prefix + " -o " + logfile + "\n"
        string += self.batch_prefix + " -e " + logfile
        return string

    def set_jobid(self):
        """Set job id."""
        logfile = self.task.create_submission_log(self.task_settings.joboutdir)
        with open(logfile, mode="r", encoding="utf-8") as file_handler:
            lines = file_handler.readlines()

        answer = None
        for line in lines:
            answer = line

        if answer is None:
            raise Exception("No answer found " + str(lines) + " " + logfile)

        expected_len = 4
        answer = answer.replace("\n", "")
        words = answer.split(" ")
        if len(words) == expected_len:
            # Set job id as the second element in answer
            self.job_id = str(words[-1])
        else:
            raise Exception(
                "Expected " + str(expected_len) + " in output. Got " + str(len(words))
            )

    def set_submit_cmd(self):
        """Set submit command."""
        submit_vars = ""
        if self.task_settings.submit_variables is not None:
            for key, val in self.task_settings.submit_variables.items():
                submit_vars = f"--export {key}={val} {submit_vars}"

        cmd = f"{self.batch_sub} {submit_vars} {self.task_settings.ecf_job_at_host}"
        cmd = self.set_remote_cmd(cmd, self.task_settings.remote_submit_cmd)
        self.submit_cmd = cmd

    def set_job_name(self):
        """Set job name."""
        string = self.batch_prefix + " -J " + self.name + "\n"
        return string


class GridEngineSubmission(BatchSubmission):
    """Sun Grid Engine (SGE) job submission.

    Args:
        BatchSubmission (_type_): _description_
    """

    def __init__(
        self,
        task,
        task_settings,
        server,
        db_file=None,
        sub="qsub",
        stat="qstat -j",
        kill="qdel",
        prefix="#$",
    ):
        """Construct the GridEngineSubmission object.

        Args:
            task (scheduler.EcflowTask): Task
            task_settings (TaskSettings): Task settings
            server (scheduler.Server): Server.
            db_file (_type_, optional):  Data base for monitoring. Defaults to None.
            sub (str, optional): Sumission command. Defaults to "qsub".
            stat (str, optional): Status command. Defaults to "qstat -j".
            kill (str, optional): Kill command. Defaults to "qdel".
            prefix (str, optional): SGE prefix. Defaults to "#$".
        """
        BatchSubmission.__init__(
            self,
            task,
            task_settings,
            server,
            db_file=db_file,
            sub=sub,
            stat=stat,
            kill=kill,
            prefix=prefix,
        )
        name = self.task.ecf_name.split("/")
        self.name = name[-1]

    def set_output(self):
        """Set output."""
        logfile = self.task_settings.ecf_jobout_at_host
        if os.path.exists(logfile):
            os.unlink(logfile)
        string = self.batch_prefix + " -o " + logfile + "\n"
        string += self.batch_prefix + " -e " + logfile
        return string

    def set_jobid(self):
        """Set job id."""
        # Your job XXXXXX ("name") has been submitted
        logfile = self.task.create_submission_log(self.task_settings.joboutdir)
        with open(logfile, mode="r", encoding="utf-8") as file_handler:
            lines = file_handler.readlines()

        answer = None
        for line in lines:
            answer = line

        expected_len = 7
        answer = answer.replace("\n", "")
        words = answer.split(" ")
        if len(words) == expected_len:
            # Set job id as the second element in answer
            self.job_id = str(words[2])
        else:
            raise Exception(
                "Expected " + str(expected_len) + " in output. Got " + str(len(words))
            )

    def set_job_name(self):
        """Set job name."""
        string = self.batch_prefix + " -N " + self.name + "\n"
        return string
