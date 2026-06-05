"""Scheduler module."""

import json
import os
import platform
import shutil
import signal
import sys
import time
import traceback
from abc import ABC, abstractmethod
from datetime import datetime
from pathlib import Path

import yaml

from .host_actions import SelectHost
from .logs import logger
from .toolbox import Platform, RemoteHost

try:
    import ecflow
except ModuleNotFoundError:
    ecflow = None


# Base Scheduler server class
class Server(ABC):
    """Base server/scheduler class."""

    def __init__(self):
        """Construct the server."""
        self.settings = None

    @abstractmethod
    def start_server(self):
        """Start the server.

        Raises:
            NotImplementedError: Must be implemented by the child server object.
        """
        raise NotImplementedError

    @abstractmethod
    def replace(self, suite_name, def_file):
        """Create or change the suite definition.

        Args:
            suite_name (str): Name of the suite.
            def_file (str): Name of the definition file.

        Raises:
            NotImplementedError: Must be implemented by the child server object.

        """
        raise NotImplementedError

    @abstractmethod
    def begin_suite(self, suite_name):
        """Begin the suite in a server specific way.

        Args:
            suite_name (str): Name of the suite

        Raises:
            NotImplementedError: Must be implemented by the child server object.
        """
        raise NotImplementedError

    def start_suite(self, suite_name, def_file, begin=True):
        """Start the suite.

        All the servers have these methods implemented and can start the server in a
        server specific way.

        Args:
            suite_name (str): Name of the suite
            def_file (str): Name of the definition file.
            begin (bool, optional): If the suite should begin. Defaults to True.
        """
        self.start_server()
        self.replace(suite_name, def_file)
        if begin:
            self.begin_suite(suite_name)

    def replace_node(self, node_path, def_file):
        """Start the suite.

        Replace a node.

        Args:
            node_path (str): Path of the node
            def_file (str): Name of the definition file.
        """
        self.replace(node_path, def_file)


class EcflowServer(Server):
    """Ecflow server."""

    def __init__(self, ecf_host, ecf_port, start_command=None, ecf_lists=None):
        """Construct the EcflowServer.

        The values for ecf_host and ecf_port are taken from config as
        strings/integer or the two functions _select_host_from_list()
        or _set_port_from_user() defined below.

        Args:
            ecf_host (str): Ecflow host
            ecf_port (int): Ecflow port
            ecf_lists (str): Ecflow lists to use, if any
            start_command (str): Ecflow start server command.

        Raises:
            ModuleNotFoundError: If ecflow is not found.

        """
        if ecflow is None:
            raise ModuleNotFoundError("Ecflow not found")

        Server.__init__(self)
        self.ecf_host = ecf_host
        self.ecf_port = ecf_port
        self.ecf_lists = ecf_lists
        if self.ecf_lists is not None:
            os.environ["ECF_LISTS"] = self.ecf_lists

        self.start_command = start_command
        logger.debug("self.ecf_host={} self.ecf_port={}", self.ecf_host, self.ecf_port)
        self.ecf_client = ecflow.Client(self.ecf_host, self.ecf_port)
        logger.debug("self.ecf_client {}", self.ecf_client)
        self.settings = {"ECF_HOST": self.ecf_host, "ECF_PORT": self.ecf_port}

    @staticmethod
    def _set_port_from_json(json_file):
        """Set ecf_port from user id.

        Arguments:
            json_file (str): Json file user to port mapping

        Returns:
            port (int): Derived port number

        Raises:
            KeyError: For users not mapped

        """
        user = os.environ.get("USER")
        with open(json_file, "r", encoding="utf-8") as f:
            user_mapping = json.load(f)

        try:
            port = user_mapping[user]["ecf_port"]
        except KeyError as err:
            raise KeyError(f"Could not find user {user} in {json_file}") from err

        return port

    @staticmethod
    def _set_port_from_user(offset=0):
        """Set ecf_port from user id.

        Arguments:
            offset (int): Number to offset the user id with

        Returns:
            port (int): Derived port number

        """
        return os.getuid() + int(offset)

    def start_server(self):
        """Start the server.

        Raises:
            RuntimeError: Server is not running or Could not restart server.
        """
        logger.debug("Start EcFlow server")
        try:
            logger.info("ECF_HOST:{}, ECF_PORT:{}", self.ecf_host, self.ecf_port)
            self.ecf_client.ping()
            logger.info("EcFlow server is already running")
        except RuntimeError:
            logger.info("Re-Start EcFlow server")
            try:
                # Start server

                start_command = self.start_command
                if self.start_command is None:
                    os.environ["ECF_HOST"] = self.ecf_host
                    os.environ["ECF_PORT"] = str(self.ecf_port)
                    start_command = f"ecflow_start.sh -p {self.ecf_port!s}"

                logger.info(start_command)
                # TODO
                ret = os.system(start_command)
                if ret != 0:
                    raise RuntimeError from RuntimeError
            except RuntimeError as error:
                raise RuntimeError("Could not restart server!") from error

    def begin_suite(self, suite_name):
        """Begin the suite.

        Args:
            suite_name (str): Nam eof the suite.
        """
        self.ecf_client.begin_suite(suite_name)

    def force_complete(self, task):
        """Force the task complete.

        Args:
            task (scheduler.EcflowTask): Task to force complete.
        """
        ecf_name = task.ecf_name
        self.ecf_client.force_state(ecf_name, ecflow.State.complete)

    def force_aborted(self, task):
        """Force the task aborted.

        Args:
            task (scheduler.EcflowTask): Task to force aborted.
        """
        ecf_name = task.ecf_name
        self.ecf_client.force_state(ecf_name, ecflow.State.aborted)

    def replace(self, suite_name, def_file):
        """Replace the suite name from def_file.

        Args:
            suite_name (str): Suite name.
            def_file (str): Definition file.

        Raises:
            RuntimeError: If suite cannot be replaced.
        """
        logger.debug("{} {}", suite_name, def_file)
        try:
            self.ecf_client.replace("/" + suite_name, def_file)
        except RuntimeError:
            try:
                self.ecf_client.delete("/" + suite_name)
                self.ecf_client.replace("/" + suite_name, def_file)
            except RuntimeError as err:
                raise RuntimeError("Could not replace suite " + suite_name) from err

    def suite_is_complete(self, suite):
        """Returns the true if a suite is complete.

        Args:
            suite(suite object): Suite

        Returns:
            suite_is_complte (boolean): Suite has complete status
        """
        self.ecf_client.sync_local()
        return suite.get_state() == ecflow.State.complete

    def remove_suites(self, suite_list, check_if_complete=True):
        """Remove suites selected from a list.

        Args:
            suite_list (list): Suite names.
            check_if_complete (boolean): True if suite has to be complete.

        """
        self.ecf_client.sync_local()
        suites = self.ecf_client.get_defs().suites
        for suite in suites:
            suite_name = suite.name()
            if suite_name in suite_list and (
                not check_if_complete or self.suite_is_complete(suite)
            ):
                logger.info("Removing suite {}", suite_name)
                self.ecf_client.delete(suite_name)
                for directory in self.get_ecf_vars(suite):
                    if os.path.isdir(directory):
                        logger.info("Remove ecflow directory {}", directory)
                        shutil.rmtree(directory)

    def get_suites_from_server(self, ignore, complete=False):
        """Get all suites from ecflow server.

        Args:
            ignore (list): List of suites which should be ignore.
            complete (boolean): True if suite should be complete.
                Defaults: Fasle.

        Returns:
            list: List of ecflow Suite objects on server.
        """
        self.ecf_client.sync_local()
        suites = self.ecf_client.get_defs().suites
        suites = [suite for suite in suites if suite.name() not in ignore]
        if complete:
            return [
                suite for suite in suites if suite.get_state() == ecflow.State.complete
            ]
        return suites

    def suite_finish_time(self, suite, force_delete_time, last_task_name=None):
        """Get time when suite finished.

        Args:
            suite (ecflow.Suite): suite object
            force_delete_time (datetime): return this time, if files don't exists.
            last_task_name (str): name of the last task in the suite

        Returns:
            float: timestamp when suite finished
        """

        def task_mtime(task):
            jobout = next(
                (
                    var.value()
                    for var in task.get_generated_variables()
                    if var.name() == "ECF_JOBOUT"
                ),
                None,
            )

            if jobout and os.path.exists(jobout):
                return os.path.getmtime(jobout)

            return force_delete_time.timestamp()

        tasks = list(self.get_all_tasks(suite))

        if last_task_name:
            last_task = next(
                (t for t in tasks if t.name() == last_task_name),
                None,
            )

            if last_task and last_task.get_state() == ecflow.State.complete:
                logger.info(
                    "Last task %s, time %s",
                    last_task.name(),
                    task_mtime(last_task),
                )
                return task_mtime(last_task)

        endtimes = [task_mtime(task) for task in tasks]

        return max(endtimes, default=force_delete_time.timestamp())

    def get_ecf_vars(self, suite):
        """Get some ecf_vars from a ecflow suite.

        Args:
            suite (Ecflow suite): suite object.

        Returns:
            ecf_vars (list): List of Paths
        """
        suite_name = suite.name()
        ecf_vars = []
        for _ecf_var in ["ECF_OUT", "ECF_HOME", "ECF_FILES"]:
            ecf_var = suite.find_variable(_ecf_var).value()
            if len(ecf_var) == 0:
                continue
            ecf_var = Path(Path(ecf_var) / suite_name)
            if ecf_var not in ecf_vars:
                ecf_vars.append(ecf_var)

        return ecf_vars

    def get_all_tasks(self, node):
        """Recursively yield all Task nodes under a Suite or Family node."""
        for child in node.nodes:
            if isinstance(child, ecflow.Task):
                yield child
            elif isinstance(child, ecflow.Family):
                # recurse into the family
                yield from self.get_all_tasks(child)

    def get_config_of_suite(self, suite):
        """Get cinfig file of the suite."""
        return Path(suite.find_variable("CONFIG").value())

    def suspend(self, task_or_path):
        """Suspend a task, family, or suite.

        Args:
            task_or_path (EcflowTask | str): Task object or full ecFlow path
        """
        if isinstance(task_or_path, str):
            ecf_path = task_or_path
        else:
            ecf_path = task_or_path.ecf_name

        logger.info("Suspending {}", ecf_path)
        self.ecf_client.suspend(ecf_path)


class EcflowEnvironment(object):
    """A class to manage ECFLOW-related variables."""

    def __init__(
        self,
        suite_name,
        suite_def_obj,
        ecf_out,
        ecf_home=None,
        ecf_user=None,
        ecf_files=None,
        ecf_include=None,
        ecf_job=None,
        ecf_job_cmd=None,
        ecf_lists=None,
        ecf_micro="%",
        ecf_extn=".bash",
        ecf_ssl=1,
        ecf_tries=1,
        ecf_timeout=20,
        troika_config_file=None,
        remote_troika_file=None,
        ecf_remote_files=None,
        ecf_remote_user=None,
    ):
        """Initialize the EcflowEnvironment instance.

        Args:
            suite_name (str): Name of the suite.
            suite_def_obj (object): Suite definition object.
            ecf_out (str): Path to the ECF_OUT directory.
            ecf_home (str, optional): Path to the ECF_HOME directory.
                                      Defaults to None.
            ecf_user (str, optional): Ecflow user. Defaults to None.
            ecf_files (str, optional): Path to the ECF_FILES directory.
                                       Defaults to None.
            ecf_include (str, optional): Path to the ECF_INCLUDE directory.
                                         Defaults to None.
            ecf_job (str, optional): Ecflow job template.
                                     Defaults to None
                                     (should be set in config).
            ecf_job_cmd (str, optional): Ecflow job command template.
                                         Defaults to None
                                         (should be set in config).
            ecf_lists (str, optional): Ecflow lists to use, if any.
                                       Defaults to None.
            ecf_micro (str, optional): Ecflow micro character. Defaults to "%".
            ecf_extn (str, optional): Ecflow job file extension.
                                      Defaults to ".bash".
            ecf_ssl (int, optional): Ecflow ssl setting. Defaults to 1.
            ecf_tries (int, optional): Number of tries
            ecf_home: Path to the ECF_HOME directory (default: None)
            ecf_files: Path to the ECF_FILES directory (default: None)
            ecf_remote_files: Path to the ECF_FILES directory on the
                              remote server (default: None)
            ecf_timeout: Timeout for ecflow client (default: 20)
            ecf_remote_user: Remote user for ecflow server (default: None)
            remote_troika_file: Path to the troika config file on the
                                remote server (default: None)
            troika_config_file: Path to the troika config file (default: None)
        """
        self.suite_name = suite_name
        self.suite_def_obj = suite_def_obj
        self.ecf_home = ecf_home
        self.ecf_user = ecf_user
        self.ecf_files = ecf_files
        self.ecf_include = ecf_include
        self.ecf_remote_files = ecf_remote_files
        self.ecf_job = ecf_job
        self.ecf_job_cmd = ecf_job_cmd
        self.ecf_out = ecf_out
        self.ecf_lists = ecf_lists
        self.ecf_micro = ecf_micro
        self.ecf_extn = ecf_extn
        self.ecf_ssl = ecf_ssl
        self.ecf_tries = ecf_tries
        self.ecf_timeout = ecf_timeout
        self.troika_config_file = troika_config_file
        self.remote_troika_file = remote_troika_file
        self.ecf_remote_user = ecf_remote_user
        if remote_troika_file is None:
            self.remote_troika_file = troika_config_file
        if self.ecf_home is None:
            self.ecf_home = self.ecf_out
        if self.ecf_remote_files is None:
            self.ecf_remote_files = self.ecf_files

    def get_property(self, property_name):
        """Gets the value of a property dynamically.

        Args:
            property_name (str): The name of the property to retrieve.

        Returns:
            Any: The value of the property.

        Raises:
            AttributeError: If the property does not exist.
        """
        if hasattr(self, property_name):
            return getattr(self, property_name)
        raise AttributeError(f"Property '{property_name}' does not exist.")

    def set_property(self, property_name, value):
        """Sets the value of a property dynamically.

        Args:
            property_name (str): The name of the property to set.
            value (Any): The value to assign to the property.
        """
        setattr(self, property_name, value)

    def display_properties(self):
        """Displays all the properties of the class."""
        for key, value in self.__dict__.items():
            logger.info("{}: {}", key, value)

    def copy_to_remote(self, server, troika=None):
        """Copy a file from local to remote location.

        Args:
            server (EcflowServer): The remote server object.
            troika (TroikaConfig, optional): Troika configuration object.
                                             Defaults to None.
        """
        # Implement the logic to copy the file from local to remote
        # Clean, then copy troika and containers

        if self.ecf_files != self.ecf_remote_files:
            srv = f"{self.ecf_remote_user}@{server.ecf_host}"
            src = f"{self.ecf_files}/{self.suite_name}"
            dst = f"{srv}:{self.ecf_remote_files}/"

            logger.info("--- SSL protocol for remote Ecflow server detected ---")
            logger.info("--- Copying job files to remote server ---")
            logger.info("Copy ecflow files from : {} to: {}", src, dst)

            # Set up remote host
            remote_host = RemoteHost(server.ecf_host, remote_user=self.ecf_remote_user)
            # Try cleaning and copying commands. If it fails,
            # then stop with message
            joboutdir = f"{dst}/{self.suite_name}"
            remote_host.clean_remote_directory_with_ssh(joboutdir)
            # Rsync jobs
            remote_host.rsync_directory_with_ssh(src, joboutdir)

            if troika is not None:
                logger.info("--- Copying troika config file to remote server ---")

                temp_troika_config_file = "parsed_troika_config.yml"
                troika.save_as(temp_troika_config_file)

                # send troika config
                remote_host.send_file_to_with_ssh(
                    temp_troika_config_file, self.remote_troika_file
                )
            logger.info("--- File copying to Ecflow server DONE ---")


class EcflowEnvironmentFromConfig(EcflowEnvironment):
    """A class to manage ECFLOW-related variables."""

    def __init__(self, config):
        """Initialize the EcflowEnvironmentFromConfig instance.

        Args:
            config (TactusConfig): Tactus configuration object.
        """
        platf = Platform(config)
        ecfvars = {
            key: platf.substitute(val) for key, val in config["scheduler.ecfvars"].items()
        }
        update = {"scheduler": {"ecfvars": ecfvars}}
        config = config.copy(update=update)

        # Assign Ecfvars
        ecf_out = config.get("scheduler.ecfvars.ecf_jobout")
        ecf_files = config.get("scheduler.ecfvars.ecf_files")
        ecf_remote_files = config.get("scheduler.ecfvars.ecf_files_remotely")
        ecf_home = config.get("scheduler.ecfvars.ecf_home")

        ecf_user = config.get("scheduler.ecfvars.ecf_user")
        ecf_remote_user = config.get("scheduler.ecfvars.ecf_remoteuser")

        suite_def = config.get("suite_control.suite_definition", "TactusSuiteDefinition")

        suite_name = config.get("general.case")
        suite_name = Platform(config).substitute(suite_name)

        ecf_include = config.get("scheduler.ecfvars.ecf_include")
        if ecf_include is None:
            ecf_include = ecf_files

        if ecf_home is None:
            ecf_home = ecf_out

        ecf_extn = ".bash"
        ecf_micro = "%"

        # Get the troika config file - in case defined in ecfvars, use that to allow
        # for scheduler specific troika config file
        troika_config_file = Platform(config).substitute(
            config.get(
                "scheduler.ecfvars.troika.config_file", config["troika.config_file"]
            )
        )
        if ecf_home != ecf_out:
            remote_troika_config_file = os.path.join(
                ecf_remote_files, suite_name, os.path.basename(troika_config_file)
            )
        else:
            remote_troika_config_file = troika_config_file

        super().__init__(
            suite_name,
            suite_def,
            ecf_out,
            ecf_home=ecf_home,
            ecf_user=ecf_user,
            troika_config_file=troika_config_file,
            remote_troika_file=remote_troika_config_file,
            ecf_remote_files=ecf_remote_files,
            ecf_remote_user=ecf_remote_user,
            ecf_files=ecf_files,
            ecf_include=ecf_include,
            ecf_micro=ecf_micro,
            ecf_extn=ecf_extn,
        )


class EcflowServerFromConfig(EcflowServer):
    """Create an ecflow server object from a tactus configuration file."""

    def __init__(self, config, start_command=None):
        """Initialize the EcflowServerFromConfig instance.

        Args:
            config (TactusConfig): Tactus configuration object.
            start_command (str, optional): Command to start the ecflow server.
                                           Defaults to None.
        """
        platf = Platform(config)
        ecfvars = {
            key: platf.substitute(val) for key, val in config["scheduler.ecfvars"].items()
        }
        update = {"scheduler": {"ecfvars": ecfvars}}
        config = config.copy(update=update)

        logger.info("Starting suite...")
        logger.info("Ecflow settings: ")

        # Assign Ecfvars
        ecf_host = config["scheduler.ecfvars.ecf_host"]
        ecf_host = platf.substitute(ecf_host)
        ecf_host = platf.evaluate(ecf_host, object_=SelectHost)
        ecf_port = config["scheduler.ecfvars.ecf_port"]
        try:
            ecf_port = int(ecf_port)
        except ValueError:
            ecf_port = platf.substitute(ecf_port)
            ecf_port = platf.evaluate(ecf_port, object_=EcflowServer)

        # Create server object (possibly start it)
        super().__init__(ecf_host, ecf_port, start_command=start_command)


class EcflowLogServer:
    """Ecflow log server."""

    def __init__(self, config):
        """Constuct the ecflow log server.

        Args:
            config (dict): Configuration
        """
        self.config = config
        self.ecf_loghost = config.get("ECF_LOGHOST")
        self.ecf_logport = config.get("ECF_LOGPORT")


class EcflowTask:
    """Ecflow scheduler task."""

    def __init__(self, ecf_name, ecf_tryno, ecf_pass, ecf_rid, ecf_timeout=20):
        """Construct a task running and communicating with ecflow server.

        Args:
            ecf_name (str): Full name of ecflow task.
            ecf_tryno (int): Ecflow task try number
            ecf_pass (str): Ecflow task password
            ecf_rid (int): Ecflow runtime ID
            ecf_timeout (int, optional): _description_. Defaults to 20.

        """
        self.ecf_name = ecf_name
        self.ecf_tryno = int(ecf_tryno)
        self.ecf_pass = ecf_pass
        if not ecf_rid:
            ecf_rid = os.getpid()
        self.ecf_rid = int(ecf_rid)
        self.ecf_timeout = int(ecf_timeout)
        ecf_name_parts = self.ecf_name.split("/")
        self.ecf_task = ecf_name_parts[-1]
        ecf_families = None
        if len(ecf_name_parts) > 2:
            ecf_families = ecf_name_parts[1:-1]
        self.ecf_families = ecf_families
        self.family1 = None
        if self.ecf_families is not None:
            self.family1 = self.ecf_families[-1]

    def __str__(self):
        """String representation of the task.

        Returns:
            str: String representation of the task.
        """
        return f"{self.ecf_name}"


class EcflowClient(object):
    """An ecflow client.

    Encapsulate communication with the ecflow server. This will automatically call
    the child command init()/complete(), for job start/finish. It will also
    handle exceptions and signals, by calling the abort child command.
    *ONLY* one instance of this class, should be used. Otherwise zombies will be created.
    """

    def __init__(self, server, task):
        """Construct the ecflow client.

        Args:
            server (EcflowServer): Ecflow server object.
            task (EcflowTask): Ecflow task object.

        """
        logger.debug("Creating Client")
        self.server = server
        self.client = server.ecf_client
        # self.ci.set_host_port("%ECF_HOST%", "%ECF_PORT%") #noqa E800
        self.client.set_child_pid(task.ecf_rid)
        self.client.set_child_path(task.ecf_name)
        self.client.set_child_password(task.ecf_pass)
        self.client.set_child_try_no(task.ecf_tryno)
        logger.info(
            "   Only wait {} seconds, if the server cannot be contacted "
            "(note default is 24 hours) before failing",
            str(task.ecf_timeout),
        )
        self.client.set_child_timeout(task.ecf_timeout)
        self.task = task

        # Abort the task for the following signals
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGHUP, self.signal_handler)
        signal.signal(signal.SIGQUIT, self.signal_handler)
        signal.signal(signal.SIGILL, self.signal_handler)
        signal.signal(signal.SIGTRAP, self.signal_handler)
        signal.signal(signal.SIGIOT, self.signal_handler)
        signal.signal(signal.SIGBUS, self.signal_handler)
        signal.signal(signal.SIGFPE, self.signal_handler)
        signal.signal(signal.SIGUSR1, self.signal_handler)
        signal.signal(signal.SIGUSR2, self.signal_handler)
        signal.signal(signal.SIGPIPE, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
        signal.signal(signal.SIGXCPU, self.signal_handler)
        if platform.system() != "Darwin":
            signal.signal(signal.SIGPWR, self.signal_handler)

    @staticmethod
    def at_time():
        """Generate time stamp.

        Returns:
            str: Time stamp.
        """
        return datetime.fromtimestamp(time.time()).strftime("%H:%M:%S")

    def signal_handler(self, signum, extra=None):
        """Signal handler.

        Args:
            signum (_type_): _description_
            extra (_type_, optional): _description_. Defaults to None.
        """
        logger.info("   Aborting: Signal handler called with signal {}", str(signum))

        self.__exit__(
            InterruptedError, "Signal handler called with signal " + str(signum), extra
        )

    def __enter__(self):
        """Enter the object.

        Returns:
            _type_: _description_
        """
        logger.info("Calling init at: {}", self.at_time())
        if self.client is not None:
            self.client.child_init()
        return self.client

    def __exit__(self, ex_type, value, tback):
        """Exit method.

        Args:
            ex_type (_type_): _description_
            value (_type_): _description_
            tback (_type_): _description_

        Returns:
            _type_: _description_
        """
        logger.info("   Client:__exit__: ex_type: {} value: {}", str(ex_type), str(value))
        if ex_type is not None:
            logger.info("Calling abort {}", self.at_time())
            self.client.child_abort(f"Aborted with exception type {ex_type!s}:{value!s}")
            if tback is not None:
                print(tback)
                traceback.print_tb(tback, limit=1, file=sys.stdout)
                print("*** print_exception:")
                # exc_type below is ignored on 3.5 and later
                print("*** print_exc:")
                traceback.print_exc(limit=2, file=sys.stdout)
                print("*** format_exc, first and last line:")
                formatted_lines = traceback.format_exc().splitlines()
                print(formatted_lines[0])
                print(formatted_lines[-1])
                print("*** format_exception:")
                print("*** extract_tb:")
                print(repr(traceback.extract_tb(tback)))
                print("*** format_tb:")
                print(repr(traceback.format_tb(tback)))
                print("*** tb_lineno:", tback.tb_lineno)
            return False
        print("Calling complete at: " + self.at_time())
        # self.server.update_log(self.task.ecf_name + " complete") #noqa E800
        self.client.child_complete()
        return False


class TroikaConfiguration:
    """Troika configuration."""

    def __init__(self, troika_config):
        """Initialize the TroikaConfiguration instance.

        Args:
            troika_config (dict): Troika configuration settings
        """
        self.config = troika_config

    def save_as(self, filename):
        """Save the troika config to a file.

        Args:
            filename (str): Path to the file to save the config to.
        """
        with open(filename, mode="w", encoding="utf8") as outfile:
            yaml.dump(self.config, outfile, encoding="utf-8")

    def substitute_troika_config(self, platf):
        """Parse the troika config file and return a dictionary of the values.

        Args:
            platf (Platform): Platform object to use for substitution.
        """
        subbed_config = platf.sub_str_dict(self.config)
        self.config = subbed_config


class TroikaConfigurationFromConfig(TroikaConfiguration):
    """Troika configuration from Tactus Configuration object."""

    def __init__(self, config):
        """Initialize the TroikaConfigurationFromConfig instance.

        Args:
            config (dict): Tactus configuration object.
        """
        troika_config_file = Platform(config).substitute(
            config.get(
                "scheduler.ecfvars.troika.config_file", config["troika.config_file"]
            )
        )
        troika_config = yaml.safe_load(open(troika_config_file, "r", encoding="utf-8"))
        super().__init__(troika_config)
