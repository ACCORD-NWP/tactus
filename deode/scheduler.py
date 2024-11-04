"""Scheduler module."""

import os
import platform
import signal
import sys
import time
import traceback
from abc import ABC, abstractmethod
from datetime import datetime

from .logs import logger
from .os_utils import ping
from .toolbox import Platform

try:
    import ecflow
except ModuleNotFoundError:
    ecflow = None


# Base Scheduler server class
class Server(ABC):
    """Base server/scheduler class."""

    def __init__(self, config):
        """Construct the server."""
        self.settings = None
        self.config = config

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


class EcflowServer(Server):
    """Ecflow server."""

    def __init__(self, config, start_command=None):
        """Construct the EcflowServer.

        The values for ecf_host and ecf_port are taken from config as
        strings/integer or the two functions _select_host_from_list()
        or _set_port_from_user() defined below.

        Args:
            config (str): configuration settings.
            start_command (str): Ecflow start server command.

        Raises:
            ModuleNotFoundError: If ecflow is not found.
            RuntimeError: If ecf_port is not set

        """
        if ecflow is None:
            raise ModuleNotFoundError("Ecflow not found")

        Server.__init__(self, config)

        ecf_host = self.config["scheduler.ecfvars.ecf_host"]
        ecf_host = Platform(config).substitute(ecf_host)
        self.ecf_host = Platform(config).evaluate(ecf_host, object_=EcflowServer)

        ecf_port = self.config["scheduler.ecfvars.ecf_port"]
        try:
            self.ecf_port = int(ecf_port)
        except ValueError:
            self.ecf_port = Platform(config).evaluate(ecf_port, object_=EcflowServer)

        self.start_command = start_command
        logger.debug("self.ecf_host={} self.ecf_port={}", self.ecf_host, self.ecf_port)
        self.ecf_client = ecflow.Client(self.ecf_host, self.ecf_port)
        logger.debug("self.ecf_client {}", self.ecf_client)
        self.settings = {"ECF_HOST": self.ecf_host, "ECF_PORT": self.ecf_port}

    @staticmethod
    def _set_port_from_user(offset=0):
        """Set ecf_port from user id.

        Arguments:
            offset (int): Number to offset the user id with

        Returns:
            port (int): Derived port number

        """
        port = os.getuid() + int(offset)
        return port

    @staticmethod
    def _select_host_from_list(hosts, tries=3, delay=1):
        """Set ecf_host from list of options.

           Try to ping server tries times before giving up.

        Arguments:
            hosts (list): list of host options
            tries (int): number of times to try to find a host
            delay (int): number of seconds to wait between each try

        Returns:
            host (str): Selected host

        Raises:
            RuntimeError: In case no or more than one host found
        """
        found_hosts = []
        ntry = 1
        while ntry <= tries:
            for _host in hosts:
                host = _host.strip()
                if ping(host):
                    found_hosts.append(host)

            if len(found_hosts) == 0 and ntry == tries:
                host_list = ",".join(hosts)
                msg = f"No ecflow host found, tried:{host_list}"
                logger.error(msg)
                raise RuntimeError(msg)

            if len(found_hosts) == 1:
                break

            if len(found_hosts) > 1:
                host_list = ",".join(found_hosts)
                msg = f"Ambigious host selection:{host_list}"
                logger.error(msg)
                raise RuntimeError(msg)

            time.sleep(delay)
            ntry += 1

        return found_hosts[0]

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
                    start_command = f"ecflow_start.sh -p {self.ecf_port!s}"

                logger.info(start_command)
                # TODO
                ret = os.system(start_command)  # noqa
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

    def remove_suites(self, suite_list):
        """Remove suites selected from a list.

        Args:
            suite_list (list): Suite names.

        """
        self.ecf_client.sync_local()

        suites = self.ecf_client.get_defs().suites
        for suite in suites:
            if suite.name() in suite_list:
                logger.info("Removing suite {}", suite.name())
                self.ecf_client.delete(suite.name())


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
        if ecf_rid == "" or ecf_rid is None:
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
