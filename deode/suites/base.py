"""Ecflow suites base class."""

import os

from ..logs import LogDefaults, logger
from ..submission import TaskSettings
from ..toolbox import Platform

try:
    import ecflow
except ImportError:
    ecflow = None


def _get_name(cname, cls, attrname="__plugin_name__"):
    """Get name.

    Args:
        cname (_type_): cname
        cls (_type_): cls
        attrname (str, optional): _description_. Defaults to "__plugin_name__".

    Returns:
        _type_: Name

    """
    # __dict__ vs. getattr: do not inherit the attribute from a parent class
    name = getattr(cls, "__dict__", {}).get(attrname, None)
    if name is not None:
        return name
    name = cname.lower()
    return name


class SuiteDefinition(object):
    """Definition of suite."""

    def __init__(self, config, dry_run=False):
        """Construct the definition.

        Args:
            config (ParsedConfig): Parsed configuration
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.

        Raises:
            ModuleNotFoundError: If ecflow is not loaded and not dry_run

        """
        if ecflow is None and not dry_run:
            raise ModuleNotFoundError("Ecflow not found")

        suite_name = config["general.case"]
        task_settings = TaskSettings(config)

        self.name = suite_name
        self.config = config
        self.task_settings = task_settings

        self.platform = Platform(config)
        self.config = config

        ecf_out = self.config["scheduler.ecfvars.ecf_out"]
        ecf_files = self.config["scheduler.ecfvars.ecf_files"]
        ecf_user = self.config["scheduler.ecfvars.ecf_user"]
        joboutdir = self.config["scheduler.ecfvars.ecf_jobout"]
        ecf_files_remotely = self.config["scheduler.ecfvars.ecf_files_remotely"]
        ecf_home = self.config["scheduler.ecfvars.ecf_home"]
        ecf_ssl = self.config["scheduler.ecfvars.ecf_ssl"]
        ecf_host = self.config["scheduler.ecfvars.ecf_host"]
        ecf_tries = self.config["scheduler.ecfvars.ecf_tries"]

        self.ecf_user = ecf_user
        self.ecf_host = ecf_host
        self.ecf_ssl = ecf_ssl
        self.joboutdir = joboutdir

        try:
            ecf_include = self.config["scheduler.ecfvars.ecf_include"]
        except KeyError:
            ecf_include = ecf_files
        self.ecf_include = ecf_include
        self.ecf_files = ecf_files
        if ecf_home is None:
            ecf_home = joboutdir
        self.ecf_home = ecf_home
        if ecf_out is None:
            ecf_out = joboutdir
        self.ecf_out = ecf_out
        self.ecf_micro = "%"
        ecf_jobout = (
            joboutdir
            + f"/{self.ecf_micro}ECF_NAME{self.ecf_micro}."
            + f"{self.ecf_micro}ECF_TRYNO{self.ecf_micro}"
        )
        self.ecf_jobout = ecf_jobout
        self.ecf_files_remotely = ecf_files_remotely
        if ecf_files_remotely is None:
            self.ecf_files_remotely = self.ecf_files

        # Commands started from the scheduler does not have full environment
        ecf_job_cmd = (
            f"{self.ecf_micro}TROIKA{self.ecf_micro} "
            f"-c {self.ecf_micro}TROIKA_CONFIG{self.ecf_micro} submit "
            f"-o {self.ecf_micro}ECF_JOBOUT{self.ecf_micro} "
            f"{self.ecf_micro}SCHOST{self.ecf_micro} "
            f"{self.ecf_micro}ECF_JOB{self.ecf_micro}"
        )
        # %ECF_JOB%"
        self.ecf_job_cmd = ecf_job_cmd
        ecf_status_cmd = (
            f"{self.ecf_micro}TROIKA{self.ecf_micro} "
            f"-c {self.ecf_micro}TROIKA_CONFIG{self.ecf_micro} monitor "
            f"{self.ecf_micro}SCHOST{self.ecf_micro} "
            f"{self.ecf_micro}ECF_JOB{self.ecf_micro}"
        )
        self.ecf_status_cmd = ecf_status_cmd
        ecf_kill_cmd = (
            f"{self.ecf_micro}TROIKA{self.ecf_micro} "
            f"-vv -c {self.ecf_micro}TROIKA_CONFIG{self.ecf_micro} kill "
            f"{self.ecf_micro}SCHOST{self.ecf_micro} "
            f"{self.ecf_micro}ECF_JOB{self.ecf_micro}"
        )
        self.ecf_kill_cmd = ecf_kill_cmd

        platform = Platform(config)
        try:
            troika = platform.substitute(config["troika.troika"])
        except KeyError:
            troika = "troika"
        if troika is None:
            troika = "troika"
        troika_config = config["troika.config_file"]
        config_file = config.metadata["source_file_path"]
        deode_home = platform.get_platform_value("DEODE_HOME")

        keep_workdirs = "1" if config["general.keep_workdirs"] else "0"
        loglevel = config.get("general.loglevel", LogDefaults.LEVEL).upper()
        starttime = config.get("general.times.start")
        variables = {
            "ECF_USER": self.ecf_user,
            "ECFTYPES": "fc",
            "ECF_EXTN": ".bash",
            "ECF_TRIES": ecf_tries,
            "ECF_FILES": self.ecf_files_remotely,
            "ECF_INCLUDE": self.ecf_include,
            "ECF_SSL": self.ecf_ssl,
            "ECF_HOME": self.ecf_home,
            "ECF_KILL_CMD": self.ecf_kill_cmd,
            "ECF_JOB_CMD": self.ecf_job_cmd,
            "ECF_STATUS_CMD": self.ecf_status_cmd,
            "ECF_OUT": self.ecf_out,
            "ECF_JOBOUT": self.ecf_jobout,
            "ECF_TIMEOUT": 20,
            "ECF_LOGHOST": self.ecf_host,
            "ARGS": "",
            "LOGLEVEL": loglevel,
            "CONFIG": str(config_file),
            "TROIKA": troika,
            "TROIKA_CONFIG": troika_config,
            "BASETIME": starttime,
            "VALIDTIME": starttime,
            "DEODE_HOME": deode_home,
            "NPROC": "",
            "NPROC_IO": "",
            "NPROCX": "",
            "NPROCY": "",
            "KEEP_WORKDIRS": keep_workdirs,
        }

        self.suite = EcflowSuite(
            suite_name,
            ecf_files,
            variables=variables,
            dry_run=dry_run,
            ecf_files_remotely=self.ecf_files_remotely,
        )

    def save_as_defs(self, def_file):
        """Save definition file.

        Args:
            def_file (str): Name of definition file
        """
        logger.debug("Saving def file {}", def_file)
        self.suite.save_as_defs(def_file)


class EcflowNode:
    """A Node class is the abstract base class for Suite, Family and Task.

    Every Node instance has a name, and a path relative to a suite
    """

    def __init__(
        self,
        name,
        node_type,
        parent,
        ecf_files,
        variables=None,
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
        cron=None,
    ):
        """Construct the EcflowNode.

        Args:
            name (str): Name of node
            node_type (str): Node type
            parent (EcflowNode): Parent node
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): Remote file prefix
            cron (EcflowSuiteCron): Cron. Defauts to None

        Raises:
            NotImplementedError: Node type not implemented
            TypeError: "Triggers must be an EcflowSuiteTriggers object"
            TypeError: "defstatus must be either str or an ecflow.Defstatus object"

        """
        self.name = name
        self.node_type = node_type

        has_node = True
        if parent is None:
            has_node = False
        if (
            has_node
            and node_type != "suite"
            and hasattr(parent, "ecf_node")
            and parent.ecf_node is None
        ):
            has_node = False
        if not has_node:
            self.ecf_node = None
            path = ""
        else:
            if self.node_type == "family":
                self.ecf_node = parent.ecf_node.add_family(self.name)
            elif self.node_type == "task":
                self.ecf_node = parent.ecf_node.add_task(self.name)
            elif self.node_type == "suite":
                self.ecf_node = parent.add_suite(self.name)
            else:
                raise NotImplementedError

            path = self.ecf_node.get_abs_node_path()

        self.path = path
        logger.debug("path={} ecf_files_remotely={}", self.path, ecf_files_remotely)
        if ecf_files_remotely is None:
            ecf_files_remotely = ecf_files
        self.ecf_local_container_path = ecf_files + self.path
        self.ecf_remote_container_path = ecf_files_remotely + self.path
        logger.debug(
            "path={} local_container={} remote_container={}",
            self.path,
            self.ecf_local_container_path,
            self.ecf_remote_container_path,
        )
        if variables is not None:
            for key, value in variables.items():
                logger.debug("key={} value={}", key, value)
                if self.ecf_node is not None:
                    self.ecf_node.add_variable(key, value)

        if trigger is not None:
            if isinstance(trigger, EcflowSuiteTriggers):
                if trigger.trigger_string is not None:
                    if self.ecf_node is not None:
                        self.ecf_node.add_trigger(trigger.trigger_string)
                else:
                    logger.warning("Empty trigger")
            else:
                raise TypeError("Triggers must be an EcflowSuiteTriggers object")
        self.trigger = trigger

        if cron is not None:
            if isinstance(cron, EcflowSuiteCron):
                self.ecf_node.add_cron(cron.cron)
            else:
                raise TypeError("Cron must be an EcflowSuiteCron object")

        if def_status is not None and self.ecf_node is not None:
            if isinstance(def_status, str):
                self.ecf_node.add_defstatus(ecflow.Defstatus(def_status))
            elif isinstance(def_status, ecflow.Defstatus):
                self.ecf_node.add_defstatus(def_status)
            else:
                raise TypeError(
                    "defstatus must be either str or an ecflow.Defstatus object"
                )


class EcflowNodeContainer(EcflowNode):
    """Ecflow node container."""

    def __init__(
        self,
        name,
        node_type,
        parent,
        ecf_files,
        variables=None,
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
        cron=None,
    ):
        """Construct EcflowNodeContainer.

        Args:
            name (str): Name of the node container.
            node_type (str): What kind of node.
            parent (EcflowNode): Parent to this node.
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server
            cron (EcflowSuiteCron): Cron. Defauts to None


        """
        EcflowNode.__init__(
            self,
            name,
            node_type,
            parent,
            variables=variables,
            ecf_files=ecf_files,
            trigger=trigger,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
            cron=cron,
        )


class EcflowSuite(EcflowNodeContainer):
    """EcflowSuite."""

    def __init__(
        self,
        name,
        ecf_files,
        variables=None,
        dry_run=False,
        def_status=None,
        ecf_files_remotely=None,
    ):
        """Construct the Ecflow suite.

        Args:
            name (str): Name of suite
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server

        """
        if dry_run:
            self.defs = None
        else:
            self.defs = ecflow.Defs({})

        EcflowNodeContainer.__init__(
            self,
            name,
            "suite",
            self.defs,
            ecf_files,
            variables=variables,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
        )

    def save_as_defs(self, def_file):
        """Save defintion file.

        Args:
            def_file (str): Name of the definition file.
        """
        if self.defs is not None:
            self.defs.save_as_defs(def_file)
        logger.info("def file saved to {}", def_file)


class EcflowSuiteFamily(EcflowNodeContainer):
    """A family in ecflow."""

    def __init__(
        self,
        name,
        parent,
        ecf_files,
        variables=None,
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
        cron=None,
    ):
        """Construct the family.

        Args:
                    name (str): Name of the family.
                    parent (EcflowNodeContainer): Parent node.
                    ecf_files (str): Location of ecf files
                    variables (dict, optional): Variables to map. Defaults to None
                    trigger (EcflowSuiteTriggers): Trigger. Defaults to None
                    def_status (str, ecflow.Defstatus): Def status. Defaults to None
                    ecf_files_remotely(str, optional): ECF_FILES on ecflow server
                    cron (EcflowSuiteCron): Cron. Defaut None
        """
        EcflowNodeContainer.__init__(
            self,
            name,
            "family",
            parent,
            ecf_files,
            variables=variables,
            trigger=trigger,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
            cron=cron,
        )
        logger.debug(self.ecf_remote_container_path)
        if self.ecf_node is not None:
            self.ecf_node.add_variable("ECF_FILES", self.ecf_remote_container_path)


class EcflowSuiteTask(EcflowNode):
    """A task in an ecflow suite/family."""

    def __init__(
        self,
        name,
        parent,
        config,
        task_settings,
        ecf_files,
        input_template=None,
        parse=True,
        variables=None,
        ecf_micro="%",
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
        cron=None,
    ):
        """Constuct the EcflowSuiteTask.

        Args:
            name (str): Name of task
            parent (EcflowNode): Parent node.
            ecf_files (str): Path to ecflow containers
            task_settings (TaskSettings): Submission configuration
            config (deode.ParsedConfig): Configuration file
            task_settings (deode.TaskSettings): Task settings
            input_template(str, optional): Input template
            parse (bool, optional): To parse template file or not
            variables (dict, optional): Variables to map. Defaults to None
            ecf_micro (str, optional): ECF_MICRO. Defaults to %
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server
            cron (EcflowSuiteCron): Cron. Defaut None

        Raises:
            ValueError: If input template is to be parsed but it is not passed.
            FileNotFoundError: If the task container is not found.

        """
        EcflowNode.__init__(
            self,
            name,
            "task",
            parent,
            ecf_files,
            variables=variables,
            trigger=trigger,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
            cron=cron,
        )

        logger.debug(parent.path)
        logger.debug(parent.ecf_local_container_path)
        task_container = parent.ecf_local_container_path + "/" + name + ".bash"
        if parse:
            if input_template is None:
                raise ValueError("Must pass input template if it is to be parsed")

            variables = task_settings.get_settings(name)
            logger.debug("vars {}", variables)
            for var, value_ in variables.items():
                value = value_
                if isinstance(value, int):
                    value = str(value)
                logger.debug("var={} value={}", var, value)
                if self.ecf_node is not None:
                    self.ecf_node.add_variable(var, value)
            task_settings.parse_job(
                task=name,
                config=config,
                input_template_job=input_template,
                task_job=task_container,
                variables=variables,
                ecf_micro=ecf_micro,
            )
        elif not os.path.exists(task_container):
            raise FileNotFoundError(f"Container {task_container} is missing!")


class EcflowSuiteTriggers:
    """Triggers to an ecflow suite."""

    def __init__(self, triggers, mode="AND"):
        """Construct EcflowSuiteTriggers.

        Args:
            triggers (list): List of EcflowSuiteTrigger objects.
            mode (str, optional): Cat mode. Defaults to "AND".

        """
        trigger_string = self.create_string(triggers, mode)
        self.trigger_string = trigger_string

    @staticmethod
    def create_string(triggers, mode):
        """Create the trigger string.

        Args:
            triggers (list): List of trigger objects
            mode (str): Concatenation type.

        Raises:
            ValueError: If there are no triggers to be processed
            TypeError: If trigger is not an EcflowSuiteTrigger object

        Returns:
            str: The trigger string based on trigger objects.

        """
        if not isinstance(triggers, list):
            triggers = [triggers]

        if len(triggers) == 0:
            raise ValueError("No triggers to be processed")

        trigger_string = "("
        first = True
        for trigger in triggers:
            if trigger is not None:
                cat = ""
                if not first:
                    cat = " " + mode + " "
                if isinstance(trigger, EcflowSuiteTriggers):
                    trigger_string = trigger_string + cat + trigger.trigger_string
                elif isinstance(trigger, EcflowSuiteTrigger):
                    trigger_string = (
                        trigger_string + cat + trigger.node.path + " == " + trigger.mode
                    )
                else:
                    raise TypeError("Trigger must be an EcflowSuiteTrigger object")
                first = False
        trigger_string = trigger_string + ")"
        # If no triggers were found/set
        if first:
            trigger_string = None
        return trigger_string

    def add_triggers(self, triggers, mode="AND"):
        """Add triggers.

        Args:
            triggers (EcflowSuiteTriggers): The triggers
            mode (str, optional): Cat mode. Defaults to "AND".

        """
        cat_string = " " + mode + " "
        trigger_string = self.create_string(triggers, mode)
        if trigger_string is not None:
            self.trigger_string = self.trigger_string + cat_string + trigger_string


class EcflowSuiteTrigger:
    """EcFlow Trigger in a suite."""

    def __init__(self, node, mode="complete"):
        """Create a EcFlow trigger object.

        Args:
            node (scheduler.EcflowNode): The node to trigger on
            mode (str, optional): Trigger type. Defaults to "complete".

        """
        self.node = node
        self.mode = mode


class EcflowSuiteCron:
    """EcFlow Cron in a suite."""

    def __init__(self, days_of_week, time):
        """Create a EcFlow cron oject.

        Args:
            days_of_week (list of int):  0-6, Sunday-Saturday
            time   (datatime):  time to start
        """
        time_str = time.strftime("%H:%M")
        days_of_week = list(days_of_week)
        logger.info("days: {}, time: {}", days_of_week, time_str)
        self.cron = ecflow.Cron(time_str, days_of_week=days_of_week)
