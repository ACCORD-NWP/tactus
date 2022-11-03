"""Ecflow suites."""

import os

from .logs import get_logger

try:
    import ecflow
except ImportError:
    ecflow = None


logger = get_logger(__name__, "DEBUG")


class SuiteDefinition(object):
    """The definition of the suite."""

    def __init__(
        self,
        suite_name,
        joboutdir,
        ecf_files,
        config,
        task_settings,
        loglevel,
        ecf_home=None,
        ecf_include=None,
        ecf_out=None,
        ecf_jobout=None,
        ecf_micro="%",
    ):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the definition.

        Args:
            suite_name (str): Name of suite
            joboutdir (str): Path to jobfiles
            ecf_files (str): Path to ecflow containers
            task_settings (TaskSettings): Submission configuration
            config (deode.parsedConfig): Configuration
            task_settings (deode.TaskSettings): Task settings
            loglevel (str): Loglevel
            ecf_home (str, optional): ECF_HOME. Defaults to None.
            ecf_include (str, optional): ECF_INCLUDE.
                                         Defaults to None which uses ecf_files.
            ecf_out (str, optional): ECF_OUT. Defaults to None.
            ecf_jobout (str, optional): ECF_JOBOUT. Defaults to None.
            ecf_micro (str, optional): ECF_MICRO. Defaults to %

        Raises:
            Exception: Unless ecflow is not loaded

        """
        if ecflow is None:
            raise Exception("Ecflow not loaded properly")

        name = suite_name
        self.joboutdir = joboutdir
        if ecf_include is None:
            ecf_include = ecf_files
        self.ecf_include = ecf_include
        self.ecf_files = ecf_files
        if ecf_home is None:
            ecf_home = joboutdir
        self.ecf_home = ecf_home
        if ecf_out is None:
            ecf_out = joboutdir
        self.ecf_out = ecf_out
        self.ecf_micro = ecf_micro
        if ecf_jobout is None:
            ecf_jobout = (
                joboutdir
                + f"/{ecf_micro}ECF_NAME{ecf_micro}.{ecf_micro}ECF_TRYNO{ecf_micro}"
            )
        self.ecf_jobout = ecf_jobout

        self.task_settings = task_settings

        # Commands started from the scheduler does not have full environment
        ecf_job_cmd = (
            f"{ecf_micro}TROIKA{ecf_micro} "
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} submit "
            f"-o {ecf_micro}ECF_JOBOUT{ecf_micro} "
            f"{ecf_micro}SCHOST{ecf_micro} "
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        )
        # %ECF_JOB%"
        self.ecf_job_cmd = ecf_job_cmd
        ecf_status_cmd = (
            f"{ecf_micro}TROIKA{ecf_micro} "
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} monitor "
            f"{ecf_micro}SCHOST{ecf_micro} "
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        )
        self.ecf_status_cmd = ecf_status_cmd
        ecf_kill_cmd = (
            f"{ecf_micro}TROIKA{ecf_micro} "
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} kill "
            f"{ecf_micro}SCHOST{ecf_micro} "
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        )
        self.ecf_kill_cmd = ecf_kill_cmd

        troika = "/opt/troika/bin/troika"
        troika_config = "/opt/troika/etc/troika.yml"
        variables = {
            "ECF_EXTN": ".py",
            "ECF_FILES": self.ecf_files,
            "ECF_INCLUDE": self.ecf_include,
            "ECF_TRIES": 1,
            "ECF_HOME": self.ecf_home,
            "ECF_KILL_CMD": self.ecf_kill_cmd,
            "ECF_JOB_CMD": self.ecf_job_cmd,
            "ECF_STATUS_CMD": self.ecf_status_cmd,
            "ECF_OUT": self.ecf_out,
            "ECF_JOBOUT": self.ecf_jobout,
            "ECF_TIMEOUT": 20,
            "LOGLEVEL": loglevel,
            "CONFIG": str(config),
            "TROIKA": troika,
            "TROIKA_CONFIG": troika_config,
        }

        input_template = ecf_files + "/default.py"
        self.suite = EcflowSuite(name, ecf_files, variables=variables)

        family = EcflowSuiteFamily("TestFamily", self.suite, ecf_files)
        # Background dos not work. Deode is ot able to run on vm with shared HOME as
        # hpc-login
        # EcflowSuiteTask("Background", family, self.task_settings, ecf_files,
        #                input_template=input_template)

        logger.debug(self.task_settings.get_task_settings("Forecast"))

        variables = {"ECF_TIMEOUT": 5}
        EcflowSuiteTask(
            "Forecast",
            family,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=variables,
        )
        family2 = EcflowSuiteFamily("TestFamily2", family, ecf_files)
        variables = {"ECF_TIMEOUT": 10}
        EcflowSuiteTask(
            "Forecast",
            family2,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=variables,
        )
        EcflowSuiteTask(
            "Serial",
            family2,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
        )

    def save_as_defs(self, def_file):
        """Save definition file.

        Args:
            def_file (str): Name of definition file
        """
        self.suite.save_as_defs(def_file)


class EcflowNode:
    """A Node class is the abstract base class for Suite, Family and Task.

    Every Node instance has a name, and a path relative to a suite
    """

    def __init__(self, name, node_type, parent, ecf_files, variables=None):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the EcflowNode.

        Args:
            name (str): Name of node
            node_type (str): Node type
            parent (EcflowNode): Parent node
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None

        Raises:
            NotImplementedError: Node type not implemented

        """
        self.name = name
        self.node_type = node_type

        if self.node_type == "family":
            self.ecf_node = parent.ecf_node.add_family(self.name)
        elif self.node_type == "task":
            self.ecf_node = parent.ecf_node.add_task(self.name)
        elif self.node_type == "suite":
            self.ecf_node = parent.add_suite(self.name)
        else:
            raise NotImplementedError

        self.path = self.ecf_node.get_abs_node_path()
        self.ecf_container_path = ecf_files + self.path
        if variables is not None:
            for key, value in variables.items():
                logger.debug("key=%s value=%s", key, value)
                self.ecf_node.add_variable(key, value)


class EcflowNodeContainer(EcflowNode):
    """Ecflow node container.

    Args:
        EcflowNode (EcflowNode): Parent class.
    """

    def __init__(self, name, node_type, parent, ecf_files, variables=None):
        # TODO: Document the variables that right now only are described as "?"
        """Construct EcflowNodeContainer.

        Args:
            name (str): Name of the node container.
            node_type (str): What kind of node.
            parent (EcflowNode): Parent to this node.
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None

        """
        EcflowNode.__init__(
            self, name, node_type, parent, variables=variables, ecf_files=ecf_files
        )


class EcflowSuite(EcflowNodeContainer):
    """EcflowSuite.

    Args:
        EcflowNodeContainer
        (EcflowNodeContainer): A child of the EcflowNodeContainer class.
    """

    def __init__(self, name, ecf_files, variables=None):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the Ecflow suite.

        Args:
            name (str): Name of suite
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None

        """
        self.defs = ecflow.Defs({})

        EcflowNodeContainer.__init__(
            self, name, "suite", self.defs, ecf_files, variables=variables
        )

    def save_as_defs(self, def_file):
        """Save defintion file.

        Args:
            def_file (str): Name of the definition file.
        """
        self.defs.save_as_defs(def_file)
        logger.info("def file saved to %s", def_file)


class EcflowSuiteFamily(EcflowNodeContainer):
    """A family in ecflow.

    Args:
        EcflowNodeContainer (_type_): _description_
    """

    def __init__(self, name, parent, ecf_files, variables=None):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the family.

        Args:
            name (str): Name of the family.
            parent (EcflowNodeContainer): Parent node.
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None

        """
        EcflowNodeContainer.__init__(
            self, name, "family", parent, ecf_files, variables=variables
        )
        logger.debug(self.ecf_container_path)
        self.ecf_node.add_variable("ECF_FILES", self.ecf_container_path)


class EcflowSuiteTask(EcflowNode):
    """A task in an ecflow suite/family.

    Args:
        EcflowNode (EcflowNodeContainer): The node container.
    """

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
    ):
        """Constuct the EcflowSuiteTask.

        Args:
            name (str): Name of task
            parent (EcflowNode): Parent node.
            ecf_files (str): Path to ecflow containers
            task_settings (TaskSettings): Submission configuration
            config (deode.parsedConfig): Configuration
            task_settings (deode.TaskSettings): Task settings
            input_template(str, optional): Input template
            parse (bool, optional): To parse template file or not
            variables (dict, optional): Variables to map. Defaults to None
            ecf_micro (str, optional): ECF_MICRO. Defaults to %

        Raises:
            Exception: Safety check
            FileNotFoundError: If the task container is not found.

        """
        EcflowNode.__init__(self, name, "task", parent, ecf_files, variables=variables)

        logger.debug(parent.path)
        logger.debug(parent.ecf_container_path)
        task_container = parent.ecf_container_path + "/" + name + ".py"
        if parse:
            if input_template is None:
                raise Exception("Input template is missing")

            variables = task_settings.get_settings(name)
            logger.debug("vars %s", variables)
            for var, value in variables.items():
                logger.debug("var=%s value=%s", var, value)
                self.ecf_node.add_variable(var, value)
            task_settings.parse_job(
                name,
                config,
                input_template,
                task_container,
                variables=variables,
                ecf_micro=ecf_micro,
            )
        else:
            if not os.path.exists(task_container):
                raise FileNotFoundError(f"Container {task_container} is missing!")
