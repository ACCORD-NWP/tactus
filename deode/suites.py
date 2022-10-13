"""Ecflow suites."""
import os
import logging
try:
    import ecflow
except ImportError:
    ecflow = None


class SuiteDefinition(object):
    """The definition of the suite."""

    def __init__(self, suite_name, joboutdir, ecf_files,
                 submission_file, server_logfile, loglevel,
                 ecf_home=None, ecf_include=None, ecf_out=None,
                 ecf_jobout=None):
        """Construct the definition.

        Args:
            suite_name (str): Name of suite
            joboutdir (str): Path to jobfiles
            ecf_files (str): Path to ecflow containers
            submssion_file (dict): Submission configuration
            ecf_home (str, optional): ECF_HOME. Defaults to None.
            ecf_include (str, optional): ECF_INCLUDE.
                                         Defaults to None which uses ecf_files.
            ecf_out (str, optional): ECF_OUT. Defaults to None.
            ecf_jobout (str, optional): ECF_JOBOUT. Defaults to None.

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
        if ecf_jobout is None:
            ecf_jobout = joboutdir + "/%ECF_NAME%.%ECF_TRYNO%"
        self.ecf_jobout = ecf_jobout

        self.submission_file = submission_file

        ecf_job_cmd = "deode submit " \
            "--submit %SUBMISSION_FILE% " \
            "--joboutdir %ECF_OUT% " \
            "--log %SERVER_LOGFILE% " \
            "--ecf_host %ECF_HOST% " \
            "--ecf_port %ECF_PORT% " \
            "--ecf_name %ECF_NAME% " \
            "--ecf_tryno %ECF_TRYNO% " \
            "--ecf_pass %ECF_PASS% " \
            "--ecf_rid %ECF_RID% "
        self.ecf_job_cmd = ecf_job_cmd
        ecf_status_cmd = "deode status " \
            "--submit %SUBMISSION_FILE% " \
            "--joboutdir %ECF_OUT% " \
            "--ecf_host %ECF_HOST% " \
            "--ecf_port %ECF_PORT% " \
            "--log %SERVER_LOGFILE% " \
            "--ecf_name %ECF_NAME% " \
            "--ecf_tryno %ECF_TRYNO% " \
            "--ecf_pass %ECF_PASS% " \
            "--ecf_rid %ECF_RID% " \
            "--submission_id %SUBMISSION_ID%"

        self.ecf_status_cmd = ecf_status_cmd
        ecf_kill_cmd = "deode kill " \
            "--submit %SUBMISSION_FILE% " \
            "--joboutdir %ECF_OUT% " \
            "--ecf_host %ECF_HOST% " \
            "--ecf_port %ECF_PORT% " \
            "--log %SERVER_LOGFILE% " \
            "--ecf_name %ECF_NAME% " \
            "--ecf_tryno %ECF_TRYNO% " \
            "--ecf_pass %ECF_PASS% " \
            "--ecf_rid %ECF_RID% " \
            "--submission_id %SUBMISSION_ID%"
        self.ecf_kill_cmd = ecf_kill_cmd

        variables = {
            "ECF_EXTN": ".py",
            "ECF_FILES": self.ecf_files,
            "ECF_INCLUDE": self.ecf_include,
            "ECF_TRIES": 1,
            "SUBMISSION_ID": "",
            "ECF_HOME": self.ecf_home,
            "ECF_KILL_CMD": self.ecf_kill_cmd,
            "ECF_JOB_CMD": self.ecf_job_cmd,
            "ECF_STATUS_CMD": self.ecf_status_cmd,
            "ECF_OUT": self.ecf_out,
            "ECF_JOBOUT": self.ecf_jobout,
            "SUBMISSION_FILE": self.submission_file,
            "SERVER_LOGFILE": server_logfile,
            "LOGLEVEL": loglevel
        }

        self.suite = EcflowSuite(name, variables=variables)

        family = EcflowSuiteFamily("TestFamily", self.suite)
        EcflowSuiteTask("Background", family, ecf_files=self.ecf_files, ecf_timeout=5)
        EcflowSuiteTask("Forecast", family, ecf_files=self.ecf_files, ecf_timeout=5)

    def save_as_defs(self, def_file):
        """Save definition file.

        Args:
            def_file (str): Name of definition file
        """
        self.suite.save_as_defs(def_file)


class EcflowNode():
    """A Node class is the abstract base class for Suite, Family and Task.

    Every Node instance has a name, and a path relative to a suite
    """

    def __init__(self, name, node_type, parent, variables=None):
        """Construct the EcflowNode.

        Args:
            name (str): Name of node
            node_type (str): Node type
            parent (EcflowNode): Parent node

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
        if variables is not None:
            for key, value in variables.items():
                print(key, value)
                self.ecf_node.add_variable(key, value)


class EcflowNodeContainer(EcflowNode):
    """Ecflow node container.

    Args:
        EcflowNode (EcflowNode): Parent class.
    """

    def __init__(self, name, node_type, parent, **kwargs):
        """Construct EcflowNodeContainer.

        Args:
            name (str): Name of the node container.
            node_type (str): What kind of node.
            parent (EcflowNode): Parent to this node.

        """
        EcflowNode.__init__(self, name, node_type, parent, **kwargs)


class EcflowSuite(EcflowNodeContainer):
    """EcflowSuite.

    Args:
        EcflowNodeContainer
        (EcflowNodeContainer): A child of the EcflowNodeContainer class.
    """

    def __init__(self, name, **kwargs):
        """Construct the Ecflow suite.

        Args:
            name (_type_): _description_

        """
        self.defs = ecflow.Defs({})

        EcflowNodeContainer.__init__(self, name, "suite", self.defs, **kwargs)

    def save_as_defs(self, def_file):
        """Save defintion file.

        Args:
            def_file (str): Name of the definition file.
        """
        self.defs.save_as_defs(def_file)
        logging.info("def file saved to %s", def_file)


class EcflowSuiteFamily(EcflowNodeContainer):
    """A family in ecflow.

    Args:
        EcflowNodeContainer (_type_): _description_
    """

    def __init__(self, name, parent, **kwargs):
        """Construct the family.

        Args:
            name (str): Name of the family.
            parent (EcflowNodeContainer): Parent node.

        """
        EcflowNodeContainer.__init__(self, name, "family", parent, **kwargs)


class EcflowSuiteTask(EcflowNode):
    """A task in an ecflow suite/family.

    Args:
        EcflowNode (EcflowNodeContainer): The node container.
    """

    def __init__(self, name, parent, **kwargs):
        """Constuct the EcflowSuiteTask.

        Args:
            name (str): Name of task
            parent (EcflowNode): Parent node.

        Raises:
            Exception: Safety check
        """
        EcflowNode.__init__(self, name, "task", parent)

        ecf_files = kwargs.get("ecf_files")
        if ecf_files is not None:

            if name == "default":
                raise Exception("Job should not be called default")
            else:
                default_job = ecf_files + "/default.py"
                task_job = ecf_files + "/" + name + ".py"
                if not os.path.exists(task_job) and not \
                        os.path.islink(task_job):
                    print(default_job + " - > " + task_job)
                    os.symlink(default_job, task_job)
