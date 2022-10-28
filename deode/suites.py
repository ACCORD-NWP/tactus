"""Ecflow suites."""

import os
import json
import subprocess
from .logs import get_logger

try:
    import ecflow
except ImportError:
    ecflow = None


logger = get_logger(__name__, "DEBUG")


class TaskSettings(object):
    """Set the task specific setttings."""

    def __init__(self, submission_defs):
        """Construct the task specific settings.

        Args:
             submission_defs(dict): Submission definitions
        """
        self.submission_defs = submission_defs

    def parse_submission_defs(self, task):
        """"Parse the submssion definitions.

        Returns:
            dict: parsed setting
        """
        task_settings = {
            "BATCH": {},
            "ENV": {}
        }
        all_defs = self.submission_defs
        submit_types = all_defs["submit_types"]
        default_submit_type = all_defs["default_submit_type"]
        task_submit_type = None
        for s_t in submit_types:
            if s_t in all_defs and "tasks" in all_defs[s_t]:
                for tname in all_defs[s_t]["tasks"]:
                    if tname == task:
                        task_submit_type = s_t
        if task_submit_type is None:
            task_submit_type = default_submit_type

        if task_submit_type in all_defs:
            for setting in all_defs[task_submit_type]:
                if setting != "tasks":
                    task_settings.update({setting: all_defs[task_submit_type][setting]})

        # TODO do it recursively
        if "task_exceptions" in all_defs:
            if task in all_defs["task_exceptions"]:
                keywords = ["BATCH", "ENV"]
                for kword in keywords:
                    if kword in all_defs["task_exceptions"][task]:
                        kword_settings = all_defs["task_exceptions"][task][kword]
                        for key, value in kword_settings.items():
                            if key in task_settings:
                                logger.warning("key=%s already exists in "
                                    "task_settings", key)
                            task_settings[kword].update({key: value})

        logger.debug(task_settings)
        return task_settings

    def get_task_settings(self, task, key=None, variables=None, ecf_micro="%"):
        """Get task settings.

        Args:
            task (_type_): _description_
            key (_type_, optional): _description_. Defaults to None.
            variables (_type_, optional): _description_. Defaults to None.
            ecf_micro (str, optional): _description_. Defaults to "%".

        Returns:
            _type_: _description_
        """
        task_settings = self.parse_submission_defs(task)
        if key is None:
            return task_settings
        else:
            if key in task_settings:
                m_task_settings = {}
                logger.debug(type(task_settings[key]))
                if isinstance(task_settings[key], dict):
                    for setting, value in task_settings[key].items():
                        logger.debug("%s %s variables: %s", setting, value, variables)
                        if variables is not None:
                            if setting in variables:
                                value = f"{ecf_micro}{setting}{ecf_micro}"
                                logger.debug(value)
                        m_task_settings.update({setting: value})
                    logger.debug(m_task_settings)
                    return m_task_settings
                else:
                    value = task_settings[key]
                    if variables is not None:
                        if key in variables:
                            value = f"{ecf_micro}{variables[setting]}{ecf_micro}"
                    return value
            return None

    def recursive_items(self, dictionary):
        """Recursive loop of dict.

        Args:
            dictionary (_type_): _description_

        Yields:
            _type_: _description_
        """
        for key, value in dictionary.items():
            if isinstance(value, dict):
                yield (key, value)
                yield from self.recursive_items(value)
            else:
                yield (key, value)

    def get_settings(self, task):
        """Get the settings.

        Args:
            task (_type_): _description_

        Returns:
            _type_: _description_
        """
        settings = {}
        task_settings = self.parse_submission_defs(task)
        keys = []
        for key, value in self.recursive_items(task_settings):
            if isinstance(value, str):
                logger.debug(key)
                keys.append(key)
        logger.debug(keys)
        for key, value in self.recursive_items(task_settings):
            logger.debug("key=%s value=%s", key, value)
            if key in keys:
                logger.debug("update %s %s", key, value)
                settings.update({key: value})
        return settings

    def parse_job(self, task, input_template_job, task_job, variables=None,
                  ecf_micro='%'):
        """Read default job and change interpretor.

        Args:
            task(str): Task name
            input_template_job (str): Input container template.
            task_job (str): Task container
            variables (_type_, optional): _description_. Defaults to None.
            ecf_micro (str, optional): _description_. Defaults to '%'.

        """
        interpreter = self.get_task_settings(task, "INTERPRETER")
        logger.debug(interpreter)
        if interpreter is None:
            cmd = ['type', 'python3']
            returned_output = \
                subprocess.check_output(cmd).decode("utf-8").strip().split(' ')[-1]
            interpreter = f"#!{returned_output}"

        logger.debug(interpreter)
        with open(input_template_job, mode="r", encoding="utf-8") as file_handler:
            input_content = file_handler.read()
        dir_name = os.path.dirname(os.path.realpath(task_job))
        if not os.path.exists(dir_name):
            os.makedirs(dir_name, exist_ok=True)
        with open(task_job, mode="w", encoding="utf-8") as file_handler:
            file_handler.write(f"{interpreter}\n")
            batch_settings = self.get_task_settings(task, "BATCH", variables=variables,
                                                    ecf_micro=ecf_micro)
            logger.debug("batch settings %s", batch_settings)
            for __, b_setting in batch_settings.items():
                file_handler.write(f"{b_setting}\n")
            env_settings = self.get_task_settings(task, "ENV", variables=variables,
                                                  ecf_micro=ecf_micro)
            logger.debug(env_settings)
            python_task_env = ""
            for __, e_setting in env_settings.items():
                python_task_env = python_task_env + f"{e_setting}\n"
            input_content = input_content.replace("#@ENV_SUB@", python_task_env)
            file_handler.write(input_content)


class TaskSettingsJson(TaskSettings):
    """Set the task specific setttings."""

    def __init__(self, submission_defs_file):
        """Construct the task specific settings.

        Args:
            task (str): Submission definition json file
        """
        with open(submission_defs_file, mode="r", encoding="utf-8") as file_handler:
            submission_defs = json.load(file_handler)
        TaskSettings.__init__(self, submission_defs)


class NoSchedulerSubmission():
    """Create and submit job without a scheduler"""

    def __init__(self, task_settings):
        """Construct the task specific settings.

        Args:
             submission_defs(dict): Submission definitions
        """
        self.task_settings = task_settings

    def submit(self, task, template_job, task_job, output, job_type,
               troika="troika",
               troika_config="/opt/troika/etc/troika.yml"):
        """Submit task.

        Args:
            task (str): Task name
            template_job (str): Task template job file
            task_job (str): Task job file
            output(str): Output file
            job_type (str): Job type
            troika (str, optional): troika binary. Defaults to "troika".
            troika_config (str, optional): Troika config file.
                                           Defaults to "/opt/troika/etc/troika.yml".

        Raises:
            Exception: Submission failure
        """
        self.task_settings.parse_job(task, template_job, task_job)
        cmd = f"{troika} -c {troika_config} submit {job_type} {task_job} -o {output}"
        try:
            subprocess.check_call(cmd.split())
        except Exception as exc:
            raise Exception(f"Submission failed with {repr(exc)}") from exc


class SuiteDefinition(object):
    """The definition of the suite."""

    def __init__(
        self,
        suite_name,
        joboutdir,
        ecf_files,
        task_settings,
        loglevel,
        ecf_home=None,
        ecf_include=None,
        ecf_out=None,
        ecf_jobout=None,
        ecf_micro="%"
    ):
        """Construct the definition.

        Args:
            suite_name (str): Name of suite
            joboutdir (str): Path to jobfiles
            ecf_files (str): Path to ecflow containers
            task_settings (TaskSettings): Submission configuration
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
        self.ecf_micro = ecf_micro
        if ecf_jobout is None:
            ecf_jobout = joboutdir + \
                f"/{ecf_micro}ECF_NAME{ecf_micro}.{ecf_micro}ECF_TRYNO{ecf_micro}"
        self.ecf_jobout = ecf_jobout

        self.task_settings = task_settings

        # Commands started from the scheduler does not have full environment
        ecf_job_cmd = f"{ecf_micro}TROIKA{ecf_micro} " \
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} submit " \
            f"-o {ecf_micro}ECF_JOBOUT{ecf_micro} " \
            f"{ecf_micro}SCHOST{ecf_micro} " \
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        # %ECF_JOB%"
        self.ecf_job_cmd = ecf_job_cmd
        ecf_status_cmd = f"{ecf_micro}TROIKA{ecf_micro} " \
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} monitor " \
            f"{ecf_micro}SCHOST{ecf_micro} " \
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        self.ecf_status_cmd = ecf_status_cmd
        ecf_kill_cmd = f"{ecf_micro}TROIKA{ecf_micro} " \
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} kill " \
            f"{ecf_micro}SCHOST{ecf_micro} " \
            f"{ecf_micro}ECF_JOB{ecf_micro}"
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
            "TROIKA": troika,
            "TROIKA_CONFIG": troika_config
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
        EcflowSuiteTask("Forecast", family, self.task_settings, ecf_files,
                        input_template=input_template, variables=variables)
        family2 = EcflowSuiteFamily("TestFamily2", family, ecf_files)
        variables = {"ECF_TIMEOUT": 10}
        EcflowSuiteTask("Forecast", family2, self.task_settings, ecf_files,
                        input_template=input_template, variables=variables)
        EcflowSuiteTask("Serial", family2, self.task_settings, ecf_files,
                        input_template=input_template)

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
        self.ecf_container_path = ecf_files + self.path
        if variables is not None:
            for key, value in variables.items():
                logger.debug("key=%s value=%s",key, value)
                self.ecf_node.add_variable(key, value)


class EcflowNodeContainer(EcflowNode):
    """Ecflow node container.

    Args:
        EcflowNode (EcflowNode): Parent class.
    """

    def __init__(self, name, node_type, parent, ecf_files, variables=None):
        """Construct EcflowNodeContainer.

        Args:
            name (str): Name of the node container.
            node_type (str): What kind of node.
            parent (EcflowNode): Parent to this node.

        """
        EcflowNode.__init__(self, name, node_type, parent, variables=variables,
                            ecf_files=ecf_files)


class EcflowSuite(EcflowNodeContainer):
    """EcflowSuite.

    Args:
        EcflowNodeContainer
        (EcflowNodeContainer): A child of the EcflowNodeContainer class.
    """

    def __init__(self, name, ecf_files, variables=None):
        """Construct the Ecflow suite.

        Args:
            name (_type_): _description_

        """
        self.defs = ecflow.Defs({})

        EcflowNodeContainer.__init__(self, name, "suite", self.defs, ecf_files,
                                     variables=variables)

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
        """Construct the family.

        Args:
            name (str): Name of the family.
            parent (EcflowNodeContainer): Parent node.

        """
        EcflowNodeContainer.__init__(self, name, "family", parent, ecf_files,
                                     variables=variables)
        logger.debug(self.ecf_container_path)
        self.ecf_node.add_variable("ECF_FILES", self.ecf_container_path)


class EcflowSuiteTask(EcflowNode):
    """A task in an ecflow suite/family.

    Args:
        EcflowNode (EcflowNodeContainer): The node container.
    """

    def __init__(self, name, parent, task_settings, ecf_files, input_template=None,
                 parse=True, variables=None, ecf_micro="%"):
        """Constuct the EcflowSuiteTask.

        Args:
            name (str): Name of task
            parent (EcflowNode): Parent node.

        Raises:
            Exception: Safety check
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
            task_settings.parse_job(name, input_template, task_container,
                                    variables=variables, ecf_micro=ecf_micro)
        else:
            if not os.path.exists(task_container):
                raise FileNotFoundError(f"Container {task_container} is missing!")
