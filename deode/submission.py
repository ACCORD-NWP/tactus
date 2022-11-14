"""Module to handle submissions."""

import json
import os
import subprocess

from .logs import get_logger

logger = get_logger(__name__, "DEBUG")


class TaskSettings(object):
    """Set the task specific setttings."""

    def __init__(self, submission_defs):
        """Construct the task specific settings.

        Args:
             submission_defs(dict): Submission definitions
        """
        self.submission_defs = submission_defs
        self.job_type = None

    def parse_submission_defs(self, task):
        """Parse the submssion definitions.

        Args:
            task (str): The name of the task

        Returns:
            dict: Parsed settings

        """
        task_settings = {"BATCH": {}, "ENV": {}}
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
                                logger.warning(
                                    "key=%s already exists in " "task_settings", key
                                )
                            task_settings[kword].update({key: value})

        if "SCHOST" in task_settings:
            self.job_type = task_settings["SCHOST"]
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

    def parse_job(
        self, task, config, input_template_job, task_job, variables=None, ecf_micro="%"
    ):
        """Read default job and change interpretor.

        Args:
            task (str): Task name
            config (str): The configuration
            input_template_job (str): Input container template.
            task_job (str): Task container
            variables (_type_, optional): _description_. Defaults to None.
            ecf_micro (str, optional): _description_.

        """
        interpreter = self.get_task_settings(task, "INTERPRETER")
        logger.debug(interpreter)
        if interpreter is None:
            cmd = ["type", "python3"]
            returned_output = (
                subprocess.check_output(cmd).decode("utf-8").strip().split(" ")[-1]
            )
            interpreter = f"#!{returned_output}"

        logger.debug(interpreter)
        with open(input_template_job, mode="r", encoding="utf-8") as file_handler:
            input_content = file_handler.read()
        dir_name = os.path.dirname(os.path.realpath(task_job))
        if not os.path.exists(dir_name):
            os.makedirs(dir_name, exist_ok=True)
        with open(task_job, mode="w", encoding="utf-8") as file_handler:
            file_handler.write(f"{interpreter}\n")
            batch_settings = self.get_task_settings(
                task, "BATCH", variables=variables, ecf_micro=ecf_micro
            )
            logger.debug("batch settings %s", batch_settings)
            for __, b_setting in batch_settings.items():
                file_handler.write(f"{b_setting}\n")
            env_settings = self.get_task_settings(
                task, "ENV", variables=variables, ecf_micro=ecf_micro
            )
            logger.debug(env_settings)
            python_task_env = ""
            for __, e_setting in env_settings.items():
                python_task_env = python_task_env + f"{e_setting}\n"
            input_content = input_content.replace("# @ENV_SUB@", python_task_env)
            input_content = input_content.replace("@STAND_ALONE_TASK_NAME@", task)
            input_content = input_content.replace(
                "@STAND_ALONE_TASK_CONFIG@", str(config)
            )
            input_content = input_content.replace("@STAND_ALONE_TASK_LOGLEVEL@", "DEBUG")
            file_handler.write(input_content)
        # Make file executable for user
        os.chmod(task_job, 0o744)


class TaskSettingsJson(TaskSettings):
    """Set the task specific setttings."""

    def __init__(self, submission_defs_file):
        """Construct the task specific settings.

        Args:
            submission_defs_file (str): Submission definition json file
        """
        with open(submission_defs_file, mode="r", encoding="utf-8") as file_handler:
            submission_defs = json.load(file_handler)
        TaskSettings.__init__(self, submission_defs)


class NoSchedulerSubmission:
    """Create and submit job without a scheduler."""

    def __init__(self, task_settings):
        """Construct the task specific settings.

        Args:
             task_settings (dict): Submission definitions
        """
        self.task_settings = task_settings

    def submit(
        self,
        task,
        config,
        template_job,
        task_job,
        output,
        troika="troika",
        troika_config="/opt/troika/etc/troika.yml",
    ):
        """Submit task.

        Args:
            task (str): Task name
            config (str): Config
            template_job (str): Task template job file
            task_job (str): Task job file
            output(str): Output file
            troika (str, optional): troika binary. Defaults to "troika".
            troika_config (str, optional): Troika config file.
                                           Defaults to "/opt/troika/etc/troika.yml".

        Raises:
            Exception: Submission failure
        """
        self.task_settings.parse_job(task, config, template_job, task_job)
        cmd = f"{troika} -c {troika_config} submit {self.task_settings.job_type} {task_job} -o {output}"
        try:
            subprocess.check_call(cmd.split())
        except Exception as exc:
            raise Exception(f"Submission failed with {repr(exc)}") from exc
