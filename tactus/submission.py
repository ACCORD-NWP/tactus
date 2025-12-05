"""Module to handle submissions."""

import collections.abc
import os
import subprocess
import sys
from pathlib import Path
from typing import Optional

from tactus.config_parser import ParsedConfig
from tactus.derived_variables import derived_variables
from tactus.logs import logger
from tactus.os_utils import deodemakedirs
from tactus.plugin import DeodePluginRegistryFromConfig
from tactus.tasks.discover_task import available_tasks
from tactus.toolbox import FileManager, Platform


class ProcessorLayout:
    """Set processor information."""

    def __init__(self, kwargs: dict):
        """Construct object.

           Use integers internally.

        Args:
            kwargs(dict): Processor information

        """
        self.wrapper = kwargs.get("WRAPPER")
        self.nproc = kwargs.get("NPROC")
        if self.nproc == "":
            self.nproc = None
        if isinstance(self.nproc, str):
            self.nproc = int(self.nproc)
        self.nproc_io = kwargs.get("NPROC_IO")
        if self.nproc_io == "":
            self.nproc_io = None
        if isinstance(self.nproc_io, str):
            self.nproc_io = int(self.nproc_io)
        self.nprocx = kwargs.get("NPROCX")
        if self.nprocx == "":
            self.nprocx = None
        if isinstance(self.nprocx, str):
            self.nprocx = int(self.nprocx)
        self.nprocy = kwargs.get("NPROCY")
        if self.nprocy == "":
            self.nprocy = None
        if isinstance(self.nprocy, str):
            self.nprocy = int(self.nprocy)

    def get_proc_dict(self):
        """Generate a processor dict."""
        procs = {}
        nproc = self.nproc
        nproc_io = self.nproc_io
        nprocx = self.nprocx
        nprocy = self.nprocy
        procs.update(
            {"nproc": nproc, "nproc_io": nproc_io, "nprocx": nprocx, "nprocy": nprocy}
        )
        return procs

    def get_wrapper(self):
        """Get and potentially parse the wrapper."""
        wrapper = self.wrapper
        if wrapper is not None and self.nproc is not None:
            nproc = self.nproc
            if not isinstance(nproc, str):
                nproc = str(nproc)
            wrapper = wrapper.replace("@NPROC@", nproc)
        return wrapper


class TaskSettings(object):
    """Set the task specific setttings."""

    def __init__(self, config):
        """Construct the task specific settings.

        Args:
             config(tactus.ParserdConfig): Configuration
        """
        self.config = config
        self.submission_defs = self.config["submission"].dict()
        self.job_type = None
        self.processor_layout = None

        self.fmanager = FileManager(self.config)
        self.platform = self.fmanager.platform
        self.unix_group = self.platform.get_value("platform.unix_group")

    def update_task_setting(self, dic, upd):
        """Update task settings dictionary.

        Args:
            dic (dict): Dictionary to update
            upd (dict): Values to update

        Returns:
            dict: updated dictionary
        """
        for key, _val in upd.items():
            if isinstance(_val, collections.abc.Mapping):
                dic[key] = self.update_task_setting(dic.get(key, {}), _val)
            else:
                val = self.platform.substitute(_val)
                logger.debug("key={} value={}", key, val)
                dic[key] = val
        return dic

    def parse_submission_defs(self, task):
        """Parse the submssion definitions.

        Args:
            task (str): The name of the task

        Returns:
            dict: Parsed settings

        Raises:
            RuntimeError: Undefined submit type

        """
        task_settings = {"BATCH": {}, "ENV": {}, "MODULES": {}}
        all_defs = self.submission_defs
        try:
            all_types = all_defs["types"]
        except KeyError:
            all_types = {}

        submit_types = list(all_types)
        default_submit_type = all_defs["default_submit_type"]
        if default_submit_type not in submit_types:
            raise RuntimeError(
                f"Default submit type: {default_submit_type} is not defined"
            )
        logger.debug("default_submit_type={}", default_submit_type)
        task_submit_type = None
        for s_t in submit_types:
            if s_t in all_types and "tasks" in all_types[s_t]:
                for tname in all_types[s_t]["tasks"]:
                    if tname == task:
                        task_submit_type = s_t
        if task_submit_type is None:
            task_submit_type = default_submit_type

        logger.debug("task_submit_type for task {}: {}", task, task_submit_type)

        # Update task_settings
        task_settings = self.update_task_setting(
            task_settings, all_types[task_submit_type]
        )

        for task_name in ["@TASK_NAME@", "@STAND_ALONE_TASK_NAME@"]:
            if (
                "BATCH" in task_settings
                and "NAME" in task_settings["BATCH"]
                and task_name in task_settings["BATCH"]["NAME"]
            ):
                task_settings["BATCH"]["NAME"] = task_settings["BATCH"]["NAME"].replace(
                    task_name, task
                )
                break

        if "task_exceptions" in all_defs and task in all_defs["task_exceptions"]:
            logger.debug("Task task_exceptions for task {}", task)
            task_settings = self.update_task_setting(
                task_settings, all_defs["task_exceptions"][task]
            )

        if "SCHOST" in task_settings:
            self.job_type = task_settings["SCHOST"]

        # Set task specific processor layout
        self.processor_layout = ProcessorLayout(task_settings)

        logger.debug("Task settings for task {}: {}", task, task_settings)
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

        if key in task_settings:
            m_task_settings = {}
            logger.debug(type(task_settings[key]))
            if isinstance(task_settings[key], dict):
                for setting, value_ in task_settings[key].items():
                    value = value_
                    logger.debug("{} {} variables: {}", setting, value, variables)
                    if variables is not None and setting in variables:
                        value = f"{ecf_micro}{setting}{ecf_micro}"
                        logger.debug(value)
                    if isinstance(value, str):
                        value = value.replace("@TASK_NAME@", task)

                    m_task_settings.update({setting: value})
                logger.debug(m_task_settings)
                return m_task_settings

            value = task_settings[key]
            if variables is not None and key in variables:
                value = f"{ecf_micro}{variables[key]}{ecf_micro}"
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
            if isinstance(value, (int, str)):
                logger.debug(key)
                keys.append(key)
        logger.debug(keys)
        for key, value in self.recursive_items(task_settings):
            logger.debug("key={} value={}", key, value)
            if key in keys:
                logger.debug("update {} {}", key, value)
                settings.update({key: value})
        return settings

    def parse_job(
        self,
        task: str,
        config: ParsedConfig,
        input_template_job: str,
        task_job: Path,
        member: Optional[int] = None,
        variables=None,
        ecf_micro: str = "%",
        scheduler: str = "ecflow",
    ):
        """Read default job and change interpretor.

        Args:
            task                   (str): Task name
            config        (tactus.config): The configuration
            input_template_job     (str): Input container template.
            task_job              (Path): Task container
            member       (int, optional): Member number for which to parse job.
                Defaults to None.
            variables (_type_, optional): _description_. Defaults to None.
            ecf_micro    (str, optional): _description_.
            scheduler    (str, optional): Scheduler. Defaults to ecflow.

        Raises:
            RuntimeError: In case of missing module env file

        """
        interpreter = self.get_task_settings(task, "INTERPRETER")
        logger.debug(interpreter)
        if interpreter is None:
            interpreter = f"{sys.executable}"

        logger.debug(interpreter)
        dir_name = task_job.resolve().parent

        deodemakedirs(dir_name, unixgroup=self.unix_group)

        with open(task_job, mode="w", encoding="utf-8") as file_handler:
            file_handler.write("#!/bin/bash\n")

            # Batch settings
            batch_settings = self.get_task_settings(
                task, "BATCH", variables=variables, ecf_micro=ecf_micro
            )
            # Add account if not set
            if "account" not in batch_settings and "account" in config["submission"]:
                batch_settings["ACCOUNT"] = config["submission.account"]

            logger.debug("batch settings {}", batch_settings)
            for b_setting in batch_settings.values():
                file_handler.write(f"{b_setting}\n")

            if scheduler is not None and scheduler == "ecflow":
                ecf_vars = [
                    "ECF_HOST",
                    "ECF_PORT",
                    "ECF_NAME",
                    "ECF_PASS",
                    "ECF_TRYNO",
                    "ECF_RID",
                    "ECF_TIMEOUT",
                    "BASETIME",
                    "VALIDTIME",
                    "LOGLEVEL",
                    "ARGS",
                    "WRAPPER",
                    "NPROC",
                    "NPROC_IO",
                    "NPROCX",
                    "NPROCY",
                    "CONFIG",
                    "DEODE_HOME",
                    "KEEP_WORKDIRS",
                    "MEMBER",
                ]
                for ecf_var in ecf_vars:
                    file_handler.write(f'export {ecf_var}="%{ecf_var}%"\n')

            # Module settings
            module_settings = self.get_task_settings(
                task, "MODULES", variables=variables, ecf_micro=ecf_micro
            )
            logger.debug("module settings {}", module_settings)

            if module_settings is not None and len(module_settings) > 0:
                env_file = (
                    f"{self.submission_defs['module_initfile']}"
                    if "module_initfile" in self.submission_defs
                    else f"{self.submission_defs['module_initpath']}/bash"
                )
                if not os.path.isfile(env_file):
                    raise RuntimeError(
                        f"Environment file {env_file} is not a file or does not exists"
                    )

                file_handler.write(f". {env_file}\n")
                for key in module_settings.values():
                    # Skip empty sections
                    if len(key) == 0:
                        continue
                    if len(key) < 2 or len(key) > 3:
                        raise RuntimeError(f"Module command has the wrong length:{key}")
                    cmd = "module " + " ".join([f"{x}" for x in key])
                    file_handler.write(f"{cmd}\n")

            # Environment settings
            env_settings = self.get_task_settings(
                task, "ENV", variables=variables, ecf_micro=ecf_micro
            )
            logger.debug(env_settings)

            for key, _val in env_settings.items():
                val = self.platform.substitute(_val)
                file_handler.write(f'export {key}="{val}"\n')

            if scheduler is None:
                file_handler.write(f'export STAND_ALONE_TASK_NAME="{task}"\n')

                deode_home = self.platform.get_platform_value("DEODE_HOME")

                file_handler.write(f'export STAND_ALONE_DEODE_HOME="{deode_home}"\n')
                config_file = config.metadata["source_file_path"]

                file_handler.write(f'export STAND_ALONE_TASK_CONFIG="{config_file!s}"\n')
                if member is not None:
                    file_handler.write(f"export STAND_ALONE_MEMBER={member}\n")

            file_handler.write(f"{interpreter} {input_template_job} || exit 1\n")
        # Make file executable for user
        task_job.chmod(0o744)


class NoSchedulerSubmission:
    """Create and submit job without a scheduler."""

    def __init__(self, task_settings):
        """Construct the task specific settings.

        Args:
             task_settings (dict): Submission definitions
        """
        self.task_settings: TaskSettings = task_settings

    def submit(
        self,
        task: str,
        config: ParsedConfig,
        template_job: str,
        task_job: Path,
        output: Path,
        member: Optional[int] = None,
        troika: str = "troika",
    ):
        """Submit task.

        Args:
            task                  (str): Task name
            config (tactus.ParsedConfig): Config
            template_job          (str): Task template job file
            task_job             (Path): Task job file
            output               (Path): Output file
            member      (int, optional): Member number for which to submit job.
                Defaults to None.
            troika      (str, optional): troika binary. Defaults to "troika".

        Raises:
            RuntimeError: Submission failure.
        """
        name = task.lower()
        if name not in available_tasks(DeodePluginRegistryFromConfig(config)):
            raise NotImplementedError(f"Task {name} not implemented")

        troika_config = Platform(config).get_value("troika.config_file")
        self.task_settings.parse_job(
            task=task,
            config=config.copy(
                # Use derived variables (nproc etc)
                update=derived_variables(
                    config,
                    processor_layout=ProcessorLayout(
                        self.task_settings.get_settings(task)
                    ),
                )
            ),
            input_template_job=template_job,
            task_job=task_job,
            member=member,
            scheduler=None,
        )
        cmd = (
            f"{troika} -c {troika_config} submit {self.task_settings.job_type} "
            f"{task_job} -o {output}"
        )
        try:
            subprocess.check_call(cmd.split())  # noqa S603
        except subprocess.CalledProcessError as exc:
            raise RuntimeError(f"Submission failed with {exc!r}") from exc
