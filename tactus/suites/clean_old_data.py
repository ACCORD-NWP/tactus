"""Ecflow suites."""

from pathlib import Path

from ..datetime_utils import as_datetime
from ..os_utils import tactusmakedirs
from .base import EcflowSuiteCron, EcflowSuiteFamily, EcflowSuiteTask, SuiteDefinition


class DeodeCleaningSuiteDefinition(SuiteDefinition):
    """Definition of suite."""

    def __init__(
        self,
        config,
        dry_run=False,
    ):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the definition.

        Args:
            config (tactus.ParsedConfig): Configuration file
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.

        Raises:
            ModuleNotFoundError: If ecflow is not loaded and not dry_run

        """
        SuiteDefinition.__init__(self, config, dry_run=dry_run)

        self.suite_name = "Clean_old_data"
        self.config = config
        self.clean_scratch = self.config[
            "suite_control.DeodeCleaningSuiteDefinition.do_clean_scratch_data"
        ]
        self.clean_suites = self.config[
            "suite_control.DeodeCleaningSuiteDefinition.do_clean_suites"
        ]
        self.clean_IFS_data = self.config[
            "suite_control.DeodeCleaningSuiteDefinition.do_clean_IFS"
        ]
        self.clean_ehype_data = self.config[
            "suite_control.DeodeCleaningSuiteDefinition.do_clean_ehype_data"
        ]
        self.cron_days = self.config[
            "suite_control.DeodeCleaningSuiteDefinition.days_in_week"
        ]
        self.cron_time = as_datetime(
            self.config["suite_control.DeodeCleaningSuiteDefinition.time_to_run"]
        )

        max_ecf_tasks = -1
        try:
            max_ecf_tasks = self.config["submission.max_ecf_tasks"]
        except KeyError:
            max_ecf_tasks = -1

        if max_ecf_tasks > 0 and self.suite.ecf_node is not None:
            self.suite.ecf_node.add_limit("max_ecf_tasks", max_ecf_tasks)
            self.suite.ecf_node.add_inlimit(
                "max_ecf_tasks", f"/{self.suite_name}", max_ecf_tasks
            )

        unix_group = self.platform.get_platform_value("unix_group")
        tactusmakedirs(self.joboutdir, unixgroup=unix_group)

        input_template = (
            Path(__file__).parent.resolve() / "../templates/ecflow/default.py"
        )
        input_template = input_template.as_posix()

        cron_one_per_week = EcflowSuiteCron(self.cron_days, self.cron_time)

        clean_family = EcflowSuiteFamily(
            "Clean",
            self.suite,
            self.ecf_files,
            cron=cron_one_per_week,
            ecf_files_remotely=self.ecf_files_remotely,
        )

        if self.clean_scratch:
            EcflowSuiteTask(
                "CleanScratchData",
                clean_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
            )
        if self.clean_suites:
            EcflowSuiteTask(
                "CleanSuites",
                clean_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
            )

        if self.clean_IFS_data:
            EcflowSuiteTask(
                "CleanIFSData",
                clean_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
            )

        if self.clean_ehype_data:
            EcflowSuiteTask(
                "CleanEhypeData",
                clean_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
            )
