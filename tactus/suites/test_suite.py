"""Ecflow suites."""

from pathlib import Path

from tactus.os_utils import tactusmakedirs
from tactus.suites.base import EcflowSuiteTask, SuiteDefinition


class TestSuiteDefinition(SuiteDefinition):
    """Definition of suite for tactus."""

    def __init__(
        self,
        config,
        dry_run=False,
    ):
        """Construct the definition.

        Args:
            config (tactus.ParsedConfig): Configuration file
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.

        Raises:
            ModuleNotFoundError: If ecflow is not loaded and not dry_run

        """
        # Call the base class constructor
        SuiteDefinition.__init__(self, config, dry_run=dry_run)
        # Construct directories
        unix_group = self.platform.get_platform_value("unix_group")
        tactusmakedirs(self.joboutdir, unixgroup=unix_group)

        # Get the default input template path
        input_template = (
            Path(__file__).parent.resolve() / "../templates/ecflow/default.py"
        )
        input_template = input_template.as_posix()

        prep_run = EcflowSuiteTask(
            "PrepRun",
            self.suite,
            config,
            self.task_settings,
            self.ecf_files,
            input_template=input_template,
            ecf_files_remotely=self.ecf_files_remotely,
        )

        collect_logs = EcflowSuiteTask(
                "CollectLogsTest",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=prep_run,
                variables=None,
                ecf_files_remotely=self.ecf_files_remotely,
            )

        EcflowSuiteTask(
                    "PostMortem",
                    self.suite,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    trigger=collect_logs,
                    ecf_files_remotely=self.ecf_files_remotely,
        )
