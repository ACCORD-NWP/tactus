"""Ecflow obs processing suite."""

from pathlib import Path

from tactus.os_utils import tactusmakedirs
from tactus.suites.base import EcflowSuiteTask, SuiteDefinition
from tactus.suites.da_components import ObservationFamily


class ObsprocSuiteDefinition(SuiteDefinition):
    """Obsprocessing."""

    def __init__(
        self,
        config,
        dry_run=False,
    ):
        """Construct the definition.

        Args:
            config (ParsedConfig): Configuration file
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.

        Raises:
            ModuleNotFoundError: If ecflow is not loaded and not dry_run

        """
        # Call the base class constructor
        SuiteDefinition.__init__(self, config, dry_run=dry_run)

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

        obs_trigger = []
        for da_stream in ("surface", "upper_air"):
            if config.get(f"da.{da_stream}.active", False):
                obs_family = ObservationFamily(
                    self.suite,
                    config,
                    self.task_settings,
                    input_template,
                    self.ecf_files,
                    trigger=prep_run,
                    da_stream=da_stream,
                    ecf_files_remotely=self.ecf_files_remotely,
                )
                obs_trigger.append(obs_family)

        if config["suite_control.do_cleaning"]:
            EcflowSuiteTask(
                "PostMortem",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=obs_trigger,
                variables={
                    "TACTUS_TASK": "Cleaning",
                    "ARGS": "cleaning_type=PostMortem",
                },
                ecf_files_remotely=self.ecf_files_remotely,
            )
