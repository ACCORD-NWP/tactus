"""Ecflow suites."""

from pathlib import Path

from tactus.os_utils import tactusmakedirs
from tactus.suites.base import EcflowSuiteTask, SuiteDefinition
from tactus.suites.tactus_suite_components import StaticDataFamily, TimeDependentFamily


class TactusSuiteDefinition(SuiteDefinition):
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

        # Set max_ecf_tasks from config
        max_ecf_tasks = self.config.get("submission.max_ecf_tasks", -1)
        # Add limits to suite
        if max_ecf_tasks > 0 and self.suite.ecf_node is not None:
            self.suite.ecf_node.add_limit("max_ecf_tasks", max_ecf_tasks)
            self.suite.ecf_node.add_inlimit("max_ecf_tasks", f"/{self.name}")

        # Adjust parameters depending on suite_control.mode
        self.do_prep = True
        create_static_data = config["suite_control.create_static_data"]
        if config["suite_control.mode"] == "restart":
            self.do_prep = False
            create_static_data = False

        # Construct the suite from individual ecFlow components
        final_cleaning_trigger = None
        time_dependent_trigger_node = None
        prep_run = EcflowSuiteTask(
            "PrepRun",
            self.suite,
            config,
            self.task_settings,
            self.ecf_files,
            input_template=input_template,
            ecf_files_remotely=self.ecf_files_remotely,
        )
        # Update triggers for final cleaning and time dependent nodes
        if config["suite_control.do_cleaning"]:
            final_cleaning_trigger = [prep_run]
            time_dependent_trigger_node = prep_run

        if create_static_data:
            static_data = StaticDataFamily(
                self.suite,
                config,
                self.task_settings,
                input_template,
                self.ecf_files,
                trigger=prep_run,
                ecf_files_remotely=self.ecf_files_remotely,
                dry_run=dry_run,
            )
            # Update trigger for time dependent node
            time_dependent_trigger_node = static_data
            collect_logs_trigger = [static_data]

            if config["suite_control.do_archiving"]:
                archive_static = EcflowSuiteTask(
                    "ArchiveStatic",
                    self.suite,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables=None,
                    trigger=static_data,
                )
                collect_logs_trigger.append(archive_static)

            collect_logs = EcflowSuiteTask(
                "CollectLogsStatic",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=collect_logs_trigger,
                variables=None,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            # Update triggers for final cleaning node
            final_cleaning_trigger = [collect_logs]

        last_time_dependent_part = None
        if config["suite_control.create_time_dependent_suite"]:
            time_dependent_family = TimeDependentFamily(
                self.suite,
                config,
                self.task_settings,
                input_template,
                self.ecf_files,
                time_dependent_trigger_node,
                ecf_files_remotely=self.ecf_files_remotely,
                do_prep=self.do_prep,
                dry_run=dry_run,
            )
            # Update trigger for next time dependent node
            last_time_dependent_part = time_dependent_family.last_node

        if config["suite_control.do_cleaning"]:
            if last_time_dependent_part is not None:
                # Update triggers for final cleaning node
                final_cleaning_trigger.append(last_time_dependent_part)

            EcflowSuiteTask(
                "PostMortem",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=final_cleaning_trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )
