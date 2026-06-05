"""Ecflow compilation suite."""

from pathlib import Path

from tactus.os_utils import tactusmakedirs
from tactus.suites.base import SuiteDefinition
from tactus.suites.tactus_suite_components import CompilationFamily


class CompilationSuiteDefinition(SuiteDefinition):
    """Compilation."""

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

        unix_group = self.platform.get_platform_value("unix_group")
        tactusmakedirs(self.joboutdir, unixgroup=unix_group)

        # Get the default input template path
        input_template = (
            Path(__file__).parent.resolve() / "../templates/ecflow/default.py"
        )
        input_template = input_template.as_posix()

        ecf_remote_files = self.ecflow_env.get_property("ecf_remote_files")
        ecf_files = self.ecflow_env.get_property("ecf_files")
        CompilationFamily(
            self.suite,
            config,
            self.task_settings,
            ecf_files,
            trigger=None,
            input_template=input_template,
            ecf_remote_files=ecf_remote_files,
        )
