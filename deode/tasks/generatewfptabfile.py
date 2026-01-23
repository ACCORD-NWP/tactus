"""Task to generate location/turbine tab-files as input for windfarm parameterization."""
import os

from json2tab import json2tab
from json2tab.logs import logger as json2tab_logger

from deode.datetime_utils import as_datetime
from deode.logs import LogDefaults, logger

from .base import Task


class GenerateWfpTabFile(Task):
    """Generates TAB-file for WFP."""

    def __init__(self, config):
        """Construct GenerateWfpTabFile object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

    def execute(self):
        """Run task.

        Run sequence: define domain, call json-2-tab, store tab-output.

        """
        if self.platform.substitute(self.config["json2tab.enabled"]):
            output_dir = self.platform.substitute(
                self.config["json2tab.output.directory"]
            )
            location_tab_file = self.platform.substitute(
                self.config["json2tab.output.files.location_tab"]
            )
            type_tab_prefix = self.platform.substitute(
                self.config["json2tab.output.files.type_tab_prefix"]
            )

            config_file = self.platform.substitute(self.config["json2tab.config_file"])
            force_rewrite = self.platform.substitute(
                self.config["json2tab.force_rewrite"]
            )

            if not os.path.exists(f"{output_dir}/{location_tab_file}") or force_rewrite:
                if os.path.exists(f"{output_dir}/{location_tab_file}") and force_rewrite:
                    logger.info(
                        "Force rewrite is enabled, JSON-2-TAB will generate new "
                        "tab-files and overwrite existing files."
                    )
                logger.info(
                    f"Wind turbine location tab file '{output_dir}/{location_tab_file}' "
                    "is missing, call JSON-2-TAB to generate one."
                )

                loglevel = self.config.get("json2tab.loglevel", logger.level).upper()
                if loglevel == "AUTO":
                    # Set json-2-tab's logger to same level as deode log level
                    loglevel = self.config.get(
                        "general.loglevel", LogDefaults.LEVEL
                    ).upper()

                json2tab_logger.setLevel(loglevel)

                turbine_databases = [
                    self.platform.substitute(file)
                    for file in self.config["json2tab.input.turbine_database"]
                ]
                turbine_locations = self.platform.substitute(
                    self.config["json2tab.input.turbine_locations"]
                )

                domain = self.config["domain"].dict()

                situation_date = as_datetime(self.config["general.times.start"]).date()

                logger.info(f"Config file = '{config_file}'")
                logger.info(f"Turbinetype database file = '{turbine_databases}'")
                logger.info(f"Turbine location file = '{turbine_locations}'")
                logger.info(f"Domain = '{domain}'")
                logger.info(f"Situation date = '{situation_date}'")

                logger.info(f"Output directory = '{output_dir}'")
                logger.info(f"Output location_tab_file = '{location_tab_file}'")
                logger.info(f"Output type_tab_prefix = '{type_tab_prefix}'")

                json2tab.json2tab(
                    config_path=config_file,
                    turbine_databases=turbine_databases,
                    turbine_locations=turbine_locations,
                    domain_dict=domain,
                    situation_date=situation_date,
                    output_dir=output_dir,
                    location_file=location_tab_file,
                    type_file_prefix=type_tab_prefix,
                )
            else:
                logger.info(
                    f"Windturbine location file found in '{output_dir}', "
                    "no new TAB-file will be generated."
                )
        else:
            logger.info(
                "JSON-2-TAB conversion is turned off in configuration; "
                "so no domain specific WFP data is generated."
            )
