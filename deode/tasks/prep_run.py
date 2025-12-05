"""Preparatory task for a deode suite."""

from pathlib import Path

import yaml

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.eps.eps_setup import get_member_config
from deode.logs import logger
from deode.os_utils import deodemakedirs
from deode.tasks.cleaning_tasks import Cleaning
from deode.toolbox import Platform

from .base import Task


class PrepRun(Task):
    """Preparatory run/cleaning task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        self.name = "PrepRun"
        Task.__init__(self, config, __class__.__name__)
        # Initialize cleaining functionality if needed
        if config["suite_control.do_cleaning"]:
            self.cleaner = Cleaning(config)
            self.cleaner.prep_clean_task(self.name)
        else:
            self.cleaner = None
        # Archive the used config file
        archive_root = Path(self.platform.get_platform_value("archive_root"))
        deodemakedirs(
            archive_root, unixgroup=self.platform.get_platform_value("unix_group")
        )
        archive_config = archive_root / "config.toml"
        config.save_as(archive_config)
        logger.info("Stored used config as: {}", archive_config)

        # Create and archive expanded config file
        expanded_config = self.config.dict()
        expanded_config["general"]["times"].pop("basetime")
        expanded_config["general"]["times"].pop("validtime")
        expanded_config = ParsedConfig(expanded_config, json_schema={}).expand_macros(
            True
        )
        archive_expanded_config = archive_root / "expanded_config.toml"
        expanded_config.save_as(archive_expanded_config)
        logger.info("Stored used expanded config as: {}", archive_expanded_config)

        deode_modelname_definitions_path = (
            archive_root / "eccodes" / "definitions" / "grib2" / "localConcepts" / "lfpw"
        )
        deodemakedirs(
            deode_modelname_definitions_path,
            unixgroup=self.platform.get_platform_value("unix_group"),
        )
        self.create_famodeldefs(deode_modelname_definitions_path)

    def create_famodeldefs(self, target_eccodes_definition_path: str):
        """Create faModelName.def in deode_eccodes_path.

        Args:
            target_eccodes_definition_path (str): Path to the where model name definitions
                should be created.

        Raises:
            FileNotFoundError: If the source YAML file is not found.
            ValueError: If the YAML file does not contain the required keys.
            yaml.YAMLError: If the YAML file cannot be loaded.
        """
        logger.info(
            "Create faModelName definitions in {}", target_eccodes_definition_path
        )
        deode_eccodes_definition_path = ConfigParserDefaults.DATA_DIRECTORY / "eccodes"
        fa_model_source_file = deode_eccodes_definition_path / "destineFaModelSource.yml"

        if fa_model_source_file.is_file():
            with open(fa_model_source_file, "r") as f:
                try:
                    data = yaml.safe_load(f)
                except yaml.YAMLError as e:
                    logger.error(
                        "Failed to load YAML file {}: {}", fa_model_source_file, e
                    )

        else:
            raise FileNotFoundError(f"YAML file not found: {fa_model_source_file}")

        frameworks = data.get("frameworks")
        cycles = data.get("cycles")
        cscs = data.get("cscs")

        if not frameworks or not cycles or not cscs:
            raise ValueError(
                "Missing frameworks, cycles, or cscs in YAML file. "
                "Cannot create faModelName.def."
            )

        fa_model_name_defs = target_eccodes_definition_path / "faModelName.def"
        logger.info("Write faModelName definitions to {}", fa_model_name_defs)
        n_eps_members = len(self.config["eps.general.members"])

        with open(fa_model_name_defs, "w") as f:
            for member in self.config["eps.general.members"]:
                member_config = get_member_config(self.config, member=member)
                model_name = str(
                    Platform(member_config).substitute(member_config["general.famodel"])
                )
                cycle_dict = cycles.get(member_config["general.cycle"], {})
                csc_dict = cscs.get(member_config["general.csc"], {})
                for framework_dict in frameworks.values():
                    dicts = [framework_dict, cycle_dict, csc_dict]
                    if n_eps_members > 1:
                        logger.info(
                            "Adding EPS member {} to model name definitions", member
                        )
                        eps_key = {
                            "numberOfForecastsInEnsemble": n_eps_members,
                            "perturbationNumber": member,
                            "productDefinitionTemplateNumber": 11,
                            "typeOfEnsembleForecast": 6,
                        }
                        dicts.append(eps_key)
                    line = (
                        f"'{model_name}' = {{"
                        + "".join(f"{k} = {v}; " for d in dicts for k, v in d.items())
                        + "}\n"
                    )
                    f.write(line)
            f.write("'default' = { generatingProcessIdentifier = 255; }\n")

    def execute(self):
        """Execute the task, including cleaning if enabled."""
        if self.cleaner is not None:
            self.cleaner.execute()
