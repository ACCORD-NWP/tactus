"""Impact model classes."""

import json
import os
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional

import tomlkit
import yaml
from dicttoxml import dicttoxml as dtx

from ..config_parser import BasicConfig
from ..logs import logger
from ..toolbox import Platform
from .base import Task
from .batch import BatchJob


@dataclass()
class ImpactModel(ABC):
    """Abstract class for impact models."""

    name: str
    config: BasicConfig
    platform: Platform
    filename: Optional[str] = ""

    def __new__(cls, name: str, *_args, **_kwargs):
        """Create a new instance of a subclass based on the field_type."""
        for subclass in ImpactModel.__subclasses__():
            if subclass.name == name:
                instance = super(ImpactModel, subclass).__new__(subclass)
                return instance
        raise ValueError(f"No valid ImpactModel subclass found for name: {name}")

    @abstractmethod
    def run(self):
        """Abstract method."""

    def dump(self, to_dump):
        """Write config to selected format.

        Args:
            to_dump (dict) : config dict to write

        Raises:
            TypeError: Unknown output file type
        """
        logger.info(" Dump config to: {}", self.filename)
        if self.filename.endswith(".toml"):
            with open(self.filename, mode="w", encoding="utf8") as f_h:
                f_h.write(tomlkit.dumps(to_dump))
        elif self.filename.endswith((".xml")):
            with open(self.filename, mode="wb") as f_h:
                f_h.write(dtx(to_dump, root=False, attr_type=False))
        elif self.filename.endswith((".yml", ".yaml")):
            with open(self.filename, mode="wb") as f_h:
                yaml.dump(to_dump, f_h, encoding="utf-8", default_flow_style=False)
        elif self.filename.endswith(".json"):
            json_object = json.dumps(to_dump, indent=4)
            with open(self.filename, "w", encoding="utf8") as f_h:
                f_h.write(json_object)
        else:
            raise TypeError(f"Unknown filetype:{self.filename}")

    def execute(self):
        """Prepares and runs the impact model commands."""
        logger.info("Impact model:{} ", str(self))
        self.filename = self.platform.substitute(self.config["config_name"])
        # List and parse settings
        logger.info(" communication keys:")
        com = {}
        for key, value in self.config["communicate"].items():
            v = self.platform.substitute(value)
            logger.info("  {} = {}", key, v)
            com[key] = v

        # Write the config file
        self.dump(com)
        # Execute the impact model specific command
        self.run()

    def __str__(self):
        return self.name


@dataclass()
class Ehype(ImpactModel):
    """EHYPE specific methods."""

    name = "ehype"

    def run(self):
        """Starts the EHYPE suite."""
        path = self.platform.substitute(self.config["path"])
        args = self.platform.substitute(self.config["arguments"])
        cmd = f"{path}/deploy_suite.sh {args}"
        BatchJob(os.environ, wrapper="").run(cmd)


class ImpactModels(Task):
    """Create info to and start impact models."""

    def __init__(self, config, taskname=None):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            taskname (str): Indicating taskname
        """
        Task.__init__(self, config, __class__.__name__)

        impact = self.config.get("impact", BasicConfig({})).dict()
        installed_impact = self.config.get("platform.impact", {})
        self.impact = {}
        for name, impact_model in impact.items():
            if (
                impact_model["active"]
                and name in installed_impact
                and taskname in impact_model
            ):
                self.impact[name] = impact_model[taskname]
                for conf in ["communicate", "path", "config_name"]:
                    self.impact[name][conf] = impact_model[conf]

        self.is_active = len(self.impact) > 0

    def execute(self):
        """Start the impact model(s)."""
        for name, impact_model in self.impact.items():
            model = ImpactModel(name=name, config=impact_model, platform=self.platform)
            model.execute()


class StartImpactModels(ImpactModels):
    """Starts the impact models."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        ImpactModels.__init__(self, config, __class__.__name__)
