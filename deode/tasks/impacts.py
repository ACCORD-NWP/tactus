"""Impact model classes."""

import contextlib
import json
import os
from abc import ABC
from dataclasses import dataclass
from typing import Optional

import tomlkit
import yaml
from dicttoxml import dicttoxml as dtx

from deode.config_parser import BasicConfig
from deode.datetime_utils import as_datetime, as_timedelta
from deode.host_actions import HostNotFoundError, SelectHost
from deode.logs import logger
from deode.scheduler import EcflowServer
from deode.tasks.base import Task
from deode.tasks.batch import BatchJob
from deode.toolbox import Platform


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

    def run(self):
        """Starts a plugin suite."""
        poetry = self.platform.substitute(self.config["poetry"])
        path = self.platform.substitute(self.config["path"])
        args = self.platform.substitute(self.config["arguments"])
        logger.info("Change directory to: {}", path)
        os.chdir(path)
        if isinstance(args, str):
            args = [args]
        for arg in args:
            cmd = self.platform.substitute(f"{poetry} run {arg}")
            logger.info(cmd)
            BatchJob(os.environ, wrapper="").run(cmd)

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
                f_h.write(dtx(to_dump, attr_type=False))
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
        com = self.platform.resolve_macros(self.config["communicate"])
        logger.info(" communication keys: {}", com)
        # Write the config file
        if len(com) > 0:
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
        logger.info(cmd)
        BatchJob(os.environ, wrapper="").run(cmd)


@dataclass()
class Verification(ImpactModel):
    """Verification specific methods."""

    name = "verification"


@dataclass()
class AQModels(ImpactModel):
    """Verification specific methods."""

    name = "aq"


class ImpactModels(Task):
    """Create info to and start impact models."""

    def __init__(self, config, taskname=None):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            taskname (str): Indicating taskname
        """
        Task.__init__(self, config, __class__.__name__)

        self.impact = get_impact(config, taskname)

    def execute(self):
        """Start the impact model(s)."""
        for name, impact_model in self.impact.items():
            impact_model_ = impact_model
            model = ImpactModel(name=name, config=impact_model_, platform=self.platform)
            model.execute()


def get_fdb_info(config):
    """Build a fdb request.

    Args:
        config (deode.ParsedConfig): Configuration

    Returns:
        fdb_req (dict): fdb request with appropriate keys
    """
    include_keys = ("class", "dataset", "expver", "georef", "stream")
    fdb_req = {x: config.get(f"fdb.grib_set.{x}") for x in include_keys}

    if fdb_req["expver"] is None:
        return None

    add_keys = {
        "number": "eps.general.members",
        "basetime": "general.times.basetime",
        "forecast_range": "general.times.forecast_range",
    }
    for key, value in add_keys.items():
        fdb_req[key] = config.get(value)

    # Handle members
    if fdb_req["stream"] == "enfo":
        fdb_req["number"] = list(fdb_req["number"])
    else:
        fdb_req.pop("number")

    # Date/time
    fdb_req["basetime"] = as_datetime(fdb_req["basetime"])
    fdb_req["date"] = fdb_req["basetime"].strftime("%Y%m%d")
    fdb_req["time"] = fdb_req["basetime"].strftime("%H%M")
    fdb_req.pop("basetime")

    # Assumes hourly data!!!
    fdb_req["forecast_range"] = as_timedelta(fdb_req["forecast_range"])
    total_hours = int(fdb_req["forecast_range"].total_seconds() / 3600)
    fdb_req["step"] = list(range(total_hours + 1))
    fdb_req.pop("forecast_range")

    return fdb_req


def get_impact(config, taskname):
    """Gather impact settings.

    Args:
        config (deode.ParsedConfig): Configuration
        taskname (str): Indicating taskname

    Returns:
        impact (dict): Impact model settings
    """
    _impact = config.get("impact", BasicConfig({})).dict()
    installed_impact = config.get("platform.impact", {})
    impact = {}

    fdb_keys = get_fdb_info(config)
    # Resolve ecf_host/ecf_port if used
    with contextlib.suppress(HostNotFoundError):
        ecf_host = config.get("scheduler.ecfvars.ecf_host")
        ecf_port = config.get("scheduler.ecfvars.ecf_port")

        if ecf_host is not None and ecf_port is not None:
            pl = Platform(config)
            ecf_host = pl.substitute(ecf_host)
            ecf_host = pl.evaluate(ecf_host, object_=SelectHost)
            ecf_port = pl.substitute(ecf_port)
            ecf_port = pl.evaluate(ecf_port, object_=EcflowServer)

            config = config.copy(
                update={
                    "scheduler": {
                        "ecfvars": {
                            "ecf_host_resolved": ecf_host,
                            "ecf_port_resolved": ecf_port,
                        }
                    }
                }
            )

    for name, impact_model in _impact.items():
        if (
            impact_model["active"]
            and name in installed_impact
            and taskname in impact_model
        ):
            impact[name] = impact_model[taskname]
            for conf, alt in {
                "communicate": {},
                "poetry": "poetry",
                "path": None,
                "config_name": "",
            }.items():
                impact[name][conf] = impact_model.get(conf, alt)

            if fdb_keys is not None:
                impact[name]["communicate"]["fdb_request"] = fdb_keys
            # Update user ecf host and port with resolved values
            with contextlib.suppress(KeyError):
                user_ecf_host = impact[name]["communicate"].get("user_ecf_host")
                user_ecf_port = impact[name]["communicate"].get("user_ecf_port")
                if user_ecf_host is not None and user_ecf_port is not None:
                    pl = Platform(config)
                    impact[name]["communicate"]["user_ecf_host"] = pl.substitute(
                        user_ecf_host
                    )
                    impact[name]["communicate"]["user_ecf_port"] = pl.substitute(
                        user_ecf_port
                    )

    return impact


class StartImpactModels(ImpactModels):
    """Starts plugins and impact models."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        ImpactModels.__init__(self, config, __class__.__name__)
