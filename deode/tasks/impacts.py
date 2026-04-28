"""Impact model classes."""

import contextlib
import copy
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

import tomlkit
from git import GitCommandError, Repo

from deode.config_parser import BasicConfig
from deode.datetime_utils import as_datetime, as_timedelta
from deode.general_utils import recursive_substitute
from deode.host_actions import HostNotFoundError, SelectHost
from deode.logs import logger
from deode.os_utils import deodemakedirs
from deode.scheduler import EcflowServer
from deode.tasks.base import Task
from deode.tasks.batch import BatchJob
from deode.toolbox import Platform


@dataclass()
class BaseImpactModel:
    """Abstract class for impact models."""

    name: str
    config: dict
    platform: Platform

    def __new__(cls, name: str, *_args, **_kwargs):
        """Create a new instance of a subclass based on the field_type."""
        for subclass in BaseImpactModel.__subclasses__():
            if subclass.name == name:
                return super(BaseImpactModel, subclass).__new__(subclass)
        raise ValueError(f"No valid BaseImpactModel subclass found for name: {name}")

    def adjust(self, com):
        """Basic dummy method."""
        return com

    def run(self):
        """Starts a plugin suite.

        Returns:
            path where impact model can run

        Raises:
            KeyError: Didn't found a handle to run
        """
        # Fetch path from config
        if self.config.get("git", {}).get("active", False):
            path = self.process_git(self.config["git"])
        else:
            path = self.platform.substitute(self.config.get("path"))

        if path:
            logger.info("Change directory to: {}", path)
            os.chdir(path)
        else:
            logger.debug("Path is empty or none; so don't change directory.")

        # Get the runner to run this plugin
        poetry = self.config.get("poetry")
        runner = f"{poetry} run" if poetry is not None else self.config.get("runner")

        args = self.config.get("arguments", [])
        if isinstance(args, str):
            args = [args] if args else []

        if runner is not None:
            logger.info("Run argument(s) via: {}", runner)
            for arg in args:
                cmd = self.platform.substitute(f"{runner} {arg}")
                BatchJob(os.environ, wrapper="").run(cmd)
        elif len(args) > 0:
            raise KeyError(f"Didn't found a runner to run {self.name}")
        # else: just return the path, let the impact model implement the actual run

        return path

    def execute(self):
        """Prepares and runs the impact model commands."""
        logger.info("Impact model:{} ", str(self))

        # Recursively substitute variables in communicate config
        for key, value in self.config["communicate"].items():
            if isinstance(value, dict):
                self.config["communicate"][key] = recursive_substitute(
                    value, self.platform, pos=[key]
                )
            else:
                self.config["communicate"][key] = self.platform.substitute(value)

        # Write the config file
        com = self.platform.resolve_macros(self.config["communicate"])
        config_name = self.platform.substitute(self.config.get("config_name"))

        if config_name is not None:
            com = self.adjust(com)
            logger.debug(" communication keys: {}", com)
            # Write the config file
            if len(com) > 0:
                deodemakedirs(
                    Path(config_name).parent,
                    unixgroup=self.platform.get_value("platform.unix_group"),
                    exist_ok=True,
                )
                logger.info("Save impact model config as {}", config_name)
                BasicConfig(com).save_as(config_name)
        elif len(com) > 0:
            logger.warning("Found keys to communicate but config_name not set.")

        # Execute the impact model specific command
        self.run()

    def process_git(self, git_config):
        """Process git-configuration block where plugin should be located.

        Args:
            git_config: git config section for impact model

        Returns:
            plugin_dir: Path where plugin is located
        """
        logger.info(f"Git source is active for {self.name}")
        plugin_dir = self.platform.substitute(git_config.get("dir"))
        do_git_checkout = plugin_dir is None or not os.path.isdir(plugin_dir)
        unixgroup = self.platform.get_value("platform.unix_group")

        if do_git_checkout:
            logger.info("No pre-stored git.dir found; checkout new repository...")

            # Checkout new git repository
            path = self.platform.substitute(self.config["path"])

            # Create already path if it doesn't exist
            deodemakedirs(path, unixgroup=unixgroup)

            if plugin_dir is None:
                plugin_dir = tempfile.NamedTemporaryFile(
                    prefix=f"{self.name}_", dir=path, delete=True
                ).name
            deodemakedirs(plugin_dir, unixgroup=unixgroup)

            # Check-out repo
            remote_url = git_config["remote_url"]
            branch = git_config["branch"]
            logger.info(f"Clone {branch} from {remote_url} in {plugin_dir}")
            try:
                Repo.clone_from(remote_url, plugin_dir, branch=branch)
            except GitCommandError as ex:
                logger.info(
                    f"Catched exception {ex} when checking out repo; "
                    "retry using plain git command."
                )
                git_cmd = f"""
                git clone --single-branch --branch {branch} {remote_url} {plugin_dir};
                """
                BatchJob(os.environ, wrapper="").run(git_cmd)
        else:
            logger.info(f"Found installed {self.name} in {plugin_dir}; use that.")

        # Add plugin_dir to plugin's plugin_registry
        config_name = self.platform.substitute(self.config.get("config_name"))
        if config_name is not None:
            plugin_comm = BasicConfig.from_file(config_name)
            key = f"general.plugin_registry.plugins.{self.name}"
            plugin_comm = plugin_comm.update(key, plugin_dir)

            # Write plugin dir to appropriate key as specified by plugin_dir_key
            plugin_dir_key = self.config.get("plugin_dir_key")
            if plugin_dir_key is not None:
                plugin_comm = plugin_comm.update(plugin_dir_key, plugin_dir)

            plugin_comm.save_as(config_name)

        if do_git_checkout:
            # Inject checkout folder into main config file

            deode_config_file = self.platform.substitute("@CONFIG@")
            deode_cfg = BasicConfig.from_file(deode_config_file)

            remove_prefix = f"impact.{self.name}.git"
            deode_cfg = deode_cfg.update(f"{remove_prefix}.remove_dir", do_git_checkout)
            deode_cfg = deode_cfg.update(f"{remove_prefix}.dir", plugin_dir)

            logger.warning(
                f"Updated config file {deode_config_file} to inject {remove_prefix} keys"
            )
            deode_cfg.save_as(deode_config_file)

        return plugin_dir

    def __str__(self):
        return self.name

    def get_fdb_output_info(self):
        """Build a dict of fdb keys for output.

        Returns:
            fdb_out (dict): Dict with appropriate grib keys to set
        """
        fdb_out = None
        expver = self.config.get("communicate").get("output_expver")
        if expver is not None:
            fdb_out = self.config.get("communicate").get("fdb")
            fdb_out = fdb_out["grib_set"]
            fdb_out["expver"] = expver

        return fdb_out


@dataclass()
class Ehype(BaseImpactModel):
    """EHYPE specific methods."""

    name = "ehype"

    def adjust(self, com):
        """Adjust keys to fit EHYPE needs."""
        new_com = copy.deepcopy(com)

        new_com["fdb_output"] = self.get_fdb_output_info()

        # Remove EHYPE hostile keys
        if "fdb_request" in new_com:
            for key in ["number", "step"]:
                new_com["fdb_request"].pop(key, None)

        for key in ["fdb", "output_expver"]:
            new_com.pop(key, None)

        _members = (
            new_com["members"]
            if isinstance(new_com["members"], list)
            else list(new_com["members"])
        )
        members = ",".join([f"mbr{x:03}" for x in _members])
        new_com["members"] = members

        return new_com


@dataclass()
class EPSUpscaling(BaseImpactModel):
    """EPS upscaling specific methods."""

    name = "eps_upscaling"


@dataclass()
class Nwp2Windpower(BaseImpactModel):
    """NWP2windpower specific methods."""

    name = "nwp2windpower"


@dataclass()
class Verification(BaseImpactModel):
    """Verification specific methods."""

    name = "verification"


@dataclass()
class AQModels(BaseImpactModel):
    """Verification specific methods."""

    name = "aq"


@dataclass()
class Nowcasting(BaseImpactModel):
    """Nowcasting specific methods."""

    name = "nowcasting"


class Dwml(BaseImpactModel):
    """DWML Multi-domain specific methods."""

    name = "dwml"


@dataclass()
class WildFire(BaseImpactModel):
    """Wildfire specific methods."""

    name = "wildfire"

    def run(self):
        """Starts the WildFire suite."""
        # Let BaseImpactModel handle the path preparation
        tmp_path = super().run()

        config_name = self.platform.substitute(self.config.get("config_name"))

        logger.info(" Load config from: {}", config_name)
        config_ = tomlkit.loads(Path(config_name).read_text())

        # expands ecf variables
        ecf_host = config_["ecfvars"]["ECF_HOST"]
        ecf_host = self.platform.substitute(ecf_host)
        ecf_host = self.platform.evaluate(ecf_host, object_=SelectHost)
        config_["ecfvars"]["ECF_HOST"] = ecf_host

        troika_config_file = config_["ecfvars"]["TROIKA_CONFIG"]
        endpart = troika_config_file.split(")", 2)[-1]
        path = str(self.platform.evaluate(troika_config_file, object_="deode.os_utils"))
        config_["ecfvars"]["TROIKA_CONFIG"] = path + endpart

        path = self.platform.substitute(config_["workdir"])
        deodemakedirs(path, unixgroup=config_["unix_group"])

        path = self.platform.substitute(config_["archive"])
        deodemakedirs(path, unixgroup=config_["unix_group"])

        # adds path to wf suite
        suite_path = os.path.join(str(tmp_path), "IPMA-FIRE")
        sys.path.append(suite_path)

        # updates path to cloned repo in the original deode config file
        config_["repo_home"] = str(tmp_path)

        # updates ecflow server variables
        ecf_files = os.path.join(suite_path, "wf-suite/ecf_files")
        config_["ecfvars"]["ECF_FILES"] = ecf_files

        ecf_include = os.path.join(suite_path, "wf-suite/include")
        config_["ecfvars"]["ECF_INCLUDE"] = ecf_include

        # writes wildfire config file to disk
        logger.info(" Dump modified config to: {}", config_name)
        with open(config_name, mode="w", encoding="utf-8") as f_h:
            f_h.write(tomlkit.dumps(config_))

        os.chdir(suite_path + "/wf-suite/pytools")
        uv_path = shutil.which("uv")
        if uv_path is None:
            raise RuntimeError("uv not found in PATH")

        subprocess.run([uv_path, "venv", "--clear"], check=True)
        subprocess.run([uv_path, "sync"], check=True)

        # creates and loads wildfire applications suite
        import wf_suite

        wf_suite.load_suite(config_)


class ImpactModels(Task):
    """Create info to and start impact models."""

    def __init__(self, config, taskname=None):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            taskname (str): Indicating name of task in suite
        """
        Task.__init__(self, config, __class__.__name__)

        self.impact = get_impact(config, taskname)

    def execute(self):
        """Start the impact model(s)."""
        for name, impact_config in self.impact.items():
            model = BaseImpactModel(
                name=name,
                config=impact_config,
                platform=self.platform,
            )
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
        taskname (str): Indicating name of task in suite

    Returns:
        impact (dict): Impact model settings
    """
    _impact = config.get_as_dict("impact", {})
    installed_impact = config.get_as_dict("platform.impact", {})

    impact = {}

    fdb_keys = get_fdb_info(config)
    # Resolve ecf_host/ecf_port if used
    with contextlib.suppress(HostNotFoundError):
        ecf_host = config.get_as_dict("scheduler.ecfvars.ecf_host")
        ecf_port = config.get_as_dict("scheduler.ecfvars.ecf_port")

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
            impact_model.get("active", False)
            and name in installed_impact
            and (taskname in impact_model or impact_model.get("task") == taskname)
        ):
            impact[name] = impact_model.get(taskname, {})

            # Fetch path, poetry etc from platform.impact and load config from impact.*
            for source in [installed_impact.get(name, {}), impact_model]:
                if isinstance(source, (BasicConfig, dict)):
                    for conf, value in source.items():
                        if conf not in impact[name]:
                            impact[name][conf] = value

            if "communicate" not in impact[name]:
                impact[name]["communicate"] = {}

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
