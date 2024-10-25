"""Experiment tools."""

import collections
import contextlib
import os
import subprocess
from pathlib import Path
from typing import List

import tomlkit

from .config_parser import ConfigParserDefaults, ParsedConfig
from .derived_variables import set_times
from .logs import logger
from .os_utils import resolve_path_relative_to_package
from .toolbox import Platform


class Exp:
    """Experiment class."""

    def __init__(
        self,
        config,
        merged_config,
    ):
        """Instanciate an object of the main experiment class.

        Args:
            config (.config_parser.ParsedConfig): Parsed config file contents.
            merged_config (dict): Experiment configuration

        """
        logger.debug("Construct Exp")
        config = config.copy(update=merged_config)
        config = config.copy(update={"git_info": get_git_info()})
        self.config = config


class ExpFromFiles(Exp):
    """Generate Exp object from existing files. Use config files from a setup."""

    def __init__(
        self,
        config,
        exp_dependencies,
        mod_files: List[Path],
        host=None,
        merged_config=None,
    ):
        """Construct an Exp object from files.

        Args:
            config (.config_parser.ParsedConfig): Parsed config file contents.
            exp_dependencies (dict): Exp dependencies
            mod_files (List[Path]): Case modifications
            host (DeodeHost, optional): Deode host. Defaults to None.
            merged_config (dict, optional): Possible merged input configuration.
                                            Defaults to None.

        Raises:
            FileNotFoundError: If host file(s) not found

        """
        logger.debug("Construct ExpFromFiles")
        logger.debug("Experiment dependencies: {}", exp_dependencies)

        config_dirs = str(exp_dependencies.get("config_dir")).split(":")
        include_paths = {}
        if host is not None:
            host = host.detect_deode_host()
            logger.info("Setting up for host {}", host)
            include_needs = {
                "scheduler": f"scheduler/ecflow_{host}.toml",
                "platform": f"platform_paths/{host}.toml",
                "submission": f"submission/{host}.toml",
            }
            missing_include = {}
            for include, include_path in include_needs.items():
                missing_include[include] = []
                for config_dir in config_dirs:
                    incp = f"{config_dir}/include/{include_path}"
                    if os.path.exists(incp):
                        include_paths.update({include: incp})
                        break
                    else:  # noqa RET508
                        missing_include[include].append(incp)
            if len(include_paths) != 3:
                for include, include_path in missing_include.items():
                    logger.error(" No {} include files as {}", include, include_path)
                raise FileNotFoundError

        config_dict = config.dict()
        for inct, incp in include_paths.items():
            if inct in ["platform", "submission"]:
                del config_dict[inct]
                config_dict.update({inct: {}})

            logger.info("Input file loaded {}", incp)
            with open(incp, mode="r", encoding="utf8") as fh:
                mod_config = tomlkit.load(fh)

            config_dict = self.deep_update(config_dict, mod_config)
        config = ParsedConfig(
            config_dict, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )

        mods = {}
        for _mod in mod_files:
            # Skip empty paths
            if _mod == Path():
                continue
            mod = Path(str(_mod).replace("@HOST@", host)) if host is not None else _mod
            mod = resolve_path_relative_to_package(mod, ignore_errors=True)
            # First check if mod exists as is
            if os.path.exists(mod):
                try:
                    lmod = ExpFromFiles.toml_load(mod)
                except tomlkit.exceptions.ParseError as exc:
                    logger.error("Expected a toml file but got {}", mod)
                    logger.error("Did mean to write ?{}", mod)
                    raise RuntimeError from exc

                logger.info("Merging modifications from {}", mod)
                logger.debug("-> {}", lmod)
                mods = ExpFromFiles.deep_update(mods, lmod)
            else:
                logger.warning("Skip missing modification file {}", mod)

        case = exp_dependencies.get("case")
        if case is not None:
            if "general" not in mods:
                mods.update({"general": {}})
            mods["general"].update({"case": case})

        # Merge with possible incoming modifications
        if merged_config is None:
            merged_config = {}
        merged_config = ExpFromFiles.deep_update(merged_config, mods)
        Exp.__init__(
            self,
            config,
            merged_config,
        )

    @staticmethod
    def toml_load(fname):
        """Load from toml file.

        Using tomlkit to preserve stucture

        Args:
            fname (str): Filename

        Returns:
            dict: Loaded toml file

        """
        with open(fname, "r", encoding="utf8") as f_h:
            res = tomlkit.parse(f_h.read())
        return res

    @staticmethod
    def toml_dump(to_dump, fname):
        """Dump toml to file.

        Using tomlkit to preserve stucture

        Args:
            to_dump (dict): Data to save
            fname (str): Filename

        """
        with open(fname, mode="w", encoding="utf8") as f_h:
            f_h.write(tomlkit.dumps(to_dump))

    @staticmethod
    def deep_update(source, overrides):
        """Update a nested dictionary or similar mapping.

        Modify ``source`` in place.

        Args:
            source (dict): Source
            overrides (dict): Updates

        Returns:
            dict: Updated dictionary

        """
        for key, value in overrides.items():
            if isinstance(value, collections.abc.Mapping) and value:
                returned = ExpFromFiles.deep_update(source.get(key, {}), value)
                source[key] = returned
            else:
                override = overrides[key]

                source[key] = override

        return source

    @staticmethod
    def setup_files(
        output_file,
        case=None,
        config_dir=None,
    ):
        """Set up the files for an experiment.

        Args:
            output_file (str): Output file
            case (str, optional): Experiment name. Defaults to None.
            config_dir (str, optional): Config directory. Defaults to None.

        Returns:
            exp_dependencies(dict): Experiment dependencies from setup.

        """
        exp_dependencies = {}
        if config_dir is None:
            config_dir = ConfigParserDefaults.CONFIG_DIRECTORY
        else:
            config_dir += f":{ConfigParserDefaults.CONFIG_DIRECTORY}"
        logger.info("Setting search path(s) config include files: {}", config_dir)

        exp_dependencies.update(
            {
                "tmp_outfile": f"{output_file}.tmp.{os.getpid()}.toml",
                "config_dir": config_dir,
                "case": case,
            }
        )
        return exp_dependencies


def case_setup(
    config,
    output_file,
    mod_files: List[Path],
    case=None,
    host=None,
    config_dir=None,
):
    """Do experiment setup.

    Args:
        config (.config_parser.ParsedConfig): Parsed config file contents.
        output_file (str): Output config file.
        mod_files (list): Modifications. Defaults to None.
        case (str, optional): Case identifier. Defaults to None.
        host (str, optional): host name. Defaults to None.
        config_dir (str, optional): Configuration directory. Defaults to None.

    Returns:
        output_file (str): Output config file.

    """
    logger.info("************ CaseSetup ******************")

    exp_dependencies = ExpFromFiles.setup_files(
        output_file,
        case=case,
        config_dir=config_dir,
    )

    exp = ExpFromFiles(config, exp_dependencies, mod_files, host=host)
    if output_file is None:
        config = exp.config.copy(update=set_times(exp.config))
        output_file = config.get("general.case") + ".toml"
        output_file = Platform(config).substitute(output_file)
    logger.info("Save config to: {}", output_file)
    exp.config.save_as(output_file)

    return output_file


def get_git_info():
    """Get git information."""
    gitcmds = {
        "branch": ["git", "rev-parse", "--abbrev-ref", "HEAD"],
        "commit": ["git", "rev-parse", "HEAD"],
        "remote": ["git", "rev-parse", "--abbrev-ref", "@{u}"],
        "describe": ["git", "describe", "--long", "--always", "--tags", "--dirty"],
    }

    git_info = {}
    for label, cmd in gitcmds.items():
        with contextlib.suppress(subprocess.CalledProcessError):
            git_info[label] = (
                subprocess.check_output(cmd, stderr=subprocess.DEVNULL)  # noqa S603
                .strip()
                .decode("utf-8")
            )
    with contextlib.suppress(KeyError, subprocess.CalledProcessError):
        remote = git_info["remote"].split("/")[0]
        cmd = ["git", "remote", "get-url", remote]
        git_info["remote_url"] = (
            subprocess.check_output(cmd, stderr=subprocess.DEVNULL)  # noqa S603
            .strip()
            .decode("utf-8")
        )
    return git_info
