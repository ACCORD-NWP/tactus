"""Experiment tools."""

import collections
import contextlib
import os
import subprocess
from pathlib import Path
from typing import List

import tomlkit

from .config_parser import BasicConfig, ParsedConfig
from .derived_variables import set_times
from .host_actions import set_deode_home
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
                    logger.info("Merging modifications from {}", mod)
                    lmod = ParsedConfig.from_file(mod, json_schema={}, host=host)
                except tomlkit.exceptions.ParseError as exc:
                    logger.error("Expected a toml file but got {}", mod)
                    logger.error("Did mean to write ?{}", mod)
                    raise RuntimeError from exc

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

        # Remove sections from the input config
        remove_sections = merged_config["general"].get("remove_sections", [])
        if len(remove_sections) > 0:
            logger.info("Remove sections:{}", remove_sections)
            reduced_config = config.dict()
            for key in remove_sections:
                reduced_config.pop(key)
            merged_config["general"].pop("remove_sections")
            config = BasicConfig(reduced_config)

        Exp.__init__(
            self,
            config,
            merged_config,
        )

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
        exp_dependencies = {
            "tmp_outfile": f"{output_file}.tmp.{os.getpid()}.toml",
            "config_dir": config_dir,
            "case": case,
        }
        return exp_dependencies


def case_setup(
    config,
    output_file,
    mod_files: List[Path],
    case=None,
    host=None,
    config_dir=None,
    expand_config=False,
):
    """Do experiment setup.

    Args:
        config (.config_parser.ParsedConfig): Parsed config file contents.
        output_file (str): Output config file.
        mod_files (list): Modifications. Defaults to None.
        case (str, optional): Case identifier. Defaults to None.
        host (str, optional): host name. Defaults to None.
        config_dir (str, optional): Configuration directory. Defaults to None.
        expand_config (boolean, optional): Flag for expanding macros in config

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

    if expand_config:
        deode_home = set_deode_home(config)
        exp.config = exp.config.copy(update={"platform": {"deode_home": deode_home}})
        exp.config = exp.config.expand_macros()

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
