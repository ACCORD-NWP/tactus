#!/usr/bin/env python3
"""Handle host detection."""

import os
import re
import socket

import yaml

from .config_parser import ConfigPaths, GeneralConstants
from .logs import logger


class DeodeHost:
    """DeodeHost object."""

    def __init__(self, known_hosts=None, known_hosts_file=None):
        """Constructs the DeodeHost object."""
        self.known_hosts = self._load_known_hosts(
            known_hosts=known_hosts, known_hosts_file=known_hosts_file
        )
        self.available_hosts = list(self.known_hosts)
        self.default_host = self.available_hosts[0]
        self.deode_host = os.getenv("DEODE_HOST")
        self.hostname = socket.gethostname()

    def _load_known_hosts(self, known_hosts=None, known_hosts_file=None):
        """Loads the known_hosts config.

        Args:
            known_hosts (dict, optional): Known hosts dict. Defaults to None
            known_hosts_file (str, optional): Known hosts file. Defaults to None

        Raises:
            RuntimeError: No host identifiers loaded

        Returns:
            known_host (dict): Known hosts config

        """
        if known_hosts is not None:
            return known_hosts

        if known_hosts_file is None:
            known_hosts_file = ConfigPaths.path_from_subpath("known_hosts.yml")

        with open(known_hosts_file, "rb") as infile:
            known_hosts = yaml.safe_load(infile)

        if known_hosts is None:
            raise RuntimeError(f"No hosts available in {known_hosts_file}")

        return known_hosts

    def _detect_by_hostname(self, hostname_pattern):
        """Detect deode host by hostname regex.

        Args:
            hostname_pattern(list|str) : hostname regex to match

        Returns:
            (boolean): Match or not

        """
        logger.debug("hostname={}", self.hostname)
        hh = [hostname_pattern] if isinstance(hostname_pattern, str) else hostname_pattern
        for x in hh:
            if re.match(x, self.hostname):
                logger.debug("Deode-host detected by hostname {}", x)
                return True

        return False

    def _detect_by_env(self, env_variable):
        """Detect deode host by environment variable regex.

        Args:
            env_variable(dict) : Environment variables to search for

        Returns:
            (boolean): Match or not

        """
        for var, value in env_variable.items():
            if var in os.environ:
                vv = [value] if isinstance(value, str) else value
                for x in vv:
                    if re.match(x, os.environ[var]):
                        logger.debug(
                            "Deode-host detected by environment variable {}={}", var, x
                        )
                        return True

        return False

    def detect_deode_host(self):
        """Detect deode host by matching various properties.

        First check self.deode_host as set by os.getenv("DEODE_HOST"),
        second use the defined hosts in known_hosts.yml. If no matches
        are found return the first host defined in known_hosts.yml

        Raises:
            RuntimeError: Ambiguous matches

        Returns:
            deode_host (str): mapped hostname

        """
        if self.deode_host is not None:
            return self.deode_host

        matches = []
        for deode_host, detect_methods in self.known_hosts.items():
            for method, pattern in detect_methods.items():
                fname = f"_detect_by_{method}"
                if hasattr(self, fname):
                    function = getattr(self, fname)
                    if function(pattern):
                        matches.append(deode_host)
                        break
                else:
                    raise RuntimeError(f"No deode-host detection using {method}")

        if len(matches) == 0:
            matches = list(self.known_hosts)[0:1]
            logger.info(
                f"No deode-host detected from {self.hostname}, use {self.default_host}"
            )
        if len(matches) > 1:
            raise RuntimeError(f"Ambiguous matches: {matches}")

        return matches[0]


def set_deode_home(config, deode_home=None):
    """Set deode_home in various ways.

    Args:
        config (.config_parser.ParsedConfig): Parsed config file contents.
        deode_home (str): Externally set deode_home

    Returns:
        deode_home
    """
    if deode_home is None:
        try:
            deode_home_from_config = config["platform.deode_home"]
        except KeyError:
            deode_home_from_config = "set-by-the-system"
        if deode_home_from_config != "set-by-the-system":
            deode_home = deode_home_from_config
        else:
            deode_home = str(GeneralConstants.PACKAGE_DIRECTORY)

    return deode_home
