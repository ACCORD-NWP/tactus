#!/usr/bin/env python3
"""Handle host detection."""

import os
import re
import socket
import time
from dataclasses import dataclass

import yaml

from .config_parser import ConfigPaths, GeneralConstants
from .logs import logger
from .os_utils import ping


class DeodeHost:
    """DeodeHost object."""

    def __init__(self, known_hosts=None, known_hosts_file=None):
        """Constructs the DeodeHost object."""
        self.known_hosts = self._load_known_hosts(
            known_hosts=known_hosts, known_hosts_file=known_hosts_file
        )
        self.available_hosts = list(self.known_hosts)
        self.default_host = self.available_hosts[0]
        self.tactus_host = os.getenv("DEODE_HOST")
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
        """Detect tactus host by hostname regex.

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
        """Detect tactus host by environment variable regex.

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

    def detect_tactus_host(self, use_default=True):
        """Detect tactus host by matching various properties.

        First check self.tactus_host as set by os.getenv("DEODE_HOST"),
        second use the defined hosts in known_hosts.yml. If no matches
        are found return the first host defined in known_hosts.yml

        Args:
            use_default (boolean, optional): Flag to return default host if host not found

        Raises:
            RuntimeError: Ambiguous matches

        Returns:
            tactus_host (str): mapped hostname

        """
        if self.tactus_host is not None:
            return self.tactus_host

        matches = []
        for tactus_host, detect_methods in self.known_hosts.items():
            for method, pattern in detect_methods.items():
                fname = f"_detect_by_{method}"
                if hasattr(self, fname):
                    function = getattr(self, fname)
                    if function(pattern):
                        matches.append(tactus_host)
                        break
                else:
                    raise RuntimeError(f"No tactus-host detection using {method}")

        if len(matches) == 0:
            if use_default:
                matches = list(self.known_hosts)[0:1]
                logger.info(
                    f"No tactus-host detected from {self.hostname}, "
                    + f"use {self.default_host}"
                )
            else:
                matches = [None]
                logger.info(f"No tactus-host detected from {self.hostname}, return None")
        if len(matches) > 1:
            raise RuntimeError(f"Ambiguous matches: {matches}")

        return matches[0]


def set_tactus_home(config, tactus_home=None):
    """Set tactus_home in various ways.

    Args:
        config (.config_parser.ParsedConfig): Parsed config file contents.
        tactus_home (str): Externally set tactus_home

    Returns:
        tactus_home
    """
    if tactus_home is None:
        try:
            tactus_home_from_config = config["platform.tactus_home"]
        except KeyError:
            tactus_home_from_config = "set-by-the-system"
        if tactus_home_from_config != "set-by-the-system":
            tactus_home = tactus_home_from_config
        else:
            tactus_home = str(GeneralConstants.PACKAGE_DIRECTORY)

    return tactus_home


class HostNotFoundError(ValueError):
    """Custom exception."""


class AmbigiousHostError(ValueError):
    """Custom exception."""


@dataclass
class SelectHost:
    """Class for the host selection."""

    @staticmethod
    def _select_host_from_list(hosts, tries=3, delay=1):
        """Set ecf_host from list of options.

           Try to ping server tries times before giving up.

        Arguments:
            hosts (list): list of host options
            tries (int): number of times to try to find a host
            delay (int): number of seconds to wait between each try

        Returns:
            host (str): Selected host

        Raises:
            RuntimeError: In case no or more than one host found
        """
        found_hosts = []
        ntry = 1
        while ntry <= tries:
            for _host in hosts:
                host = _host.strip()
                if ping(host):
                    found_hosts.append(host)

            if len(found_hosts) == 0 and ntry == tries:
                host_list = ",".join(hosts)
                msg = f"No host found, tried:{host_list}"
                logger.error(msg)
                raise HostNotFoundError(msg)

            if len(found_hosts) == 1:
                break

            if len(found_hosts) > 1:
                host_list = ",".join(found_hosts)
                msg = f"Ambigious host selection:{host_list}"
                logger.error(msg)
                raise AmbigiousHostError(msg)

            time.sleep(delay)
            ntry += 1

        return found_hosts[0]
