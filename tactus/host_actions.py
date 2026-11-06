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


def _as_list(patterns):
    """Return a pattern or a list of patterns as a list.

    Args:
        patterns (str|list|tuple): One regex or a sequence of regexes

    Returns:
        (list): List of regexes

    Raises:
        TypeError: If patterns is neither a string nor a list/tuple of strings
    """
    if isinstance(patterns, str):
        return [patterns]
    if isinstance(patterns, (list, tuple)) and all(isinstance(p, str) for p in patterns):
        return list(patterns)
    raise TypeError(f"Expected a regex or a list of regexes, got {patterns!r}")


class TactusHost:
    """TactusHost object."""

    def __init__(self, known_hosts=None, known_hosts_file=None):
        """Constructs the TactusHost object."""
        self.known_hosts = self._load_known_hosts(
            known_hosts=known_hosts, known_hosts_file=known_hosts_file
        )
        # An empty TACTUS_HOST is treated as not set
        self.tactus_host = os.getenv("TACTUS_HOST") or None
        self.hostname = socket.gethostname()

    @property
    def available_hosts(self):
        """List of the known host names."""
        return list(self.known_hosts)

    @property
    def default_host(self):
        """The fallback host, i.e. the first known host (None if there are none)."""
        return next(iter(self.known_hosts), None)

    @staticmethod
    def _load_known_hosts(known_hosts=None, known_hosts_file=None):
        """Loads the known_hosts config.

        Args:
            known_hosts (dict, optional): Known hosts dict. Defaults to None
            known_hosts_file (str, optional): Known hosts file. Defaults to None

        Returns:
            known_host (dict): Known hosts config

        Raises:
            RuntimeError: No host identifiers loaded
        """
        source = "the given known_hosts"
        if known_hosts is None:
            if known_hosts_file is None:
                known_hosts_file = ConfigPaths.path_from_subpath("known_hosts.yml")
            source = known_hosts_file
            with open(known_hosts_file, "rb") as infile:
                known_hosts = yaml.safe_load(infile)

        if not known_hosts:
            raise RuntimeError(f"No hosts available in {source}")
        if not isinstance(known_hosts, dict):
            raise RuntimeError(f"Known hosts in {source} must be a mapping")

        return known_hosts

    def _detect_by_hostname(self, hostname_pattern):
        """Detect tactus host by hostname regex.

        Args:
            hostname_pattern(list|str) : hostname regex(es) to match

        Returns:
            (boolean): Match or not

        """
        return any(re.match(x, self.hostname) for x in _as_list(hostname_pattern))

    def _detect_by_env(self, env_variable):
        """Detect tactus host by environment variable regex.

        Args:
            env_variable(dict) : Environment variables to search for

        Returns:
            (boolean): True if any of the variables matches

        Raises:
            TypeError: If env_variable is not a mapping

        """
        if not isinstance(env_variable, dict):
            raise TypeError(f"env rule must be a mapping, got {env_variable!r}")

        for var, value in env_variable.items():
            env_value = os.environ.get(var)
            if env_value is not None and any(
                re.match(x, env_value) for x in _as_list(value)
            ):
                return True

        return False

    def _matched_rules(self, rules):
        """Return the detection methods of a host whose rules match.

        Args:
            rules (dict): Detection rules for one host, {method: pattern}

        Returns:
            (list): The methods that matched

        Raises:
            RuntimeError: Unknown detection method
        """
        matched = []
        for method, pattern in rules.items():
            detect = getattr(self, f"_detect_by_{method}", None)
            if detect is None:
                raise RuntimeError(f"No tactus-host detection using {method}")
            if detect(pattern):
                matched.append(method)
        return matched

    def detect_tactus_host(self, use_default=True):
        """Detect tactus host by matching various properties.

        First check self.tactus_host as set by os.getenv("TACTUS_HOST"),
        second use the defined hosts in known_hosts.yml. A host is detected
        when all of its rules match. If no host matches, return the first
        host defined in known_hosts.yml (or None if use_default is False).

        Args:
            use_default (boolean, optional): Flag to return default host if host not found

        Returns:
            tactus_host (str): mapped hostname

        Raises:
            RuntimeError: Ambiguous or incomplete matches
        """
        if self.tactus_host is not None:
            return self.tactus_host

        logger.debug("hostname={}", self.hostname)
        complete = []
        partial = {}
        for host, rules in self.known_hosts.items():
            matched = self._matched_rules(rules)
            if len(matched) == len(rules):
                complete.append(host)
            elif matched:
                partial[host] = matched

        if len(complete) > 1:
            raise RuntimeError(f"Ambiguous matches: {complete}")

        if len(complete) == 1:
            host = complete[0]
            if partial:
                logger.debug("Ignoring partial matches {}, using {}", partial, host)
            logger.debug("tactus-host detected: {}", host)
            return host

        if partial:
            for host, matched in partial.items():
                logger.error(
                    "Host {} matched {} but not all rules: {}",
                    host,
                    matched,
                    self.known_hosts[host],
                )
            raise RuntimeError(f"Incomplete match for {list(partial)}")

        return self._catch_no_detected_host(use_default)

    def _catch_no_detected_host(self, use_default):
        """Return the fallback when no host is detected.

        Args:
            use_default (boolean): Return the default host instead of None

        Returns:
            (str|None): The default host or None
        """
        host = self.default_host if use_default else None
        logger.info("No tactus-host detected from {}, use {}", self.hostname, host)
        return host


def set_tactus_home(config, tactus_home=None):
    """Set tactus_home in various ways.

    Args:
        config (ParsedConfig): Parsed config file contents.
        tactus_home (str): Externally set tactus_home

    Returns:
        tactus_home
    """
    if tactus_home is not None:
        return tactus_home

    try:
        tactus_home_from_config = config["platform.tactus_home"]
    except KeyError:
        tactus_home_from_config = "set-by-the-system"

    if tactus_home_from_config != "set-by-the-system":
        return tactus_home_from_config
    return str(GeneralConstants.PACKAGE_DIRECTORY)


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
            HostNotFoundError: In case no host is found
            AmbigiousHostError: In case more than one host is found
        """
        hosts = [host.strip() for host in hosts]
        for ntry in range(1, tries + 1):
            found_hosts = [host for host in hosts if ping(host)]

            if len(found_hosts) == 1:
                return found_hosts[0]

            if len(found_hosts) > 1:
                msg = f"Ambigious host selection:{','.join(found_hosts)}"
                logger.error(msg)
                raise AmbigiousHostError(msg)

            if hosts and ntry < tries:
                time.sleep(delay)

        msg = f"No host found, tried:{','.join(hosts)}"
        logger.error(msg)
        raise HostNotFoundError(msg)
