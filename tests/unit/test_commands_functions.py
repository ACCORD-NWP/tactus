#!/usr/bin/env python3
"""Unit tests for commands_functions.py."""
import os
from argparse import ArgumentParser

import pytest
import tomlkit

from deode.commands_functions import set_deode_home
from deode.config_parser import ParsedConfig


@pytest.fixture()
def config_platform():
    """Set the platform specific configuration."""
    task_configs = tomlkit.parse(
        """
        [general.times]
            start = "2000-01-01T00:00:00Z"
            end = "2000-01-02T00:00:00Z"
        """
    )

    return task_configs


@pytest.fixture()
def parsed_config(config_platform):
    return ParsedConfig.parse_obj(config_platform)


def test_set_deode_home(parsed_config):
    arg = ArgumentParser()
    arg.deode_home = None
    deode_home = set_deode_home(arg, parsed_config)
    assert os.path.isdir(deode_home)


if __name__ == "__main__":
    pytest.main()
