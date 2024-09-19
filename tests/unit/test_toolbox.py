"""Unit tests for os_utils."""

import inspect
import os
from unittest import mock

import pytest

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.toolbox import Platform


@pytest.fixture(name="config", scope="module")
def fixture_config():
    """Return a raw config common to all tasks."""
    return ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


@pytest.fixture(name="platform")
def fixture_platform(config):
    """Return a Platform object."""
    return Platform(config)


@pytest.fixture(name="string_object")
def fixture_string_object():
    """Return a string object."""
    return "string_object"


@pytest.fixture(name="command_function_string")
def fixture_command_function_string():
    """Return a non-existing command function string."""
    return "command_function('arg1', 'arg2',)"


class EmptyClass:
    """Empty class for testing purposes."""


class OneMethodClass:
    """Empty class for testing purposes."""

    def method(self):
        """Method for testing purposes."""


# Save the original isfunction function.
isfunction_orig = inspect.isfunction


def isfunction_patch(func):
    """Patch for inspect.isfunction to return True for specific functions."""
    if func in [os.path.join, OneMethodClass.method]:
        return True
    return isfunction_orig(func)


class TestPlatformEvaluate:
    """Test the evaluate method of the Platform class."""

    def test_no_match(self, platform: Platform, string_object: str):
        """Test that command_string is returned if no match with function pattern."""
        command_string = "command_string"
        assert platform.evaluate(command_string, string_object) == command_string

    def test_match_but_no_module(
        self, platform: Platform, string_object: str, command_function_string: str
    ):
        """Test that ModuleNotFoundError is raised if no module is found."""
        with pytest.raises(ModuleNotFoundError):
            platform.evaluate(command_function_string, string_object)

    def test_match_but_no_function(
        self, platform: Platform, command_function_string: str
    ):
        """Test that AttributeError is raised if no function is found in module."""
        with pytest.raises(AttributeError):
            platform.evaluate(command_function_string, "os.path")

    def test_match_and_function(self, platform: Platform):
        """Test that function is called if match is found."""
        # Mock inspect.isfunction to return True for os.path.join and
        # otherwise return the original value (to avoid messing up other
        # usage of inspect.isfunction).
        # Mock os.path.join to check if it is called.
        with mock.patch("inspect.isfunction", new=isfunction_patch), mock.patch(
            "os.path.join"
        ) as mock_command_function:
            platform.evaluate("join(1, 2,)", "os.path")
            mock_command_function.assert_called_once()

    def test_match_and_empty_class(self, platform: Platform, command_function_string):
        """Test that ValueError is raised if no match of function pattern in class."""
        with pytest.raises(AttributeError):
            platform.evaluate(command_function_string, EmptyClass)

    def test_match_and_class(self, platform: Platform):
        """Test that function is called if match is found in class."""
        # Mock inspect.isfunction to return True for OneMethodClass.method and
        # otherwise return the original value (to avoid messing up other
        # usage of inspect.isfunction).
        # Mock OneMethodClass.method to check if it is called.
        with mock.patch("inspect.isfunction", new=isfunction_patch), mock.patch.object(
            OneMethodClass, "method"
        ) as mock_command_function:
            platform.evaluate("method(1, 2,)", OneMethodClass)
            mock_command_function.assert_called_once()
