"""Unit tests for classes and functions in eps_setup.py."""

import random
from collections import deque
from types import GeneratorType
from typing import Dict
from unittest.mock import Mock, patch

import numpy as np
import pytest
from pydantic import ValidationError

from deode.config_parser import ParsedConfig
from deode.eps.custom_generators import BaseGenerator
from deode.eps.eps_setup import (
    EPSConfig,
    EPSGeneralConfigs,
    check_expandable_keys,
    generate_member_settings,
    generate_values,
    get_expandable_keys,
    get_member_config,
    instantiate_generators,
)


class TestEPSGeneralConfigs:
    """Unit tests of the EPSGeneralConfigs class."""

    @pytest.mark.parametrize(
        ("input_members", "expected_output"),
        [
            ("0:3,4,5,10:15", [0, 1, 2, 4, 5, 10, 11, 12, 13, 14]),
            (
                [0, 1, 2, 10, 11, 4, 5, 14, 15, 12, 12, 12, 13],
                [0, 1, 2, 4, 5, 10, 11, 12, 13, 14, 15],
            ),
        ],
    )
    def test_infer_members(
        self, input_members: list[int] | str, expected_output: list[int]
    ):
        """Test members inferrence from string and unsorted list with duplicates.

        Args:
            input_members (list[int] | str): The input members as string or list
            expected_output (list[int]): The expected list of members
        """
        result = EPSGeneralConfigs.infer_members(value=input_members)
        assert result == expected_output


class TestValidateExpandables:
    """Unit tests of the validate_expandables method of the EPSConfig class."""

    @pytest.fixture(name="eps_general_config", scope="class")
    def fixture_eps_general_config(self) -> EPSGeneralConfigs:
        """Fixture for defining a test EPSGeneralConfigs class."""
        return EPSGeneralConfigs(control_member=0, members=[0, 1])

    def test_with_valid_data(self, eps_general_config: EPSGeneralConfigs):
        """Test with valid expandable data."""
        valid_data = {
            "field1": {"0:3": 1, "3:6": 2, "3": 3, "5": 5},
            "field2": [1, 2, 3],
            "field3": "some_string",
            "field4": 123,
            "field5": True,
            "field6": 45.67,
        }

        # Test function through pydantic
        EPSConfig(eps_general_config, valid_data)

    def test_with_nested_data(self, eps_general_config: EPSGeneralConfigs):
        """Test with nested expandable data.

        To check that recursion works correctly.
        """
        valid_data = {
            "field1": {"0:3": 1, "3:6": 2, "3": 3, "5": 5},
            "field2": [1, 2, 3],
            "field3": {
                "nested_field1": {"0:3": 1, "3:6": 2, "3": 3, "5": 5},
                "nested_field2": [1, 2, 3],
            },
        }

        # Test function through pydantic
        EPSConfig(eps_general_config, valid_data)

    def test_with_invalid_data1(
        self,
        eps_general_config: EPSGeneralConfigs,
    ):
        """Test with invalid expandable data, that throws ValueError."""
        invalid_data = {"field1": {"0:3": 1, "invalid_key": 2}}
        with pytest.raises(
            ValueError,
            match=r".*must all be either integers or string slices.*",
        ):
            # Test function through pydantic
            EPSConfig(eps_general_config, invalid_data)

    @pytest.mark.parametrize(
        "invalid_data",
        [
            {"field1": [{"0:3": 1}, {"invalid_key": 2}]},
            {"field2": [[["invalid_key"]]]},
            {"field3": [{"nested_field": {"invalid_key": 2}}]},
        ],
    )
    def test_with_invalid_data2(
        self,
        invalid_data: dict,
        eps_general_config: EPSGeneralConfigs,
    ):
        """Test with invalid expandable data, that throws ValidationError."""
        with pytest.raises(ValidationError):
            # Test function through pydantic
            EPSConfig(eps_general_config, invalid_data)

    @pytest.mark.parametrize(
        "invalid_non_dict_data",
        [{"field1": None}, {"field1": bytes}],
    )
    def test_with_invalid_non_dict_data(
        self,
        invalid_non_dict_data: dict[str, None] | dict[str, type[bytes]],
        eps_general_config: EPSGeneralConfigs,
    ):
        """Test with invalid non-dict data."""
        with pytest.raises(ValidationError):
            EPSConfig(eps_general_config, invalid_non_dict_data)


@pytest.fixture(name="eps_config", scope="class")
def fixture_eps_config() -> EPSConfig:
    """Fixture for defining a test EPSConfig class."""
    general_config = EPSGeneralConfigs(control_member=0, members=[0, 1])
    return EPSConfig(general=general_config, member_settings={})


class TestGenerateMemberSettings:
    """Unit tests for the generate_member_settings function."""

    @patch("deode.eps.eps_setup.generate_values")
    def test_generate_member_settings(
        self,
        mock_generate_values: Mock,
        eps_config: EPSConfig,
    ):
        """Test that correct member is returned."""
        # Make mocked function return some test dict
        mock_generate_values.side_effect = [
            {"field1": 1},
            {"field1": 2},
        ]

        # Mock out instantiate_generators
        with patch("deode.eps.eps_setup.instantiate_generators"):
            # Call function under test
            result = list(generate_member_settings(eps_config))

        # Check that correct member is returned according to members in ensemble.
        assert result == [
            (0, {"field1": 1}),
            (1, {"field1": 2}),
        ]


class TestGenerator(BaseGenerator[bool]):
    """Test generator class to generate random boolean values."""

    def __iter__(self):
        for _ in self.members:
            yield random.choice([True, False])  # noqa: S311


class TestInstantiateGenerators:
    """Unit tests for the instantiate_generators function.

    In the context of this function, the unit test covers actually the integration
    of multiple utility functions. This integration seems as the most natural
    way of testing the instantiate_generators.
    """

    @pytest.mark.parametrize(
        "const_test_value",
        [
            True,
            "test",
            [True, False],
            ["test1", "test2"],
            {"0": True, "1": False},
            {"0": "test1", "1": "test2"},
        ],
    )
    def test_type_and_iterable(
        self,
        const_test_value,
        eps_config: EPSConfig,
    ):
        """Test that returned generators are of correct type and iterable."""
        config = {"field1": const_test_value}
        result = instantiate_generators(config, eps_config.general.members)
        assert "field1" in result

        assert isinstance(result["field1"], GeneratorType)

        # Test that generator returns the test value
        generated_values = [next(result["field1"]) for _ in range(10)]
        assert len(generated_values) > 0

    @patch("deode.custom_validators.import_from_string")
    # @patch("deode.eps.eps_setup.value_from_any_generator")
    def test_with_base_generator(
        self,
        # mock_value_from_any_generator: Mock,
        mock_import_from_string: Mock,
        eps_config: EPSConfig,
    ):
        """Test instantiate_generators with BaseGenerator."""
        mock_import_from_string.return_value = TestGenerator

        config = {"field1": "some.generator.Class"}
        result = instantiate_generators(config, eps_config.general.members)

        assert "field1" in result
        assert isinstance(result["field1"], GeneratorType)

    def test_with_nested_dict(
        self,
        eps_config: EPSConfig,
    ):
        """Test instantiate_generators with non-expandable keys."""
        config = {"field1": {"nested_field": 1}}
        result = instantiate_generators(config, eps_config.general.members)

        assert "field1" in result, "field1 not in result"
        assert "nested_field" in result["field1"], "nested_field not in result"
        assert isinstance(
            result["field1"]["nested_field"], GeneratorType
        ), "nested_field is not a generator"

    def test_instantiate_generators_with_mixed_keys(
        self,
        eps_config: EPSConfig,
    ):
        """Test instantiate_generators with a mix of expandable and non-expandable keys."""
        config = {
            "field1": {"0:3": 1, "3:6": 2},
            "field2": {"nested_field": 1},
            "field3": [1, 2, 3],
        }
        result = instantiate_generators(config, eps_config.general.members)

        assert "field1" in result
        assert "field2" in result
        assert "field3" in result

        assert isinstance(result["field1"], GeneratorType)
        assert isinstance(result["field2"]["nested_field"], GeneratorType)
        assert isinstance(result["field3"], GeneratorType)

    def test_instantiate_generators_with_empty_config(
        self,
        eps_config: EPSConfig,
    ):
        """Test instantiate_generators with an empty config."""
        config: dict = {}
        result = instantiate_generators(config, eps_config.general.members)

        assert not result

    def test_instantiate_generators_with_invalid_generator(
        self,
        eps_config: EPSConfig,
    ):
        """Test instantiate_generators with an invalid generator string.

        Expected output is, that the generator returns the string for every
        member.
        """
        test_string = "invalid.generator.Class"
        config = {"field1": test_string}
        result = instantiate_generators(config, eps_config.general.members)

        assert "field1" in result, "field1 not in result"
        assert isinstance(result["field1"], GeneratorType), "field1 is not a generator"

        # Test that generator returns the expected value
        generated_values = [next(result["field1"]) for _ in range(10)]
        assert np.all(
            np.array(generated_values) == test_string
        ), "Generated values are not equal to the test string"


@pytest.fixture(name="none_value_generators", scope="class")
def fixture_none_value_generators() -> dict:
    """Fixture for defining test none_value_generators."""
    return {
        "field1": iter([None, None, None]),
        "field2": iter([4, 5, 6]),
    }


class TestGenerateValues:
    """Unit tests for the generate_values function."""

    @pytest.fixture(name="generators")
    def fixture_generators(self) -> dict:
        """Fixture for defining test generators."""
        return {
            "field1": iter([1, 2, 3]),
            "field2": iter([4, 5, 6]),
            "nested_field": {
                "subfield1": iter([7, 8, 9]),
                "subfield2": iter([10, 11, 12]),
            },
        }

    def test_with_flat_config(self, generators: dict):
        """Test generate_values with a flat config."""
        config = {"field1": 0, "field2": 0}
        result = generate_values(config, generators)
        assert result == {
            "field1": 1,
            "field2": 4,
        }, "Wrong value for field1 or field2 for member 0"

        result = generate_values(config, generators)
        assert result == {
            "field1": 2,
            "field2": 5,
        }, "Wrong value for field1 or field2 for member 1"

        result = generate_values(config, generators)
        assert result == {
            "field1": 3,
            "field2": 6,
        }, "Wrong value for field1 or field2 for member 2"

    def test_with_nested_config(self, generators: dict):
        """Test generate_values with a nested config."""
        config = {"nested_field": {"subfield1": 0, "subfield2": 0}}
        result = generate_values(config, generators)
        assert result == {
            "nested_field": {"subfield1": 7, "subfield2": 10}
        }, "Wrong value for nested_field for member 0"

        result = generate_values(config, generators)
        assert result == {
            "nested_field": {"subfield1": 8, "subfield2": 11}
        }, "Wrong value for nested_field for member 1"

        result = generate_values(config, generators)
        assert result == {
            "nested_field": {"subfield1": 9, "subfield2": 12}
        }, "Wrong value for nested_field for member 2"

    def test_with_mixed_config(self, generators: dict):
        """Test generate_values with a mixed config."""
        config = {"field1": 0, "nested_field": {"subfield1": 0}}
        result = generate_values(config, generators)
        assert result == {
            "field1": 1,
            "nested_field": {"subfield1": 7},
        }, "Wrong value for field1 or nested_field for member 0"

        result = generate_values(config, generators)
        assert result == {
            "field1": 2,
            "nested_field": {"subfield1": 8},
        }, "Wrong value for field1 or nested_field for member 1"

        result = generate_values(config, generators)
        assert result == {
            "field1": 3,
            "nested_field": {"subfield1": 9},
        }, "Wrong value for field1 or nested_field for member 2"

    def test_with_empty_config(self, generators: dict):
        """Test generate_values with an empty config."""
        config: dict = {}
        result = generate_values(config, generators)
        assert not result, "Result should be empty"

    def test_with_invalid_key_in_config(self, generators: dict):
        """Test generate_values with an invalid key in the config."""
        config = {"invalid_field": 0}
        with pytest.raises(KeyError):
            generate_values(config, generators)

    @pytest.mark.parametrize(
        ("expected_result", "member"),
        [
            ({"field2": 4}, 1),
            ({"field2": 5}, 2),
            ({"field2": 6}, 3),
        ],
    )
    def test_with_none_value_generator(
        self, none_value_generators, expected_result: Dict[str, int], member: int
    ):
        """Test generate_values with a generator that returns None."""
        config = {"field1": 0, "field2": 0}
        result = generate_values(config, none_value_generators)
        assert (
            result == expected_result
        ), f"Wrong value for field1 or field2 for member {member}"


class TestCheckExpandableKeys:
    """Unit tests for the check_expandable_keys function."""

    @pytest.mark.parametrize(
        ("input_mapping", "expected_output"),
        [
            ({"0": 1, "1": 2, "2": 3}, [True, True, True]),
            ({"0:3": 1, "3:6": 2, "6:": 3}, [True, True, True]),
            ({"field1": 1, "field2": 2}, [False, False]),
            ({"0": 1, "field1": 2}, [True, False]),
            ({"0:3": 1, "field1": 2}, [True, False]),
        ],
    )
    def test_check_expandable_keys(self, input_mapping, expected_output):
        """Test that check_expandable_keys correctly identifies expandable keys."""
        result = check_expandable_keys(input_mapping)
        assert result == expected_output

    def test_check_expandable_keys_empty_dict(self):
        """Test that check_expandable_keys returns an empty list for an empty dictionary."""
        result = check_expandable_keys({})
        assert result == []

    def test_check_expandable_keys_invalid_keys(self):
        """Test that check_expandable_keys raises exception on invalid keys."""
        with pytest.raises(TypeError, match=r".*must be strings.*"):
            check_expandable_keys({None: 1, 123: 2})


class TestGetExpandableKeys:
    """Unit tests for the get_expandable_keys function."""

    @staticmethod
    def depth(d):
        queue = deque([(id(d), d, 1)])
        memo = set()
        while queue:
            id_, o, level = queue.popleft()
            if id_ in memo:
                continue
            memo.add(id_)
            if isinstance(o, dict):
                queue += ((id(v), v, level + 1) for v in o.values())
        return level

    @pytest.fixture()
    def mock_check_expandable_keys(self, mocker):
        return mocker.patch("deode.eps.eps_setup.check_expandable_keys")

    def test_empty_dict(self, mock_check_expandable_keys):
        """Test that get_expandable_keys returns an empty dict for an empty dictionary."""
        mock_check_expandable_keys.return_value = []
        result = get_expandable_keys({})
        assert result == {}

    def test_no_expandable_keys(self, mock_check_expandable_keys):
        """Test that get_expandable_keys returns an empty dict when no expandable keys are found."""
        mock_check_expandable_keys.return_value = [False, False, False]
        result = get_expandable_keys({"test": [1, 2, 3]})
        assert result == {}

    def test_some_expandable_keys(self, mock_check_expandable_keys):
        """Test that get_expandable_keys returns a dict with expandable keys."""
        test_dict = {"a": {"1:4": 1, "4": 2}, "b": 3}

        def side_effect(mapping):
            # Determine the recursion level based on the depth of the mapping
            depth_ = self.depth(mapping)
            if depth_ == 2:
                return [True, True]  # Covers the '"a": {"1:4": 1, "4": 2}' part
            if depth_ == 1:
                return [False]  # Covers the '"b": 3' part
            return []

        # # Set the side effect for the mock
        mock_check_expandable_keys.side_effect = side_effect
        result = get_expandable_keys(test_dict)
        assert result == {"a": None}

    def test_recursive_dict(self, mock_check_expandable_keys):
        """Test that get_expandable_keys works recursively."""
        test_dict = {"a": {"b": {"1:4": 1, "4": 2}, "c": 2}, "d": 3}

        def side_effect(mapping):
            # Determine the recursion level based on the depth of the mapping
            depth_ = self.depth(mapping)
            if depth_ == 3:
                return [
                    False,
                    False,
                ]  # Covers the '"a": {"b": {"1:4": 1, "4": 2}, "c": 2}' part
            if depth_ == 2:
                return [True, True]  # Covers the '"b": {"1:4": 1, "4": 2}' part
            if depth_ == 1:
                return [False]  # Covers the '"d": 3' part
            return []

        mock_check_expandable_keys.side_effect = side_effect
        result = get_expandable_keys(test_dict)
        assert result == {"a": {"b": None}}


class TestGetMemberConfig:
    """Unit tests for the get_member_config function."""

    @pytest.mark.parametrize(
        ("member", "csc", "cycle"),
        [(0, "AROME", "CY49t2"), (1, "ALARO", "CY48t3"), (2, "HARMONIE_AROME", "CY46h1")],
    )
    def test_get_member_config(
        self, default_config: ParsedConfig, member: int, csc: str, cycle: str
    ):
        """Test that get_member_config correctly updates the config for each member."""
        # Define some test values based on member for uniqueness
        forecast_range_value = f"PT{member}H"
        climdir_value = f"climdir{member}"

        # Add test member settings to the default config
        default_config = default_config.copy(
            update={
                "eps": {
                    "general": {"members": [0, 1, 2]},
                    "members": {
                        str(member): {
                            "general": {
                                "csc": csc,
                                "cycle": cycle,
                                "times": {
                                    "forecast_range": forecast_range_value,
                                },
                            },
                            "system": {
                                "climdir": climdir_value,
                            },
                        }
                    },
                    "member_settings": {},
                }
            }
        )

        # Call the function under test
        updated_config = get_member_config(default_config, member)

        # Assert that the fields in the original default_config are updated
        assert updated_config["general"]["csc"] == csc
        assert updated_config["general"]["cycle"] == cycle
        assert updated_config["general"]["member"] == member
        assert (
            updated_config["general"]["times"]["forecast_range"] == forecast_range_value
        )
        assert updated_config["system"]["climdir"] == climdir_value

    @pytest.mark.parametrize(
        "member",
        [-1, 3, 100],
    )
    def test_get_member_config_invalid_member(
        self, default_config: ParsedConfig, member: int
    ):
        """Test that get_member_config raises an exception on invalid member index."""
        # Prepare the config with some eps settings
        default_config = default_config.copy(
            update={
                "eps": {
                    "general": {"members": [1]},
                    "members": {"1": {}},
                    "member_settings": {},
                }
            }
        )

        # Test invalid member indices
        with pytest.raises(ValueError, match=r".*not in the members list.*"):
            get_member_config(default_config, member)
