"""Module with tests for the general_utils.py module."""

from unittest.mock import Mock, patch

import pytest

from tactus.general_utils import (
    expand_dict_key_slice,
    merge_dicts,
    recursive_delete_keys,
    recursive_dict_deviation,
    value_from_any_generator,
    value_from_mapping_generator,
    value_from_sequence_generator,
)


class TestMergeDicts:
    """Unit tests for the merge_dicts function."""

    def test_merge_dict(self):
        """Test merge of two dictionaries."""
        dict1 = {0: [0, 1, 2], 1: {0: [0]}, 2: 0}
        dict2 = {0: [3, 4], 1: {0: [1], 1: 0}, 3: [0, 1]}
        dict_ref = {0: [0, 1, 2, 3, 4], 1: {0: [0, 1], 1: 0}, 2: 0, 3: [0, 1]}
        dict_merged = merge_dicts(dict1, dict2)
        assert dict_ref == dict_merged

    def test_invalid_no_overwrite_merge_dict(self):
        """Test merge of two dictionaries."""
        dict1 = {0: {0: 0}}
        dict2 = {0: "x"}
        with pytest.raises(RuntimeError):
            merge_dicts(dict1, dict2)

    def test_merge_dict_overwrite(self):
        """Test merge of two dictionaries."""
        dict1 = {0: {0: 0}}
        dict2 = {0: "x"}
        dict_ref = {0: "x"}
        dict_merged = merge_dicts(dict1, dict2, overwrite=True)
        assert dict_ref == dict_merged


class TestValueFromAnyGenerator:
    """Unit tests for the value_from_any_generator function."""

    def test_with_single_value(self):
        """Test value_from_any_generator with a single value."""
        value = "single_value"
        generator = value_from_any_generator(value, indices=[])
        assert next(generator) == "single_value"
        assert next(generator) == "single_value"  # Always returns the same value

    @patch("tactus.general_utils.value_from_sequence_generator")
    def test_call_when_sequence(self, mock_value_from_sequence: Mock):
        """Test that value_from_sequence is called when input is a sequence."""
        sequence = [1, 2, 3]

        generator = value_from_any_generator(sequence, indices=[])
        next(generator)

        mock_value_from_sequence.assert_called_once_with(sequence)


class TestValueFromSequence:
    """Unit tests for the value_from_sequence_generator function."""

    def test_with_expected_input(self):
        """Test with expected input parameters."""
        sequence = [1, 2, 3]
        generator = value_from_sequence_generator(sequence)
        assert next(generator) == 1
        assert next(generator) == 2
        assert next(generator) == 3
        assert next(generator) == 1  # Cycle repeats

    def test_with_empty_sequence(self):
        """Test with an empty sequence."""
        sequence = []
        generator = value_from_sequence_generator(sequence)
        with pytest.raises(StopIteration):
            next(generator)

    def test_combining_types(self):
        """Test sequence with different types."""
        sequence = [1, "a", 3.0]
        generator = value_from_sequence_generator(sequence)
        assert next(generator) == 1
        assert next(generator) == "a"
        assert next(generator) == 3.0
        assert next(generator) == 1


class TestValueFromMapping:
    """Unit tests for the value_from_mapping_generator function."""

    def test_with_expected_input(self):
        """Test expected input, and that StopIteration is raised."""
        mapping = {0: "a", 1: "b", 2: "c"}
        indices = [0, 1, 2]
        generator = value_from_mapping_generator(mapping, indices, default_value="x")
        assert next(generator) == "a"
        assert next(generator) == "b"
        assert next(generator) == "c"

        with pytest.raises(StopIteration):
            next(generator)

    def test_with_default_value(self):
        """Test that default value is used."""
        mapping = {0: "a"}
        indices = [0, 1]
        generator = value_from_mapping_generator(mapping, indices, default_value="x")
        assert next(generator) == "a"
        assert next(generator) == "x"  # Default value for missing key

    def test_with_empty_mapping(self):
        """Test that default value is used with an empty mapping."""
        mapping = {}
        indices = [0]
        generator = value_from_mapping_generator(mapping, indices, default_value="x")
        assert next(generator) == "x"  # Default value for missing key

    def test_combining_types(self):
        """Test with different types in mapping."""
        mapping = {0: 1, 1: "a", 2: 3.0}
        indices = [0, 1, 2]
        generator = value_from_mapping_generator(mapping, indices, default_value="x")
        assert next(generator) == 1
        assert next(generator) == "a"
        assert next(generator) == 3.0


class TestExpandDictKeySlice:
    """Unit tests for the expand_dict_key_slice function."""

    def test_with_expected_input(self):
        """Test with expected input."""
        dict_ = {"0:3": "a", "3:7:2": "b", "6:": "c"}
        indices = [0, 1, 2, 3, 5, 6, 7]
        result = expand_dict_key_slice(dict_, indices)
        expected_result = {0: "a", 1: "a", 2: "a", 3: "b", 5: "b", 6: "c", 7: "c"}
        assert result == expected_result

    def test_with_non_slice_keys(self):
        """Test with non-slice keys."""
        dict_ = {0: "a", 1: "b", 2: "c"}
        indices = [0, 1, 2]
        result = expand_dict_key_slice(dict_, indices)
        expected_result = {0: "a", 1: "b", 2: "c"}
        assert result == expected_result

    def test_with_mixed_keys(self):
        """Test with a mix of slice and non-slice keys."""
        dict_ = {":2": "a", 2: "b", "3:5": "c"}
        indices = [0, 1, 2, 3, 4]
        result = expand_dict_key_slice(dict_, indices)
        expected_result = {0: "a", 1: "a", 2: "b", 3: "c", 4: "c"}
        assert result == expected_result

    def test_with_empty_dict(self):
        """Test with an empty dictionary."""
        dict_ = {}
        indices = [0, 1, 2]
        result = expand_dict_key_slice(dict_, indices)
        expected_result = {}
        assert result == expected_result

    def test_with_empty_indices(self):
        """Test with empty indices."""
        dict_ = {"0:3": "a", "3:6": "b"}
        indices = []
        result = expand_dict_key_slice(dict_, indices)
        expected_result = {}
        assert result == expected_result

    def test_with_invalid_slice(self):
        """Test with an invalid slice."""
        dict_ = {"invalid_slice": "a"}
        indices = [0, 1, 2]
        with pytest.raises(ValueError, match=r".*could not be converted to int.*"):
            expand_dict_key_slice(dict_, indices)

    def test_with_no_start(self):
        """Test with no start in slice."""
        dict_ = {":2": "a"}
        indices = [0, 1]
        result = expand_dict_key_slice(dict_, indices)
        expected_result = {0: "a", 1: "a"}
        assert result == expected_result


class TestRecursiveDictDeviation:
    """Unit tests for the recursive_dict_deviation function."""

    def test_with_expected_input(self):
        """Test with same keys in base and deviating dicts."""
        base_dict = {"a": 1, "b": 2, "e": 4}
        deviating_dict = {"a": 1, "b": 3, "e": 6}
        result = recursive_dict_deviation(base_dict, deviating_dict)
        expected_result = {"b": 3, "e": 6}
        assert result == expected_result

    def test_with_nested_dicts(self):
        """Test with deeply nested dictionaries."""
        base_dict = {"a": 1, "b": {"c": 2, "d": {"e": 3}}}
        deviating_dict = {"a": 1, "b": {"c": 2, "d": {"e": 4}}}
        result = recursive_dict_deviation(base_dict, deviating_dict)
        expected_result = {"b": {"d": {"e": 4}}}
        assert result == expected_result

    def test_with_empty_dicts(self):
        """Test with empty dictionaries."""
        base_dict = {}
        deviating_dict = {}
        result = recursive_dict_deviation(base_dict, deviating_dict)
        expected_result = {}
        assert result == expected_result

    def test_with_partial_overlap(self):
        """Test with partial overlap of keys."""
        base_dict = {"a": 1, "b": {"c": 2}}
        deviating_dict = {"a": 1, "b": {"c": 3, "d": 4}}
        result = recursive_dict_deviation(base_dict, deviating_dict)
        expected_result = {"b": {"c": 3, "d": 4}}
        assert result == expected_result

    def test_with_different_types(self):
        """Test with different types for the same key."""
        base_dict = {"a": 1, "b": {"c": 2}}
        deviating_dict = {"a": 1, "b": {"c": "different_type"}}
        result = recursive_dict_deviation(base_dict, deviating_dict)
        expected_result = {"b": {"c": "different_type"}}
        assert result == expected_result


class TestRecursiveDeleteKeys:
    """Unit tests for the recursive_delete_keys function."""

    def test_with_empty_dicts(self):
        """Test with empty dictionaries."""
        mapping = {}
        keys_dict = {}
        expected_result = {}
        recursive_delete_keys(mapping, keys_dict)
        assert mapping == expected_result

    def test_with_no_keys_to_delete(self):
        """Test with no keys to delete."""
        mapping = {"a": 1, "b": 2}
        keys_dict = {}
        expected_result = mapping
        recursive_delete_keys(mapping, keys_dict)
        assert mapping == expected_result

    def test_with_single_key_to_delete(self):
        """Test with a single key to delete."""
        mapping = {"a": 1, "b": 2}
        keys_dict = {"a": None}
        expected_result = {"b": 2}
        recursive_delete_keys(mapping, keys_dict)
        assert mapping == expected_result

    def test_with_nested_dicts(self):
        """Test with nested dictionaries."""
        mapping = {"a": 1, "b": {"c": 2, "d": 3}}
        keys_dict = {"b": {"c": None}}
        expected_result = {"a": 1, "b": {"d": 3}}
        recursive_delete_keys(mapping, keys_dict)
        assert mapping == expected_result

    def test_empty_dict_deletion(self):
        """Test with recursive delete."""
        mapping = {"a": 1, "b": {"c": 2, "d": {"e": 3}}}
        keys_dict = {"b": {"d": {"e": None}}}
        expected_result = {"a": 1, "b": {"c": 2}}
        recursive_delete_keys(mapping, keys_dict)
        assert mapping == expected_result
