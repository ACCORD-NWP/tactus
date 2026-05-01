#!/usr/bin/env python3
"""General utils for use throughout the package."""

import copy
from collections import defaultdict
from collections.abc import Mapping, Sequence
from typing import Any, Dict, Generator, List, Optional, Tuple, Union, cast

from frozendict import frozendict
from loguru import logger


def get_empty_nested_defaultdict():
    """Return an empty nested (recursive) defaultdict object."""
    return defaultdict(get_empty_nested_defaultdict)


def recursive_dict_deviation(base_dict: dict, deviating_dict: dict) -> dict:
    """Calculate the (recursive) difference between two dicts.

    Args:
        base_dict: The base dictionary to calculate deviation from.
        deviating_dict: The dict to calculate the deviation of w.r.t to
            the base_dict

    Returns:
        The deviation as a dictionary.

    """
    deviation = {}
    for key, value in deviating_dict.items():
        if isinstance(value, dict):
            # If the value is a dict, and the key exists in the base_dict, recurse
            if key in base_dict and isinstance(base_dict[key], dict):
                deviation[key] = recursive_dict_deviation(base_dict[key], value)
                # Check if the deviation is empty and delete it if it is
                if not deviation[key]:
                    del deviation[key]
            # If the key does not exist in the base_dict, add the whole dict
            else:
                deviation[key] = value
        # If value is not dict, we have reached the end of the current branch
        # of deviating_dict. Update deviation if the value is different from
        # the base_dict value, or if the key does not exist in base_dict.
        elif (key in base_dict and base_dict[key] != value) or key not in base_dict:
            deviation[key] = value

    return deviation


def value_from_sequence_generator(
    sequence: Sequence[Any],
) -> Generator[Any, None, None]:
    """Yield alternately one of the values from a sequence of values.

    The order of the yielded values is determined by the order of the sequence.

    Args:
        sequence: The sequence to yield values from.

    Yields:
        One of the values from sequence in alternate order.

    """
    index = 0
    len_sequence = len(sequence)

    if len_sequence:
        while True:
            yield sequence[index]
            index = (index + 1) % len_sequence
    return


def value_from_mapping_generator(
    mappable: Mapping[int, Any], keys: List[int], default_value: Any
) -> Generator[Any, None, None]:
    """Yield values from a dictionary according to keys.

    Args:
        mappable: The mappable to yield values from
        keys: The keys for which to retrieve corresponding values from the dictionary.
        default_value: The default value to use if a key is not found.

    Yields:
        The value corresponding to the key.

    """
    for key in keys:
        yield mappable.get(key, default_value)


def value_from_any_generator(
    any_: Union[Any, Sequence[Any], Mapping[int, Any]],
    indices: List[int],
    default_value: Optional[str] = None,
) -> Generator[str, None, None]:
    """Yield values from any type.

    Args:
        any_: The input object to yield values from.
        indices: The indices to retrieve from the value.
        default_value: The default value to use if an index is not found in Mapping.

    Yields:
        The value from the input object.

    """
    if isinstance(any_, (Tuple, List)):
        yield from value_from_sequence_generator(any_)
    elif isinstance(any_, Mapping):
        yield from value_from_mapping_generator(any_, indices, default_value)
    while True:
        yield any_


def expand_string_slice(
    string: int | str, indices: List[int]
) -> Generator[int | List[int], None, None]:
    """Expand a string slice into a list of integers returned as generator.

    Args:
        string (int | str): The string to expand
        indices (List[int]): Indices to respect, i.e. for max/min bounds

    Yields:
        Generator[int | List[int], None, None]: The expanded string returned as
            a generator.

    Raises:
        ValueError: If string, that is not a slice string, cannot be converted
            to int
    """
    # Check if key is a slice
    if ":" in str(string):
        # Parse slice
        start, *args = (int(x) if x else None for x in str(string).split(":"))
        stop, step = args if len(args) == 2 else (args[0], None)

        # Set bounds of start/stop if indices is not empty
        if len(indices) > 0:
            # If start is None, set it to min index (permits strings like ":5")
            start = cast(int, start or min(indices))
            # If stop is None, set it to max index (permits strings like "5:")
            # +1 to include the last index
            stop = stop or max(indices) + 1
        else:
            logger.debug(
                "Indices is empty, cannot set bounds of slice strings."
                + " Return from generator"
            )
            return

        # Make type checker understand that now start and stop are not None
        start = cast(int, start)
        stop = cast(int, stop)

        # Iterate over the expanded strings and yield them together with the value
        for string_expanded in range(start, stop, step or 1):
            yield string_expanded
    else:
        # Return string as int, and value as is if string is not a slice
        try:
            yield int(string)
        except ValueError as exc:
            raise ValueError(
                f"string '{string}' could not be converted to int. "
                "If string is not string slice, it should be convertible"
                " to int."
            ) from exc


def expand_dict_key_slice(
    dict_: Dict[Union[int, str], Any], indices: List[int]
) -> Dict[int, Any]:
    """Expand key slices of a Dict.

    Handles slices in the form of "start:stop:step", expands them to
    individual keys, and assigns the original value to all individual keys.
    Keys are converted to integers.

    Any of the start, stop and step can be ommited. If start is ommited, it is
    set to the minimum value of indices. If stop is ommited, it is set to the
    maximum value of indices. If step is ommited, it is set to 1.

    Args:
        dict_: The dict, which keys shall be expanded.
        indices: The indices to respect when expanding, i.e. if expanded index is
            not in indices, it will not be added to the new dict.

    Returns:
        dict: New dict with expanded keys.
    """

    def generate_key_value_pairs() -> Generator[Tuple[int, Any], None, None]:
        for key, value in dict_.items():
            for expanded_key in expand_string_slice(key, indices):
                yield expanded_key, value

    return {key: value for key, value in generate_key_value_pairs() if key in indices}


def merge_dicts(
    dict1: dict, dict2: dict, overwrite: bool = False, remove_none: bool = False
) -> dict:
    """Merge two dictionaries with values from dict2 taking precedence.

    If values are lists, they are concatenated.

    Args:
        dict1 (dict): Reference dict
        dict2 (dict): Update dict
        overwrite (bool): Whether to overwrite values in dict1 with values from dict2
                        if the keys are the same, but the types of the values
                        are not lists or dicts.
        remove_none(bool): Whether to delete value in dict1 when
                          the value in dict2 is None
    Returns:
        (dict): Merged dict

    Raises:
        RuntimeError: Invalid type

    """
    new_dict = dict(dict1.items())

    for key2, val2 in dict2.items():
        key2_exist = key2 in new_dict
        if val2 is None:
            if key2_exist:
                if remove_none:
                    new_dict.pop(key2)
                else:
                    continue
        else:
            if not key2_exist:
                new_dict[key2] = {}
            if isinstance(val2, dict):
                new_dict[key2] = merge_dicts(
                    new_dict[key2], val2, overwrite=overwrite, remove_none=remove_none
                )
            elif isinstance(val2, list):
                if isinstance(new_dict[key2], list):
                    new_dict[key2].extend([
                        val
                        for val in val2
                        if val not in new_dict[key2] and val is not None
                    ])
                else:
                    new_dict[key2] = val2
            elif overwrite or not key2_exist:
                new_dict[key2] = val2
            else:
                raise RuntimeError("Invalid type:", type(val2), val2)
    return new_dict


def recursive_delete_keys(mapping: Dict[str, Any], keys_dict: Dict[str, bool]):
    """Recursively delete keys from a mapping based on a dictionary of keys to delete.

    Keys are deleted in-place on the `mapping` object.

    Args:
        mapping: The mapping to delete keys from.
        keys_dict: The dictionary of keys to delete.
    """
    for key, value in keys_dict.items():
        if key in mapping:
            if isinstance(value, dict):
                recursive_delete_keys(mapping[key], value)
                # Make sure to delete empty dicts
                if not mapping[key]:
                    del mapping[key]
            else:
                del mapping[key]


def recursive_unfreeze(obj, return_type=dict):
    """Transform recursively a frozendict into a type that is mutable (e.g. dict).

    Args:
        obj: the input dict
        return_type: the type to return (default = dict)

    Returns:
        The frozendict converted to return_type
    """
    if hasattr(obj, "items"):
        new_dict = dict(obj)

        for key, value in obj.items():
            new_obj = recursive_unfreeze(value, return_type)
            new_dict[key] = new_obj

        if return_type is dict:
            return new_dict
        return return_type(new_dict)

    return obj


def recursive_freeze(obj):
    """Transform recursively a dict into a immutable frozendict.

    Args:
        obj: the input dict
    Returns:
        The converted frozendict
    """
    if type(obj) is frozendict:
        return copy.deepcopy(obj)

    if hasattr(obj, "items"):
        is_dict = type(obj) is dict
        if is_dict:
            new_dict = dict(obj)
        for key, value in obj.items():
            new_obj = recursive_freeze(value)
            if is_dict:
                new_dict[key] = new_obj

        if is_dict:
            return frozendict(new_dict)

    return obj


def recursive_substitute(value, platform, pos: Optional[List[str]] = None):
    """Recursively substitute variables in a nested dictionary.

    Substitution is mainly done on value-level, but full configuration subtrees can be
    copied using the magic keys COPY (copies only all the values at the specified key)
    and COPYALL (copies all the values at the specified key, nestedly).

    Args:
        value: Value to substitute macros
        platform: The platform used to substitute values
        pos: current path-like position in the config tree (as used for COPYALL)

    Returns:
        subsituted value
    """
    key_basic_copy = "COPY"
    key_deep_copy = "COPYALL"

    if pos is None:
        pos = []

    if isinstance(value, dict):
        do_basic_copy = key_basic_copy in value
        do_deep_copy = key_deep_copy in value

        if do_deep_copy or do_basic_copy:
            key = ".".join(pos)
            config_dict = recursive_unfreeze(platform.get_value(key))

            if do_deep_copy:
                logger.info(f"Found .{key_deep_copy} key at {key}; deep copy data.")
                value.pop(key_deep_copy)
            elif do_basic_copy:
                logger.info(f"Found .{key_basic_copy} key at {key}; copy data.")
                value.pop(key_basic_copy)
                config_dict = {
                    k: v for k, v in config_dict.items() if not isinstance(v, dict)
                }

            base_value = recursive_substitute(config_dict, platform)

        else:
            base_value = {}

        if base_value:
            value = merge_dicts(base_value, value)

        for key, val in value.items():
            value[key] = recursive_substitute(val, platform, pos=[*pos, key])
    else:
        for type_name in [tuple, list]:
            if isinstance(value, type_name):
                value = type_name(map(platform.substitute, value))

        value = platform.substitute(value)

    return value
