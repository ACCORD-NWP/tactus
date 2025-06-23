#!/usr/bin/env python3
"""General utils for use throughout the package."""
import copy
from collections import defaultdict
from collections.abc import Mapping, Sequence
from typing import Any, Callable, Dict, Generator, List, Optional, Tuple, Union, cast

from loguru import logger


def get_empty_nested_defaultdict():
    """Return an empty nested (recursive) defaultdict object."""
    return defaultdict(get_empty_nested_defaultdict)


def modify_mappings(obj: Mapping, operator: Union[Mapping, Callable[[Mapping], Any]]):
    """Descend recursively into `obj` and modify encountered mappings using `operator`."""
    if not isinstance(obj, Mapping):
        raise TypeError("`obj` must be a Mapping (`dict`-like object).")

    if callable(operator):
        return _modify_mappings_via_callable(obj=obj, operator=operator)

    if isinstance(operator, Mapping):
        return _update_mapping(obj=obj, updates=operator)

    raise TypeError("`operator` must either be callable or implement an `items` method.")


def _modify_mappings_via_callable(obj, operator: Callable[[Mapping], Any]):
    """Descend recursively into `obj` and modify encountered mappings using `operator`."""
    if not isinstance(obj, Mapping):
        try:
            return copy.deepcopy(obj)
        except TypeError:
            return obj
    return operator(
        {k: _modify_mappings_via_callable(v, operator=operator) for k, v in obj.items()}
    )


def _update_mapping(obj, updates: Mapping):
    """Descend recursively into `obj` and update nested mappings using `updates`."""
    new_obj = copy.deepcopy(obj)

    if not isinstance(new_obj, Mapping):
        return new_obj

    for key, updated_value in updates.items():
        if isinstance(updated_value, Mapping):
            new_obj[key] = _update_mapping(new_obj.get(key, {}), updated_value)
        else:
            new_obj[key] = updated_value
    return new_obj


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
        elif key in base_dict and base_dict[key] != value or key not in base_dict:
            deviation[key] = value

    return deviation


def value_from_sequence_generator(sequence: Sequence[Any]) -> Generator[Any, None, None]:
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
    expanded_dict = {}

    def generate_key_value_pairs() -> Generator[Tuple[int, Any], None, None]:
        for key, value in dict_.items():
            # Check if key is a slice
            if ":" in str(key):
                # Parse slice
                start, *args = (int(x) if x else None for x in str(key).split(":"))
                stop, step = args if len(args) == 2 else (args[0], None)

                # Set bounds of start/stop if indices is not empty
                if len(indices) > 0:
                    # If start is None, set it to min index (permits keys like ":5")
                    start = cast(int, start or min(indices))
                    # If stop is None, set it to max index (permits keys like "5:")
                    # +1 to include the last index
                    stop = stop or max(indices) + 1
                else:
                    logger.debug(
                        "Indices is empty, cannot set bounds of slice keys."
                        + " Return from generator"
                    )
                    return

                # Make type checker understand that now start and stop are not None
                start = cast(int, start)
                stop = cast(int, stop)

                # Iterate over the expanded keys and yield them together with the value
                for key_expanded in range(start, stop, step or 1):
                    yield key_expanded, value
            else:
                # Return key as int, and value as is if key is not a slice
                try:
                    yield int(key), value
                except ValueError as exc:
                    raise ValueError(
                        f"Key '{key}' could not be converted to int. "
                        "If key is not string slice, it should be convertible"
                        " to int."
                    ) from exc

    for key, value in generate_key_value_pairs():
        if key in indices:
            expanded_dict[key] = value

    return expanded_dict


def merge_dicts(dict1: dict, dict2: dict, overwrite: bool = False) -> dict:
    """Merge two dictionaries with values from dict2 taking precedence.

    If values are lists, they are concatenated.

    Args:
        dict1 (dict): Reference dict
        dict2 (dict): Update dict
        overwrite (bool): Whether to overwrite values in dict1 with values from dict2
                        if the keys are the same, but the types of the values
                        are not lists or dicts.

    Returns:
        (dict): Merged dict

    Raises:
        RuntimeError: Invalid type

    """
    new_dict = copy.deepcopy(dict1)
    for key2, val2 in dict2.items():
        if key2 in new_dict:
            if val2 is None:
                continue
            if isinstance(val2, dict):
                new_dict[key2] = merge_dicts(new_dict[key2], val2, overwrite=overwrite)
            elif isinstance(val2, list):
                if isinstance(new_dict[key2], list):
                    new_dict[key2].extend(
                        [val for val in val2 if val not in new_dict[key2]]
                    )
                else:
                    new_dict[key2] = val2
            elif overwrite:
                new_dict[key2] = val2
            else:
                raise RuntimeError("Invalid type:", type(val2), val2)
        else:
            new_dict[key2] = val2

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
