"""Exports main classes for handling EPS configurations.

Exports:
    EPSConfig: Class representing the configurations for EPS.
    Expandable: Root model for typing general expandable fields.
    EPSGeneralConfigs: Class representing the general configurations for EPS.
    generate_member_settings: Generate member settings for all members.
    instantiate_generators: Recursively instantiate generators for all
        fields in the config.
    generate_values: Recursively generate values using pre-instantiated generators.
    get_expandable_keys: Check if a mapping contains expandable keys.
    get_member_config: Get the member specific config.
"""

from collections.abc import Mapping
from itertools import chain
from typing import Any, Dict, Generator, List, Tuple

from pydantic import RootModel, field_validator
from pydantic.dataclasses import dataclass as pydantic_dataclass

from deode.config_parser import ParsedConfig
from deode.custom_validators import import_from_string
from deode.eps.custom_generators import BaseGenerator
from deode.general_utils import (
    expand_dict_key_slice,
    merge_dicts,
    modify_mappings,
    recursive_delete_keys,
    value_from_any_generator,
)
from deode.logs import logger

PrimitiveTypes = int | float | str | bool
ListTypes = List[PrimitiveTypes] | List[List[PrimitiveTypes]]


class Expandable(RootModel):
    """Root model for typing general expandable fields."""

    root: PrimitiveTypes | ListTypes | Dict[str, PrimitiveTypes | ListTypes]


@pydantic_dataclass
class EPSGeneralConfigs:
    """Class representing the general configurations for EPS.

    The infer_members field validator infers members from a members string,
    before validating the members field.

    Attributes:
        members (List[int]): The members.

    """

    members: List[int]

    @field_validator("members", mode="before")
    def infer_members(cls, value: str | int | List[int]) -> List[int]:  # noqa: N805
        """Infer members from the members string. Accepts ints or list of ints too.

        The members string can be a comma separated sequence of integers or ranges.
        The string is converted into a sorted list of integers. The list is
        filtered to contain only unique values.

        E.g. "0:3,4,5,10:15" or "0,1,2,3,4,5,10,11,12,13,14"

        In case of value being an integer, it is converted to a list of integers.
        In case of value not being an int or string, it is assumed to be a list
        of integers and returned sorted.

        Args:
            value: The members string, integer or list to infer members from.

        Returns:
            List[int]: The inferred members.
        """
        if isinstance(value, int):
            value = [value]
        elif isinstance(value, str):
            _members = [
                list(range(*map(int, x.split(":")))) if ":" in x else [int(x)]
                for x in value.split(",")
            ]
            value = list(chain.from_iterable(_members))

        return sorted(set(value))

    @property
    def n_members(self) -> int:
        """Get the number of members.

        Returns:
            The number of members.

        """
        return len(self.members)


@pydantic_dataclass
class EPSConfig:
    """Class representing the configurations for EPS.

    This is the main class for EPS configurations. It contains the general
    configurations and the member settings.

    Only the format of the member settings is validated, i.e. if the user
    has correctly specified how to expand the fields for the ensemble members.

    Attributes:
        general: The general eps configurations.
        member_settings: A dictionary containing the member-specific settings.

    """

    general: EPSGeneralConfigs
    member_settings: dict

    @field_validator("member_settings", mode="before")
    def validate_expandables(
        cls, value: Any | Dict[str, Any], previous_key: Any = None  # noqa: N805
    ):
        """Validate expected format of expandable fields."""
        # If the value is a dictionary, check if it contains expandable keys.
        if isinstance(value, dict):
            expandable_keys = check_expandable_keys(value)
            # If all keys are expandable, parse into Expandable object.
            # to validate format.
            if any(expandable_keys):
                if not all(expandable_keys):
                    raise ValueError(
                        "Keys in expandable dictionaries must all be either "
                        f"integers or string slices. Got '{previous_key}: {value}'"
                    )
                Expandable(root=value)
                return value

            # If no keys are expandable, recursively validate the nested
            # dictionaries.
            for next_key, next_value in value.items():
                value[next_key] = cls.validate_expandables(next_value, next_key)
        else:
            # If the value is not a dictionary, parse into Expandable object
            # to validate format.
            Expandable(root=value)
        return value


def generate_member_settings(
    eps_config: EPSConfig,
) -> Generator[Tuple[int, dict], None, None]:
    """Generate member settings for all members.

    The member settings are generated by iterating over the member indices
    and yielding the corresponding member settings.

    For all settings, it is possible to specify single values, list or dicts
    of values. In the latter two cases, the lists/dicts are turned into
    generators to yield values for each member from the lists/dicts.

    If a single value is specified, it is turned into a generator, that simply
    yields the value for all members.

    If a string is specified, it is tried to be imported as a BaseGenerator
    class. If the import is successful, the generator is instantiated with
    the members list and the generator is iterated over to yield values for
    each member. Otherwise, the string is used as a single value for all
    members.

    Args:
        eps_config: The EPS configuration object to be used to
            generate members from.

    Yields:
        Tuple[int, dict]: The member index and the generated member settings.
    """
    # Instantiate generators for all fields in the member settings object
    generators = instantiate_generators(
        config=eps_config.member_settings,
        members=eps_config.general.members,
    )

    # Generate member settings for all members
    for member in eps_config.general.members:
        logger.debug(f"Generating member settings for mbr{member}")
        generated_values = generate_values(
            config=eps_config.member_settings, generators=generators
        )
        # Yield the member and the generated member settings object
        yield member, generated_values


def instantiate_generators(config: dict, members: List[int]) -> dict:
    """Recursively instantiate generators for all fields in the config.

    The generators will either be a `BaseGenerator` or a
    `value_from_any_generator` generator.

    Args:
        config: The configuration object.
        members: The list of members.

    Returns:
        dict: A dictionary with instantiated generators for all fields.
            Keys are the field names and values are the generators.
    """
    # Prepare types
    field_name: str
    field_value: Any | Dict[str, Any]

    generators = {}
    # Iterate over all fields in the config
    for field_name, field_value in config.items():
        local_field_value = field_value
        # If the field is a dict, check if dict contains
        # expandable keys.
        if isinstance(local_field_value, dict):
            expandable_keys = check_expandable_keys(local_field_value)
            # If not all keys are expandable, it indicates, that we have
            # a nested dictionary. Generate generators recursively.
            if not all(expandable_keys):
                generators[field_name] = instantiate_generators(
                    config=local_field_value, members=members
                )
                # When exiting from the recursion, continue with the
                # next field to avoid overwriting the generated generators
                # below.
                continue

            # Expand key slices (e.g. dicts with "slice-key-syntax": {"3:6": val})
            # when all keys are expandable.
            local_field_value = expand_dict_key_slice(
                local_field_value,
                members,
            )
        # Check if the field_value can be imported as object
        if isinstance(local_field_value, str):
            try:
                # If the field is a BaseGenerator class, make it iterable
                # and parse members to it
                imported_object = import_from_string(local_field_value)
                if issubclass(imported_object, BaseGenerator):
                    generators[field_name] = iter(imported_object(members))
                    # When exiting from the recursion, continue with the
                    # next field to avoid overwriting the generated generators
                    # below.
                    continue
                logger.warning(
                    f"The imported object {imported_object} is not a"
                    + " BaseGenerator. Skipping member settings"
                    + f"generation for field {field_name}."
                )
            except (ImportError, ValueError):
                pass

        # Expand field to give one value per member.
        # The self.general.members list is respected, so no value
        # is generated for members not in the list.
        generators[field_name] = value_from_any_generator(
            local_field_value,
            indices=members,
        )
    return generators


def generate_values(config: dict, generators: dict) -> dict:
    """Recursively generate values using pre-instantiated generators.

    The generation of values is done for all fields in the config.

    Args:
        config: The configuration object.
        generators: A (possibly nested) dictionary of pre-instantiated generators.

    Returns:
        A dictionary with generated values for all fields.
    """
    generated_values = {}
    # Iterate over all fields in the config
    for field_name, field_value in config.items():
        # If the field is a dict, check if dict contains
        # expandable keys.
        if isinstance(field_value, dict):
            expandable_keys = check_expandable_keys(field_value)
            # If not all keys are expandable, it indicates, that we have
            # a nested dictionary. Generate values recursively.
            if not all(expandable_keys):
                generated_values[field_name] = generate_values(
                    field_value, generators[field_name]
                )
                # When exiting from the recursion, continue with the
                # next field to avoid overwriting the generated values
                # below.
                continue
        # If the field is not a dict, or if the keys of the dict
        # are all expandable, generate values for the field.
        value_ = next(generators[field_name])
        # Make sure to only add the value if it is not None (default
        # value for dict generators)
        if value_ is not None:
            generated_values[field_name] = value_

    return generated_values


def check_expandable_keys(mapping: Mapping[str, Any]) -> List[bool]:
    """Check if a mapping contains expandable keys.

    By "expandable keys" we mean either integers or string slices, where string
    slices are strings of format "start:end:step", "start:end" or "start:".

    Args:
        mapping: The mapping to check.

    Raises:
        TypeError: If the keys of the mapping are not strings.

    Returns:
        A list of booleans indicating if the keys are expandable.
    """
    current_keys = mapping.keys()
    if not all(isinstance(key, str) for key in current_keys):
        raise TypeError("Keys in mapping must be strings.")

    expandable_keys = [
        key_.isdigit() or (isinstance(key_, str) and ":" in key_) for key_ in current_keys
    ]

    return expandable_keys


def get_expandable_keys(
    mapping: Mapping[str, Any],
) -> dict[str, Any | bool]:
    """Recursively get expandable keys from a mapping.

    By "expandable keys" we mean either integers or string slices, where string
    slices are strings of format "start:end:step", "start:end" or "start:".

    Args:
        mapping: The mapping to get expandable keys from.

    Returns:
        A list of booleans indicating if the keys are expandable.
    """
    expandable_keys_dict = {}
    for mapping_key, mapping_value in mapping.items():
        if isinstance(mapping_value, dict):
            expandable_keys = check_expandable_keys(mapping_value)
            if not all(expandable_keys):
                keys = get_expandable_keys(mapping_value)
                if keys:
                    expandable_keys_dict[mapping_key] = keys
            else:
                # Only keys are relevant, so just set None value
                expandable_keys_dict[mapping_key] = None

    return expandable_keys_dict


def get_member_config(config: ParsedConfig, member: int) -> ParsedConfig:
    """Get the member specific config from a member.

    The settings for the `member` EPS member overwrites any
    existing values for a given key in the config.

    Args:
        config: The parsed config.
        member: The member index.

    Raises:
        KeyError: If the members list is not present in the config.
        ValueError: If the member is not in the members list.

    Returns:
        The new instance of the dataclass.

    """
    # Get a clean dict of the default member settings. Needed to be able
    # to merge the default settings with the specific member settings.
    default_member_settings: dict = modify_mappings(
        config["eps.member_settings"], operator=dict
    )
    # Determine which keys are expandable and delete them to avoid that a member
    # specific setting defaults to an expandable dict if no member specific
    # setting is present.
    expandable_keys = get_expandable_keys(default_member_settings)
    recursive_delete_keys(default_member_settings, expandable_keys)

    # Get a clean dict of the member settings for the specific member.
    specific_member_settings = {}
    member_key = f"eps.members.{member}"

    # Check that the members list is present
    if "eps.general.members" not in config:
        raise KeyError(
            "The members list 'eps.general.members' is not present in the config."
            + " Cannot get member settings."
        )

    # Check if the member is in the members list
    if member not in config["eps.general.members"]:
        raise ValueError(
            f"Member index {member} is not in the members list."
            + " Cannot get member settings."
        )

    # Check if there are specific settings for the member
    if member_key in config:
        specific_member_settings = modify_mappings(config[member_key], operator=dict)
    else:
        logger.debug(f"No settings found for member {member}. Using defaults.")

    # Merge the default member settings with the specific member settings
    merged_settings = merge_dicts(
        default_member_settings, specific_member_settings, overwrite=True
    )
    # Update the member number in the general settings
    if "general" not in merged_settings:
        merged_settings["general"] = {}
    merged_settings["general"]["member"] = member
    merged_settings["general"]["member_str"] = f"mbr{member:03d}"

    # Return the config updated with member settings
    return config.copy(update={**merged_settings})
