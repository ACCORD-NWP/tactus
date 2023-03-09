#!/usr/bin/env python3
"""Registration and validation of options passed in the config file."""
import copy
import json
import logging
import os
from collections import defaultdict
from functools import cached_property, reduce
from operator import getitem
from pathlib import Path
from typing import Literal

import fastjsonschema
import tomlkit
import yaml
from fastjsonschema import JsonSchemaValueException

from . import PACKAGE_NAME
from .datetime_utils import ISO_8601_TIME_DURATION_REGEX

NO_DEFAULT_PROVIDED = object()

MAIN_CONFIG_JSON_SCHEMA_PATH = (
    Path(__file__).parent / "config_file_schemas" / "main_config_schema.json"
)
with open(MAIN_CONFIG_JSON_SCHEMA_PATH, "r") as schema_file:
    MAIN_CONFIG_JSON_SCHEMA = json.load(schema_file)

logger = logging.getLogger(__name__)


class ConfigFileValidationError(Exception):
    """Error to be raised when parsing the input config file fails."""


def get_default_config_path():
    """Return the default path for the config file."""
    try:
        _fpath = Path(os.getenv("DEODE_CONFIG_PATH", "config.toml"))
        default_conf_path = _fpath.resolve(strict=True)
    except FileNotFoundError:
        default_conf_path = Path(os.getenv("HOME")) / f".{PACKAGE_NAME}" / "config.toml"

    return default_conf_path


class BasicConfig:
    """Base class for configs. Arbitrary entries allowed, but no validation performed."""

    def __init__(self, **kwargs):
        """Initialise an instance with an arbitrary number of entries."""
        kwargs = _remove_none_values(kwargs)
        kwargs = _convert_lists_into_tuples(kwargs)
        kwargs = _convert_subdicts_into_model_instance(cls=BasicConfig, values=kwargs)
        for field_name, field_value in kwargs.items():
            super().__setattr__(field_name, field_value)
        super().__setattr__("__field_names__", tuple(kwargs))

    def items(self):
        """Emulate the "items" method from the dictionary type."""
        for field_name in self.__field_names__:
            yield field_name, getattr(self, field_name)

    def dict(self, descend_recursively=True):  # noqa: A003 (class attr shadowing builtin)
        """Return a dict representation of the instance and nested instances."""
        rtn = {}
        for k, v in self.items():
            if descend_recursively and isinstance(v, BasicConfig):
                rtn[k] = v.dict()
            else:
                rtn[k] = v
        return rtn

    def copy(self, update=None):
        """Return a copy of the instance.

        Args:
            update (dict): Mapping containing the fields to be updated upon copying.
                Default value = None.

        Returns:
            Any: Copy of the instance, with any values mapped from `update` updated.
        """
        if update is not None:
            return BasicConfig(**_update_nested_dict(self.dict(), update))
        return copy.deepcopy(self)

    def get_value(self, items, default=NO_DEFAULT_PROVIDED):
        """Recursively get the value of a config component.

        This allows us to use self.get_value("foo.bar.baz") even if "bar" is, for
        instance, a dictionary or any obj that implements a "getitem" method.

        Args:
            items (str): Attributes to be retrieved, as dot-separated strings.
            default (Any): Default to be returned if the attribute does not exist.

        Returns:
            Any: Value of the parsed config item.

        Raises:
            AttributeError: If the attribute does not exist and no default is provided.
        """

        def get_attr_or_item(obj, item):
            try:
                return getattr(obj, item)
            except AttributeError as attr_error:
                try:
                    return obj[item]
                except (KeyError, TypeError) as error:
                    raise AttributeError(attr_error) from error

        try:
            return reduce(get_attr_or_item, items.split("."), self)
        except AttributeError as error:
            if default is NO_DEFAULT_PROVIDED:
                raise error
            return default

    def dumps(
        self,
        section="",
        style: Literal["toml", "json", "yaml"] = "toml",
        include_metadata=False,
    ):
        """Get a nicely printed version of the models. Excludes the metadata section."""
        config = self.dict()
        if not include_metadata:
            config.pop("metadata", None)

        if section:
            section_tree = section.split(".")
            try:
                value = reduce(getitem, section_tree, config)
            except (KeyError, TypeError):
                return ""

            def _nested_defaultdict():
                return defaultdict(_nested_defaultdict)

            config = _nested_defaultdict()
            reduce(getitem, section_tree[:-1], config)[section_tree[-1]] = value

        rtn = json.dumps(config, indent=4, sort_keys=False)
        if style == "toml":
            return tomlkit.dumps(json.loads(rtn))
        if style == "yaml":
            return yaml.dump(json.loads(rtn))

        return rtn

    def __setattr__(self, key, value):
        raise TypeError(f"cannot assign to {self.__class__.__name__} objects.")

    def __getattr__(self, items):
        """Get attribute.

        Override so we can use,
        e.g., getattr(config, "general.time_windows.start.minute").

        Args:
            items (str): Attributes to be retrieved, as dot-separated strings.

        Returns:
            Any: Value of the parsed config item.
        """

        def regular_getattribute(obj, item):
            if type(obj) is type(self):
                return super().__getattribute__(item)
            return getattr(obj, item)

        return reduce(regular_getattribute, items.split("."), self)

    def __repr__(self):
        return f"{self.__class__.__name__}{self.dumps(style='json')}"

    __str__ = __repr__


class JsonSchema(dict):
    """A subclass of `dict` that has a slightly better printed representation."""

    def __repr__(self):
        return json.dumps(self, indent=4, sort_keys=False)


class ParsedConfig(BasicConfig):
    """Object that holds the validated configs."""

    def __init__(self, json_schema=None, **kwargs):
        """Initialise an instance with an arbitrary number of entries & validate them."""
        if json_schema is None:
            json_schema = MAIN_CONFIG_JSON_SCHEMA.copy()
        object.__setattr__(self, "json_schema", JsonSchema(json_schema))

        try:
            super().__init__(**self._validate(kwargs))
        except JsonSchemaValueException as err:
            error_path = " -> ".join(err.path[1:])
            human_readable_msg = err.message.replace(err.name, "").strip()

            # Give a better err msg when times/date-times/durations don't follow ISO 8601
            human_readable_msg = human_readable_msg.replace(
                f"must match pattern {ISO_8601_TIME_DURATION_REGEX}",
                "must be an ISO 8601 duration string",
            )
            for spec in ["date-time", "date", "time"]:
                human_readable_msg = human_readable_msg.replace(
                    f"must be {spec}", f"must be an ISO 8601 {spec} string"
                )

            raise ConfigFileValidationError(
                f'"{error_path}" {human_readable_msg}. '
                + f'Received type "{type(err.value).__name__}" with value "{err.value}".'
            ) from err

    @classmethod
    def parse_obj(cls, obj, json_schema=None):
        """Parse a dict object 'obj', optionally validating against a json schema."""
        return cls(json_schema=json_schema, **obj)

    @classmethod
    def from_file(cls, config_path, json_schema=None):
        """Read config file at location "config_path".

        Args:
            config_path (typing.Union[pathlib.Path, str]): The path to the config file.
            json_schema (dict): JSON schema to be used for validation.

        Returns:
            .config_parser.ParsedConfig: Parsed configs from config_path.
        """
        config_path = Path(config_path).expanduser().resolve()
        logging.info("Reading config file %s", config_path)
        raw_config = read_raw_config_file(config_path)

        # Add metadata about where the config was parsed from
        old_metadata = raw_config.get("metadata", {})
        new_metadata = {"source_file_path": config_path.as_posix()}
        old_metadata.update(new_metadata)
        raw_config["metadata"] = new_metadata

        return cls.parse_obj(obj=raw_config, json_schema=json_schema)

    def copy(self, **kwargs):
        """Return a copy of the instance. Same API as `copy` from class BasicConfig."""
        return self.__class__.parse_obj(
            super().copy(**kwargs).dict(), json_schema=self.json_schema
        )

    def __repr__(self):
        rtn = f"{self.__class__.__name__}(**{self.dumps(style='json')}, "
        rtn += f"json_schema={json.dumps(self.json_schema, indent=4, sort_keys=False)})"
        return rtn

    @cached_property
    def _validate(self):
        """Return a validation function compiled with the instance's json schema."""
        if not self.json_schema:
            # No json schema: bypassing validation
            return lambda obj: obj
        return fastjsonschema.compile(self.json_schema)


def _convert_lists_into_tuples(values):
    """Convert 'list' inputs into tuples. Helps serialisation, needed for dumps."""
    new_d = values.copy()
    for k, v in values.items():
        if isinstance(v, list):
            new_d[k] = tuple(v)
        elif isinstance(v, dict):
            new_d[k] = _convert_lists_into_tuples(v)
    return new_d


def _remove_none_values(values):
    """Recursively remove None entries from the input dict."""
    new_d = {}
    for k, v in values.items():
        if isinstance(v, dict):
            new_d[k] = _remove_none_values(v)
        elif v is not None:
            new_d[k] = v
    return new_d


def _convert_subdicts_into_model_instance(cls, values):
    """Convert nested dicts into instances of the model."""
    new_d = values.copy()
    for k, v in values.items():
        if isinstance(v, dict):
            new_d[k] = cls(**_convert_subdicts_into_model_instance(cls, v))
    return new_d


def _update_nested_dict(my_dictionary, dict_with_updates):
    """Recursively update nested dict entries according to `dict_with_updates`."""
    new_dict = my_dictionary.copy()
    for key, value in dict_with_updates.items():
        if isinstance(value, dict):
            new_dict[key] = _update_nested_dict(new_dict.get(key, {}), value)
        else:
            new_dict[key] = value
    return new_dict


def read_raw_config_file(config_path):
    """Read raw configs from files in miscellaneous formats."""
    config_path = Path(config_path)
    with open(config_path, "rb") as config_file:
        if config_path.suffix == ".toml":
            return tomlkit.load(config_file)

        if config_path.suffix == ".yaml":
            return yaml.load(config_file, Loader=yaml.loader.SafeLoader)

        if config_path.suffix == ".json":
            return json.load(config_file)

        raise NotImplementedError(
            f'Unsupported config file format "{config_path.suffix}"'
        )
