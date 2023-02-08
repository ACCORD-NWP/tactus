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
        super().__setattr__("__kwargs__", tuple(kwargs))

    def dict(self):  # noqa: A003 (class attribute shadowing builtin)
        """Return a dict representation of the instance and nested instances."""
        rtn = {}
        for k, v in self.__dict__.copy().items():
            if k not in self.__kwargs__:
                continue
            if isinstance(v, BasicConfig):
                rtn[k] = v.dict()
            else:
                rtn[k] = v
        return rtn

    def items(self):
        """Emulate the "items" method from the dictionary type."""
        return self.dict().items()

    def copy(self):
        """Return a deepcopy of the instance."""
        return copy.deepcopy(self)

    def get_value(self, items):
        """Recursively get the value of a config component.

        This allows us to use self.get_value("foo.bar.baz") even if "bar" is, for
        instance, a dictionary or any obj that implements a "getitem" method.

        Args:
            items (str): Attributes to be retrieved, as dot-separated strings.

        Returns:
            Any: Value of the parsed config item.
        """

        def get_attr_or_item(obj, item):
            try:
                return getattr(obj, item)
            except AttributeError as attr_error:
                try:
                    return obj[item]
                except (KeyError, TypeError) as error:
                    raise AttributeError(attr_error) from error

        return reduce(get_attr_or_item, items.split("."), self)

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

    def __repr__(self):
        return str(self.dict())

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

    def __str__(self):
        return self.dumps(style="json")


class ParsedConfig(BasicConfig):
    """Object that holds the validated configs."""

    def __init__(self, json_schema=None, **kwargs):
        """Initialise an instance with an arbitrary number of entries & validate them."""
        if json_schema is None:
            json_schema = MAIN_CONFIG_JSON_SCHEMA.copy()
        object.__setattr__(self, "json_schema", json_schema)

        try:
            super().__init__(**self.validate(kwargs))
        except JsonSchemaValueException as err:
            error_path = " -> ".join(err.path[1:])
            human_readable_msg = err.message.replace(err.name, "")
            raise ConfigFileValidationError(
                f'"{error_path}" {human_readable_msg}. '
                + f'Received {type(err.value)} value "{err.value}"'
            ) from err

    @cached_property
    def validate(self):
        """Return a validation function compiled with the instance's json schema."""
        return fastjsonschema.compile(self.json_schema)

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
        with open(config_path, "rb") as config_file:
            raw_config = tomlkit.load(config_file)

        # Add metadata about where the config was parsed from
        old_metadata = raw_config.get("metadata", {})
        new_metadata = {"source_file_path": config_path.as_posix()}
        old_metadata.update(new_metadata)
        raw_config["metadata"] = new_metadata

        return cls.parse_obj(obj=raw_config, json_schema=json_schema)


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
