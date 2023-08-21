#!/usr/bin/env python3
"""Registration and validation of options passed in the config file."""
import copy
import json
import logging
import os
from collections.abc import Mapping
from functools import reduce
from operator import getitem
from pathlib import Path
from types import MappingProxyType
from typing import Any, Callable, Iterator, Literal, Union

import fastjsonschema
import tomlkit
import yaml
from fastjsonschema import JsonSchemaValueException

from . import PACKAGE_DIRECTORY
from .datetime_utils import ISO_8601_TIME_DURATION_REGEX
from .general_utils import get_empty_nested_defaultdict, modify_mappings


def _get_main_config_schema():
    with open(MAIN_CONFIG_JSON_SCHEMA_PATH, "r") as schema_file:
        return modify_mappings(obj=json.load(schema_file), operator=MappingProxyType)


PACKAGE_CONFIG_DIR = PACKAGE_DIRECTORY / "data" / "config_files"
PACKAGE_CONFIG_PATH = PACKAGE_CONFIG_DIR / "config.toml"
PACKAGE_CONFIG_INCLUDE_DIR = PACKAGE_CONFIG_DIR / "include"
CONFIG_SCHEMAS_DIR = PACKAGE_CONFIG_DIR / "config_file_schemas"
MAIN_CONFIG_JSON_SCHEMA_PATH = CONFIG_SCHEMAS_DIR / "main_config_schema.json"
MAIN_CONFIG_JSON_SCHEMA = _get_main_config_schema()

logger = logging.getLogger(__name__)


class ConfigFileValidationError(Exception):
    """Error to be raised when parsing the input config file fails."""


class ConflictingValidationSchemasError(Exception):
    """Error to be raised when more than one schema is defined for a config section."""


def get_default_config_path():
    """Return the default path to the adopted config file."""
    try:
        return Path(os.getenv("DEODE_CONFIG_PATH", "config.toml")).resolve(strict=True)
    except FileNotFoundError:
        return Path(PACKAGE_CONFIG_PATH).resolve(strict=True)


class BaseMapping(Mapping):
    """Immutable mapping that will serve as basis for all config-related classes."""

    def __init__(self, *args, **kwargs) -> None:
        """Initialise an instance the same way a `dict` is initialised."""
        self.data = dict(*args, **kwargs)

    @property
    def data(self):
        """Return the underlying data stored by the instance."""
        return getattr(self, "_data", None)

    @data.setter
    def data(self, new, nested_maps_type=dict):
        """Set the value of the `data` property."""
        self._data = modify_mappings(
            obj=new,
            operator=lambda x: {
                k: nested_maps_type(v) if isinstance(v, Mapping) else v
                for k, v in x.items()
            },
        )

    def dict(self):  # noqa: A003 (class attr shadowing builtin)
        """Return a `dict` representation, converting also nested `Mapping`-type items."""
        return modify_mappings(obj=self, operator=dict)

    def copy(self, update: Union[Mapping, Callable[[Mapping], Any]] = None):
        """Return a copy of the instance, optionally updated according to `update`."""
        new = copy.deepcopy(self)
        if update:
            new.data = modify_mappings(obj=self.dict(), operator=update)
        return new

    def dumps(self, section="", style: Literal["toml", "json", "yaml"] = "toml"):
        """Get a nicely printed version of the container's contents."""
        if section:
            section_tree = section.split(".")
            config = get_empty_nested_defaultdict()
            reduce(getitem, section_tree[:-1], config)[section_tree[-1]] = self[section]
        else:
            config = self

        rtn = json.dumps(config, indent=2, sort_keys=False, default=dict)
        if style == "toml":
            rtn = tomlkit.dumps(json.loads(rtn))
        elif style == "yaml":
            rtn = yaml.dump(json.loads(rtn))

        return rtn

    def __repr__(self):
        return f"{self.__class__.__name__}({self.dumps(style='json')})"

    # Implement the abstract methods __getitem__, __iter__ and __len__ from from Mapping
    def __getitem__(self, item):
        """Get items from container.

        The behaviour is similar to a `dict`, except for the fact that
        `self["A.B.C.D. ..."]` will behave like `self["A"]["B"]["C"]["D"][...]`.

        Args:
            item (str): Item to be retrieved. Use dot-separated keys to retrieve a nested
                item in one go.

        Returns:
            Any: Value of the item.
        """
        return reduce(getitem, item.split("."), self.data)

    def __iter__(self) -> Iterator:
        return iter(self.data)

    def __len__(self) -> int:
        return len(self.data)


class BasicConfig(BaseMapping):
    """Base class for configs. Arbitrary entries allowed: no validation is performed."""

    def __init__(self, *args, _metadata=None, **kwargs):
        """Initialise an instance in a `dict`-like fashion."""
        super().__init__(*args, **kwargs)
        self.metadata = _metadata

    @classmethod
    def from_file(cls, path, **kwargs):
        """Retrieve configs from a file in miscellaneous formats.

        Args:
            path (typing.Union[pathlib.Path, str]): Path to the config file.
            kwargs: Arguments passed to the class constructor.

        Returns:
            cls: Configs retrieved from the specified path.
        """
        path = Path(path).resolve().as_posix()
        configs = _read_raw_config_file(path)
        return cls(configs, _metadata={"source_file_path": path}, **kwargs)

    @BaseMapping.data.setter
    def data(self, new):
        """Set the underlying data stored by the instance."""
        input_sanitation_ops = [
            lambda x: {k: v for k, v in x.items() if v is not None},
            lambda x: {k: tuple(v) if isinstance(v, list) else v for k, v in x.items()},
        ]
        new = reduce(modify_mappings, input_sanitation_ops, new)
        BaseMapping.data.fset(self, new, nested_maps_type=BasicConfig)

    @property
    def metadata(self):
        """Get the metadata associated with the instance."""
        return getattr(self, "_metadata", {})

    @metadata.setter
    def metadata(self, new):
        """Set the metadata associated with the instance."""
        if new is not None:
            self._metadata = modify_mappings(obj=new, operator=dict)


class JsonSchema(BaseMapping):
    """Class to use for JSON schemas. Provides a `validate` method to validate data."""

    @property
    def _validation_function(self):
        return _get_json_validation_function(self)

    def validate(self, data):
        """Return a copy of `data` validated against the stored JSON schema."""
        return self._validation_function(data)


class ParsedConfig(BasicConfig):
    """Object that holds parsed configs validated against a `json_schema`."""

    def __init__(self, *args, json_schema, include_dir=PACKAGE_CONFIG_DIR, **kwargs):
        """Initialise an instance with an arbitrary number of entries & validate them."""
        self.include_dir = include_dir
        self.json_schema = json_schema
        super().__init__(*args, **kwargs)

    @BasicConfig.data.setter
    def data(self, new):
        """Set the underlying data stored by the instance."""
        new, json_schema = _expand_config_include_section(
            raw_config=new,
            json_schema=self.json_schema,
            config_include_search_dir=self.include_dir,
        )
        ParsedConfig.json_schema.fset(self, json_schema, _validate_data=False)
        BasicConfig.data.fset(self, self.json_schema.validate(new))

    @property
    def include_dir(self):
        """Return the search dir used sections in the raw config's `include` section."""
        return self._include_dir

    @include_dir.setter
    def include_dir(self, new):
        """Set the search dir for `include` config sections."""
        self._include_dir = Path(new)

    @property
    def json_schema(self):
        """Return the instance's JSON schema."""
        return self._json_schema

    @json_schema.setter
    def json_schema(self, new, _validate_data=True):
        self._json_schema = JsonSchema(new)
        if _validate_data and self.data is not None:
            self.data = self.data

    @classmethod
    def from_file(cls, path, include_dir=None, **kwargs):
        """Do as in `BasicConfig`. If `None`, `include_dir` will become `path.parent`."""
        if include_dir is None:
            include_dir = Path(path).parent
        return super().from_file(path=path, include_dir=include_dir, **kwargs)

    def __repr__(self):
        rtn = super().__repr__().strip(")")
        rtn += f", json_schema={self.json_schema.dumps(style='json')})"
        return rtn


def _read_raw_config_file(config_path):
    """Read raw configs from files in miscellaneous formats."""
    config_path = Path(config_path)
    logger.info("Reading configs from file <%s>", config_path)
    with open(config_path, "rb") as config_file:
        if config_path.suffix == ".toml":
            return tomlkit.load(config_file)

        if config_path.suffix in [".yaml", ".yml"]:
            return yaml.load(config_file, Loader=yaml.loader.SafeLoader)

        if config_path.suffix == ".json":
            return json.load(config_file)

        raise NotImplementedError(
            f'Unsupported config file format "{config_path.suffix}"'
        )


def _get_config_include_definitions(raw_config):
    config_includes = raw_config.get("include", {}).copy()
    overlapping_sections = [key for key in config_includes if key in raw_config]
    if overlapping_sections:
        msg = f"`[include]` section(s) [{', '.join(overlapping_sections)}] "
        msg += "already present in parent config."
        raise ValueError(msg)
    return config_includes


def _expand_config_include_section(
    raw_config,
    json_schema,
    config_include_search_dir=PACKAGE_CONFIG_DIR,
    schemas_path=CONFIG_SCHEMAS_DIR,
    _parent_sections=(),
):
    """Merge config includes and return new config & corresponding validation schema."""
    raw_config = modify_mappings(obj=raw_config, operator=dict)
    json_schema = modify_mappings(obj=json_schema, operator=dict)

    config_include_defs = _get_config_include_definitions(raw_config)
    if not config_include_defs:
        return raw_config, json_schema

    if "properties" not in json_schema:
        json_schema["properties"] = {}
    config_include_search_dir = Path(config_include_search_dir).resolve()
    config_include_sections = {}
    for section_name, include_path in config_include_defs.items():
        include_path = Path(include_path)
        if not include_path.is_absolute():
            include_path = config_include_search_dir / include_path
        included_config_section = _read_raw_config_file(include_path)

        _sections_traversed = _parent_sections + (section_name,)
        sections_traversed_str = " -> ".join(_sections_traversed)
        if section_name in json_schema["properties"]:
            msg = f'Validation schema for `[include]` section "{sections_traversed_str}" '
            msg += "also detected in its parent section's schehma. "
            msg += "`[include]` schemas must NOT be added to their parent's schemas, but "
            msg += "rather in their own separate files."
            raise ConflictingValidationSchemasError(msg)

        schema_file = schemas_path / f"{section_name}_section_schema.json"
        if not schema_file.is_file():
            logger.warning(
                'No validation schema for config section "%s". Using default.',
                sections_traversed_str,
            )
            schema_file = schemas_path / "default_section_schema.json"

        updated_config, updated_schema = _expand_config_include_section(
            raw_config=included_config_section,
            json_schema={"allOf": [{"$ref": f"file:{schema_file}"}]},
            config_include_search_dir=config_include_search_dir,
            schemas_path=schemas_path,
            _parent_sections=_sections_traversed,
        )
        config_include_sections[section_name] = updated_config
        json_schema["properties"][section_name] = updated_schema

    raw_config.update(config_include_sections)
    raw_config.pop("include")

    return raw_config, json_schema


def _get_json_validation_function(json_schema):
    """Return a validation function compiled with schema `json_schema`."""
    if not json_schema:
        # Validation will just convert everything to dict in this case
        return lambda obj: modify_mappings(obj=obj, operator=dict)
    json_schema = modify_mappings(obj=json_schema, operator=dict)
    validation_func = fastjsonschema.compile(json_schema)

    def validate(obj):
        try:
            return validation_func(modify_mappings(obj=obj, operator=dict))
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

    return validate
