#!/usr/bin/env python3
"""Registration and validation of options passed in the config file."""
import contextlib
import json
import logging
import os
from collections import defaultdict
from functools import reduce
from operator import getitem
from pathlib import Path
from typing import Literal, Union

import fastjsonschema
import tomlkit
import yaml
from pydantic import BaseModel, Extra, root_validator

from . import PACKAGE_NAME

logger = logging.getLogger(__name__)


def get_default_config_path():
    """Return the default path for the config file."""
    try:
        _fpath = Path(os.getenv("DEODE_CONFIG_PATH", "config.toml"))
        default_conf_path = _fpath.resolve(strict=True)
    except FileNotFoundError:
        default_conf_path = Path(os.getenv("HOME")) / f".{PACKAGE_NAME}" / "config.toml"

    return default_conf_path


def parsed_config_class_factory(json_schema):
    with contextlib.suppress(TypeError):
        with open(Path(json_schema), "r") as schema_file:
            json_schema = json.load(schema_file)

    class ParsedConfig(BaseModel, extra=Extra.allow, frozen=True):
        """Base model for all models defined in this file."""

        json_validator = fastjsonschema.compile(json_schema)

        @classmethod
        def parse_obj(cls, obj):
            return super().parse_obj(cls.json_validator(dict(obj)))

    return ParsedConfig


main_config_json_schema = (
    Path(__file__).parent / "config_file_schemas" / "main_config_schema.json"
)


class ParsedConfig(parsed_config_class_factory(main_config_json_schema)):
    @root_validator(pre=True)
    def convert_lists_into_tuples(cls, values):  # noqa: N805
        """Convert 'list' inputs into tuples. Helps serialisation, needed for dumps."""

        def iterdict(d):
            new_d = dict(d)
            for k, v in d.items():
                if isinstance(v, dict):
                    new_d[k] = iterdict(v)
                elif isinstance(v, list):
                    new_d[k] = tuple(v)
            return new_d

        return iterdict(values)

    @root_validator(pre=True)
    def convert_subdicts_into_model_instance(cls, values):  # noqa: N805
        """Convert nested dicts into instances of the model."""

        def iterdict(d):
            new_d = dict(d)
            for k, v in d.items():
                if isinstance(v, dict):
                    new_d[k] = cls.construct(**iterdict(v))
            return new_d

        return iterdict(values)

    @classmethod
    def from_file(cls, config_path):
        """Read config file at location "config_path".

        Args:
            config_path (Union[pathlib.Path, str]): The path to the config file.

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

        return cls.parse_obj(raw_config)

    def items(self):
        """Emulate the "items" method from the dictionary type."""
        return iter(self)

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
        exclude_unset=False,
        include_metadata=False,
    ):
        """Get a nicely printed version of the models. Excludes the metadata section."""
        exclude = {"metadata"}
        if include_metadata:
            exclude = None

        config = self.dict(
            exclude_unset=exclude_unset, exclude_none=True, exclude=exclude
        )

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

        rtn = json.dumps(config, indent=4, sort_keys=False, default=lambda obj: str(obj))
        if style == "toml":
            return tomlkit.dumps(json.loads(rtn))
        if style == "yaml":
            return yaml.dump(json.loads(rtn))

        return rtn

    def __str__(self):
        return self.dumps(style="json")
