#!/usr/bin/env python3
"""Registration and validation of options passed in the config file."""
import datetime
import io
import json
import logging
import os
import sys
from collections import defaultdict
from contextlib import redirect_stderr
from functools import cached_property, reduce, wraps
from operator import getitem
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Literal, Optional, Tuple, Union

import tomlkit
import yaml
from datamodel_code_generator import InputFileType, PythonVersion, generate
from pandas.tseries.frequencies import to_offset
from pydantic import BaseModel, Extra, root_validator

from . import PACKAGE_NAME
from .datetime_utils import TimeWindowContainer, as_datetime

logger = logging.getLogger(__name__)


def get_default_config_path():
    """Return the default path for the config file."""
    try:
        _fpath = Path(os.getenv("DEODE_CONFIG_PATH", "config.toml"))
        default_conf_path = _fpath.resolve(strict=True)
    except FileNotFoundError:
        default_conf_path = Path(os.getenv("HOME")) / f".{PACKAGE_NAME}" / "config.toml"

    return default_conf_path


# Define custom types for use with pydantic
# See <https://pydantic-docs.helpmanual.io/usage/types/#custom-data-types>
class InputDateTime(datetime.datetime):
    """Return a datetime object constructed from passed config input."""

    @classmethod
    def __get_validators__(cls):
        yield as_datetime


class PandasOffsetFreqStr:
    """Return a validated date offset string."""

    @classmethod
    def _validator(cls, string_repr):
        _ = to_offset(string_repr)
        return string_repr

    @classmethod
    def __get_validators__(cls):
        yield cls._validator

    @classmethod
    def __modify_schema__(cls, field_schema):
        field_schema.update(type="string")


class ParsedPath(Path):
    """Return pathlib.Path obj from string, with envvars & user expanded."""

    @classmethod
    def __get_validators__(cls):
        yield lambda path: Path(os.path.expandvars(str(path))).expanduser()


class _ConfigsBaseModel(BaseModel, extra=Extra.allow, frozen=True):
    """Base model for all models defined in this file."""

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

        values = iterdict(values)

        return values

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
        new_config_obj = cls.parse_obj(raw_config)

        return new_config_obj

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
                except KeyError as key_error:
                    raise KeyError(key_error) from attr_error

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


###########################################################################
# Registering config options (known opts, defaults, restrictions, etc).   #
# This is where you should change, e.g., in order to add new config opts. #
###########################################################################
class _InputDatesModel(_ConfigsBaseModel):
    """Model for the input dates."""

    start: Optional[InputDateTime]
    end: Optional[InputDateTime]
    cycle_length: PandasOffsetFreqStr = "3H"
    list: Optional[Tuple[InputDateTime, ...]]  # noqa: A003

    # pylint: disable=E0213
    @root_validator()
    def _check_consistency_between_date_input_items(cls, values):  # noqa: N805
        date_list, start, end = values.get("list"), values.get("start"), values.get("end")
        fail_cond_1 = date_list and any((start, end))
        fail_cond_2 = not date_list and not all((start, end))
        if fail_cond_1 or fail_cond_2:
            raise ValueError("Must specify either only 'list' or both 'start' and 'end'")
        return values

    @cached_property
    def _normalized(self):
        """Return an iterable with the dates corresponding to the passed configs."""
        if self.list:
            return TimeWindowContainer(data=self.list, cycle_length=self.cycle_length)
        return TimeWindowContainer.from_start_end_and_length(
            start=self.start, end=self.end, cycle_length=self.cycle_length
        )

    def __getitem__(self, item):
        return self._normalized[item]

    def __iter__(self):
        return iter(self._normalized)

    def __len__(self):
        return len(self._normalized)


def gen_parsed_config_obj_from_json_scheme(json_schema_path):
    """Return a pydantic model for a given json schema."""
    with open(json_schema_path, "r") as schema_file:
        json_schema = json.dumps(json.load(schema_file))

    with TemporaryDirectory() as temporary_directory_name:
        temporary_directory = Path(temporary_directory_name)
        with open(temporary_directory / "__init__.py", "w"):
            pass

        generate(
            json_schema,
            input_filename=json_schema_path,
            input_file_type=InputFileType.JsonSchema,
            output=Path(temporary_directory / "generated_model.py"),
            # TODO: Change base_class to use a relative import
            base_class="deode.config_parser._ConfigsBaseModel",
            class_name="ParsedConfig",
            target_python_version=PythonVersion.PY_38,
        )

        # print(Path(temporary_directory / "generated_model.py").read_text())
        sys.path.append(temporary_directory.as_posix())
        from generated_model import ParsedConfig as ConfigParsedFromJsonSchmema

        class ParsedConfig(ConfigParsedFromJsonSchmema):
            # pylint: disable=E0213
            @root_validator(pre=False)
            def validate_assimilation_times_input(cls, values):  # noqa: N805
                try:
                    assimilation_times = values["general"].get_value("assimilation_times")
                except KeyError:
                    return values
                validated_assimilation_times = _InputDatesModel.parse_obj(
                    assimilation_times
                )

                values["general"] = values["general"].copy(
                    update={"assimilation_times": validated_assimilation_times}
                )

                return values

    return ParsedConfig


json_schema_file_path = (
    Path(__file__).parent / "config_file_schemas" / "main_config_schema.json"
)
ParsedConfig = gen_parsed_config_obj_from_json_scheme(json_schema_file_path)
