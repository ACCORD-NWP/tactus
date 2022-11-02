#!/usr/bin/env python3
"""Registration and validation of options passed in the config file."""
import datetime
import json
import logging
import os
import tempfile
from collections import defaultdict
from functools import cached_property, reduce
from operator import getitem
from pathlib import Path
from typing import Literal, Optional, Tuple, Union

import tomlkit
from pandas.tseries.frequencies import to_offset
from pydantic import (
    BaseModel,
    Extra,
    PositiveFloat,
    PositiveInt,
    confloat,
    root_validator,
)

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


class ParsedPath(Path):
    """Return pathlib.Path obj from string, with envvars & user expanded."""

    @classmethod
    def __get_validators__(cls):
        yield lambda path: Path(os.path.expandvars(str(path))).expanduser()


class _ConfigsBaseModel(BaseModel, extra=Extra.ignore, frozen=True):
    """Base model for all models defined in this file."""

    def __getattr__(self, items):
        """Get attribute.

        Override so we can use,
        e.g., getattr(config, "general.time_windows.start.minute").

        Args:
            items (str): Attributes to be retrieved, as dot-separated strings.

        Returns:
            Any: Parsed command line arguments.
        """

        def regular_getattribute(obj, item):
            if type(obj) is type(self):
                return super().__getattribute__(item)
            return getattr(obj, item)

        return reduce(regular_getattribute, items.split("."), self)

    def dumps(
        self, section="", style: Literal["toml", "json"] = "toml", exclude_unset=False
    ):
        """Get a nicely printed version of the models."""
        config = self.dict(exclude_unset=exclude_unset, exclude_none=True)
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


# "general" section
class _GeneralSectionModel(_ConfigsBaseModel):
    """Model for the 'general' section."""

    data_rootdir: ParsedPath = Path(".")
    assimilation_times: _InputDatesModel
    outdir: ParsedPath = Path(tempfile.gettempdir()).resolve() / f"{PACKAGE_NAME}_output"


# "commands" section
class _CommandSectionModel(_ConfigsBaseModel):
    """Model for the 'commands' section."""

    class _SelectCommandSectionModel(_ConfigsBaseModel):
        station_rejection_tol: confloat(ge=0, le=1) = 0.3
        save_obsoul: bool = True

    select: _SelectCommandSectionModel = _SelectCommandSectionModel()


# domain and projection
class _DomainSectionModel(_ConfigsBaseModel):
    """Model for the 'domain' section."""

    # These defaults are for the METCOOP25C domain. See
    # <https://hirlam.org/trac/wiki/HarmonieSystemDocumentation/ModelDomain>
    # <https://hirlam.org/trac/browser/Harmonie/scr/Harmonie_domains.pm>
    name: str = "Domain"
    shape: Tuple[PositiveInt, PositiveInt] = (900, 960)  # (n_lon. n_lat)
    cell_sizes: Tuple[PositiveFloat, PositiveFloat] = (2500.0, 2500.0)  # In meters
    center_lonlat: Tuple[confloat(ge=-180, lt=180), confloat(ge=-90, le=90)] = (
        16.763011639,
        63.489212956,
    )
    projparams: Union[dict, str] = "+proj=lcc +lon_0=15 +lat_0=63 +lat_1=63"


class ParsedConfig(_ConfigsBaseModel):
    """Model for validation of the data in the whole config file."""

    general: _GeneralSectionModel
    commands: _CommandSectionModel = _CommandSectionModel()
    domain: _DomainSectionModel = _DomainSectionModel()

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

        return cls.parse_obj(raw_config)
