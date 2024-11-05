#!/usr/bin/env python3
"""Registration and validation of options passed in the config file."""
import contextlib
import glob
import json
import os
import tempfile
from functools import reduce
from pathlib import Path

import fastjsonschema
import jsonref
import tomli
import tomlkit
import yaml
from fastjsonschema import JsonSchemaValueException
from json_schema_for_humans.generate import (
    GenerationConfiguration,
    generate_from_file_object,
)
from toml_formatter.formatter import FormattedToml
from toml_formatter.formatter_options import FormatterOptions

from . import GeneralConstants
from .aux_types import BaseMapping, QuasiConstant
from .datetime_utils import DatetimeConstants
from .general_utils import modify_mappings
from .logs import logger
from .os_utils import resolve_path_relative_to_package
from .toolbox import Platform


class ConfigParserDefaults(QuasiConstant):
    """Defaults related to the parsing of config files."""

    DATA_DIRECTORY = GeneralConstants.PACKAGE_DIRECTORY / "data"
    CONFIG_DIRECTORY = DATA_DIRECTORY / "config_files"
    PACKAGE_INCLUDE_DIR = CONFIG_DIRECTORY / "include"

    PACKAGE_CONFIG_PATH = (CONFIG_DIRECTORY / "config.toml").resolve(strict=True)
    # Define the default path to the config file
    try:
        CONFIG_PATH = Path(os.getenv("DEODE_CONFIG_PATH", "config.toml"))
        CONFIG_PATH = CONFIG_PATH.resolve(strict=True)
    except FileNotFoundError:
        CONFIG_PATH = PACKAGE_CONFIG_PATH

    SCHEMAS_DIRECTORY = CONFIG_DIRECTORY / "config_file_schemas"
    MAIN_CONFIG_JSON_SCHEMA_PATH = SCHEMAS_DIRECTORY / "main_config_schema.json"
    MAIN_CONFIG_JSON_SCHEMA = json.loads(MAIN_CONFIG_JSON_SCHEMA_PATH.read_text())


class ConfigPaths:
    """Support multiple path search."""

    _env_data_paths = os.getenv("DEODE_DATA_PATH")
    DATA_SEARCHPATHS = _env_data_paths.split(":") if _env_data_paths is not None else []
    erroneous_paths = [path for path in DATA_SEARCHPATHS if not os.path.isabs(path)]
    if len(erroneous_paths) > 0:
        raise RuntimeError(f"DEODE_DATA_PATH is not absolute: {erroneous_paths}")
    DATA_SEARCHPATHS.append(ConfigParserDefaults.DATA_DIRECTORY)
    DATA_SEARCHPATHS = tuple(DATA_SEARCHPATHS)

    @staticmethod
    def print():
        """Prints the available paths."""
        dirmap = {
            "config_file_schemas": "config_files/config_file_schemas",
            "data_input": "input",
        }

        path_info = {}
        for dir_ in [
            "config_files",
            "config_file_schemas",
            "namelist_generation_input",
            "data_input",
        ]:
            rdir = dirmap.get(dir_, dir_)
            path_info[dir_] = []
            pattern = f"**/{rdir}"
            for searchpath in ConfigPaths.DATA_SEARCHPATHS:
                res = list(Path(searchpath).rglob(pattern))
                if len(res) > 0:
                    path_info[dir_].append(str(res[0]))

        logger.info("DEODE paths")
        logger.info(" Package directory: {}", GeneralConstants.PACKAGE_DIRECTORY)
        logger.info(f" Data paths in search order: {json.dumps(path_info, indent=4)}")

    @staticmethod
    def path_from_subpath(subpath, additional_path=None, insert_index=0) -> Path:
        """Interface to find full path given any subpath, by searching 'searchpaths'.

        Arguments:
            subpath (str): Subpath to search for
            additional_path (Path) : Extra path to search in
            insert_index (integer): Where to insert the additional path

        Returns:
            (Path): Full path to target

        Raises:
            RuntimeRerror: Various errors
        """
        pattern = f"**/{subpath}"
        searchpaths = list(ConfigPaths.DATA_SEARCHPATHS)
        #logger.info("Searchpaths: {}", searchpaths)
        #logger.info("additional_path: {}", additional_path)
        if additional_path is not None:
            searchpaths.insert(insert_index, additional_path)
        #logger.info("Searchpaths: {}", searchpaths)
        for searchpath in searchpaths:
            results = list(Path(searchpath).rglob(pattern))
            if len(results) > 0:
                logger.info("Found: {}", results[0])
                return results[0]

        raise RuntimeError(f"Could not find {subpath}")


class ConfigFileValidationError(Exception):
    """Error to be raised when parsing the input config file fails."""


class ConflictingValidationSchemasError(Exception):
    """Error to be raised when more than one schema is defined for a config section."""


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
            **kwargs: Arguments passed to the class constructor.

        Returns:
            cls: Configs retrieved from the specified path.
        """
        path = Path(path).resolve()

        configs = _read_raw_config_file(path)
        return cls(configs, _metadata={"source_file_path": path}, **kwargs)

    def save_as(self, config_file):
        """Save config file.

        Args:
            config_file (str): Path to config file
        """
        with open(config_file, mode="w", encoding="utf8") as fh:
            tomlkit.dump(self.dict(), fh)
        config = FormatterOptions.from_toml_file("pyproject.toml")
        formatted_toml = FormattedToml.from_file(
            path=config_file, formatter_options=config
        )
        with open(config_file, mode="w", encoding="utf8") as f:
            f.write(str(formatted_toml))

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

    def __init__(self, *args, **kwargs):
        """Initialise instance."""
        super().__init__(*args, **kwargs)
        self.data = jsonref.replace_refs(self.data)

    @property
    def _validation_function(self):
        return _get_json_validation_function(self)

    def validate(self, data):
        """Return a copy of `data` validated against the stored JSON schema."""
        return self._validation_function(data)

    def get_markdown_doc(self):
        """Return human-readable doc for the schema in markdown format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with open(Path(tmpdir) / "schema.json", "w") as schema_file:
                schema_file.write(json.dumps(self.dict()))

            with open(
                Path(tmpdir) / "schema_doc.md", "w"
            ) as doc_file, contextlib.redirect_stdout(None):
                generate_from_file_object(
                    schema_file=schema_file,
                    result_file=doc_file,
                    config=GenerationConfiguration(
                        template_name="md",
                        show_toc=False,
                        template_md_options={"show_heading_numbers": False},
                        with_footer=False,
                        properties_table_columns=[
                            "property",
                            "type",
                            "required",
                            "default",
                            "title/description",
                        ],
                    ),
                )

            with open(Path(tmpdir) / "schema_doc.md", "r") as doc_file:
                schema_doc = doc_file.read()

        return schema_doc


class ParsedConfig(BasicConfig):
    """Object that holds parsed configs validated against a `json_schema`."""

    def __init__(
        self,
        *args,
        json_schema,
        include_dir=ConfigParserDefaults.CONFIG_DIRECTORY,
        **kwargs,
    ):
        """Initialise an instance with an arbitrary number of entries & validate them."""
        self.include_dir = include_dir
        self.json_schema = json_schema
        self.host = None
        if "host" in kwargs:
            self.host = kwargs["host"]
        super().__init__(*args, **kwargs)

    @BasicConfig.data.setter
    def data(self, new):
        """Set the underlying data stored by the instance."""
        new, json_schema = _expand_config_include_section(
            raw_config=new,
            json_schema=self.json_schema,
            config_include_search_dir=self.include_dir,
            host=self.host,
        )
        ParsedConfig.json_schema.fset(self, json_schema, _validate_data=False)

        # Make sure all sections defined in the schema are also present in the new config
        sections_that_should_not_be_defaulted = [
            "include",
            *new,
            *json_schema.get("required", []),
        ]
        for property_name, property_schema in json_schema.get("properties", {}).items():
            if property_name in sections_that_should_not_be_defaulted:
                continue
            if property_schema.get("type", "") == "object":
                new[property_name] = {}

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

    def expand_macros(self):
        """Expand macros in config recursively."""
        _config = self.copy(update={"macros": self.get("macros.case", self["macros"])})
        macro_platform = Platform(_config)
        config = macro_platform.resolve_macros(self.dict())
        config = self.copy(update=config)

        return config


def _read_raw_config_file(config_path: Path):
    """Read raw configs from files in miscellaneous formats.

    Args:
        config_path (Path): Path to the config file.

    Raises:
        NotImplementedError: If the config file format is not supported.

    Returns:
        dict: Configs read from the specified path.
    """
    config_path = resolve_path_relative_to_package(config_path)

    logger.debug("Reading configs from file <{}>", config_path)

    with open(config_path, "rb") as config_file:
        if config_path.suffix == ".toml":
            return tomli.load(config_file)

        if config_path.suffix in [".yaml", ".yml"]:
            return yaml.safe_load(config_file)

        if config_path.suffix == ".json":
            return json.load(config_file)

    raise NotImplementedError(f'Unsupported config file format "{config_path.suffix}"')


def _get_config_include_definitions(raw_config):
    config_includes = raw_config.get("include", {}).copy()
    overlapping_sections = [key for key in config_includes if key in raw_config]
    if overlapping_sections:
        msg = f"`[include]` section(s) [{', '.join(overlapping_sections)}] "
        msg += "already present in parent config."
        raise ValueError(msg)
    return config_includes


def _get_all_json_schemas(json_schema, schemas_path):
    """Load and add all json schema files in the schemas_path directory.

    Args:
        json_schema (dict): Input json schema
        schemas_path (str): Path to json files

    Returns:
        json_schema (dict): Updated json dict

    """
    exclude = ["main_config_schema.json", "default_config_schema.json"]

    for filename in glob.glob(f"{schemas_path}/*.json"):
        if os.path.basename(filename) in exclude:
            continue
        section_name = os.path.basename(filename).replace("_section_schema.json", "")
        updated_schema = {"$ref": f"file:{filename}"}
        json_schema["properties"].update({section_name: updated_schema})

    return json_schema


def _expand_config_include_section(
    raw_config,
    json_schema,
    config_include_search_dir=ConfigParserDefaults.CONFIG_DIRECTORY,
    schemas_path=ConfigParserDefaults.SCHEMAS_DIRECTORY,
    _parent_sections=(),
    host=None,
):
    """Merge config includes and return new config & corresponding validation schema."""
    raw_config = modify_mappings(obj=raw_config, operator=dict)
    json_schema = modify_mappings(obj=json_schema, operator=dict)

    config_include_defs = _get_config_include_definitions(raw_config)

    if "properties" not in json_schema:
        json_schema["properties"] = {}
    config_include_search_dir = Path(config_include_search_dir).resolve()
    config_include_sections = {}
    if len(config_include_defs) == 0:
        json_schema = _get_all_json_schemas(json_schema, str(schemas_path))
    else:
        for section_name, include_path_ in config_include_defs.items():
            if isinstance(include_path_, str):
                include_path_ = (
                    include_path_.replace("@HOST@", host)
                    if host is not None
                    else include_path_
                )
                include_path = Path(include_path_)
                if not include_path.is_absolute():
                    include_path = ConfigPaths.path_from_subpath(
                        include_path, config_include_search_dir, -1
                    )
                included_config_section = _read_raw_config_file(include_path)
            else:
                included_config_section = include_path_
            _sections_traversed = (*_parent_sections, section_name)
            sections_traversed_str = " -> ".join(_sections_traversed)
            if "include" in raw_config and section_name in json_schema["properties"]:
                msg = "Validation schema for `[include]` section "
                msg += f' "{sections_traversed_str}" '
                msg += "also detected in its parent section's schema. "
                msg += "`[include]` schemas must NOT be added to their parent's schemas,"
                msg += "but rather in their own separate files."
                raise ConflictingValidationSchemasError(msg)

            schema_file = schemas_path / f"{section_name}_section_schema.json"
            if not schema_file.is_file():
                logger.warning(
                    'No validation schema for config section "{}". Using default.',
                    sections_traversed_str,
                )
                schema_file = schemas_path / "default_section_schema.json"

            updated_config, updated_schema = _expand_config_include_section(
                raw_config=included_config_section,
                json_schema={"$ref": f"file:{schema_file}"},
                config_include_search_dir=config_include_search_dir,
                schemas_path=schemas_path,
                _parent_sections=_sections_traversed,
            )
            config_include_sections.update(updated_config)
            json_schema["properties"].update({section_name: updated_schema})

    raw_config.update(config_include_sections)
    if "include" in raw_config:
        raw_config.pop("include")

    return raw_config, json_schema


def _get_json_validation_function(json_schema):
    """Return a validation function compiled with schema `json_schema`."""
    if not json_schema:
        # Validation will just convert everything to dict in this case
        return lambda obj: modify_mappings(obj=obj, operator=dict)
    validation_func = fastjsonschema.compile(json_schema.dict())

    def validate(obj):
        try:
            return validation_func(modify_mappings(obj=obj, operator=dict))
        except JsonSchemaValueException as err:
            error_path = " -> ".join(err.path[1:])
            human_readable_msg = err.message.replace(err.name, "").strip()

            # Give a better err msg when times/date-times/durations don't follow ISO 8601
            human_readable_msg = human_readable_msg.replace(
                f"must match pattern {DatetimeConstants.ISO_8601_TIME_DURATION_REGEX}",
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
