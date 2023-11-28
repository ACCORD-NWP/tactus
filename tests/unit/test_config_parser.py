#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import datetime
import itertools
import json
import re
import uuid
from collections import namedtuple
from pathlib import Path

import pytest
import tomlkit

from deode.config_parser import (
    BasicConfig,
    ConfigFileValidationError,
    ConfigParserDefaults,
    ConflictingValidationSchemasError,
    JsonSchema,
    ParsedConfig,
)
from deode.datetime_utils import DatetimeConstants, as_datetime


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            times.list = ["2000-01-01T00:00:00Z"]
        """
    )


@pytest.fixture()
def raw_config_with_task(minimal_raw_config):
    rtn = minimal_raw_config.copy()
    task_configs = tomlkit.parse(
        """
        [task.forecast]
            wrapper = "time"
            command = "echo Hello world && touch output"
            input_data.input_file = "/dev/null"
            output_data.output = "archived_file"
        """
    )
    rtn.update(task_configs)

    return rtn


@pytest.fixture()
def raw_config_with_non_recognised_options(minimal_raw_config):
    raw_config = minimal_raw_config.copy()

    new_section = tomlkit.parse(
        """
        [unrecognised_section_name]
            foo = "bar"
        """
    )
    raw_config.update(new_section)

    raw_config["general"].update(
        tomlkit.parse(
            """
            baz = "qux"
            unknown_field = ["A", "B"]
            """
        )
    )

    return raw_config


@pytest.fixture()
def minimal_parsed_config(minimal_raw_config):
    return ParsedConfig(
        minimal_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )


@pytest.fixture()
def parsed_config_with_task(raw_config_with_task):
    return ParsedConfig(
        raw_config_with_task,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


@pytest.fixture()
def json_schema_for_iso_8601_time_specs_tests():
    schema = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "title": "Test Schema",
        "type": "object",
        "properties": {
            "a_date_field": {
                "title": "A 'date' Field That Should Follow ISO 8601.",
                "default": "2000-01-01",
                "type": "string",
                "format": "date",
            },
            "a_time_field": {
                "title": "A 'time' Field That Should Follow ISO 8601.",
                "default": "00:00:00+00:00",
                "type": "string",
                "format": "time",
            },
            "a_date_time_field": {
                "title": "A 'date-time' Field That Should Follow ISO 8601.",
                "default": "2000-01-01T00:00:00Z",
                "type": "string",
                "format": "date-time",
            },
            "a_duration_field": {
                "title": "A 'duration' Field That Should Follow ISO 8601.",
                "default": "PT3H",
                "type": "string",
                "pattern": DatetimeConstants.ISO_8601_TIME_DURATION_REGEX,
            },
        },
    }

    return schema


@pytest.fixture()
def tmp_test_data_dir(tmpdir_factory):
    return Path(tmpdir_factory.mktemp("deode_test_rootdir"))


@pytest.fixture()
def config_path(minimal_raw_config, tmp_test_data_dir):
    config_path = tmp_test_data_dir / "config.toml"
    with open(config_path, "w") as config_file:
        tomlkit.dump(minimal_raw_config, config_file)
    return config_path


@pytest.fixture()
def package_main_config_without_validation():
    return ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH, json_schema={}
    )


class TestGeneralBehaviour:
    def test_config_model_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, ParsedConfig)

    def test_nested_mappings_become_basic_config(
        self, package_main_config_without_validation
    ):
        def nested_mappings_are_basic_config(obj):
            rtn = isinstance(obj, BasicConfig)
            for v in obj.values():
                if not rtn:
                    break
                if not hasattr(v, "items"):
                    continue
                rtn = rtn and nested_mappings_are_basic_config(v)
            return rtn

        assert nested_mappings_are_basic_config(package_main_config_without_validation)

    def test_no_lists_are_present(self):
        def mapping_contains_lists(obj):
            rtn = isinstance(obj, list)
            if not hasattr(obj, "items"):
                return rtn
            for v in obj.values():
                rtn = rtn or mapping_contains_lists(v)
                if rtn:
                    break
            return rtn

        input_data = {"a": {}, "b": [], "c": {"d": [1, 2, 3]}}
        config = ParsedConfig(input_data, json_schema={})
        assert not mapping_contains_lists(config), config

    def test_no_none_values_are_stored(self):
        def no_none_values_stored(obj):
            if obj is None:
                return False

            if not hasattr(obj, "items"):
                return True

            return all(no_none_values_stored(v) for v in obj.values())

        input_data = {"a": {}, "b": None, "c": 1}
        config = ParsedConfig(input_data, json_schema={})
        assert no_none_values_stored(config), config

    def test_data_validation_is_triggered_when_json_schema_is_modified(self):
        input_data = {"a": {}, "b": None, "c": 1}
        config = ParsedConfig(input_data, json_schema={})
        with pytest.raises(
            ConfigFileValidationError, match=re.escape("must contain ['general']")
        ):
            config.json_schema = ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA

    def test_config_model_can_be_printed(self):
        parsed_config = ParsedConfig({}, json_schema={})
        _ = str(parsed_config)
        _ = repr(parsed_config)

    def test_json_schema_class_can_be_printed(self):
        json_schema = JsonSchema({})
        _ = str(json_schema)
        _ = repr(json_schema)

    def test_config_recursive_item_access(self, minimal_parsed_config):
        recursively_retrieved_value = minimal_parsed_config["general.times.list"]
        assert isinstance(recursively_retrieved_value, tuple)
        assert (
            recursively_retrieved_value
            is minimal_parsed_config["general"]["times"]["list"]
        )

    def test_config_recursive_attr_access_task(self, parsed_config_with_task):
        with pytest.raises(KeyError, match="'foo'"):
            _ = parsed_config_with_task["task.forecast.foo"]
        recursively_retrieved_value = parsed_config_with_task["task.forecast.wrapper"]

        assert recursively_retrieved_value == "time"

    def test_unrecognised_options_are_supported(
        self, raw_config_with_non_recognised_options
    ):
        raw_config = raw_config_with_non_recognised_options.copy()
        parsed_config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        extra_section = parsed_config["unrecognised_section_name"]
        assert extra_section

    def test_config_data_is_immutable(self, minimal_parsed_config):
        with pytest.raises(TypeError, match="object does not support item assignment"):
            minimal_parsed_config["general"] = "foo"
        with pytest.raises(KeyError, match="'foo'"):
            _ = minimal_parsed_config["foo"]
        with pytest.raises(TypeError, match="object does not support item assignment"):
            minimal_parsed_config["foo"] = "bar"

    def test_config_get_value(self, raw_config_with_non_recognised_options):
        config = ParsedConfig(
            raw_config_with_non_recognised_options,
            json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        )
        assert config["general.times.cycle_length"] == "PT3H"

        non_existing_key = str(uuid.uuid4())
        with pytest.raises(KeyError, match=f"'{non_existing_key}'"):
            config[non_existing_key]

        random_value = str(uuid.uuid4())
        assert config.get("non_existing_key", default=random_value) == random_value

    def test_config_can_be_printed(self, raw_config_with_non_recognised_options):
        config = ParsedConfig(
            raw_config_with_non_recognised_options,
            json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        )
        _ = str(config)

    def test_config_section_can_be_printed(self, raw_config_with_non_recognised_options):
        config = ParsedConfig(
            raw_config_with_non_recognised_options,
            json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        )
        section_dumps = config.dumps(section="general")
        expected_section_dumps = BasicConfig(general=config["general"]).dumps()
        assert section_dumps == expected_section_dumps

        # Check that it won't print an inexistent section
        random_key = str(uuid.uuid4())
        with pytest.raises(KeyError, match=f"'{random_key}'"):
            _ = config.dumps(random_key)

    def test_parsed_config_passes_toml_readwrite_roundtrip(self, minimal_parsed_config):
        toml_dumps = minimal_parsed_config.dumps(style="toml")
        reloaded_parsed_config = ParsedConfig(
            tomlkit.loads(toml_dumps),
            json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
        )
        new_toml_dumps = reloaded_parsed_config.dumps(style="toml")
        assert new_toml_dumps == toml_dumps

    def test_parsed_config_has_metadata_attr(self, minimal_parsed_config):
        assert hasattr(minimal_parsed_config, "metadata")

    def test_parsed_config_does_not_have_file_metadata_when_not_read_from_file(
        self, minimal_parsed_config
    ):
        config = minimal_parsed_config.copy()
        with pytest.raises(KeyError, match="'source_file_path'"):
            _ = config.metadata["source_file_path"]

    @pytest.mark.parametrize("fmt", ["toml", "yaml", "json"])
    def test_can_read_configs_from_supported_file_formats(
        self, fmt, config_path, minimal_parsed_config
    ):
        new_config_path = config_path.parent / f"{config_path.stem}.{fmt}"
        with open(new_config_path, "w") as f:
            f.write(minimal_parsed_config.dumps(style=fmt))

        new_config_as_dict = ParsedConfig.from_file(
            new_config_path, json_schema={}
        ).dict()
        old_config_as_dict = minimal_parsed_config.dict()
        assert new_config_as_dict == old_config_as_dict

    def test_catch_attempting_to_read_configs_from_unsupported_file_formats(
        self, config_path, minimal_parsed_config
    ):
        fmt = "__UNKNOWN_FORMAT__"
        new_config_path = config_path.parent / f"{config_path.stem}.{fmt}"
        with open(new_config_path, "w") as f:
            f.write(minimal_parsed_config.dumps(style=fmt))

        with pytest.raises(NotImplementedError, match="Unsupported config file format"):
            _ = ParsedConfig.from_file(new_config_path, json_schema={})

    def test_parsed_config_registers_file_metadata_when_read_from_file(self, config_path):
        config = ParsedConfig.from_file(
            config_path, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        config_source_file_path = config.metadata["source_file_path"]
        assert isinstance(config_source_file_path, str)
        assert Path(config_source_file_path) == Path(config_path)

    def test_can_modify_model_upon_copy(self, minimal_raw_config):
        raw_config = minimal_raw_config.copy()
        raw_config["general"].update({"case": "foo"})
        parsed_config = ParsedConfig(raw_config, json_schema={})

        original_value = parsed_config["general.case"]
        new_value = "bar"
        new_parsed_config = parsed_config.copy(update={"general": {"case": new_value}})

        assert original_value != new_value
        assert parsed_config["general.times"]
        assert new_parsed_config["general.times"]
        assert new_parsed_config["general.times"] == parsed_config["general.times"]
        assert parsed_config["general.case"] == original_value
        assert new_parsed_config["general.case"] == new_value

    def test_can_modify_with_list_value_upon_model_copy(self, minimal_raw_config):
        raw_config = minimal_raw_config.copy()
        raw_config["general"].update({"case": "foo"})
        parsed_config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )

        original_value = parsed_config["general.times"]
        new_value = [
            "2000-01-01T22:00:00Z",
            datetime.datetime.now(datetime.timezone.utc).isoformat(),
        ]
        new_parsed_config = parsed_config.copy(
            update={"general": {"times": {"list": new_value}}}
        )

        assert original_value != new_value
        assert parsed_config["general.case"] == "foo"
        assert new_parsed_config["general.case"] == "foo"
        assert parsed_config["general.times"] == original_value
        assert new_parsed_config["general.times.list"] == tuple(new_value)


class TestValidators:
    @pytest.mark.parametrize(
        "dt_input",
        [
            "2018-10-10T00:00:00+00:00",
            "2018-10-10T00:00:00Z",
            "2018-10-10T00:00:00.000000+00:00",
            datetime.datetime.now(datetime.timezone.utc).isoformat(),
        ],
        ids=itertools.count(),
    )
    def test_validator_works_with_input_datetime(self, dt_input, minimal_raw_config):
        minimal_raw_config["general"]["times"]["list"] = [dt_input]
        parsed_config = ParsedConfig(
            minimal_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        validated_value = parsed_config["general.times.list"][0]
        assert isinstance(validated_value, str)
        assert as_datetime(validated_value) == as_datetime(dt_input)

    def test_parsing_complains_about_incompatible_type(self, minimal_raw_config):
        minimal_raw_config["general"]["times"]["list"] = datetime.datetime.now()
        with pytest.raises(
            ConfigFileValidationError,
            match="must be array",
        ):
            _ = ParsedConfig(
                minimal_raw_config,
                json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
            )

    @pytest.mark.parametrize(
        ("start", "end", "dates_list"),
        [
            ("2000-01-01T00:00:00Z", "2000-01-01T00:00:00Z", ["2000-01-01T00:00:00Z"]),
            ("2000-01-01T00:00:00Z", None, ["2000-01-01T00:00:00Z"]),
            (None, "20000101T00:00:00Z", ["20000101T00:00:00Z"]),
            ("2000-01-01T00:00:00Z", None, None),
            (None, "2000-01-01T00:00:00Z", None),
            (None, None, None),
        ],
    )
    def test_parsing_complains_about_incompatible_date_specs(
        self, minimal_raw_config, start, end, dates_list
    ):
        raw_config = minimal_raw_config.copy()

        new_config_times = ""
        if start is not None:
            new_config_times = f"""
                 {new_config_times}
                    start = '{start}'
            """
        if end is not None:
            new_config_times = f"""
                {new_config_times}
                    end = '{end}'
            """
        if dates_list is not None:
            new_config_times = f"""
                {new_config_times}
                    list = {dates_list}
            """

        raw_config["general"]["times"] = tomlkit.parse(new_config_times)

        with pytest.raises(
            ConfigFileValidationError, match="must be valid exactly by one definition"
        ):
            _ = ParsedConfig(
                raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
            )


class TestPossibilityOfISO8601ComplianceEnforcement:
    Input = namedtuple("Input", ["name", "correct_value", "wrong_value"])
    iso_8601_test_inputs = [
        Input("date", "2020-01-01", "20200101"),
        Input("time", "00:00:00+00:00", "00"),
        Input("date-time", "2000-01-01T00:00:00Z", "20200101 00:00:00"),
        Input("duration", "PT3H", "3H"),
    ]

    @pytest.mark.parametrize(
        "tested_param",
        iso_8601_test_inputs,
        ids=(item.name for item in iso_8601_test_inputs),
    )
    def test_parsing_complains_about_non_iso_8601_compliant_date_and_time_specs(
        self, tested_param, json_schema_for_iso_8601_time_specs_tests
    ):
        param_name_in_schema = f"a_{tested_param.name}_field".replace("-", "_")
        raw_config = {param_name_in_schema: tested_param.wrong_value}
        with pytest.raises(
            ConfigFileValidationError,
            match=f"must be an ISO 8601 {tested_param.name} string",
        ):
            _ = ParsedConfig(
                raw_config, json_schema=json_schema_for_iso_8601_time_specs_tests
            )

        raw_config = {param_name_in_schema: tested_param.correct_value}
        _ = ParsedConfig(
            raw_config, json_schema=json_schema_for_iso_8601_time_specs_tests
        )


@pytest.fixture()
def valid_config_include_section():
    files_under_include_dir = list(ConfigParserDefaults.PACKAGE_INCLUDE_DIR.glob("*"))
    include_section = {
        fpath.stem: fpath.as_posix()
        for fpath in files_under_include_dir
        if fpath.is_file()
    }
    return include_section


@pytest.fixture()
def raw_config_with_include_section(minimal_raw_config, valid_config_include_section):
    raw_config = minimal_raw_config.copy()
    include_section = valid_config_include_section.copy()
    raw_config["include"] = include_section
    return raw_config


@pytest.fixture()
def parsed_config_with_included_sections(raw_config_with_include_section):
    return ParsedConfig(
        raw_config_with_include_section,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


class TestConfigIncludeSection:
    def test_package_config_include_sections(
        self, parsed_config_with_included_sections, valid_config_include_section
    ):
        config = parsed_config_with_included_sections.copy()
        assert isinstance(config, ParsedConfig)
        assert "include" not in config
        assert set(valid_config_include_section.keys()).issubset(config.keys())

    def test_includes_do_not_duplicate_sections(self, raw_config_with_include_section):
        raw_config = raw_config_with_include_section.copy()
        raw_config["foo_section"] = {"bar": 1}
        raw_config["include"].update({"foo_section": "foo/bar"})
        with pytest.raises(
            ValueError,
            match=re.escape("`[include]` section(s) [foo_section] already present"),
        ):
            _ = ParsedConfig(raw_config, json_schema={})

    def test_includes_do_not_duplicate_schemas(self, raw_config_with_include_section):
        raw_config = raw_config_with_include_section.copy()
        raw_config["include"].update({"foo_section": "include/macros.toml"})

        with open(
            ConfigParserDefaults.SCHEMAS_DIRECTORY / "default_section_schema.json", "r"
        ) as schema_file:
            schema = json.load(schema_file)

        schema["properties"].update({"foo_section": {}})

        with pytest.raises(
            ConflictingValidationSchemasError,
            match="also detected in its parent section's schehma",
        ):
            _ = ParsedConfig(raw_config, json_schema=schema)
