#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""

import datetime
import itertools
import json
import os
import re
import uuid
from collections import namedtuple
from pathlib import Path

import frozendict
import pytest
import tomli
import tomlkit

from tactus.aux_types import BaseMapping, recursive_freeze
from tactus.config_parser import (
    BasicConfig,
    ConfigFileValidationError,
    ConfigParserDefaults,
    ConfigPaths,
    ConflictingValidationSchemasError,
    JsonSchema,
    ParsedConfig,
)
from tactus.datetime_utils import DatetimeConstants, as_datetime
from tactus.derived_variables import set_times
from tactus.general_utils import recursive_unfreeze


@pytest.fixture
def minimal_raw_config():
    return tomlkit.parse("""
        [general]
            times.list = ["2000-01-01T00:00:00Z"]
        """)


@pytest.fixture
def raw_config_with_task(minimal_raw_config):
    rtn = minimal_raw_config.copy()
    task_configs = tomlkit.parse("""
        [task.forecast]
            wrapper = "time"
            command = "echo Hello world && touch output"
            input_data.input_file = "/dev/null"
            output_data.output = "archived_file"
        """)
    rtn.update(task_configs)

    return rtn


@pytest.fixture
def raw_config_with_non_recognised_options(minimal_raw_config):
    raw_config = minimal_raw_config.copy()

    new_section = tomlkit.parse("""
        [unrecognised_section_name]
            foo = "bar"
        """)
    raw_config.update(new_section)

    raw_config["general"].update(
        tomlkit.parse("""
            baz = "qux"
            unknown_field = ["A", "B"]
            """)
    )

    return raw_config


@pytest.fixture
def minimal_parsed_config(minimal_raw_config):
    return ParsedConfig(
        minimal_raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    )


@pytest.fixture
def parsed_config_with_task(raw_config_with_task):
    return ParsedConfig(
        raw_config_with_task,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )


@pytest.fixture
def json_schema_for_iso_8601_time_specs_tests():
    return {
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


@pytest.fixture
def tmp_test_data_dir(tmpdir_factory):
    return Path(tmpdir_factory.mktemp("tactus_test_rootdir"))


@pytest.fixture
def config_path(minimal_raw_config, tmp_test_data_dir):
    config_path = tmp_test_data_dir / "config.toml"
    with open(config_path, "w") as config_file:
        tomlkit.dump(minimal_raw_config, config_file)
    return config_path


@pytest.fixture
def package_main_config_without_validation():
    return ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH, json_schema={}, host="atos_bologna"
    )


class TestFrozenDict:
    @staticmethod
    def _nested_mappings_type_count(obj, count_dict, count_frozendict):
        if not hasattr(obj, "items"):
            return count_dict, count_frozendict

        inc_dict = 1 if type(obj) is dict else 0
        inc_frozendict = 1 if type(obj) is frozendict.frozendict else 0

        if inc_dict + inc_frozendict != 1:
            raise Exception(f"{type(obj)}")

        total_count_dict = count_dict + inc_dict
        total_count_frozendict = count_frozendict + inc_frozendict

        for v in obj.values():
            if not hasattr(v, "items"):
                continue
            v_count_dict, v_frozendict = TestFrozenDict._nested_mappings_type_count(
                v, 0, 0
            )
            total_count_dict += v_count_dict
            total_count_frozendict += v_frozendict

        return total_count_dict, total_count_frozendict

    @staticmethod
    def _nested_mappings_are_dict(obj):
        count_dict, count_frozendict = TestFrozenDict._nested_mappings_type_count(
            obj, 0, 0
        )
        return count_dict > 0 and count_frozendict == 0

    @staticmethod
    def _nested_mappings_are_frozendict(obj):
        count_dict, count_frozendict = TestFrozenDict._nested_mappings_type_count(
            obj, 0, 0
        )
        return count_frozendict > 0 and count_dict == 0

    @staticmethod
    def is_dict(obj):
        return TestFrozenDict._nested_mappings_are_dict(
            obj
        ) and not TestFrozenDict._nested_mappings_are_frozendict(obj)

    @staticmethod
    def is_frozen(obj):
        return not TestFrozenDict._nested_mappings_are_dict(
            obj
        ) and TestFrozenDict._nested_mappings_are_frozendict(obj)

    def test_nested_dictionary_freeze(self):

        data = {
            "user": {"id": 1, "name": "Alice", "roles": ["admin", "user"]},
            "settings": {
                "theme": "dark",
                "notifications": {"email": True, "sms": False},
            },
        }
        # initial check
        assert TestFrozenDict.is_dict(data)

        # Freeze data
        frozen_data = recursive_freeze(data)
        assert TestFrozenDict.is_frozen(frozen_data)

        # Unfreeze data
        unfrozen_data = recursive_unfreeze(frozen_data)
        assert TestFrozenDict.is_dict(unfrozen_data)

        # Check initial data were not touched
        assert TestFrozenDict.is_dict(data)
        assert TestFrozenDict.is_frozen(frozen_data)
        assert TestFrozenDict.is_dict(unfrozen_data)

    def test_configuration_freeze(
        self, default_config, package_main_config_without_validation
    ):

        for config in [default_config, package_main_config_without_validation]:
            assert TestFrozenDict.is_dict(config.dict())
            assert TestFrozenDict.is_frozen(config.data)

            new_config = config.copy()
            assert type(new_config) is type(config)
            assert TestFrozenDict.is_dict(new_config.dict())
            assert TestFrozenDict.is_frozen(new_config.data)

            new_config = config.copy(update={})
            assert type(new_config) is type(config)
            assert TestFrozenDict.is_dict(new_config.dict())
            assert TestFrozenDict.is_frozen(new_config.data)

            new_config = config.copy(update=set_times(config))
            assert type(new_config) is type(config)
            assert TestFrozenDict.is_dict(new_config.dict())
            assert TestFrozenDict.is_frozen(new_config.data)

            new_config = config.copy(update=set_times(config))
            assert TestFrozenDict.is_dict(new_config.dict())
            assert TestFrozenDict.is_frozen(new_config.data)

            general_config = config.get("general")
            assert type(general_config) is BasicConfig

            general_config_dict = config.get_as_dict("general")
            assert type(general_config_dict) is dict

            assert general_config_dict == general_config.dict()

            frozen = config["general"]
            assert type(frozen) is frozendict.frozendict


class TestGeneralBehaviour:
    def test_config_model_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, ParsedConfig)

    def test_nested_mappings_become_basic_config(
        self, package_main_config_without_validation
    ):
        def nested_mappings_are_basic_config(obj, custom_type):
            rtn = isinstance(obj, custom_type)
            if not rtn:
                raise RuntimeError(f"{type(obj)}")
            for v in obj.values():
                if not rtn:
                    break
                if not hasattr(v, "items"):
                    continue
                rtn = rtn and nested_mappings_are_basic_config(v, custom_type)
            return rtn

        assert nested_mappings_are_basic_config(
            package_main_config_without_validation, (BasicConfig, frozendict.frozendict)
        )
        assert nested_mappings_are_basic_config(
            package_main_config_without_validation.dict(), dict
        )
        assert nested_mappings_are_basic_config(
            package_main_config_without_validation.copy(),
            (BasicConfig, frozendict.frozendict),
        )
        assert nested_mappings_are_basic_config(
            package_main_config_without_validation.get("general"),
            (BasicConfig, frozendict.frozendict),
        )

        assert nested_mappings_are_basic_config(
            package_main_config_without_validation.get_as_dict("general"), dict
        )

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
        assert isinstance(config_source_file_path, Path)
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

    def test_partly_resolved_keys_are_basic_config(self, parsed_config_with_task):

        assert isinstance(parsed_config_with_task, BasicConfig)
        assert isinstance(parsed_config_with_task["task"], frozendict.frozendict)
        assert isinstance(parsed_config_with_task["task"]["forecast"]["wrapper"], str)

        assert isinstance(parsed_config_with_task.get("task"), BaseMapping)
        assert isinstance(parsed_config_with_task.get_as_dict("task"), dict)

        assert isinstance(parsed_config_with_task.get("task.forecast.wrapper"), str)

        task_cfg = parsed_config_with_task["task"]
        assert isinstance(task_cfg, frozendict.frozendict)

        # access read-only should work
        assert task_cfg["forecast"]["wrapper"] == "time"

        # modify readonly should return an error
        with pytest.raises(TypeError):
            task_cfg["forecast"]["wrapper"] = "new_time_wont_work"

        assert parsed_config_with_task.get("task.forecast.wrapper") == "time"

        config = parsed_config_with_task.get("task")
        task_dict = config.dict()
        assert task_dict["forecast"]["wrapper"] == "time"

        assert parsed_config_with_task.get("task.forecast.wrapper") == "time"
        assert parsed_config_with_task.get_as_dict("task.forecast.wrapper") == "time"
        assert parsed_config_with_task.get("task").get("forecast.wrapper") == "time"
        assert parsed_config_with_task.get("task.forecast").get("wrapper") == "time"


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


@pytest.fixture
def valid_config_include_section():
    files_under_include_dir = list(ConfigParserDefaults.PACKAGE_INCLUDE_DIR.glob("*"))

    inc_files = [fpath.as_posix() for fpath in files_under_include_dir if fpath.is_file()]
    include_section = {}
    for inc_file in inc_files:
        with open(inc_file, mode="rb") as fh:
            inc_file_content = tomli.load(fh)
        for key in inc_file_content:
            include_section.update({key: inc_file})
    return include_section


@pytest.fixture
def raw_config_with_include_section(minimal_raw_config, valid_config_include_section):
    raw_config = minimal_raw_config.copy()
    include_section = valid_config_include_section.copy()
    raw_config["include"] = include_section
    return raw_config


@pytest.fixture
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
            match="also detected in its parent section's schema",
        ):
            _ = ParsedConfig(raw_config, json_schema=schema)


class TestConfigPaths:
    def test_show_paths(self):
        ConfigPaths.print(ConfigParserDefaults.PACKAGE_CONFIG_PATH, "atos_bologna")

    def test_match_new_path(self, tmp_test_data_dir):
        test_path1 = tmp_test_data_dir / "test" / "config_files"
        os.makedirs(test_path1, exist_ok=True)

        ConfigPaths.CONFIG_DATA_SEARCHPATHS = [
            tmp_test_data_dir / "test",
            ConfigParserDefaults.DATA_DIRECTORY,
        ]

        path = ConfigPaths.path_from_subpath("config_files")
        assert path == test_path1

        path = ConfigPaths.path_from_subpath("input")
        assert path == ConfigParserDefaults.DATA_DIRECTORY / "input"

    def test_multiple_paths(self, tmp_test_data_dir):
        test_path1 = tmp_test_data_dir / "test" / "config_files"
        test_path2 = tmp_test_data_dir / "test" / "test" / "config_files"
        os.makedirs(test_path1, exist_ok=True)
        os.makedirs(test_path2, exist_ok=True)

        ConfigPaths.CONFIG_DATA_SEARCHPATHS = [
            tmp_test_data_dir / "test",
            tmp_test_data_dir / "test" / "test",
            ConfigParserDefaults.DATA_DIRECTORY,
        ]

        path = ConfigPaths.path_from_subpath("config_files")
        assert path == test_path1


class TestConfigExpand:
    def test_expand_config(self, parsed_config_with_included_sections):
        """Test function for expanding macros."""
        config = parsed_config_with_included_sections
        config = config.copy(
            update={
                "general": {"case": "@CSC@", "csc": "AROME"},
                "macros": {"case": {"gen_macros": ["general.csc"]}},
            }
        )
        _config = config.expand_macros()
        assert _config["general.case"] == "AROME"


@pytest.fixture
def competing_schemas(tmp_test_data_dir):
    """Two schema directories that both define 'competing_section_schema.json'.

    The high-priority schema requires the key 'from_high'; the low-priority schema
    requires 'from_low'.  Both set additionalProperties=False so that only the
    correct key is accepted, making it easy to identify which schema was applied.
    """
    dir_high = tmp_test_data_dir / "high_priority_schemas"
    dir_low = tmp_test_data_dir / "low_priority_schemas"
    dir_high.mkdir()
    dir_low.mkdir()

    schema_high = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "additionalProperties": False,
        "required": ["from_high"],
        "properties": {"from_high": {"type": "string"}},
    }
    schema_low = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "additionalProperties": False,
        "required": ["from_low"],
        "properties": {"from_low": {"type": "string"}},
    }
    (dir_high / "competing_section_schema.json").write_text(json.dumps(schema_high))
    (dir_low / "competing_section_schema.json").write_text(json.dumps(schema_low))
    return dir_high, dir_low


class TestSchemasSearchPaths:
    """Tests for multi-path JSON schema search (ConfigPaths.SCHEMAS_SEARCHPATHS)."""

    STRICT_PLUGIN_SCHEMA = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "title": "PluginSection",
        "type": "object",
        "additionalProperties": False,
        "required": ["plugin_key"],
        "properties": {"plugin_key": {"type": "string"}},
    }

    @pytest.fixture(autouse=True)
    def restore_schemas_searchpaths(self):
        """Save and restore SCHEMAS_SEARCHPATHS around each test."""
        original = ConfigPaths.SCHEMAS_SEARCHPATHS[:]
        yield None
        ConfigPaths.SCHEMAS_SEARCHPATHS[:] = original

    @pytest.fixture
    def plugin_schemas_dir(self, tmp_test_data_dir):
        """Temporary schema directory containing a strict 'plugin' schema."""
        schemas_dir = tmp_test_data_dir / "plugin_schemas"
        schemas_dir.mkdir()
        (schemas_dir / "plugin_section_schema.json").write_text(
            json.dumps(self.STRICT_PLUGIN_SCHEMA)
        )
        return schemas_dir

    def test_schemas_searchpaths_is_mutable_list(self):
        assert isinstance(ConfigPaths.SCHEMAS_SEARCHPATHS, list)

    def test_tactus_schemas_directory_in_searchpaths(self):
        assert ConfigParserDefaults.SCHEMAS_DIRECTORY in ConfigPaths.SCHEMAS_SEARCHPATHS

    def test_schemas_dir_can_be_prepended_to_searchpaths(self, plugin_schemas_dir):
        original_length = len(ConfigPaths.SCHEMAS_SEARCHPATHS)
        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, plugin_schemas_dir)
        assert ConfigPaths.SCHEMAS_SEARCHPATHS[0] == plugin_schemas_dir
        assert len(ConfigPaths.SCHEMAS_SEARCHPATHS) == original_length + 1

    def test_schema_from_extra_dir_validates_section_without_includes(
        self, plugin_schemas_dir, minimal_raw_config
    ):
        """A section schema only present in an extra dir is applied to config validation."""
        raw_config = minimal_raw_config.copy()
        raw_config["plugin"] = {"plugin_key": "hello"}

        # Without extra dir: plugin has no strict schema, so any content is allowed.
        config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        assert "plugin" in config

        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, plugin_schemas_dir)

        # With extra dir: strict schema is enforced - valid data passes.
        config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        assert config["plugin.plugin_key"] == "hello"

        # With extra dir: data that violates the strict schema is rejected.
        raw_config["plugin"] = {"wrong_key": "oops"}
        with pytest.raises(ConfigFileValidationError):
            ParsedConfig(
                raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
            )

    def test_schema_from_extra_dir_validates_include_section(
        self, plugin_schemas_dir, minimal_raw_config, tmp_test_data_dir
    ):
        """A schema from an extra dir is picked up and enforced for [include] sections."""
        include_file = tmp_test_data_dir / "plugin.toml"
        raw_config = minimal_raw_config.copy()
        raw_config["include"] = {"plugin": str(include_file)}

        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, plugin_schemas_dir)

        # Valid include file: passes strict schema from extra dir.
        include_file.write_text('[plugin]\nplugin_key = "hello"\n')
        config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        assert config["plugin.plugin_key"] == "hello"

        # Include file that violates the strict schema raises.
        include_file.write_text('[plugin]\nwrong_key = "oops"\n')
        with pytest.raises(ConfigFileValidationError):
            ParsedConfig(
                raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
            )

    def test_first_searchpath_wins_for_include_section(
        self, competing_schemas, minimal_raw_config, tmp_test_data_dir
    ):
        """First dir in SCHEMAS_SEARCHPATHS that has a matching schema wins for [include]."""
        dir_high, dir_low = competing_schemas

        # Include file satisfies the high-priority schema (has 'from_high').
        include_file = tmp_test_data_dir / "competing.toml"
        include_file.write_text('[competing]\nfrom_high = "yes"\n')

        raw_config = minimal_raw_config.copy()
        raw_config["include"] = {"competing": str(include_file)}

        # dir_high at index 0 -> its schema is applied -> config with 'from_high' is valid.
        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, dir_low)
        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, dir_high)
        config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        assert "competing" in config

    def test_first_searchpath_wins_for_section_without_includes(
        self, competing_schemas, minimal_raw_config
    ):
        """First dir in SCHEMAS_SEARCHPATHS wins when applying schemas to inline sections."""
        dir_high, dir_low = competing_schemas

        raw_config = minimal_raw_config.copy()
        # Config satisfies the high-priority schema (has 'from_high', not 'from_low').
        raw_config["competing"] = {"from_high": "yes"}

        # dir_high at index 0 -> its schema is applied -> config with 'from_high' is valid.
        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, dir_low)
        ConfigPaths.SCHEMAS_SEARCHPATHS.insert(0, dir_high)
        config = ParsedConfig(
            raw_config, json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
        )
        assert "competing" in config

    def test_backward_compatible_with_single_path_as_schemas_path(
        self, minimal_raw_config
    ):
        """_expand_config_include_section still accepts a single Path for schemas_path."""
        from tactus.config_parser import _expand_config_include_section

        result_config, _ = _expand_config_include_section(
            raw_config=dict(minimal_raw_config),
            json_schema={},
            schemas_path=ConfigParserDefaults.SCHEMAS_DIRECTORY,
        )
        assert result_config is not None
