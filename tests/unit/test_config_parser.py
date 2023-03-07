#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import datetime
import itertools
from collections import namedtuple
from pathlib import Path

import pytest
import tomlkit

from deode.config_parser import ConfigFileValidationError, JsonSchema, ParsedConfig
from deode.datetime_utils import ISO_8601_TIME_DURATION_REGEX, as_datetime


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
    return ParsedConfig.parse_obj(minimal_raw_config)


@pytest.fixture()
def parsed_config_with_task(raw_config_with_task):
    return ParsedConfig.parse_obj(raw_config_with_task)


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
                "pattern": ISO_8601_TIME_DURATION_REGEX,
            },
        },
    }

    return schema


@pytest.fixture()
def tmp_test_data_dir(tmpdir_factory, minimal_raw_config):
    return Path(tmpdir_factory.mktemp("deode_test_rootdir"))


@pytest.fixture()
def config_path(minimal_raw_config, tmp_test_data_dir):
    config_path = tmp_test_data_dir / "config.toml"
    with open(config_path, "w") as config_file:
        tomlkit.dump(minimal_raw_config, config_file)
    return config_path


class TestGeneralBehaviour:
    # pylint: disable=no-self-use

    def test_config_model_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, ParsedConfig)

    def test_config_model_can_be_printed(self):
        parsed_config = ParsedConfig.parse_obj({}, json_schema={})
        _ = str(parsed_config)
        _ = repr(parsed_config)

    def test_json_schema_class_can_be_printed(self):
        json_schema = JsonSchema({})
        _ = str(json_schema)
        _ = repr(json_schema)

    def test_config_recursive_attr_access(self, minimal_parsed_config):
        recursively_retrieved_value = getattr(minimal_parsed_config, "general.times.list")
        assert isinstance(recursively_retrieved_value, tuple)
        assert recursively_retrieved_value is minimal_parsed_config.general.times.list

    def test_config_recursive_attr_access_task(self, parsed_config_with_task):
        with pytest.raises(AttributeError, match="object has no attribute 'foo'"):
            _ = parsed_config_with_task.task.forecast.foo
        recursively_retrieved_value = parsed_config_with_task.get_value(
            "task.forecast.wrapper"
        )
        assert recursively_retrieved_value == "time"

    def test_unrecognised_options_are_supported(
        self, raw_config_with_non_recognised_options
    ):
        raw_config = raw_config_with_non_recognised_options.copy()
        parsed_config = ParsedConfig.parse_obj(raw_config)
        extra_section = parsed_config.unrecognised_section_name
        assert extra_section

    def test_config_is_immutable(self, minimal_parsed_config):
        with pytest.raises(TypeError, match="cannot assign"):
            minimal_parsed_config.general = "foo"
        with pytest.raises(AttributeError, match="has no attribute 'foo'"):
            _ = minimal_parsed_config.foo
        with pytest.raises(TypeError, match="cannot assign"):
            minimal_parsed_config.foo = "bar"

    def test_config_can_be_printed(self, raw_config_with_non_recognised_options):
        config = ParsedConfig.parse_obj(raw_config_with_non_recognised_options)
        _ = str(config)

    def test_parsed_config_passes_toml_readwrite_roundtrip(self, minimal_parsed_config):
        toml_dumps = minimal_parsed_config.dumps(style="toml")
        reloaded_parsed_config = ParsedConfig.parse_obj(tomlkit.loads(toml_dumps))
        new_toml_dumps = reloaded_parsed_config.dumps(style="toml")
        assert new_toml_dumps == toml_dumps

    def test_parsed_config_does_not_have_file_metadata_when_not_read_from_file(
        self, minimal_parsed_config
    ):
        config = minimal_parsed_config.copy()
        with pytest.raises(AttributeError, match="has no attribute 'metadata'"):
            _ = config.get_value("metadata.source_file_path")

    @pytest.mark.parametrize("fmt", ["toml", "yaml", "json"])
    def test_can_read_configs_from_supported_file_formats(
        self, fmt, config_path, minimal_parsed_config
    ):
        new_config_path = config_path.parent / f"{config_path.stem}.{fmt}"
        with open(new_config_path, "w") as f:
            f.write(minimal_parsed_config.dumps(style=fmt))

        new_config_as_dict = ParsedConfig.from_file(new_config_path).dict()
        new_config_as_dict.pop("metadata")
        old_config_as_dict = minimal_parsed_config.dict()
        assert new_config_as_dict == old_config_as_dict

    def test_parsed_config_registers_file_metadata_when_read_from_file(self, config_path):
        config = ParsedConfig.from_file(config_path)
        config_source_file_path = config.get_value("metadata.source_file_path")
        assert Path(config_source_file_path) == Path(config_path)

    def test_can_modify_model_upon_copy(self, minimal_raw_config):
        raw_config = minimal_raw_config.copy()
        raw_config["general"].update({"loglevel": "INFO"})
        parsed_config = ParsedConfig.parse_obj(raw_config)

        original_value = parsed_config.get_value("general.loglevel")
        new_value = "DEBUG"
        new_parsed_config = parsed_config.copy(
            update={"general": {"loglevel": new_value}}
        )

        assert original_value != new_value
        assert parsed_config.get_value("general.times")
        assert new_parsed_config.get_value("general.times")
        assert (
            new_parsed_config.get_value("general.times").dict()
            == parsed_config.get_value("general.times").dict()
        )
        assert parsed_config.get_value("general.loglevel") == original_value
        assert new_parsed_config.get_value("general.loglevel") == new_value

    def test_can_modify_with_list_value_upon_model_copy(self, minimal_raw_config):
        raw_config = minimal_raw_config.copy()
        raw_config["general"].update({"loglevel": "INFO"})
        parsed_config = ParsedConfig.parse_obj(raw_config)

        original_value = parsed_config.get_value("general.times")
        new_value = [
            "2000-01-01T22:00:00Z",
            datetime.datetime.now(datetime.timezone.utc).isoformat(),
        ]
        new_parsed_config = parsed_config.copy(
            update={"general": {"times": {"list": new_value}}}
        )

        assert original_value != new_value
        assert parsed_config.get_value("general.loglevel") == "INFO"
        assert new_parsed_config.get_value("general.loglevel") == "INFO"
        assert parsed_config.get_value("general.times").dict() == original_value.dict()
        assert new_parsed_config.get_value("general.times.list") == tuple(new_value)


class TestValidators:
    # pylint: disable=no-self-use

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
        parsed_config = ParsedConfig.parse_obj(minimal_raw_config)
        validated_value = parsed_config.general.times.list[0]
        assert isinstance(validated_value, str)
        assert as_datetime(validated_value) == as_datetime(dt_input)

    def test_parsing_complains_about_incompatible_type(self, minimal_raw_config):
        minimal_raw_config["general"]["times"]["list"] = datetime.datetime.now()
        with pytest.raises(
            ConfigFileValidationError,
            match="must be array",
        ):
            _ = ParsedConfig.parse_obj(minimal_raw_config)

    @pytest.mark.parametrize(
        ("start", "end", "dates_list"),
        [
            ("2000-01-01T00:00", "2000-01-01T00:00", ["2000-01-01T00:00"]),
            ("2000-01-01T00", None, ["2000-01-01T00:00"]),
            (None, "20000101T00:00", ["20000101T00:00"]),
            ("2000-01-01T00:00", None, None),
            (None, "2000-01-01T00:00", None),
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
            _ = ParsedConfig.parse_obj(raw_config)


class TestPossibilityOfISO8601ComplianceEnforcement:
    # pylint: disable=no-self-use

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
            _ = ParsedConfig.parse_obj(
                raw_config, json_schema=json_schema_for_iso_8601_time_specs_tests
            )

        raw_config = {param_name_in_schema: tested_param.correct_value}
        _ = ParsedConfig.parse_obj(
            raw_config, json_schema=json_schema_for_iso_8601_time_specs_tests
        )
