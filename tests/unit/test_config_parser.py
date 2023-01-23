#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import datetime
from pathlib import Path

import pytest
import tomlkit
from pandas.tseries.frequencies import to_offset
from pydantic import BaseModel
from pydantic.error_wrappers import ValidationError

from deode.config_parser import ParsedConfig
from deode.datetime_utils import as_datetime


@pytest.fixture()
def minimal_raw_config():
    return tomlkit.parse(
        """
        [general]
            data_rootdir = "."
            assimilation_times.list = ["2000-01-01T00:00"]
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
def tmp_test_data_dir(tmpdir_factory, minimal_raw_config):
    return Path(tmpdir_factory.mktemp("deode_test_rootdir"))


@pytest.fixture()
def config_path(minimal_raw_config, tmp_test_data_dir):
    config_path = tmp_test_data_dir / "config.toml"

    raw_config = minimal_raw_config.copy()
    general_section_update = f"""
        data_rootdir = "{tmp_test_data_dir.as_posix()}"
        outdir = "{tmp_test_data_dir.as_posix()}"
    """
    raw_config["general"].update(tomlkit.parse(general_section_update))

    with open(config_path, "w") as config_file:
        tomlkit.dump(raw_config, config_file)

    return config_path


class TestGeneralBehaviour:
    # pylint: disable=no-self-use

    def test_config_model_can_be_instantiated(self, minimal_parsed_config):
        assert isinstance(minimal_parsed_config, BaseModel)

    def test_config_recursive_attr_access(self, minimal_parsed_config):
        recursively_retrieved_value = getattr(
            minimal_parsed_config, "general.assimilation_times.list"
        )
        assert isinstance(recursively_retrieved_value, tuple)
        assert (
            recursively_retrieved_value
            is minimal_parsed_config.general.assimilation_times.list
        )

    def test_config_recursive_attr_access_task(self, parsed_config_with_task):
        with pytest.raises(AttributeError, match="object has no attribute 'forecast'"):
            # Since we still don't have a model for "task" in config_parser.py, it will be
            # returned as a dictionary. The line below should therefore fail with the
            # error specified above.
            # TODO: Remove this part of the test once we define a model for "task" in the
            #       config_parser.py file.
            _ = parsed_config_with_task.task.forecast.wapper
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
        with pytest.raises(TypeError, match="is immutable"):
            minimal_parsed_config.general = "foo"
        with pytest.raises(AttributeError, match="has no attribute 'foo'"):
            _ = minimal_parsed_config.foo
        with pytest.raises(TypeError, match="is immutable"):
            minimal_parsed_config.foo = "bar"

    def test_config_can_be_printed(self, raw_config_with_non_recognised_options):
        config = ParsedConfig.parse_obj(raw_config_with_non_recognised_options)
        _ = str(config)

    def test_parsed_config_passes_toml_readwrite_roundtrip(self, minimal_parsed_config):
        toml_dumps = minimal_parsed_config.dumps(style="toml", exclude_unset=False)
        reloaded_parsed_config = ParsedConfig.parse_obj(tomlkit.loads(toml_dumps))
        new_toml_dumps = reloaded_parsed_config.dumps(style="toml", exclude_unset=False)
        assert new_toml_dumps == toml_dumps

    def test_parsed_config_does_not_have_file_metadata_when_not_read_from_file(
        self, minimal_parsed_config
    ):
        config = minimal_parsed_config.copy()
        assert config.get_value("metadata.source_file_path") is None

    def test_parsed_config_retains_file_metadata_when_read_from_file(self, config_path):
        config = ParsedConfig.from_file(config_path)
        config_source_file_path = config.get_value("metadata.source_file_path")
        assert isinstance(config_source_file_path, Path)
        assert config_source_file_path == config_path


class TestValidators:
    # pylint: disable=no-self-use

    @pytest.mark.parametrize("picked_type", [int, float, bool, str])
    def test_validator_performs_simple_type_casting(
        self, minimal_raw_config, picked_type
    ):
        """Pick a config field that should be a string, just to check."""
        minimal_raw_config["domain"] = {"name": picked_type(1.0)}
        parsed_config = ParsedConfig.parse_obj(minimal_raw_config)
        assert isinstance(parsed_config.domain.name, str)

    def test_validator_works_with_pandas_offset_freq_string(self, minimal_raw_config):
        input_freqstr = "3H"
        minimal_raw_config["general"]["assimilation_times"][
            "cycle_length"
        ] = input_freqstr
        parsed_config = ParsedConfig.parse_obj(minimal_raw_config)
        validated_freqstr = parsed_config.general.assimilation_times.cycle_length
        assert to_offset(validated_freqstr) == to_offset(input_freqstr)

    def test_validator_works_with_parsed_path(self, minimal_raw_config):
        minimal_raw_config["general"]["data_rootdir"] = "~/foo"
        parsed_config = ParsedConfig.parse_obj(minimal_raw_config)
        validated_path = parsed_config.general.data_rootdir
        assert isinstance(validated_path, Path)
        assert validated_path == Path.home() / "foo"

    @pytest.mark.parametrize(
        "dt_input",
        ["20181010T00", "2018-10-10T00", "20181010T00:10", "1", datetime.datetime.now()],
    )
    def test_validator_works_with_input_datetime(self, dt_input, minimal_raw_config):
        minimal_raw_config["general"]["assimilation_times"]["list"] = [dt_input]
        parsed_config = ParsedConfig.parse_obj(minimal_raw_config)
        validated_value = parsed_config.general.assimilation_times.list[0]
        assert isinstance(validated_value, datetime.datetime)
        assert validated_value == as_datetime(dt_input)

    def test_parsing_complains_about_incompatible_type(self, minimal_raw_config):
        minimal_raw_config["general"]["assimilation_times"][
            "list"
        ] = datetime.datetime.now()
        with pytest.raises(ValidationError, match="value is not a valid (tuple|list)"):
            _ = ParsedConfig.parse_obj(minimal_raw_config)

    @pytest.mark.parametrize(
        ("start", "end", "dates_list"),
        [
            ("20000101T00", "20000101T00", ["20000101T00"]),
            ("20000101T00", None, ["20000101T00"]),
            (None, "20000101T00", ["20000101T00"]),
            ("20000101T00", None, None),
            (None, "20000101T00", None),
            (None, None, None),
        ],
    )
    def test_parsing_complains_about_incompatible_date_specs(
        self, minimal_raw_config, start, end, dates_list
    ):
        raw_config = minimal_raw_config.copy()

        new_config_assim_times = "[general.assimilation_times]"
        if start is not None:
            new_config_assim_times = f"""
                 {new_config_assim_times}
                    start = '{start}'
            """
        if end is not None:
            new_config_assim_times = f"""
                {new_config_assim_times}
                    end = '{end}'
            """
        if dates_list is not None:
            new_config_assim_times = f"""
                {new_config_assim_times}
                    list = {dates_list}
            """
        raw_config["general"]["assimilation_times"].update(
            tomlkit.parse(new_config_assim_times)
        )

        with pytest.raises(ValidationError, match="Must specify either only"):
            _ = ParsedConfig.parse_obj(raw_config)


if __name__ == "__main__":
    pytest.main()
