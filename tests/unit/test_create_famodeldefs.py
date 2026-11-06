import re
from unittest.mock import patch

import yaml

from tactus.config_parser import ConfigParserDefaults
from tactus.tasks.prep_run import PrepRun

FA_MODEL_SOURCE_YML = (
    ConfigParserDefaults.DATA_DIRECTORY / "eccodes" / "FaModelSource.yml"
)


def test_create_famodeldefs_output_format(tmp_path, default_config):
    # Copy the faModelSource.yml to the expected location
    config = default_config.copy(
        update={
            "suite_control": {"do_cleaning": False},
            "general": {
                "times": {
                    "basetime": "2023-01-01T00:00:00Z",
                    "validtime": "2023-01-01T01:00:00Z",
                }
            },
        }
    )
    eccodes_dir = tmp_path / "eccodes"
    eccodes_dir.mkdir()
    yaml_path = eccodes_dir / "FaModelSource.yml"
    yaml_path.write_text(FA_MODEL_SOURCE_YML.read_text())
    output_dir = tmp_path / "output"
    output_dir.mkdir()
    prep = PrepRun(config)
    prep.create_famodeldefs(output_dir)
    output_file = output_dir / "faModelName.def"
    assert output_file.exists()
    lines = output_file.read_text().splitlines()
    assert lines, "Output file is empty"
    line_re = re.compile(r"^'.+'\s*=\s*\{.*;\s*\}$")
    for line in lines:
        if line.startswith("'default'"):
            assert line.startswith("'default' = {"), (
                f"Line does not start with expected prefix: {line}"
            )
            assert line.endswith("}"), f"Line does not end with expected suffix: {line}"
        else:
            assert line_re.match(line), f"Line does not match format: {line}"


def test_create_famodeldefs_productdefinitiontemplatenumber_first(
    tmp_path, default_config
):
    config = default_config.copy(
        update={
            "suite_control": {"do_cleaning": False},
            "general": {
                "times": {
                    "basetime": "2023-01-01T00:00:00Z",
                    "validtime": "2023-01-01T01:00:00Z",
                }
            },
            "eps": {"general": {"members": [0, 1]}},
        }
    )
    eccodes_dir = tmp_path / "eccodes"
    eccodes_dir.mkdir()
    yaml_path = eccodes_dir / "FaModelSource.yml"
    yaml_path.write_text(FA_MODEL_SOURCE_YML.read_text())
    output_dir = tmp_path / "output"
    output_dir.mkdir()
    prep = PrepRun(config)
    prep.create_famodeldefs(output_dir)
    output_file = output_dir / "faModelName.def"
    assert output_file.exists()
    lines = output_file.read_text().splitlines()
    assert lines, "Output file is empty"
    for line in lines:
        if "productDefinitionTemplateNumber" in line:
            content = line.split("{", 1)[1].strip()
            first_key = content.split("=", 1)[0].strip()
            assert first_key == "productDefinitionTemplateNumber"


def test_create_famodeldefs_strings_vs_integers(tmp_path, default_config):

    config = default_config.copy(
        update={
            "suite_control": {"do_cleaning": False},
            "general": {
                "times": {
                    "basetime": "2023-01-01T00:00:00Z",
                    "validtime": "2023-01-01T01:00:00Z",
                },
                "cycle": "CY48t3",
                "csc": "AROME",
                "famodel": "mock_model",
            },
            "eps": {"general": {"members": [0]}},
        }
    )

    eccodes_dir = tmp_path / "eccodes"
    eccodes_dir.mkdir(exist_ok=True)
    yaml_path = eccodes_dir / "FaModelSource.yml"

    mock_yaml_content = """
frameworks:
  mock_fw:
    int_key: 123
    string_key: "string_val"
cycles:
  CY48t3:
    cycle_int: 456
    cycle_string: "abc"
cscs:
  AROME:
    csc_int: 789
    csc_string: "xyz"
"""
    yaml_path.write_text(mock_yaml_content)

    output_dir = tmp_path / "output"
    output_dir.mkdir(exist_ok=True)
    prep = PrepRun(config)

    with patch(
        "tactus.tasks.prep_run.yaml.safe_load",
        return_value=yaml.safe_load(mock_yaml_content),
    ):
        prep.create_famodeldefs(output_dir)

    output_file = output_dir / "faModelName.def"
    assert output_file.exists()
    content = output_file.read_text()

    assert "int_key = 123;" in content
    assert "string_key = 'string_val';" in content
    assert "cycle_int = 456;" in content
    assert "cycle_string = 'abc';" in content
    assert "csc_int = 789;" in content
    assert "csc_string = 'xyz';" in content
