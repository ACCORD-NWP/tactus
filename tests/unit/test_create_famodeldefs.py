import re

from tactus.config_parser import ConfigParserDefaults
from tactus.tasks.prep_run import PrepRun

FA_MODEL_SOURCE_YML = (
    ConfigParserDefaults.DATA_DIRECTORY / "eccodes" / "destineFaModelSource.yml"
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
    yaml_path = eccodes_dir / "destineFaModelSource.yml"
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
            assert line.startswith(
                "'default' = {"
            ), f"Line does not start with expected prefix: {line}"
            assert line.endswith("}"), f"Line does not end with expected suffix: {line}"
        else:
            assert line_re.match(line), f"Line does not match format: {line}"
