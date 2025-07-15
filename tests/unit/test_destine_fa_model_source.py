import yaml

from deode.config_parser import ConfigParserDefaults

FA_MODEL_SOURCE_YML = (
    ConfigParserDefaults.DATA_DIRECTORY / "eccodes" / "destineFaModelSource.yml"
)


def get_allowed_cycles_and_cscs():
    schema = ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA
    general_props = schema["definitions"]["general"]["properties"]
    cycles = general_props["cycle"]["enum"]
    cscs = general_props["csc"]["enum"]
    return cycles, cscs


def test_destine_fa_model_source_required_entries():
    cycles, cscs = get_allowed_cycles_and_cscs()

    with open(FA_MODEL_SOURCE_YML) as f:
        data = yaml.safe_load(f)

    # Check cycles
    for cycle in cycles:
        assert cycle in data["cycles"], f"Missing cycle: {cycle}"

    # Check cscs
    for csc in cscs:
        assert csc in data["cscs"], f"Missing csc: {csc}"
