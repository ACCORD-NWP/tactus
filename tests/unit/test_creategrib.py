"""unit tests for creategrib."""
import os
from pathlib import Path

import pytest

from deode.config_parser import ConfigParserDefaults, ParsedConfig
from deode.derived_variables import set_times
from deode.tasks.creategrib import CreateGrib
from deode.toolbox import Platform


@pytest.fixture(scope="module")
def tmpdir(tmp_path_factory):
    return tmp_path_factory.getbasetemp().as_posix()


@pytest.fixture(scope="module")
def basic_config(tmpdir):
    config = ParsedConfig.from_file(
        ConfigParserDefaults.PACKAGE_CONFIG_PATH,
        json_schema=ConfigParserDefaults.MAIN_CONFIG_JSON_SCHEMA,
    )
    scratch = str(tmpdir)
    config = config.copy(update=set_times(config))
    config = config.copy(update={"platform": {"scratch": scratch}})
    return config


def test_create_list(basic_config):
    filetype = "history"
    pf = Platform(basic_config)
    fname = pf.substitute(basic_config[f"file_templates.{filetype}.archive"])
    archive = pf.substitute(basic_config["system.archive"])

    cg = CreateGrib(basic_config)
    output_list = cg.create_list(
        cg.file_templates[filetype]["archive"], cg.output_settings[filetype]
    )

    assert output_list[cg.basetime] == f"{archive}/{fname}"


@pytest.mark.parametrize("filetype", ["surfex", "history"])
def test_convert2grib(basic_config, filetype):
    cg = CreateGrib(basic_config)
    cg.wrapper = "echo"
    cg.gl = "gl"

    output_list = cg.create_list(
        cg.file_templates[filetype]["archive"], cg.output_settings[filetype]
    )
    infile = output_list[cg.basetime]
    inpath = os.path.dirname(infile)
    os.makedirs(inpath, exist_ok=True)
    Path(infile).touch()
    cg.convert2grib(output_list[cg.basetime], "foo", filetype)
