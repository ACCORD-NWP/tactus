"""unit tests for creategrib."""
import os
from pathlib import Path

import pytest

from deode.derived_variables import set_times
from deode.tasks.creategrib import CreateGrib
from deode.toolbox import Platform


@pytest.fixture(scope="module")
def basic_config(tmp_directory, default_config):
    config = default_config
    scratch = str(tmp_directory)
    config = config.copy(update=set_times(config))
    update = {
        "platform": {"scratch": scratch},
        "task": {"CreateGrib": {"conversions": ["surfex", "history"]}},
    }
    config = config.copy(update=update)
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
def test_convert2grib(basic_config, filetype, tmp_directory):
    cg = CreateGrib(basic_config)
    cg.wrapper = "echo"
    cg.gl = "gl"

    prev_cwd = Path.cwd()
    os.chdir(tmp_directory)
    output_list = cg.create_list(
        cg.file_templates[filetype]["archive"], cg.output_settings[filetype]
    )
    infile = output_list[cg.basetime]
    inpath = os.path.dirname(infile)
    os.makedirs(inpath, exist_ok=True)
    Path(infile).touch()
    cg.convert2grib(infile, "foo", filetype)
    os.chdir(prev_cwd)
