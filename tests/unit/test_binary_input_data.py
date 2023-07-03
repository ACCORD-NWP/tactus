"""Test binary input data to surfex commands."""
import contextlib
import json
import logging
import os
from pathlib import Path

import f90nml
import pytest

from deode.config_parser import ParsedConfig, read_raw_config_file
from deode.datetime_utils import as_datetime
from deode.tasks.sfx import InputDataFromNamelist
from deode.toolbox import Platform


@pytest.fixture(scope="module")
def raw_config():
    """Return a raw config common to all tasks."""
    base_raw_config = read_raw_config_file("deode/data/config_files/config.toml")
    return ParsedConfig.parse_obj(base_raw_config, json_schema={})


@pytest.fixture()
def binary_input_data():
    return json.load(
        open("deode/data/input/sfx_input_CY46h1.json", "r", encoding="utf-8")
    )


@contextlib.contextmanager
def working_directory(path):
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.fixture()
def f90ml_namelist(tmp_path_factory):
    nml = tmp_path_factory.getbasetemp() / "nml"
    nml_input = """
        &NAM_FRAC
            LECOSG = .True.
        /
        &NAM_IO_OFFLINE
            CSURF_FILETYPE = "FA"
        /
        &NAM_DATA_ISBA
            NTIME = 36
            CFNAM_ALBNIR_SOIL(1,1) = "filename_albnir_soil_1_0105"
            CFTYP_ALBNIR_SOIL(1,1) = "DIRTYP"
            CFNAM_ALBNIR_SOIL(20,1) = "filename_albnir_soil_20_0105"
            CFTYP_ALBNIR_SOIL(20,1) = "DIRTYP"
            CFNAM_ALBNIR_SOIL(2,2) = "filename_albnir_soil_2_0115"
            CFTYP_ALBNIR_SOIL(2,2) = "DIRTYP"
            CFNAM_ALBNIR_SOIL(20,36) = "filename_albnir_soil_20_1225"
            CFTYP_ALBNIR_SOIL(20,36) = "DIRTYP"
            CFNAM_ALBVIS_SOIL(1,1) = "filename_albvis_soil_1_0105"
            CFTYP_ALBVIS_SOIL(1,1) = "DIRTYP"
            CFNAM_ALBVIS_SOIL(20,1) = "filename_albvis_soil_20_0105"
            CFTYP_ALBVIS_SOIL(20,1) = "DIRTYP"
            CFNAM_ALBVIS_SOIL(2,2) = "filename_albvis_soil_2_0115"
            CFTYP_ALBVIS_SOIL(2,2) = "DIRTYP"
            CFNAM_ALBVIS_SOIL(20,36) = "filename_albvis_soil_20_1225"
            CFTYP_ALBVIS_SOIL(20,36) = "DIRTYP"
            CFNAM_H_TREE(1) = "filename_h_tree_1"
            CFTYP_H_TREE(1) = "DIRTYP"
            CFNAM_H_TREE(20) = "filename_h_tree_20"
            CFTYP_H_TREE(20) = "DIRTYP"
        /
        &NAM_COVER
            YCOVER = "ecosg_final_map"
            YCOVERFILETYPE = "DIRECT"
        /
        &NAM_ASSIM
            CASSIM_ISBA = "EKF"
            CFILE_FORMAT_LSM = "ASCII"
            CFILE_FORMAT_FG = "FA"
            LLINCHECK = .True.
            NENS_M = 16
        /
        &NAM_VAR
            NNCV(1) = 0
            NNCV(2) = 1
            NNCV(3) = 1
            NNCV(4) = 0
        /
        &NAM_ZS
            YZS = "gmted2010file"
            YZSFILETYPE = "DIRECT"
        /
        &NAM_PREP_SURF_ATM
            CFILE = "my_prep_file"
            CFILETYPE = "FA"
        /
    """
    with open(nml, mode="w", encoding="utf-8") as nml_file:
        nml_file.write(nml_input)
    return nml


def test_new_binary_input(raw_config, f90ml_namelist, binary_input_data):

    update = {
        "system": {"climdir": "/climdir"},
        "platform": {
            "first_guess_dir": "/fg",
            "albnir_soil_dir": "/albnir_soil_dir",
            "albnir_veg_dir": "/albnir_veg_dir",
            "albvis_soil_dir": "/albvis_soil_dir",
            "albvis_veg_dir": "/albvis_veg_dir",
            "lai_dir": "/lai_dir",
            "tree_height_dir": "/tree_height_dir",
            "ecoclimap_sg": "/ecoclimap",
            "ecoclimap_bin_dir": "/eco_bin",
            "ecosg_data_path": "/ecoclimap",
            "oi_coeffs_dir": "/oi",
            "ascat_dir": "/ascat",
            "gmted": "/gmted",
        },
    }
    config = raw_config
    config = config.copy(update=update)

    platform = Platform(config)
    basetime = as_datetime("2022-02-20T06:00:00Z")
    validtime = as_datetime("2022-02-20T06:00:00Z")

    with open(f90ml_namelist, mode="r", encoding="utf-8") as nml_fh:
        nml = f90nml.read(nml_fh)

    input_data = binary_input_data
    # PGD
    binary_data = InputDataFromNamelist(
        nml, input_data.copy(), "pgd", platform, basetime=basetime, validtime=validtime
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert (
        binary_data.data["filename_albnir_soil_2_0115.dir"]
        == "/albnir_soil_dir/ALB_SAT_NI_0115_c.dir"
    )
    assert (
        binary_data.data["filename_albnir_soil_20_1225.dir"]
        == "/albnir_soil_dir/ALB_SAT_NI_1225_c.dir"
    )
    assert (
        binary_data.data["filename_albvis_soil_2_0115.dir"]
        == "/albvis_soil_dir/ALB_SAT_VI_0115_c.dir"
    )
    assert binary_data.data["filename_h_tree_1.dir"] == "/tree_height_dir/new_ht_c.dir"
    assert binary_data.data["filename_h_tree_20.dir"] == "/tree_height_dir/new_ht_c.dir"
    assert binary_data.data["gmted2010file.dir"] == "/climdir/gmted2010.dir"

    # Prep
    input_data_copy = input_data.copy()
    input_data_copy["prep"]["NAM_PREP_SURF_ATM#CFILETYPE"]["FA"][
        "NAM_PREP_SURF_ATM#CFILE"
    ] = "@first_guess_dir@/MYFILE"
    binary_data = InputDataFromNamelist(
        nml, input_data_copy, "prep", platform, basetime=basetime, validtime=validtime
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert binary_data.data["my_prep_file"] == "/fg/MYFILE"

    # Offline "ecoclimapI_covers_param.bin": "@ecoclimap_bin_dir@/ecoclimapI_covers_param.bin",
    nml["NAM_FRAC"]["LECOSG"] = False
    binary_data = InputDataFromNamelist(
        nml,
        input_data.copy(),
        "forecast",
        platform,
        basetime=basetime,
        validtime=validtime,
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert (
        binary_data.data["ecoclimapI_covers_param.bin"]
        == "/eco_bin/ecoclimapI_covers_param.bin"
    )
