#!/usr/bin/env python3
"""Unit tests for commands_functions.py."""
import filecmp
import os
from argparse import ArgumentParser
from pathlib import Path

import pytest

from deode.commands_functions import (
    namelist_convert,
    namelist_format,
    namelist_integrate,
    show_namelist,
)
from deode.os_utils import resolve_path_relative_to_package


@pytest.fixture()
def set_arg():
    arg = ArgumentParser()
    arg.deode_home = None
    arg.namelist_type = "master"
    arg.namelist = "forecast"
    arg.namelist_name = None
    arg.domain = "test"
    arg.substitute = False
    return arg


@pytest.fixture()
def nlint_arg(tmp_directory):
    arg = ArgumentParser()
    arg.deode_home = None
    arg.namelist = [
        resolve_path_relative_to_package(
            Path("deode/data/namelists/unit_testing/nl_master_integrate")
        )
    ]
    arg.yaml = resolve_path_relative_to_package(
        Path("deode/data/namelists/unit_testing/nl_master_base.yml")
    )
    arg.tag = "nl_master_base"
    arg.output = f"{tmp_directory}/nl_master_integrated.yml"
    arg.domain = "test"
    return arg


@pytest.mark.parametrize(
    "param",
    [
        {
            "config": {
                "general": {"accept_static_namelists": False},
            },
            "path": "test1",
            "clean": False,
        },
        {
            "config": {
                "general": {"accept_static_namelists": True},
                "system": {"namelists": "to-be-replaced"},
            },
            "path": "test2",
            "clean": True,
        },
        {
            "config": {
                "general": {"accept_static_namelists": True},
                "system": {"namelists": "to-be-replaced"},
            },
            "path": "test3",
            "clean": False,
        },
    ],
)
def test_show_namelist(set_arg, default_config, param, tmp_directory):
    update = param["config"]
    pth = param["path"]
    outpath = f"{tmp_directory}/{pth}"
    if "system" in update and "namelists" in update["system"]:
        update["system"]["namelists"] = outpath
    update["fullpos"] = {"selection": {"test": ["master_selection_AROME"]}}
    config = default_config.copy(update=update)

    prev_cwd = Path.cwd()
    os.makedirs(outpath, mode=0o1777, exist_ok=True)
    os.chdir(outpath)
    show_namelist(set_arg, config)
    os.chdir(prev_cwd)
    assert os.path.isfile(f"{outpath}/namelist_master_forecast")
    assert os.path.isfile(f"{outpath}/xxt00000000")
    assert os.path.isfile(f"{outpath}/xxtddddhh00")
    if param["clean"]:
        os.remove(f"{outpath}/xxt00000000")
        os.remove(f"{outpath}/xxtddddhh00")


def test_namelist_integrate(nlint_arg, default_config):
    if os.path.exists(nlint_arg.output):
        os.remove(nlint_arg.output)
    namelist_integrate(nlint_arg, default_config)
    assert os.path.isfile(nlint_arg.output)


@pytest.fixture()
def nlconyml_arg(tmp_directory):
    arg = ArgumentParser()
    arg.namelist = resolve_path_relative_to_package(
        Path("deode/data/namelists/unit_testing/nl_master_base.yml")
    )
    arg.output = f"{tmp_directory}/nl_master_base.49t2.yml"
    arg.from_cycle = "CY48t2"
    arg.to_cycle = "CY49t2"
    arg.format = "yaml"
    arg.output_reference = resolve_path_relative_to_package(
        Path("deode/data/namelists/unit_testing/reference/nl_master_base.49t2.yml")
    )
    return arg


def test_namelist_convert_yml(nlconyml_arg, default_config):
    if os.path.exists(nlconyml_arg.output):
        os.remove(nlconyml_arg.output)

    namelist_convert(nlconyml_arg, default_config)

    assert os.path.isfile(nlconyml_arg.output)
    assert filecmp.cmp(nlconyml_arg.output_reference, nlconyml_arg.output)


@pytest.fixture()
def nlconftn_arg(tmp_directory):
    arg = ArgumentParser()
    arg.namelist = str(
        resolve_path_relative_to_package(
            Path("deode/data/namelists/unit_testing/nl_master_base")
        )
    )
    arg.output = f"{tmp_directory}/nl_master_base.49t2"
    arg.from_cycle = "CY48t2"
    arg.to_cycle = "CY49t2"
    arg.format = "ftn"
    arg.output_reference = (
        "deode/data/namelists/unit_testing/reference/nl_master_base.49t2"
    )
    return arg


def test_namelist_convert_ftn(nlconftn_arg, default_config):
    if os.path.exists(nlconftn_arg.output):
        os.remove(nlconftn_arg.output)

    namelist_convert(nlconftn_arg, default_config)
    assert os.path.isfile(nlconftn_arg.output)
    assert filecmp.cmp(nlconftn_arg.output_reference, nlconftn_arg.output)


@pytest.fixture()
def nlformatyml_arg(tmp_directory):
    arg = ArgumentParser()
    arg.namelist = resolve_path_relative_to_package(
        Path("deode/data/namelists/unit_testing/nl_master_base.yml")
    )
    arg.output = f"{tmp_directory}/nl_master_base.format.yml"
    arg.format = "yaml"
    arg.output_reference = resolve_path_relative_to_package(
        Path("deode/data/namelists/unit_testing/reference/nl_master_base.format.yml")
    )
    return arg


def test_namelist_format_yml(nlformatyml_arg, default_config):
    if os.path.exists(nlformatyml_arg.output):
        os.remove(nlformatyml_arg.output)

    namelist_format(nlformatyml_arg, default_config)

    assert os.path.isfile(nlformatyml_arg.output)
    assert filecmp.cmp(nlformatyml_arg.output_reference, nlformatyml_arg.output)


@pytest.fixture()
def nlformatftn_arg(tmp_directory):
    arg = ArgumentParser()
    arg.namelist = str(
        resolve_path_relative_to_package(
            Path("deode/data/namelists/unit_testing/nl_master_base")
        )
    )
    arg.output = f"{tmp_directory}/nl_master_base.format"
    arg.format = "ftn"
    arg.output_reference = resolve_path_relative_to_package(
        Path("deode/data/namelists/unit_testing/reference/nl_master_base.format")
    )
    return arg


def test_namelist_format_ftn(nlformatftn_arg, default_config):
    if os.path.exists(nlformatftn_arg.output):
        os.remove(nlformatftn_arg.output)

    namelist_format(nlformatftn_arg, default_config)
    assert os.path.isfile(nlformatftn_arg.output)
    assert filecmp.cmp(nlformatftn_arg.output_reference, nlformatftn_arg.output)


if __name__ == "__main__":
    pytest.main()
