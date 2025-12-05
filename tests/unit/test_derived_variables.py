#!/usr/bin/env python3
"""Unit tests for the derived_types generation module."""

import re

import pytest

from tactus.derived_variables import derived_variables, set_times


def test_set_times(default_config):
    config = default_config.copy(
        update={"general": {"times": {"end": "P1D", "start": "P2D"}}}
    )
    with pytest.raises(ValueError, match=re.escape("cannot be larger than")):
        config = set_times(config)


@pytest.fixture(scope="module")
def test_config(default_config):
    config = default_config.copy(update=set_times(default_config))
    config = config.copy(update={"domain": {"nimax": 49, "njmax": 69}})
    return config


@pytest.fixture(params=["linear", "quadratic", "cubic"])
def set_trunc(request):
    return request.param


@pytest.fixture(params=["linear", "quadratic", "cubic"])
def set_trunc_oro(request):
    return request.param


@pytest.fixture(params=["truncation", "spectral"])
def smoothing_method(request):
    return request.param


def test_truncation_settings(test_config, set_trunc, set_trunc_oro, smoothing_method):
    nsmax_map = {"linear": 39, "quadratic": 26, "cubic": 19}
    nmsmax_map = {"linear": 29, "quadratic": 19, "cubic": 14}
    xtrunc_map = {"linear": 2, "quadratic": 3, "cubic": 4}

    if smoothing_method == "spectral":
        lspsmoro_map = test_config["domain.spectral_smoothing_by_gridtype"]
        truth = {
            "lspsmoro": lspsmoro_map[set_trunc],
            "nsmax": nsmax_map[set_trunc],
            "nsmax_oro": nsmax_map[set_trunc],
            "nmsmax": nmsmax_map[set_trunc],
            "nmsmax_oro": nmsmax_map[set_trunc],
            "xtrunc": xtrunc_map[set_trunc],
        }
    elif smoothing_method == "truncation":
        truth = {
            "lspsmoro": False,
            "nsmax": nsmax_map[set_trunc],
            "nsmax_oro": nsmax_map[set_trunc_oro],
            "nmsmax": nmsmax_map[set_trunc],
            "nmsmax_oro": nmsmax_map[set_trunc_oro],
            "xtrunc": xtrunc_map[set_trunc],
        }
    else:
        raise NotImplementedError(
            f"Test for orographic_smoothing_method={smoothing_method} is not implemented"
        )

    config = test_config.copy(
        update={
            "domain": {
                "gridtype": set_trunc,
                "gridtype_oro": set_trunc_oro,
                "orographic_smoothing_method": smoothing_method,
            }
        }
    )

    update = derived_variables(config)

    subset = {
        "lspsmoro": update["domain"]["lspsmoro"],
        "nsmax": update["domain"]["nsmax"],
        "nmsmax": update["domain"]["nmsmax"],
        "nsmax_oro": update["domain"]["nsmax_oro"],
        "nmsmax_oro": update["domain"]["nmsmax_oro"],
        "xtrunc": update["domain"]["xtrunc"],
    }

    assert subset == truth
