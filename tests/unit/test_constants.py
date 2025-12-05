#!/usr/bin/env python3
"""Tests for the QuasiConstant class and related elements."""
import uuid

import pytest

from tactus.aux_types import QuasiConstant


@pytest.fixture()
def constants():
    class ConstantDefinitions(QuasiConstant):
        FOO = "foo"
        SEQUENCE = [0, 1, 2, 3, 4]
        SET = set(SEQUENCE)
        MAPPING = {"foo": {"bar": {"baz": "qux"}}}

    return ConstantDefinitions


def test_cannot_instanciate_quasi_constant():
    class MyConstants(QuasiConstant):
        FOO = "bar"

    with pytest.raises(TypeError, match=f"Cannot instanciate {MyConstants.__name__}"):
        _ = MyConstants()


def test_cannot_reassign_existing_attribute(constants):
    for attr, value in constants:
        with pytest.raises(AttributeError, match="Cannot assign attribute"):
            setattr(constants, attr, str(value))


def test_cannot_add_new_attribute(constants):
    with pytest.raises(AttributeError, match="Cannot assign attribute"):
        setattr(constants, uuid.uuid4().hex, uuid.uuid4())


def test_sequences_becomes_tuple(constants):
    assert isinstance(constants.SEQUENCE, tuple)


def test_sets_become_frozen(constants):
    assert isinstance(constants.SET, frozenset)


def test_mappings_become_locked(constants):
    with pytest.raises(TypeError, match="object does not support item assignment"):
        constants.MAPPING["foo"]["bar"]["baz"] = "fred"


def test_can_convert_to_dict(constants):
    constants_as_dict = constants.dict()
    assert isinstance(constants_as_dict, dict)
    for attr, value in constants:
        assert constants_as_dict[attr] == value


def test_print_has_info_about_attributes(constants):
    str_form = str(constants)
    for attr, _ in constants:
        assert f'"{attr}": ' in str_form
