#!/usr/bin/env python3
"""Aux types used in the package."""

import copy
import json
from collections.abc import Mapping, MutableMapping, MutableSequence, MutableSet
from functools import reduce
from operator import getitem
from typing import Any, Callable, Iterator, Literal, Optional, Union

import tomlkit
import yaml

from .general_utils import (
    get_empty_nested_defaultdict,
    merge_dicts,
    recursive_freeze,
    recursive_unfreeze,
)


class QuasiConstantMetaclass(type):
    """Metaclass to help making a class behave as if it had quasi-constant attributes."""

    def __init__(cls, *args, **kwargs):
        """Initialise and perform type conversions on attributes."""
        super().__init__(*args, **kwargs)
        for attr, value in cls:
            for original_type, conversion_function in cls.type_conversions.items():
                if isinstance(value, original_type):
                    super().__setattr__(attr, conversion_function(value))

    @property
    def type_conversions(cls):
        """Type conversions to be performed on the attributes."""
        return {
            MutableMapping: lambda x: recursive_freeze(obj=x),
            MutableSequence: tuple,
            MutableSet: frozenset,
        }

    def dict(cls):
        """Return a `dict` form of the instance, with nested instances also converted."""
        return {
            attr: value.dict() if isinstance(value, type(cls)) else value
            for attr, value in cls
        }

    def __setattr__(cls, _attr, _value):
        raise AttributeError(f"Cannot assign attribute of {cls.__name__} object.")

    def __iter__(cls):
        for attr, value in cls.__dict__.items():
            if not attr.startswith("_"):
                yield attr, value

    def __repr__(cls):
        str_dict = json.dumps(cls.dict(), indent=4, sort_keys=False, default=str)
        return f"{cls.__name__}({str_dict})"


class QuasiConstant(metaclass=QuasiConstantMetaclass):
    """Inheriting from this will make the class' attributes (almost) immutable."""

    def __new__(cls, *_args, **_kwargs):
        """Prevent instanciation. The class will be used for its class variables."""
        raise TypeError(f"Cannot instanciate {cls.__name__}.")


class BaseMapping(Mapping):
    """Immutable mapping that will serve as basis for all config-related classes."""

    def __init__(self, *args, **kwargs) -> None:
        """Initialise an instance the same way a `dict` is initialised."""
        self.data = dict(*args, **kwargs)

    @property
    def data(self):
        """Return the underlying data stored by the instance."""
        return getattr(self, "_data", None)

    @data.setter
    def data(self, new):
        """Set the value of the `data` property."""
        self._data = recursive_freeze(new)

    def dict(self):
        """Returns a dict deepcopy of the internal data.

        Returns:
            The dictionary representation
        """
        return recursive_unfreeze(self._data)

    def copy(
        self,
        update: Optional[Union[Mapping, Callable[[Mapping], Any]]] = None,
        validate=True,
    ):
        """Return a copy of the instance, optionally updated according to `update`."""
        if not update:
            return copy.deepcopy(self)
        data = self._data
        self._data = None
        new = copy.deepcopy(self)
        self._data = data
        new_data = merge_dicts(data, update, overwrite=True, remove_none=True)
        new.validated = False
        if validate:
            new.data = new_data
        else:
            BaseMapping.data.fset(new, new_data)
        return new

    def update(self, key: str, value):
        """Return a copy of the instance, with updated key=value according to argument."""
        key_tree = key.split(".")
        key_tree[-1] = {key_tree[-1]: value}
        update = reduce(lambda x, y: {y: x}, reversed(key_tree))
        return self.copy(update=update)

    def dumps(
        self,
        section="",
        style: Literal["toml", "json", "yaml"] = "toml",
        toml_formatting_function: Optional[Callable] = None,
    ):
        """Get a nicely printed version of the container's contents."""
        if section:
            section_tree = section.split(".")
            mapping = get_empty_nested_defaultdict()
            reduce(getitem, section_tree[:-1], mapping)[section_tree[-1]] = self[section]
        else:
            mapping = self

        # Sorting keys, as a json object is an unordered set of name/value pairs, so we
        # can't guarantee a particular order.
        rtn = json.dumps(mapping, indent=2, sort_keys=True, default=dict)
        if style == "toml":
            if toml_formatting_function is None:
                rtn = tomlkit.dumps(json.loads(rtn))
            else:
                rtn = toml_formatting_function(tomlkit.dumps(json.loads(rtn)))
        elif style == "yaml":
            rtn = yaml.dump(json.loads(rtn))

        return rtn

    def __repr__(self):
        return f"{self.__class__.__name__}({self.dumps(style='json')})"

    # Implement the abstract methods __getitem__, __iter__ and __len__ from from Mapping
    def __getitem__(self, item):
        """Get items from container.

        The behaviour is similar to a `dict`, except for the fact that
        `self["A.B.C.D. ..."]` will behave like `self["A"]["B"]["C"]["D"][...]`.

        Args:
            item (str): Item to be retrieved. Use dot-separated keys to retrieve a nested
                item in one go.

        Returns:
            Any: Value of the item. Dictionaries are returned as 'frozendict'
        """
        try:
            # Try regular getitem first in case "A.B. ... C" is actually a single key
            return getitem(self.data, item)
        except KeyError:
            return reduce(getitem, item.split("."), self.data)

    def __iter__(self) -> Iterator:
        return iter(self.data)

    def __len__(self) -> int:
        return len(self.data)
