"""Define custom validators for Pydantic models.

Exports:
    validate_durations: Validate the format of a duration string against ISO8601.
    import_from_string: Validate if a string can be imported and return the
    imported object.
"""

import importlib
import sys
from types import ModuleType
from typing import Type, Union


def import_from_string(value: str) -> Union[ModuleType, Type]:
    """Validate if a string can be imported and return the imported object.

    Only supports importing objects specified with full module path, e.g.
    `module.submodule.object`.

    Args:
        value: The string to import.

    Raises:
        ImportError: If the module or object cannot be imported.
        TypeError: If the input is not a string.

    Returns:
        The imported object.
    """
    if not isinstance(value, str):
        raise TypeError("string required")

    module_name, _, object_name = value.rpartition(".")
    if module_name:
        # Check if the module is already imported
        if module_name in sys.modules:
            module = sys.modules[module_name]
        else:
            module = importlib.import_module(module_name)

        # Get the object from the module
        return getattr(module, object_name)
    raise ImportError(f"No module path found in string '{value}'")
