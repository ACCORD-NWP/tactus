"""Discover suites."""
import importlib
import inspect
import pkgutil
import sys
import types

from ..logs import logger
from ..plugin import TactusPluginRegistry, TactusPluginRegistryFromConfig
from .base import SuiteDefinition, _get_name


def discover_modules(package, what="plugin"):
    """Discover plugin modules.

    Args:
        package (types.ModuleType): Namespace package containing the plugins
        what (str, optional): String describing what is supposed to be discovered.
                              Defaults to "plugin".

    Yields:
        tuple:
            str: Name of the imported module
            types.ModuleType: The imported module

    """
    path = package.__path__
    prefix = package.__name__ + ".suites."

    logger.debug("{} search path: {}", what.capitalize(), path)
    for _finder, mname, _ispkg in pkgutil.iter_modules(path):
        fullname = prefix + mname
        logger.debug("Loading module {}", fullname)
        try:
            mod = importlib.import_module(fullname)
        except ImportError as exc:
            logger.warning("Could not load {}: {}", fullname, repr(exc))
            continue
        yield fullname, mod


def get_suite(name, config):
    """Create a `tactus.suites.SuiteDefinition` object from configuration.

    Args:
        name (_type_): _description_
        config (_type_): _description_

    Raises:
        NotImplementedError: If SuiteDefinition `name` is not amongst
                             the known SuiteDefinition names.

    Returns:
        _type_: _description_
    """
    reg = TactusPluginRegistryFromConfig(config)
    known_types = available_suites(reg)
    try:
        cls = known_types[name.lower()]
    except KeyError as error:
        raise NotImplementedError(f'SuiteDefinition "{name}" not implemented.') from error

    return cls(config)


def available_suites(reg: TactusPluginRegistry):
    """Create a list of available tasks.

    Args:
        reg (TactusPluginRegistry): tactus plugin registry

    Returns:
        known_types (list): Suite objects

    """
    known_types = {}
    for plg in reg.plugins:
        if plg.suites_path.exists():
            suites = types.ModuleType(plg.name)
            suites.__path__ = [str(plg.suites_path)]
            sys.path.insert(0, str(plg.path))
            found_types = discover(suites, SuiteDefinition)
            for ftype, cls in found_types.items():
                if ftype in known_types:
                    logger.warning("Overriding suite {}", ftype)
                known_types[ftype] = cls
        else:
            logger.warning("Plug-in task {} not found", plg.suites_path)
    return known_types


def discover(package, base):
    """Discover SuiteDefinition classes.

    Plugin classes are discovered in a given namespace package, deriving from
    a given base class. The base class itself is ignored, as are classes
    imported from another module (based on ``cls.__module__``). Each discovered
    class is identified by the class name by changing it to
    lowercase and stripping the name of the base class, if it appears as a
    suffix.

    Args:
        package (types.ModuleType): Namespace package containing the plugins
        base (type): Base class for the plugins

    Returns:
        (dict of str: type): Discovered plugin classes
    """

    def pred(x):
        return inspect.isclass(x) and issubclass(x, base) and x is not base

    discovered = {}
    for fullname, mod in discover_modules(package):
        logger.debug("fullname={} mod={}", fullname, mod)
        for cname, cls in inspect.getmembers(mod, pred):
            tname = _get_name(cname, cls)
            logger.debug("tname={}", tname, cname)
            if cls.__module__ != fullname:
                logger.debug("Skipping {} imported by {}", tname, fullname)
                continue
            if tname in discovered:
                logger.warning("type {} is defined more than once", tname)
                continue
            discovered[tname] = cls
    return discovered
