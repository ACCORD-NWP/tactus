"""Discover tasks."""
import contextlib
import importlib
import inspect
import pkgutil

from .. import tasks
from ..logs import LoggerHandlers, logger
from .base import Task, _get_name


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
    prefix = package.__name__ + "."

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


def get_task(name, config):
    """Create a `deode.tasks.Task` object from configuration.

    Args:
        name (_type_): _description_
        config (_type_): _description_

    Raises:
        NotImplementedError: If task `name` is not amongst the known task names.

    Returns:
        _type_: _description_
    """
    with contextlib.suppress(KeyError):
        # loglevel may have been overridden, e.g., via ECFLOW UI
        logger.configure(
            handlers=LoggerHandlers(default_level=config["general.loglevel"])
        )
        logger.debug("Logger reset to level {}", config["general.loglevel"])

    known_types = available_tasks()
    try:
        cls = known_types[name.lower()]
    except KeyError as error:
        raise NotImplementedError(f'Task "{name}" not implemented') from error

    return cls(config)


def available_tasks():
    """Create a list of available tasks.

    Returns:
        known_types (list): Task objects

    """
    known_types = discover(tasks, Task, attrname="__type_name__")

    return known_types


def discover(package, base, attrname="__plugin_name__"):
    """Discover task classes.

    Plugin classes are discovered in a given namespace package, deriving from
    a given base class. The base class itself is ignored, as are classes
    imported from another module (based on ``cls.__module__``). Each discovered
    class is identified by a name that is either the value of attribute
    ``attrname`` if present, or deduced from the class name by changing it to
    lowercase and stripping the name of the base class, if it appears as a
    suffix.

    Args:
        package (types.ModuleType): Namespace package containing the plugins
        base (type): Base class for the plugins
        attrname (str): Name of the attribute that contains the name for the plugin

    Returns:
        (dict of str: type): Discovered plugin classes
    """
    what = base.__name__

    def pred(x):
        return inspect.isclass(x) and issubclass(x, base) and x is not base

    discovered = {}
    for fullname, mod in discover_modules(package, what=what):
        for cname, cls in inspect.getmembers(mod, pred):
            tname = _get_name(cname, cls, what.lower(), attrname=attrname)
            if cls.__module__ != fullname:
                logger.debug(
                    "Skipping {} %r imported by %r", what.lower(), tname, fullname
                )
                continue
            if tname in discovered:
                logger.warning(
                    "{} type %r is defined more than once", what.capitalize(), tname
                )
                continue
            discovered[tname] = cls
    return discovered
