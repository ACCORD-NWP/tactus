"""Discover tasks."""

import contextlib
import importlib
import inspect
import os
import pkgutil
import sys
import types

from ..logs import LoggerHandlers, logger
from ..plugin import DeodePluginRegistry, DeodePluginRegistryFromConfig
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
    prefix = package.__name__ + ".tasks."

    logger.info("{} search path: {}", what.capitalize(), path)
    for _finder, mname, _ispkg in pkgutil.iter_modules(path):
        fullname = prefix + mname
        logger.info("Loading module {}", fullname)
        try:
            mod = importlib.import_module(fullname)
        except ImportError as exc:
            logger.warning("Could not load {}: {}", fullname, repr(exc))
            continue
        yield fullname, mod


def get_task(name, config) -> Task:
    """Create a `tactus.tasks.Task` object from configuration.

    Args:
        name (_type_): _description_
        config (_type_): _description_

    Raises:
        NotImplementedError: If task `name` is not amongst the known task names.

    Returns:
        Task: The task object with name `name`. The task object has to be a subclass
        of Task to be retrievable.
    """
    with contextlib.suppress(KeyError):
        # loglevel may have been overridden, e.g., via ECFLOW UI
        logger.configure(
            handlers=LoggerHandlers(default_level=config["general.loglevel"])
        )
        logger.debug("Logger reset to level {}", config["general.loglevel"])
    reg = DeodePluginRegistryFromConfig(config)
    known_types = available_tasks(reg)
    try:
        cls = known_types[name.lower()]
    except KeyError as error:
        raise NotImplementedError(f'Task "{name}" not implemented') from error

    return cls(config)


def available_tasks(reg: DeodePluginRegistry):
    """Create a list of available tasks.

    Args:
        reg (DeodePluginRegistry): Deode plugin registry

    Returns:
        known_types (list): Task objects

    """
    known_types = {}
    abstract_classes = ["pysurfexbase"]
    for plg in reg.plugins:
        if os.path.exists(plg.tasks_path):
            tasks = types.ModuleType(plg.name)
            tasks.__path__ = [str(plg.tasks_path)]
            sys.path.insert(0, str(plg.path))
            found_types = discover(tasks, Task)
            for ftype, cls in found_types.items():
                if ftype not in abstract_classes:
                    if ftype in known_types:
                        logger.warning("Overriding suite {}", ftype)
                    known_types[ftype] = cls
        else:
            logger.warning("Plug-in task {} not found", plg.tasks_path)
    return known_types


def discover(package, base):
    """Discover task classes.

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
    what = base.__name__

    def pred(x):
        return inspect.isclass(x) and issubclass(x, base) and x is not base

    discovered = {}
    for fullname, mod in discover_modules(package, what=what):
        for cname, cls in inspect.getmembers(mod, pred):
            tname = _get_name(cname, cls, what.lower())
            if cls.__module__ != fullname:
                logger.info(
                    "Skipping {} {} imported by {}", what.lower(), tname, fullname
                )
                continue
            if tname in discovered:
                logger.warning(
                    "{} type {} is defined more than once", what.capitalize(), tname
                )
                continue
            discovered[tname] = cls
    return discovered
