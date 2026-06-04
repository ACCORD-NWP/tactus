"""Discover tasks."""

import contextlib
import importlib
import inspect
import json
import os
import pkgutil
import sys
import types
from pathlib import Path

from ..logs import LoggerHandlers, logger
from ..os_utils import tactusmakedirs
from ..plugin import TactusPluginRegistry, TactusPluginRegistryFromConfig
from ..toolbox import Platform
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
            logger.error("Could not load {}: {}", fullname, repr(exc))
            raise RuntimeError("Failed to load module") from exc
        yield fullname, mod


def _task_index_file(config):
    """Defines the task index file.

    Args:
        config (ConfigParse): Config

    Returns:
        task_index_file (str): Full path to task index_file

    """
    task_index_file_path = Platform(config).get_system_value("casedir")
    return Path(task_index_file_path) / "tasks_index.json"


def load_task_index(config):
    """Load a task index file.

    Args:
        config (ConfigParse): Config

    Returns:
        known_types(dict): Dict of known tasks, and their location

    """
    task_index_file = _task_index_file(config)

    if os.path.isfile(task_index_file):
        logger.info("Read task index from {}", task_index_file)
        with open(task_index_file, "r", encoding="utf-8") as infile:
            known_types = json.load(infile)
    else:
        logger.info("Create task index file {}", task_index_file)
        known_types = create_task_index(config)

    return known_types


def create_task_index(config):
    """Create a task index file.

    Args:
        config (ConfigParse): Config

    Returns:
        known_types(dict): Dict of known tasks, and their location

    """
    task_index_file = _task_index_file(config)

    reg = TactusPluginRegistryFromConfig(config)
    known_types = {k: str(v).split("'")[1] for k, v in available_tasks(reg).items()}

    task_index_file_dir = os.path.dirname(task_index_file)
    unix_group = config.get("platform.unix_group")
    tactusmakedirs(task_index_file_dir, unixgroup=unix_group)
    with open(task_index_file, mode="w", encoding="utf8") as outfile:
        json.dump(known_types, outfile, indent=True)
    logger.info("Stored task index in {}", task_index_file)

    return known_types


def get_task(name, config) -> Task:
    """Create a `tactus.tasks.Task` object from configuration.

    Args:
        name (_type_): _description_
        config (_type_): _description_

    Returns:
        Task: The task object with name `name`. The task object has to be a subclass
        of Task to be retrievable.

    Raises:
        NotImplementedError: If task `name` is not amongst the known task names.
    """
    with contextlib.suppress(KeyError):
        # loglevel may have been overridden, e.g., via ECFLOW UI
        logger.configure(
            handlers=LoggerHandlers(default_level=config["general.loglevel"])
        )
        logger.debug("Logger reset to level {}", config["general.loglevel"])
    known_types = load_task_index(config)
    try:
        cls = known_types[name.lower()]
    except KeyError:
        known_types = create_task_index(config)
        try:
            cls = known_types[name.lower()]
        except KeyError as error:
            raise NotImplementedError(f'Task "{name}" not implemented') from error

    if isinstance(cls, str):
        module_path, class_name = cls.rsplit(".", 1)
        module = importlib.import_module(module_path)
        cls = getattr(module, class_name)

    return cls(config)


def available_tasks(reg: TactusPluginRegistry):
    """Create a list of available tasks.

    Args:
        reg (TactusPluginRegistry): tactus plugin registry

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
