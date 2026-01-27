"""Test plugin fuctionality."""
import os
from pathlib import Path

import yaml

import tactus
from tactus.plugin import (
    TactusPlugin,
    TactusPluginRegistry,
    TactusPluginRegistryFromConfig,
    TactusPluginRegistryFromFile,
)
from tactus.suites.discover_suite import available_suites
from tactus.tasks.discover_task import available_tasks


def create_task_class(name, path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, mode="w", encoding="utf8") as fh:
        fh.write(
            f"from tactus.tasks.base import Task\nclass {name}(Task):\n    def __init__(self, config):\n        Task.__init__(self, config)\n"
        )


def create_suite_class(name, path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, mode="w", encoding="utf8") as fh:
        fh.write(
            f"from tactus.suites.base import SuiteDefinition\nclass {name}(SuiteDefinition):\n    def __init__(self, config):\n        SuiteDefinition.__init__(self, config)\n"
        )


def test_plugin(tmp_directory, default_config):
    """Simple plugin test."""
    tasks_dir = f"{tmp_directory}/extension/tasks"
    suites_dir = f"{tmp_directory}/extension/suites"
    reg_config_file = f"{tmp_directory}/plugins.yml"
    config = {"plugins": {"extension": tmp_directory}}
    with open(reg_config_file, mode="w", encoding="utf8") as fh:
        yaml.safe_dump(config, fh)

    reg = TactusPluginRegistry()
    for plg in reg.plugins:
        if plg.name == "tactus":
            assert Path(tactus.__path__[0]).parent / "tactus/tasks" == plg.tasks_path
            assert Path(tactus.__path__[0]).parent / "tactus/suites" == plg.suites_path
        elif plg.name == "extension":
            assert tasks_dir == plg.tasks_path
            assert suites_dir == plg.suites_path
        else:
            raise NotImplementedError

    reg.save_registry(reg_config_file)
    reg = TactusPluginRegistryFromFile(reg_config_file)
    for plg in reg.plugins:
        if plg.name == "tactus":
            assert Path(tactus.__path__[0]).parent / "tactus/tasks" == plg.tasks_path
            assert Path(tactus.__path__[0]).parent / "tactus/suites" == plg.suites_path
        elif plg.name == "extension":
            assert tasks_dir == plg.tasks_path
            assert suites_dir == plg.suites_path
        else:
            raise NotImplementedError

    update = {"general": {"plugin_registry": {"extension": tmp_directory}}}
    config = default_config
    config = config.copy(update=update)
    reg = TactusPluginRegistryFromConfig(config)
    for plg in reg.plugins:
        if plg.name == "tactus":
            assert Path(tactus.__path__[0]).parent / "tactus/tasks" == plg.tasks_path
            assert Path(tactus.__path__[0]).parent / "tactus/suites" == plg.suites_path
        elif plg.name == "extension":
            assert tasks_dir == plg.tasks_path
            assert suites_dir == plg.suites_path
        else:
            raise NotImplementedError


def test_empty_config():
    """Test empty plugins."""
    reg = TactusPluginRegistry()

    for plg in reg.plugins:
        assert Path(tactus.__path__[0]).parent / "tactus/tasks" == plg.tasks_path
        assert Path(tactus.__path__[0]).parent / "tactus/suites" == plg.suites_path


def test_tasks(tmp_directory):
    reg = TactusPluginRegistry()

    create_task_class("MyExtension", f"{tmp_directory}/extension/tasks/mod_file.py")
    create_suite_class("MyExtension", f"{tmp_directory}/extension/suites/mod_suite.py")

    plg = TactusPlugin("extension", Path(tmp_directory))
    reg.register_plugin(plg)
    known_tasks = available_tasks(reg)
    assert "pgd" in known_tasks
    assert "myextension" in known_tasks

    known_suites = available_suites(reg)
    assert "tactussuitedefinition" in known_suites
    assert "myextension" in known_suites
