"""Plug-in functionality."""
from pathlib import Path
from typing import List

import yaml

import tactus
from tactus.os_utils import resolve_path_relative_to_package

from .logs import logger


class TactusPluginRegistry:
    """Registry of plugins."""

    def __init__(self, config=None):
        """Construct the plugin registry.

        Args:
            config (dict, optional): Registry config. Defaults to None.

        Raises:
            RuntimeError: "Can not set plugin registry"

        """
        if config is None:
            config = {}
        self.config_input = config
        self.config = self.get_registry_config()
        self.plugins: List[TactusPlugin] = []
        self.tactus_plugin()
        self.load_plugins()

    def tactus_plugin(self):
        """Base tactus plugin."""
        path = Path(tactus.__path__[0]).parent
        plugin = TactusPlugin("tactus", path)
        self.register_plugin(plugin)

    def load_plugin(self, plugin: "TactusPlugin"):
        """Load plugin.

        Args:
            plugin (TactusPlugin): tactus plugin

        """
        self.plugins += [plugin]
        self.config["plugins"].update({plugin.name: str(plugin.path)})

    def load_plugins(self):
        """Load all registered plugins."""
        plugins = self.config["plugins"]
        for name, path in plugins.items():
            path_ = resolve_path_relative_to_package(Path(path))
            if name != "tactus":
                plugin = TactusPlugin(name, path_)
                self.load_plugin(plugin)

    def plugin_exists(self, plugin: "TactusPlugin"):
        """Check if plugin exists.

        Args:
            plugin (TactusPlugin): tactus plugin

        Returns:
            bool: True if already exists in registry.

        """
        return any(plg.name == plugin.name for plg in self.plugins)

    def register_plugin(self, plugin: "TactusPlugin"):
        """Register plugin.

        Args:
            plugin (TactusPlugin): tactus plugin

        """
        if not self.plugin_exists(plugin):
            self.config["plugins"].update({plugin.name: str(plugin.path)})
            self.load_plugin(plugin)
        else:
            logger.warning("Plugin {} does already exists", plugin.name)

    def get_registry_config(self):
        """Get the registry config."""
        config = self.config_input
        if "plugins" not in config:
            config.update({"plugins": {}})
        return config

    def save_registry(self, config_file):
        """Save registry config.

        Args:
            config_file (str): Filename
        """
        with open(config_file, mode="w", encoding="utf8") as fh:
            yaml.safe_dump(self.config, fh)


class TactusPluginRegistryFromFile(TactusPluginRegistry):
    """Registry file of plugins."""

    def __init__(self, config_file):
        """Construct the plugin registry.

        Args:
            config_file (str, optional): Registry config. Defaults to None.

        Raises:
            RuntimeError: "Can not set plugin registry"

        """
        with open(config_file, mode="r", encoding="utf8") as fh:
            config = yaml.safe_load(fh)
        TactusPluginRegistry.__init__(self, config)


class TactusPluginRegistryFromConfig(TactusPluginRegistry):
    """Create a registry from a tactus config file."""

    def __init__(self, config):
        """Construct a registry from a tactus config.

        Args:
            config (ParsedConfig): tactus config.

        """
        try:
            plugin_registry = config["general.plugin_registry"].dict()
        except KeyError:
            plugin_registry = None
        TactusPluginRegistry.__init__(self, plugin_registry)


class TactusPlugin:
    """Tactus plugin."""

    def __init__(self, name: str, path: Path):
        """Construct the plugin.

        Args:
            name (str): Lower case name of plugin
            path (Path): Root path to plugin.

        """
        self.name = name.lower()
        self.path = path
        self.tasks_path = self.path / self.name / "tasks"
        self.suites_path = self.path / self.name / "suites"


class TactusPluginFromConfigFile(TactusPlugin):
    """Tactus plugin."""

    def __init__(self, config_file):
        """Construct the plugin from a config file.

        Args:
            config_file (str): Config file.

        """
        config = self.get_plugin_config(config_file)
        name = config["name"]
        path = Path(config["path"])
        TactusPlugin.__init__(self, name, path)

    @staticmethod
    def get_plugin_config(config_file):
        """Get the registry config."""
        with open(config_file, mode="r", encoding="utf8") as fh:
            config = yaml.safe_load(fh)
        return config
