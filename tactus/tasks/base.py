"""Base site class."""

import atexit
import contextlib
import os
import shutil
import socket

from ..config_parser import ConfigParserDefaults
from ..logs import logger
from ..os_utils import tactusmakedirs
from ..toolbox import FileManager


def _get_name(cname, cls, suffix, attrname="__plugin_name__"):
    """Get name.

    Args:
        cname (_type_): cname
        cls (_type_): cls
        suffix (str): suffix
        attrname (str, optional): _description_. Defaults to "__plugin_name__".

    Returns:
        _type_: Name

    """
    # __dict__ vs. getattr: do not inherit the attribute from a parent class
    name = getattr(cls, "__dict__", {}).get(attrname, None)
    if name is not None:
        return name
    name = cname.lower()
    if name.endswith(suffix):
        name = name[: -len(suffix)]
    return name


class Task(object):
    """Base Task class."""

    def __init__(self, config, name):
        """Construct base task.

        Args:
            config (tactus.ParsedConfig): Configuration
            name (str): Task name

        Raises:
            ValueError: "You must set wrk"

        """
        self.config = config
        if "." in name:
            name = name.split(".")[-1]
        self.name = name
        self.fmanager = FileManager(self.config)
        self.platform = self.fmanager.platform
        self.wrapper = self.config["submission.task.wrapper"]

        self.wrk = self.platform.get_system_value("wrk")
        if self.wrk is None:
            raise ValueError("You must set wrk", self.wrk)

        wdir = f"{self.wrk}/{socket.gethostname()}{os.getpid()!s}"
        self.wdir = wdir
        self.unix_group = self.platform.get_value("platform.unix_group")

        logger.info("Task {} running in {}", self.name, self.wdir)

        self._set_eccodes_environment()

    def _set_eccodes_environment(self):
        """Set correct path for ECCODES tables.

        Respect ECCODES_DEINITION_PATH if set or combine local settings with library
        ones in different ways depending on version

        """
        eccodes_definition_path = os.getenv("ECCODES_DEFINITION_PATH")
        if eccodes_definition_path is not None:
            logger.info("Use ECCODES_DEFINITION_PATH {}", eccodes_definition_path)
            return

        # Path to modelname definitions
        tactus_eccodes_modelname_path = os.path.join(
            self.platform.get_platform_value("archive_root"), "eccodes", "definitions"
        )
        # Path to local tables
        tactus_eccodes_definition_path = str(
            ConfigParserDefaults.DATA_DIRECTORY / "eccodes/definitions"
        )
        tactus_eccodes_definition_path = ":".join(
            [tactus_eccodes_definition_path, tactus_eccodes_modelname_path]
        )

        try:
            eccodes_version = tuple(
                [int(x) for x in os.getenv("ECCODES_VERSION").split(".")]
            )
        except AttributeError:
            eccodes_version = (2, 30, 0)

        eccodes_definition_path = tactus_eccodes_definition_path
        if eccodes_version < (2, 30, 0):
            try:
                eccodes_dir = os.environ["ECCODES_DIR"]
                eccodes_definition_path = ":".join(
                    [
                        eccodes_definition_path,
                        f"{eccodes_dir}/share/eccodes/definitions",
                    ]
                )
            except KeyError:
                pass

        os.environ["ECCODES_DEFINITION_PATH"] = str(eccodes_definition_path)
        logger.info(
            "Set ECCODES_DEFINITION_PATH to {}", os.environ["ECCODES_DEFINITION_PATH"]
        )

    def archive_logs(self, files, target=None):
        """Archive files in a log directory.

        Args:
            files (str,list): File(s) to be archived
            target (str): Target directory for archiving

        """
        if target is None:
            target = self.wrk
        logdir = os.path.join(target, "logs", self.name)
        tactusmakedirs(logdir, unixgroup=self.unix_group)

        if isinstance(files, str):
            self.fmanager.output(files, logdir, provider_id="copy")
        else:
            for f in files:
                self.fmanager.output(f, logdir, provider_id="copy")

    def create_wrkdir(self):
        """Create a cycle working directory."""
        tactusmakedirs(self.wrk, unixgroup=self.unix_group)

    def create_wdir(self):
        """Create task working directory and check for unix group and set permissions."""
        tactusmakedirs(self.wdir, unixgroup=self.unix_group)

    def change_to_wdir(self):
        """Change to task working dir."""
        os.chdir(self.wdir)

    def remove_wdir(self, wdir=None):
        """Remove working directory."""
        if wdir is None:
            wdir = self.wdir
        shutil.rmtree(wdir)
        logger.info("Remove working directory {}", wdir)

    def rename_wdir(self, prefix="Failed_task_", source=None, target=None):
        """Rename failed/completed working directory."""
        if source is None:
            source = self.wdir
        if target is None:
            target = self.name
        if os.path.isdir(source):
            fdir = f"{self.wrk}/{prefix}{target}"
            if os.path.exists(fdir):
                logger.debug("{} exists. Remove it", fdir)
                shutil.rmtree(fdir)
            pid = os.path.basename(source)
            fdir = f"{fdir}_{pid}"
            shutil.move(source, fdir)
            logger.info("Renamed {} to {}", source, fdir)

    def get_binary(self, binary_name, task_name=None):
        """Determine binary path from task or system config section.

        Args:
            binary_name (str): Name of binary
            task_name (str, Optional): Optional name for task specific setting

        Returns:
            bindir (str): full path to binary

        """
        binary = binary_name
        task = task_name if task_name is not None else self.name
        with contextlib.suppress(KeyError):
            binary = self.config[f"submission.task_exceptions.{task}.binary"]

        try:
            bindir = self.config[f"submission.task_exceptions.{task}.bindir"]
        except KeyError:
            try:
                binaries = self.config[
                    f"submission.task_exceptions.{self.name}.binaries.{binary_name}"
                ]
                logger.debug("binaries:{}", binaries)

                with contextlib.suppress(KeyError):
                    binary = binaries["binary"]
                with contextlib.suppress(KeyError):
                    bindir = binaries["bindir"]
            except KeyError:
                bindir = self.config["submission.bindir"]

        bindir = self.platform.substitute(bindir)
        bindir = os.path.realpath(bindir)

        logger.debug("binary:{}", binary)
        logger.debug("bindir:{}", bindir)

        return f"{bindir}/{binary}"

    def execute(self):
        """Do nothing for base execute task."""
        logger.debug("Using empty base class execute")

    def prep(self):
        """Do default preparation before execution.

        E.g. clean

        """
        logger.debug("Base class prep")
        self.create_wdir()
        self.change_to_wdir()

        outfile = f"{self.wdir}/config.toml"
        logger.info("Store used config as {}", outfile)
        self.config.save_as(outfile)

        atexit.register(self.rename_wdir)

    def post(self, source=None, target=None):
        """Do default postfix.

        E.g. clean

        """
        logger.debug("Base class post")
        # Clean workdir
        if self.config["general.keep_workdirs"]:
            self.rename_wdir(prefix="Finished_task_", source=source, target=target)
        else:
            self.remove_wdir(wdir=source)

    def run(self):
        """Run task.

        Define run sequence.

        """
        self.prep()
        self.execute()
        self.post()

    def get_task_setting(self, setting):
        """Get task setting.

        Args:
            setting (str): Setting to find in task.{self.name}

        Returns:
            value : Found setting

        """
        task_subsection_name_in_config = _get_name(
            self.__class__.__name__,
            self.__class__,
            Task.__name__.lower(),
            attrname="__type_name__",
        )
        setting_to_be_retrieved = f"task.{task_subsection_name_in_config}.{setting}"

        try:
            value = self.config[setting_to_be_retrieved]
        except KeyError:
            logger.exception(
                "Task setting '{}' not found in config.", setting_to_be_retrieved
            )
            return None

        logger.debug("Setting = {} value ={}", setting_to_be_retrieved, value)

        return value


class UnitTest(Task):
    """Base Task class."""

    def __init__(self, config):
        """Construct test task.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)
