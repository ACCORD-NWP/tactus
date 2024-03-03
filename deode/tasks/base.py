"""Base site class."""

import atexit
import contextlib
import os
import shutil
import socket

from ..logs import logger
from ..os_utils import deodemakedirs
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
            config (deode.ParsedConfig): Configuration
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

        logger.info("Task running in {}", self.wdir)

        self._set_eccodes_environment()

    def _set_eccodes_environment(self):
        """Set correct path for ECCODES tables.

        Respect ECCODES_DEINITION_PATH if set and
        assume ECCODES_DIR is defined.

        """
        if os.getenv("ECCODES_DEFINITION_PATH") is not None:
            return

        deode_home = self.platform.get_platform_value("DEODE_HOME")
        eccodes_definition_search_paths = [f"{deode_home}/deode/data/eccodes/definitions"]
        try:
            eccodes_dir = os.environ["ECCODES_DIR"]
            eccodes_definition_search_paths.append(
                f"{eccodes_dir}/share/eccodes/definitions"
            )
        except KeyError:
            pass
        os.environ["ECCODES_DEFINITION_PATH"] = ":".join(eccodes_definition_search_paths)
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
        deodemakedirs(logdir, unixgroup=self.unix_group)

        if isinstance(files, str):
            self.fmanager.output(files, logdir, provider_id="copy")
        else:
            for f in files:
                self.fmanager.output(f, logdir, provider_id="copy")

    def create_wrkdir(self):
        """Create a cycle working directory."""
        deodemakedirs(self.wrk, unixgroup=self.unix_group)

    def create_wdir(self):
        """Create task working directory and check for unix group and set permissions."""
        deodemakedirs(self.wdir, unixgroup=self.unix_group)

    def change_to_wdir(self):
        """Change to task working dir."""
        os.chdir(self.wdir)

    def remove_wdir(self):
        """Remove working directory."""
        os.chdir(self.wrk)
        shutil.rmtree(self.wdir)
        logger.debug("Remove {}", self.wdir)

    def rename_wdir(self, prefix="Failed_task_"):
        """Rename failed working directory."""
        if os.path.isdir(self.wdir):
            fdir = f"{self.wrk}/{prefix}{self.name}"
            if os.path.exists(fdir):
                logger.debug("{} exists. Remove it", fdir)
                shutil.rmtree(fdir)
            pid = os.path.basename(self.wdir)
            fdir = f"{fdir}_{pid}"
            shutil.move(self.wdir, fdir)
            logger.info("Renamed {} to {}", self.wdir, fdir)

    def get_binary(self, binary):
        """Determine binary path from task or system config section.

        Args:
            binary (str): Name of binary

        Returns:
            bindir (str): full path to binary

        """
        with contextlib.suppress(KeyError):
            binary = self.config[f"submission.task_exceptions.{self.name}.binary"]

        try:
            bindir = self.config[f"submission.task_exceptions.{self.name}.bindir"]
        except KeyError:
            bindir = self.config["submission.bindir"]

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

        atexit.register(self.rename_wdir)

    def post(self):
        """Do default postfix.

        E.g. clean

        """
        logger.debug("Base class post")
        # Clean workdir
        if self.config["general.keep_workdirs"]:
            self.rename_wdir(prefix="Finished_task_")

        else:
            self.remove_wdir()

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
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)
