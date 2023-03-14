"""Toolbox handling e.g. input/output."""
import os
import re

from .datetime_utils import as_datetime
from .logs import get_logger_from_config


class ArchiveError(Exception):
    """Error raised when there are problems archiving data."""


class ProviderError(Exception):
    """Error raised when there are provider-related problems."""


class Provider:
    """Base provider class."""

    def __init__(self, config, identifier, fetch=True):
        """Construct the object.

        Args:
            config (deode.ParsedConfig): Configuration
            identifier (str): Identifier string
            fetch (bool, optional): Fetch data. Defaults to False.

        """
        self.logger = get_logger_from_config(config)
        self.config = config
        self.identifier = identifier
        self.fetch = fetch
        self.logger.debug(
            "Constructed Base Provider object. %s %s ", self.identifier, self.fetch
        )

    def create_resource(self, resource):
        """Create the resource.

        Args:
            resource (Resource): The resource to be created

        Raises:
            NotImplementedError: Should be implemented
        """
        raise NotImplementedError


class Platform:
    """Platform."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Config.

        """
        self.logger = get_logger_from_config(config)
        self.config = config

    def get_system_value(self, role):
        """Get the system value.

        Args:
            role (str): Type of variable to substitute

        Returns:
            str: Value from system.[role]

        """
        role = role.lower()
        try:
            val = self.config.get_value("system." + role)
            return self.substitute(val)
        except KeyError:
            return None

    def get_value(self, setting):
        """Get the config value with substition.

        Args:
            setting (str): Type of variable to substitute

        Returns:
            str: Value from config with substituted variables

        """
        try:
            val = self.config.get_value(setting)
            return self.substitute(val)
        except KeyError:
            return None

    def get_platform_value(self, role):
        """Get the path.

        Args:
            role (str): Type of variable to substitute

        Returns:
            str: Value from platform.[role]

        """
        role = role.lower()
        try:
            val = self.config.get_value("platform." + role)
            return self.substitute(val)
        except KeyError:
            return None

    def get_platform(self):
        """Get the platform.

        Returns:
            dict: Platform specifc values.

        """
        return self.config.get_value("general.platform")

    def get_macros(self):
        """Get the macros.

        Returns:
            dict: Macros to define.

        """
        macro_list = []
        macros = self.config.get_value("platform").dict()
        for macro in macros:
            macro_list.append(macro)
        self.logger.debug("Platform macro list: %s", macro_list)
        return macro_list

    def get_system_macros(self):
        """Get the macros.

        Returns:
            dict: Macros to define.

        """
        macro_list = []
        macros = self.config.get_value("system").dict()
        for macro in macros:
            macro_list.append(macro)
        self.logger.debug("System macro list: %s", macro_list)
        return macro_list

    def get_os_macros(self):
        """Get the environment macros.

        Returns:
            dict: Environment macros to be used.

        """
        return self.config.get_value("general.os_macros")

    def get_provider(self, provider_id, target, fetch=True):
        """Get the needed provider.

        Args:
            provider_id (str): The intent of the provider.
            target (Resource): The target.
            fetch (boolean): Fetch the file or store it. Default to True.

        Returns:
            Provider: Provider

        Raises:
            NotImplementedError: If provider not defined.

        """
        # TODO handle platform differently archive etc
        if provider_id == "symlink":
            return LocalFileSystemSymlink(self.config, target, fetch=fetch)
        elif provider_id == "copy":
            return LocalFileSystemCopy(self.config, target, fetch=fetch)
        elif provider_id == "move":
            return LocalFileSystemMove(self.config, target, fetch=fetch)
        elif provider_id == "ecfs":
            return ECFS(self.config, target, fetch=fetch)
        else:
            raise NotImplementedError(f"Provider for {provider_id} not implemented")

    def sub_value(self, pattern, key, value, micro="@", ci=True):
        """Substitute the value case-insensitively.

        Args:
            pattern (str): Input string
            key (str): Key to replace
            value (str): Value to replace
            micro (str, optional): Micro character. Defaults to "@".
            ci (bool, optional): Case insensitive. Defaults to True.

        Returns:
            str: Replaces string
        """
        # create the list.
        self.logger.debug("Pattern: %s", pattern)
        self.logger.debug("key=%s value=%s", key, value)

        if ci:
            compiled = re.compile(re.escape(f"{micro}{key}{micro}"), re.IGNORECASE)
        else:
            compiled = re.compile(re.escape(f"{micro}{key}{micro}"))
        res = compiled.sub(value, pattern)

        self.logger.debug("Substituted string: %s", res)
        return res

    def substitute(self, pattern, basetime=None, validtime=None):
        """Substitute pattern.

        Args:
            pattern (str): _description_
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            str: Substituted string.

        """
        if isinstance(pattern, str):
            macros = self.get_macros()
            os_macros = self.get_os_macros()
            system_macros = self.get_system_macros()

            self.logger.debug("pattern before: %s", pattern)
            for macro in macros:
                self.logger.debug("Checking platform macro: %s", macro)
                try:
                    val = self.config.get_value(f"platform.{macro}")
                    self.logger.debug("macro=%s pattern=%s val=%s", macro, pattern, val)
                except KeyError:
                    val = None
                if val is not None:
                    self.logger.debug(
                        "before replace macro=%s pattern=%s", macro, pattern
                    )
                    pattern = self.sub_value(pattern, macro, val)
                    self.logger.debug("after replace macro=%s pattern=%s", macro, pattern)

            self.logger.debug("pattern before: %s", pattern)
            for macro in system_macros:
                self.logger.debug("Checking system macro: %s", macro)
                try:
                    val = self.config.get_value(f"system.{macro}")
                    self.logger.debug("macro=%s pattern=%s val=%s", macro, pattern, val)
                except KeyError:
                    val = None
                if val is not None:
                    self.logger.debug(
                        "before replace macro=%s pattern=%s", macro, pattern
                    )
                    pattern = self.sub_value(pattern, macro, val)
                    self.logger.debug("after replace macro=%s pattern=%s", macro, pattern)

            for macro in os_macros:
                self.logger.debug("Checking macro: %s", macro)
                try:
                    val = os.environ[macro]
                    self.logger.debug(
                        "macro=%s, value=%s pattern=%s", macro, val, pattern
                    )
                except KeyError:
                    val = None
                if val is not None:
                    pattern = self.sub_value(pattern, macro, val)
                    self.logger.debug("macro=%s pattern=%s", macro, pattern)

            domain = self.config.get_value("domain.name")
            pattern = self.sub_value(pattern, "domain", domain)
            exp_case = self.config.get_value("general.case")
            pattern = self.sub_value(pattern, "case", exp_case)
            self.logger.debug("Substituted domain: %s pattern=%s", domain, pattern)
            realization = self.config.get_value("general.realization")
            if realization is not None and realization >= 0:
                pattern = self.sub_value(pattern, "RRR", f"{realization:03d}")
                pattern = self.sub_value(pattern, "MRRR", f"mbr{realization:03d}")
            else:
                pattern = self.sub_value(pattern, "RRR", "")

            self.logger.debug(
                "Substituted realization: %s pattern=%s", realization, pattern
            )

            # Time handling
            if basetime is None:
                basetime = str(self.config.get_value("general.times.basetime"))
            if validtime is None:
                validtime = str(self.config.get_value("general.times.validtime"))
            if isinstance(basetime, str):
                basetime = as_datetime(basetime)
            if isinstance(validtime, str):
                validtime = as_datetime(validtime)

            pattern = self.sub_value(pattern, "YYYY", basetime.strftime("%Y"))
            pattern = self.sub_value(pattern, "MM", basetime.strftime("%m"), ci=False)
            pattern = self.sub_value(pattern, "DD", basetime.strftime("%d"))
            pattern = self.sub_value(pattern, "HH", basetime.strftime("%H"))
            pattern = self.sub_value(pattern, "mm", basetime.strftime("%M"), ci=False)
            if basetime is not None and validtime is not None:
                self.logger.debug(
                    "Substituted date/time info: basetime=%s validtime=%s",
                    basetime.strftime("%Y%m%d%H%M"),
                    validtime.strftime("%Y%m%d%H%M"),
                )
                lead_time = validtime - basetime
                pattern = self.sub_value(pattern, "YYYY_LL", validtime.strftime("%Y"))
                pattern = self.sub_value(
                    pattern, "MM_LL", validtime.strftime("%m"), ci=False
                )
                pattern = self.sub_value(pattern, "DD_LL", validtime.strftime("%d"))
                pattern = self.sub_value(pattern, "HH_LL", validtime.strftime("%H"))
                pattern = self.sub_value(
                    pattern, "mm_LL", validtime.strftime("%M"), ci=False
                )

                lead_seconds = int(lead_time.total_seconds())
                lead_minutes = int(lead_seconds / 3600)  # noqa
                lead_hours = int(lead_seconds / 3600)
                pattern = self.sub_value(pattern, "LL", f"{lead_hours:02d}")
                pattern = self.sub_value(pattern, "LLL", f"{lead_hours:03d}")
                pattern = self.sub_value(pattern, "LLLL", f"{lead_hours:04d}")
                tstep = self.config.get_value("general.tstep")
                if tstep is not None:
                    lead_step = int(lead_seconds / tstep)
                    pattern = self.sub_value(pattern, "TTT", f"{lead_step:03d}")
                    pattern = self.sub_value(pattern, "TTTT", f"{lead_step:04d}")

            if basetime is not None:
                pattern = self.sub_value(pattern, "YMD", basetime.strftime("%Y%m%d"))
                pattern = self.sub_value(pattern, "YYYY", basetime.strftime("%Y"))
                pattern = self.sub_value(pattern, "YY", basetime.strftime("%y"))
                pattern = self.sub_value(pattern, "MM", basetime.strftime("%m"), ci=False)
                pattern = self.sub_value(pattern, "DD", basetime.strftime("%d"))
                pattern = self.sub_value(pattern, "HH", basetime.strftime("%H"))
                pattern = self.sub_value(pattern, "mm", basetime.strftime("%M"), ci=False)

            cnmexp = self.config.get_value("general.cnmexp")
            if cnmexp is not None:
                pattern = self.sub_value(pattern, "CNMEXP", cnmexp)
                self.logger.debug("Substituted CNMEXP: %s pattern=%s", cnmexp, pattern)

        self.logger.debug("Return pattern=%s", pattern)
        return pattern


class FileManager:
    """FileManager class.

    Default DEDODE provider.

    Platform specific.

    """

    def __init__(self, config):
        """Construct the object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        self.logger = get_logger_from_config(config)
        self.config = config
        self.platform = Platform(config)
        self.logger.debug("Constructed FileManager object.")

    def get_input(
        self,
        target,
        destination,
        basetime=None,
        validtime=None,
        check_archive=False,
        provider_id="symlink",
    ):
        """Set input data to deode.

        Args:
            target (str): Input file pattern
            destination (str): Destination file pattern
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            check_archive (bool, optional): Also check archive. Defaults to False.
            provider_id (str, optional): Provider ID. Defaults to "symlink".

        Raises:
            ProviderError: "No provider found for {target}"

        Returns:
            tuple: provider, resource

        """
        destination = LocalFileOnDisk(
            self.config, destination, basetime=basetime, validtime=validtime
        )

        dest_file = destination.identifier
        self.logger.debug("Set input for target=%s to destination=%s", target, dest_file)

        if os.path.exists(dest_file):
            self.logger.debug("Destination file already exists.")
            return None, destination
        else:
            self.logger.info("Checking provider_id %s", provider_id)
            sub_target = self.platform.substitute(
                target, basetime=basetime, validtime=validtime
            )
            provider = self.platform.get_provider(provider_id, sub_target)

            if provider.create_resource(destination):
                self.logger.debug("Using provider_id %s", provider_id)
                return provider, destination

        # Try archive
        # TODO check for archive
        if check_archive:
            provider_id = "ecfs"
            target = target.replace("@ARCHIVE@", "ectmp:/@YYYY@/@MM@/@DD@/@HH@")

            if provider_id is not None:
                # Substitute based on ecfs
                sub_target = self.platform.substitute(
                    target, basetime=basetime, validtime=validtime
                )

                self.logger.info("Checking archiving provider_id %s", provider_id)
                provider = self.platform.get_provider(provider_id, sub_target)

                if provider.create_resource(destination):
                    self.logger.debug("Using provider_id %s", provider_id)
                    return provider, destination
                else:
                    self.logger.info("Could not archive %s", destination.identifier)
        # Else raise exception
        raise ProviderError(
            f"No provider found for {target} and provider_id {provider_id}"
        )

    def input(  # noqa: A003 (class attribute shadowing builtin)
        self,
        target,
        destination,
        basetime=None,
        validtime=None,  # noqa
        check_archive=False,
        provider_id="symlink",
    ):
        """Set input data to deode.

        Args:
            target (str): Input file pattern
            destination (str): Destination file pattern
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            check_archive (bool, optional): Also check archive. Defaults to False.
            provider_id (str, optional): Provider ID. Defaults to "symlink".

        """
        __, __ = self.get_input(
            target,
            destination,
            basetime=basetime,
            validtime=validtime,
            check_archive=check_archive,
            provider_id=provider_id,
        )

    def get_output(
        self,
        target,
        destination,
        basetime=None,
        validtime=None,
        archive=False,
        provider_id="move",
    ):
        """Set output data from deode.

        Args:
            target (str): Input file pattern
            destination (str): Destination file pattern
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            archive (bool, optional): Also archive data. Defaults to False.
            provider_id (str, optional): Provider ID. Defaults to "move".

        Returns:
            tuple: provider, aprovider, resource

        Raises:
            ArchiveError: Could not archive data

        """
        sub_target = self.platform.substitute(
            target, basetime=basetime, validtime=validtime
        )
        sub_destination = self.platform.substitute(
            destination, basetime=basetime, validtime=validtime
        )
        self.logger.debug(
            "Set output for target=%s to destination=%s", sub_target, sub_destination
        )
        target_resource = LocalFileOnDisk(
            self.config, sub_target, basetime=basetime, validtime=validtime
        )
        self.logger.info(
            "Checking provider_id=%s for destination=%s ", provider_id, sub_destination
        )
        provider = self.platform.get_provider(provider_id, sub_destination, fetch=False)

        if provider.create_resource(target_resource):
            target = destination
            self.logger.debug("Using provider_id %s", provider_id)

        aprovider = None
        if archive:
            # TODO check for archive and modify macros
            provider_id = "ecfs"
            destination = destination.replace("@ARCHIVE@", "ectmp:/@YYYY@/@MM@/@DD@/@HH@")

            sub_target = self.platform.substitute(
                target, basetime=basetime, validtime=validtime
            )
            sub_destination = self.platform.substitute(
                destination, basetime=basetime, validtime=validtime
            )

            self.logger.debug(
                "Set output for target=%s to destination=%s", sub_target, sub_destination
            )

            self.logger.info("Checking archive provider_id %s", provider_id)
            aprovider = self.platform.get_provider(
                provider_id, sub_destination, fetch=False
            )

            if aprovider.create_resource(target_resource):
                self.logger.debug("Using provider_id %s", provider_id)
            else:
                raise ArchiveError("Could not archive data")

        return provider, aprovider, target_resource

    def output(
        self,
        target,
        destination,
        basetime=None,
        validtime=None,
        archive=False,
        provider_id="move",
    ):
        """Set output data from deode.

        Args:
            target (str): Input file pattern
            destination (str): Destination file pattern
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            archive (bool, optional): Also archive data. Defaults to False.
            provider_id (str, optional): Provider ID. Defaults to "move".

        """
        __, __, __ = self.get_output(
            target,
            destination,
            basetime=basetime,
            validtime=validtime,
            archive=archive,
            provider_id=provider_id,
        )

    def set_resources_from_dict(self, res_dict):
        """Set resources from dict.

        Args:
            res_dict (_type_): _description_

        Raises:
            ValueError: If the passed file type is neither 'input' nor 'output'.
        """
        for ftype, fobj in res_dict.items():
            for target, settings in fobj.items():
                self.logger.debug(
                    "ftype=%s target=%s, settings=%s", ftype, target, settings
                )
                kwargs = {"basetime": None, "validtime": None, "provider_id": None}
                keys = []
                if ftype == "input":
                    keys = ["basetime", "validtime", "check_archive", "provider_id"]
                    kwargs.update({"check_archive": False})
                elif ftype == "output":
                    keys = ["basetime", "validtime", "archive", "provider_id"]
                    kwargs.update({"archive": False})

                destination = settings["destination"]
                for key in keys:
                    if key in settings:
                        kwargs.update({key: settings[key]})
                self.logger.debug("kwargs=%s", kwargs)
                if ftype == "input":
                    self.input(target, destination, **kwargs)
                elif ftype == "output":
                    self.input(target, destination, **kwargs)
                else:
                    raise ValueError(
                        f"Unknown file type '{ftype}'. Must be either 'input' or 'output'"
                    )


class LocalFileSystemSymlink(Provider):
    """Local file system."""

    def __init__(self, config, pattern, fetch=True):
        """Construct the object.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Identifier string
            fetch (bool, optional): Fetch data. Defaults to True.

        """
        Provider.__init__(self, config, pattern, fetch=fetch)

    def create_resource(self, resource):
        """Symlink the resource.

        Args:
            resource (Resource): Resource.

        Returns:
            bool: True if success

        """
        if self.fetch:
            if os.path.exists(self.identifier):
                self.logger.info("ln -sf %s %s ", self.identifier, resource.identifier)
                os.system(f"ln -sf {self.identifier} {resource.identifier}")  # noqa S605
                return True
            else:
                self.logger.warning("File is missing %s ", self.identifier)
                return False
        else:
            if os.path.exists(resource.identifier):
                self.logger.info("ln -sf %s %s ", resource.identifier, self.identifier)
                os.system(f"ln -sf {resource.identifier} {self.identifier}")  # noqa S605
                return True
            else:
                self.logger.warning("File is missing %s ", resource.identifier)
                return False


class LocalFileSystemCopy(Provider):
    """Local file system copy."""

    def __init__(self, config, pattern, fetch=True):
        """Construct the object.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Identifier string
            fetch (bool, optional): Fetch data. Defaults to False.

        """
        Provider.__init__(self, config, pattern, fetch=fetch)

    def create_resource(self, resource):
        """Create the resource.

        Args:
            resource (Resource): Resource.

        Returns:
            bool: True if success

        """
        if self.fetch:
            if os.path.exists(self.identifier):
                self.logger.info("cp %s %s ", self.identifier, resource.identifier)
                os.system(f"cp {self.identifier} {resource.identifier}")  # noqa S605
                return True
            else:
                self.logger.warning("File is missing %s ", self.identifier)
                return False
        else:
            if os.path.exists(resource.identifier):
                self.logger.info("cp %s %s ", resource.identifier, self.identifier)
                os.system(f"cp {resource.identifier} {self.identifier}")  # noqa S605
                return True
            else:
                self.logger.warning("File is missing %s ", resource.identifier)
                return False


class LocalFileSystemMove(Provider):
    """Local file system copy."""

    def __init__(self, config, pattern, fetch=False):
        """Construct the object.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Identifier string
            fetch (bool, optional): Fetch data. Defaults to False.

        """
        Provider.__init__(self, config, pattern, fetch=fetch)

    def create_resource(self, resource):
        """Create the resource.

        Args:
            resource (Resource): Resource.

        Returns:
            bool: True if success

        """
        if self.fetch:
            if os.path.exists(self.identifier):
                self.logger.info("mv %s %s ", self.identifier, resource.identifier)
                os.system(f"mv {self.identifier} {resource.identifier}")  # noqa S605
                return True
            else:
                self.logger.warning("File is missing %s ", self.identifier)
                return False
        else:
            if os.path.exists(resource.identifier):
                self.logger.info("mv %s %s ", resource.identifier, self.identifier)
                os.system(f"mv {resource.identifier} {self.identifier}")  # noqa S605
                return True
            else:
                self.logger.warning("File is missing %s ", resource.identifier)
                return False


class ArchiveProvider(Provider):
    """Data from ECFS."""

    def __init__(self, config, pattern, fetch=True):
        """Construct the object.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Filepattern
            fetch (bool, optional): Fetch the data. Defaults to True.

        """
        self.fetch = fetch
        Provider.__init__(self, config, pattern)

    def create_resource(self, resource):
        """Create the resource.

        Args:
            resource (Resource): Resource.

        Returns:
            bool: True if success

        """
        return Provider.create_resource(self, resource)


class ECFS(ArchiveProvider):
    """Data from ECFS."""

    def __init__(self, config, pattern, fetch=True):
        """Construct ECFS provider.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Filepattern
            fetch (bool, optional): Fetch the data. Defaults to True.
        """
        ArchiveProvider.__init__(self, config, pattern, fetch=fetch)

    def create_resource(self, resource):
        """Create the resource.

        Args:
            resource (Resource): Resource.

        Returns:
            bool: True if success

        """
        if self.fetch:
            self.logger.info("ecp ecfs:%s %s", self.identifier, resource.identifier)
            # os.system(f"ecp ecfs:{self.identifier} {resource.identifier}")  # noqa S605, E800
        else:
            self.logger.info("ecp %s ecfs:%s", resource.identifier, self.identifier)
            # os.system(f"ecp {resource.identifier} ecfs:{self.identifier}")  # noqa S605, E800
        return True


class Resource:
    """Resource container."""

    def __init__(self, config, identifier):
        """Construct resource.

        Args:
            config (deode.ParsedConfig): Configuration
            identifier (str): Resource identifier

        """
        self.logger = get_logger_from_config(config)
        self.identifier = identifier
        self.logger.debug("Base resource")


class LocalFileOnDisk(Resource):
    """Local file on disk."""

    def __init__(self, config, pattern, basetime=None, validtime=None):
        """Construct local file on disk.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Identifier pattern
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        """
        platform = Platform(config)
        identifier = platform.substitute(pattern, basetime=basetime, validtime=validtime)
        Resource.__init__(self, config, identifier)
