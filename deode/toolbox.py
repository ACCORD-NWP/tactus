"""Toolbox handling e.g. input/output."""
import contextlib
import os
import re

from .datetime_utils import as_datetime, get_decade
from .logs import logger


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
        self.config = config
        self.identifier = identifier
        self.fetch = fetch
        logger.debug(
            "Constructed Base Provider object. {} {} ", self.identifier, self.fetch
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
            val = self.config[f"system.{role}"]
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
            val = self.config[setting]
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
            val = self.config[f"platform.{role}"]
            return self.substitute(val)
        except KeyError:
            return None

    def get_platform(self):
        """Get the platform.

        Returns:
            dict: Platform specifc values.

        """
        return self.config["general.platform"]

    def get_macros(self):
        """Get the macros.

        Returns:
            dict: Macros to define.

        """
        return list(self.config["platform"].keys())

    def get_system_macros(self):
        """Get the macros.

        Returns:
            dict: Macros to define.

        """
        return list(self.config["system"].dict().keys())

    def get_os_macros(self):
        """Get the environment macros.

        Returns:
            dict: Environment macros to be used.

        """
        return self.config["macros.os_macros"]

    def get_gen_macros(self):
        """Get the environment macros.

        Returns:
            dict: Environment macros to be used.

        """
        return self.config["macros.gen_macros"]

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

        if provider_id == "copy":
            return LocalFileSystemCopy(self.config, target, fetch=fetch)

        if provider_id == "move":
            return LocalFileSystemMove(self.config, target, fetch=fetch)

        if provider_id == "ecfs":
            return ECFS(self.config, target, fetch=fetch)

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
        logger.debug("Pattern: {}", pattern)
        logger.debug("key={} value={}", key, value)

        if ci:
            compiled = re.compile(re.escape(f"{micro}{key}{micro}"), re.IGNORECASE)
        else:
            compiled = re.compile(re.escape(f"{micro}{key}{micro}"))
        res = compiled.sub(value, pattern)

        logger.debug("Substituted string: {}", res)
        return res

    def sub_str_dict(self, input_dict, basetime=None, validtime=None):
        """Substitute strings in dictionary.

        Args:
            input_dict (dict): Dict to be parsed
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            d (dict): Updated dict

        """
        d = input_dict.copy()
        for k, v in input_dict.items():
            if isinstance(v, dict):
                d[k] = self.sub_str_dict(v, basetime, validtime)
            elif isinstance(v, str):
                d[k] = self.substitute(v, basetime, validtime)
            else:
                d[k] = v

        return d

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
            # Collect what is defined in config.macros, the group, os and general macros
            all_macros = {}

            for source in self.config["macros.group_macros"]:
                for macro, val in self.config[source].dict().items():
                    all_macros[macro.upper()] = val

            for macro in list(self.get_os_macros()):
                with contextlib.suppress(KeyError):
                    all_macros[macro] = os.environ[macro]

            for macro in self.config["macros.gen_macros"]:
                if isinstance(macro, dict):
                    key = next(iter(macro))
                    val = self.config[macro[key].lower()]
                    key = key.upper()
                else:
                    val = self.config[macro.lower()]
                    key = macro.split(".")[-1].upper()
                all_macros[key] = val

            i = [m.start() for m in re.finditer(r"@", pattern)]
            last_pattern = "#"
            while len(i) > 0 and last_pattern != pattern:
                sub_patterns = [pattern[i[j] + 1 : i[j + 1]] for j in range(0, len(i), 2)]
                last_pattern = pattern
                for sub_pattern in sub_patterns:
                    with contextlib.suppress(KeyError):
                        val = all_macros[sub_pattern.upper()]
                        logger.debug("before replace macro={} pattern={}", macro, pattern)
                        pattern = self.sub_value(pattern, sub_pattern, val)
                        logger.debug("after replace macro={} pattern={}", macro, pattern)

                i = [m.start() for m in re.finditer(r"@", pattern)]

            # LBC number handling
            try:
                bd_nr = int(self.config["task.args.bd_nr"])
                pattern = self.sub_value(pattern, "NNN", f"{bd_nr:03d}")
            except KeyError:
                pass

            # Time handling
            if basetime is None:
                basetime = str(self.config["general.times.basetime"])
            if validtime is None:
                validtime = str(self.config["general.times.validtime"])
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
                logger.debug(
                    "Substituted date/time info: basetime={} validtime={}",
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
                lh = int(lead_seconds / 3600)
                lm = int((lead_seconds % 3600 - lead_seconds % 60) / 60)
                ls = int(lead_seconds % 60)

                pattern = self.sub_value(pattern, "LH", f"{lh:02d}")
                pattern = self.sub_value(pattern, "LL", f"{lh:02d}")
                pattern = self.sub_value(pattern, "LLH", f"{lh:03d}")
                pattern = self.sub_value(pattern, "LLL", f"{lh:03d}")
                pattern = self.sub_value(pattern, "LLLH", f"{lh:04d}")
                pattern = self.sub_value(pattern, "LLLL", f"{lh:04d}")
                pattern = self.sub_value(pattern, "LM", f"{lm:02d}")
                pattern = self.sub_value(pattern, "LS", f"{ls:02d}")
                tstep = self.config["domain.tstep"]
                if tstep is not None:
                    lead_step = lead_seconds // tstep
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
                pattern = self.sub_value(pattern, "ss", basetime.strftime("%S"), ci=False)

                one_decade_pattern = (
                    get_decade(basetime) if self.config["pgd.one_decade"] else ""
                )
                pattern = self.sub_value(pattern, "ONE_DECADE", one_decade_pattern)

        logger.debug("Return pattern={}", pattern)
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
        self.config = config
        self.platform = Platform(config)
        logger.debug("Constructed FileManager object.")

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
        self.aloc = self.platform.get_value("archiving.paths.aloc")

        destination = LocalFileOnDisk(
            self.config, destination, basetime=basetime, validtime=validtime
        )

        dest_file = destination.identifier
        logger.debug("Set input for target={} to destination={}", target, dest_file)

        if os.path.exists(dest_file):
            logger.debug("Destination file already exists.")
            return None, destination

        logger.debug("Checking provider_id {}", provider_id)
        sub_target = self.platform.substitute(
            target, basetime=basetime, validtime=validtime
        )
        provider = self.platform.get_provider(provider_id, sub_target)

        if provider.create_resource(destination):
            logger.debug("Using provider_id {}", provider_id)
            return provider, destination

        # Try archive
        # TODO check for archive
        if check_archive:
            provider_id = "ecfs"
            target = target.replace("@ARCHIVE@", "{self.aloc}/@YYYY@/@MM@/@DD@/@HH@")

            if provider_id is not None:
                # Substitute based on ecfs
                sub_target = self.platform.substitute(
                    target, basetime=basetime, validtime=validtime
                )

                logger.debug("Checking archiving provider_id {}", provider_id)
                provider = self.platform.get_provider(provider_id, sub_target)

                if provider.create_resource(destination):
                    logger.debug("Using provider_id {}", provider_id)
                    return provider, destination

                logger.info("Could not archive {}", destination.identifier)
        # Else raise exception
        raise ProviderError(
            f"No provider found for {sub_target} and provider_id {provider_id}"
        )

    def input(  # noqa: A003 (class attribute shadowing builtin)
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
        logger.debug(
            "Set output for target={} to destination={}", sub_target, sub_destination
        )
        target_resource = LocalFileOnDisk(
            self.config, sub_target, basetime=basetime, validtime=validtime
        )
        logger.debug(
            "Checking provider_id={} for destination={} ", provider_id, sub_destination
        )
        provider = self.platform.get_provider(provider_id, sub_destination, fetch=False)

        if provider.create_resource(target_resource):
            target = destination
            logger.debug("Using provider_id {}", provider_id)

        aprovider = None
        if archive:
            # TODO check for archive and modify macros
            provider_id = "ecfs"
            destination = destination.replace(
                "@ARCHIVE@", "{self.aloc}/@YYYY@/@MM@/@DD@/@HH@"
            )

            sub_target = self.platform.substitute(
                target, basetime=basetime, validtime=validtime
            )
            sub_destination = self.platform.substitute(
                destination, basetime=basetime, validtime=validtime
            )

            logger.debug(
                "Set output for target={} to destination={}", sub_target, sub_destination
            )

            logger.info("Checking archive provider_id {}", provider_id)
            aprovider = self.platform.get_provider(
                provider_id, sub_destination, fetch=False
            )

            if aprovider.create_resource(target_resource):
                logger.debug("Using provider_id {}", provider_id)
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
                logger.debug("ftype={} target={}, settings={}", ftype, target, settings)
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
                logger.debug("kwargs={}", kwargs)

                if ftype in ["input", "output"]:
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
                logger.info("ln -sf {} {} ", self.identifier, resource.identifier)
                os.system(f"ln -sf {self.identifier} {resource.identifier}")  # noqa S605
                return True

            logger.warning("File is missing {} ", self.identifier)
            return False

        if os.path.exists(resource.identifier):
            logger.info("ln -sf {} {} ", resource.identifier, self.identifier)
            os.system(f"ln -sf {resource.identifier} {self.identifier}")  # noqa S605
            return True

        logger.warning("File is missing {} ", resource.identifier)
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
                logger.info("cp {} {} ", self.identifier, resource.identifier)
                os.system(f"cp {self.identifier} {resource.identifier}")  # noqa S605
                return True

            logger.warning("File is missing {} ", self.identifier)
            return False

        if os.path.exists(resource.identifier):
            logger.info("cp {} {} ", resource.identifier, self.identifier)
            os.system(f"cp {resource.identifier} {self.identifier}")  # noqa S605
            return True

        logger.warning("File is missing {} ", resource.identifier)
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
                logger.info("mv {} {} ", self.identifier, resource.identifier)
                os.system(f"mv {self.identifier} {resource.identifier}")  # noqa S605
                return True

            logger.warning("File is missing {} ", self.identifier)
            return False

        if os.path.exists(resource.identifier):
            logger.info("mv {} {} ", resource.identifier, self.identifier)
            os.system(f"mv {resource.identifier} {self.identifier}")  # noqa S605
            return True

        logger.warning("File is missing {} ", resource.identifier)
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
        # TODO: Address the noqa check disablers
        if self.fetch:
            logger.info("ecp -pu {} {}", self.identifier, resource.identifier)
            os.system(
                f"ecp -pu {self.identifier} {resource.identifier}"  # noqa S605, E800
            )
        else:
            logger.info("ecp -pu {} {}", resource.identifier, self.identifier)
            os.system(
                f"ecp -pu {resource.identifier} {self.identifier}"  # noqa S605, E800
            )
        return True


class Resource:
    """Resource container."""

    def __init__(self, _config, identifier):
        """Construct resource.

        Args:
            config (deode.ParsedConfig): Configuration
            identifier (str): Resource identifier

        """
        self.identifier = identifier
        logger.debug("Base resource")


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
