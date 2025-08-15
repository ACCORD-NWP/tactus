"""Toolbox handling e.g. input/output."""

import ast
import contextlib
import inspect
import os
import re
import sys
from typing import Any, Union

import geohash
from troika.connections.ssh import SSHConnection

from .csc_actions import SelectTstep
from .datetime_utils import as_datetime, get_decade, oi2dt_list
from .logs import logger
from .os_utils import deodemakedirs


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
        self.unix_group = self.config.get("platform.unix_group")

        logger.debug(
            "Constructed Base Provider object. {} {} ", self.identifier, self.fetch
        )

    def create_missing_dir(self, target):
        """Create a directory if missing.

        Args:
            target (str): Name of target
        """
        target_dir = os.path.dirname(target)
        if not os.path.isdir(target_dir) and len(target_dir) > 0:
            deodemakedirs(target_dir, unixgroup=self.unix_group)

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
        self.fill_macros()

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

    def get_value(self, setting, alt=None):
        """Get the config value with substition.

        Args:
            setting (str): Type of variable to substitute
            alt (str): Alternative return value

        Returns:
            str: Value from config with substituted variables

        """
        try:
            val = self.config[setting]
            return self.substitute(val)
        except KeyError:
            return alt

    def get_platform_value(self, role, alt=None):
        """Get the path.

        Args:
            role (str): Type of variable to substitute
            alt (str): Alternative return value

        Returns:
            str: Value from platform.[role]

        """
        role = role.lower()
        try:
            val = self.config[f"platform.{role}"]
            return self.substitute(val)
        except KeyError:
            return alt

    def get_platform(self):
        """Get the platform.

        Returns:
            dict: Platform specifc values.

        """
        return self.config["general.platform"]

    def fill_each_macro(self, macro_config):
        """Fill each of the macros."""
        group_macros = f"{macro_config}.group_macros"
        for source in self.config.get(group_macros, []):
            for macro, val in self.config[source].dict().items():
                self.store_macro(macro.upper(), val)

        os_macros = f"{macro_config}.os_macros"
        for macro in list(self.config.get(os_macros, [])):
            with contextlib.suppress(KeyError):
                self.store_macro(macro, os.environ[macro])

        gen_macros = f"{macro_config}.gen_macros"
        for key, val in self.expand_macros(self.config.get(gen_macros, {})).items():
            self.store_macro(key, val)

    def fill_macros(self):
        """Fill the macros."""
        self.macros = {}
        for macro in self.config["macros.select"]:
            self.fill_each_macro(f"macros.select.{macro}")

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

    def store_macro(self, key, val):
        """Check and store a macro.

        Args:
            key (str) : Macro key
            val (str) : Macro value

        Raises:
            RuntimeError: If macro already exists

        """
        if key in self.macros:
            logger.error("Duplicated macro: {} {}", key, val)
            logger.error("Existing macro value: {}", self.macros[key])
            raise RuntimeError(f"Duplicated macro: {key}")

        self.macros[key] = val

    def expand_macros(self, macros):
        """Check and expand macros.

        Args:
            macros (dict): Input macros

        Returns:
            out_macros (dict): Stored macros

        """
        out_macros = {}
        for macro in macros:
            if isinstance(macro, dict):
                key = next(iter(macro))
                val = self.config.get(macro[key].lower(), None)
                key = key.upper()
            else:
                val = self.config.get(macro.lower(), None)
                key = macro.split(".")[-1].upper()

            if val is None:
                logger.warning("Macro {} is not defined", macro)
            else:
                out_macros[key] = val

        return out_macros

    def resolve_macros(self, config_dict, keyval=None):
        """Resolve all macros in a nested dict (both keys and values!)."""
        if isinstance(config_dict, list):
            result = [self.substitute(x, keyval=keyval) for x in config_dict]
        elif isinstance(config_dict, dict):
            result = {
                self.substitute(x): self.resolve_macros(y, keyval=x)
                for x, y in config_dict.items()
            }
        else:
            result = self.substitute(config_dict, keyval=keyval)
        return result

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

        if provider_id == "fdb":
            return FDB(self.config, target, fetch=fetch)

        if provider_id == "scp":
            return SCP(self.config, target, fetch=fetch)

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

        if not isinstance(value, str):
            value = str(value)
            logger.debug(
                "Input value {} for key={} is not a string, but {}!",
                value,
                key,
                type(value),
            )

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

    def substitute_datetime(self, pattern, datetime, suffix=""):
        """Substitute datetime related properties.

        Args:
            pattern (str): _description_
            datetime(DateTime object): datetime to treat
            suffix (str): Add on to key

        Returns:
            str: Substituted string.

        """
        datetime_substitution_map = {
            "YMD": "%Y%m%d",
            "BASETIME": "%Y-%m-%dT%H:%M:%SZ",
            "YYYY": "%Y",
            "YY": "%y",
            "MM": "%m",
            "DD": "%d",
            "HH": "%H",
            "mm": "%M",
            "ss": "%S",
        }
        for key, val in datetime_substitution_map.items():
            ci = key not in ["MM", "mm", "ss"]
            _key = key + suffix
            pattern = self.sub_value(pattern, _key, datetime.strftime(val), ci=ci)

        return pattern

    def substitute_tstep(self, pattern):
        """Substitute tstep.

        Args:
            pattern (str): _description_

        Returns:
            str: Substituted string.

        Raises:
            RuntimeError: In case of erroneous macro
        """
        if not isinstance(pattern, str):
            return pattern

        if pattern.count("@") % 2 != 0:
            message = f"Erroneous macro somewhere in pattern={pattern}"
            logger.debug(message)
            return pattern

        i = [m.start() for m in re.finditer(r"@", pattern)]
        try:
            sub_patterns = [pattern[i[j] + 1 : i[j + 1]] for j in range(0, len(i), 2)]
        except IndexError as error:
            raise IndexError(f"Could not separate pattern:{pattern} by '@'") from error

        for sub_pattern in sub_patterns:
            with contextlib.suppress(KeyError):
                val = self.macros[sub_pattern.upper()]
                try:
                    if val.count("@") > 0:
                        val = self.substitute(val)
                except AttributeError:
                    pass

                logger.debug("before replace macro={} pattern={}", sub_pattern, pattern)
                pattern = self.sub_value(pattern, sub_pattern, val)
                logger.debug("after replace macro={} pattern={}", sub_pattern, pattern)

        return pattern

    def substitute(
        self, pattern, basetime=None, validtime=None, bd_index=None, keyval=None
    ):
        """Substitute pattern.

        Args:
            pattern (str): _description_
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            bd_index (int, optional): Boundary file index. Defaults to None
            keyval (str): Key associated with pattern

        Returns:
            str: Substituted string.

        Raises:
            RuntimeError: In case of erroneous macro

        """
        if not isinstance(pattern, str):
            return pattern

        if pattern.count("@") % 2 != 0:
            if keyval is None:
                message = f"Erroneous macro somewhere in pattern={pattern}"
            else:
                message = f"Erroneous macro for config key={keyval}"
                message += f" somewhere in pattern={pattern}"
            logger.debug(message)
            return pattern

        i = [m.start() for m in re.finditer(r"@", pattern)]
        try:
            sub_patterns = [pattern[i[j] + 1 : i[j + 1]] for j in range(0, len(i), 2)]
        except IndexError as error:
            raise IndexError(f"Could not separate pattern:{pattern} by '@'") from error

        for sub_pattern in sub_patterns:
            with contextlib.suppress(KeyError):
                val = self.macros[sub_pattern.upper()]
                try:
                    if val.count("@") > 0:
                        val = self.substitute(val, basetime, validtime, bd_index, keyval)
                except AttributeError:
                    pass

                logger.debug("before replace macro={} pattern={}", sub_pattern, pattern)
                pattern = self.sub_value(pattern, sub_pattern, val)
                logger.debug("after replace macro={} pattern={}", sub_pattern, pattern)

        # LBC number handling
        try:
            if bd_index is None:
                bd_index = int(self.config["task.args.bd_index"])
            pattern = self.sub_value(pattern, "NNN", f"{bd_index:03d}")
        except KeyError:
            pass

        # Time handling
        if basetime is None:
            basetime = self.config.get("general.times.basetime", None)
        if validtime is None:
            validtime = self.config.get("general.times.validtime", None)
        if isinstance(basetime, str):
            basetime = as_datetime(basetime)
        if isinstance(validtime, str):
            validtime = as_datetime(validtime)

        if basetime is not None:
            pattern = self.substitute_datetime(pattern, basetime)
            one_decade_pattern = (
                get_decade(basetime) if self.config["pgd.one_decade"] else ""
            )
            pattern = self.sub_value(pattern, "ONE_DECADE", one_decade_pattern)

        if basetime is not None and validtime is not None:
            logger.debug(
                "Substituted date/time info: basetime={} validtime={}",
                basetime.strftime("%Y%m%d%H%M"),
                validtime.strftime("%Y%m%d%H%M"),
            )
            pattern = self.substitute_datetime(pattern, validtime, "_LL")
            lead_time = validtime - basetime
            lead_seconds = int(lead_time.total_seconds())
            lh = int(lead_seconds / 3600)
            lm = int((lead_seconds % 3600 - lead_seconds % 60) / 60)
            ls = int(lead_seconds % 60)

            pattern = self.sub_value(pattern, "L", lh)
            pattern = self.sub_value(pattern, "LH", f"{lh:02d}")
            pattern = self.sub_value(pattern, "LL", f"{lh:02d}")
            pattern = self.sub_value(pattern, "LLH", f"{lh:03d}")
            pattern = self.sub_value(pattern, "LLL", f"{lh:03d}")
            pattern = self.sub_value(pattern, "LLLH", f"{lh:04d}")
            pattern = self.sub_value(pattern, "LLLL", f"{lh:04d}")
            pattern = self.sub_value(pattern, "LM", f"{lm:02d}")
            pattern = self.sub_value(pattern, "LS", f"{ls:02d}")

            tstep = self.config["domain.tstep"]
            tstep = self.substitute_tstep(tstep)
            try:
                tstep = int(tstep)
            except ValueError:
                tstep = self.evaluate(tstep, SelectTstep)

            if tstep is not None:
                lead_step = lead_seconds // tstep
                pattern = self.sub_value(pattern, "TTT", f"{lead_step:03d}")
                pattern = self.sub_value(pattern, "TTTT", f"{lead_step:04d}")

            start = self.config.get("general.times.start", None)
            if start is not None:
                pattern = self.substitute_datetime(pattern, as_datetime(start), "_START")

            end = self.config.get("general.times.end", None)
            if end is not None:
                pattern = self.substitute_datetime(pattern, as_datetime(end), "_END")

        logger.debug("Return pattern={}", pattern)
        return pattern

    def evaluate(self, command_string: str, object_: Union[str, object]) -> Any:
        """Evaluate command string, by applying corresponding command of object.

        Args:
            command_string (str): Command string to evaluate
            object_ (Union[str, object]): Object to apply command from (if command
                is function of object). If str, the object is assumed to be a
                module. If a class, the command is assumed to be a method of
                the class.

        Raises:
            ModuleNotFoundError: If module `object_` not found
            AttributeError: If module/class `object_` has no attribute named `func`
            TypeError: If object is not a class or a string
            TypeError: If the command to evaluate is not a function

        Returns:
            any: Return original command string if it is not a function call,
                otherwise return the result of the function call.
        """
        # Check if command string is a function call
        if not isinstance(command_string, str):
            return command_string
        match = re.match(r"(\w+)\((.*)\)", command_string)
        if match:
            # Get function name and arguments
            func = match.group(1)
            args = ast.literal_eval(match.group(2))

            if not isinstance(args, list):
                args = list(args)

            # Get function from object, if object is a string, i.e. a module
            if isinstance(object_, str):
                # Try getting module
                module = sys.modules.get(object_)
                if module:
                    function = getattr(module, func)
                else:
                    raise ModuleNotFoundError(f"Module {object_} not found")
            # Get function from object, if object is a class
            elif inspect.isclass(object_):
                function = getattr(object_, func)
            else:
                raise TypeError(f"Object '{object_}' is not a class or a string")

            # Call function with arguments if function is callable
            if inspect.isfunction(function):
                return function(*args)

            raise TypeError(f"Object '{function}' is not a function")

        # Return original command string if it is not a function call
        return command_string


class FileManager:
    """FileManager class.

    Default DEODE provider.

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
            NotImplementedError: "Checking archive not implemented yet"

        Returns:
            tuple: provider, resource

        """
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

        # TODO check archive for file

        if check_archive:
            raise NotImplementedError("Checking archive not implemented yet")
            # Substitute based on ecfs
            sub_target = self.platform.substitute(
                target, basetime=basetime, validtime=validtime
            )

            logger.debug("Checking archiving provider_id {}", provider_id)
            provider = self.platform.get_provider(provider_id, sub_target)

            if provider.create_resource(destination):
                logger.debug("Using provider_id {}", provider_id)
                return provider, destination

            logger.info("Could not find in archive {}", destination.identifier)
        # Else raise exception
        raise ProviderError(
            f"No provider found for {sub_target} and provider_id {provider_id}"
        )

    def input_data_iterator(
        self,
        input_data_definition,
        basetime=None,
        validtime=None,
        provider_id="symlink",
    ):
        """Handle input data spec dict.

        Loop through the defined data types and fetch them.

        Args:
            input_data_definition (dict): Input data spec
            basetime (datetime.datetime, optional): Base time. Defaults to None.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            provider_id (str): Provider id. Defaults to "symlink"

        Raises:
            RuntimeError: "Invalid data handle type"

        """
        for data_type, data in input_data_definition.items():
            logger.info("Link data type: {}", data_type)
            files = data["files"]
            if isinstance(files, list):
                for filename in files:
                    self.input(
                        f"{data['path']}/{filename}",
                        filename,
                        basetime=basetime,
                        validtime=validtime,
                        provider_id=provider_id,
                    )
            elif isinstance(files, dict):
                for outfile, infile in files.items():
                    self.input(
                        f"{data['path']}/{infile}",
                        outfile,
                        basetime=basetime,
                        validtime=validtime,
                        provider_id=provider_id,
                    )
            else:
                raise RuntimeError(f"Unknown data type in {input_data_definition}")

    def input(
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
            NotImplementedError: Archive = True is not implemented

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
            raise NotImplementedError("Archive = True is not implemented")
            sub_target = self.platform.substitute(
                target, basetime=basetime, validtime=validtime
            )
            sub_destination = self.platform.substitute(
                destination, basetime=basetime, validtime=validtime
            )

            target_resource = LocalFileOnDisk(
                self.config, sub_target, basetime=basetime, validtime=validtime
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

    def create_list(self, basetime, forecast_range, input_template, output_settings):
        """Create list of files to process.

        Args:
            basetime (datetime.datetime): Base time,
            forecast_range (datetime.datetime): forecast range,
            input_template (str): Input template,
            output_settings (str): Output settings
        Returns:
            dict: dict of validates and grib fiels
        """
        logger.info("template: {}, settings: {}", input_template, output_settings)
        files = {}
        # Store the output
        dt_list = oi2dt_list(output_settings, forecast_range)
        for dt in dt_list:
            validtime = basetime + dt
            fname = self.platform.substitute(input_template, validtime=validtime)
            files[validtime] = f"{self.archive}/{fname}"

        return files


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
                self.create_missing_dir(resource.identifier)
                logger.info("cp {} {} ", self.identifier, resource.identifier)
                os.system(f"cp {self.identifier} {resource.identifier}")  # noqa S605
                return True

            logger.warning("File is missing {} ", self.identifier)
            return False

        if os.path.exists(resource.identifier):
            self.create_missing_dir(self.identifier)
            logger.info("cp {} {} ", resource.identifier, self.identifier)
            os.system(f"cp {resource.identifier} {self.identifier}")  # noqa S605
            return True

        logger.warning("File is missing {} ", resource.identifier)
        return False


class LocalFileSystemMove(Provider):
    """Local file system move."""

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
                self.create_missing_dir(resource.identifier)
                logger.info("mv {} {} ", self.identifier, resource.identifier)
                os.system(f"mv {self.identifier} {resource.identifier}")  # noqa S605
                return True

            logger.warning("File is missing {} ", self.identifier)
            return False

        if os.path.exists(resource.identifier):
            self.create_missing_dir(self.identifier)
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
        Provider.__init__(self, config, pattern, fetch=fetch)

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


class SCP(ArchiveProvider):
    """Transfer data with SCP."""

    def __init__(self, config, pattern, fetch=True):
        """Construct SCP provider.

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

        Raises:
            RuntimeError: If directory is not created

        Returns:
            bool: True if success

        """
        # Assumes self.identifier=host:full_file_path
        remote_host, remote_file = str(self.identifier).split(":")
        remote_dir = os.path.dirname(remote_file)

        ssh = SSHConnection({"host": remote_host}, None)
        if self.fetch:
            logger.info("scp src={} to dst={}", self.identifier, resource.identifier)
            ssh.getfile(remote_file, resource.identifier)
        else:
            if len(remote_dir) > 0:
                iret = 1
                tries = 0
                while iret != 0 and tries < 5:
                    cmd = ssh.execute(["ls", remote_dir])
                    cmd.communicate()
                    iret = cmd.returncode
                    if iret != 0:
                        ssh.execute(["mkdir", "-p", f"{remote_dir}"])
                    tries += 1

            if iret != 0:
                raise RuntimeError(f"Could not create remote directory: {remote_dir}")

            logger.info("scp src={} to dst={}", resource.identifier, self.identifier)
            ssh.sendfile(resource.identifier, remote_file)

        return True


class FDB(ArchiveProvider):
    """Dummy FDB class."""

    def __init__(self, config, pattern, fetch=True):
        """Construct FDB provider.

        Args:
            config (deode.ParsedConfig): Configuration
            pattern (str): Filepattern
            fetch (bool, optional): Fetch the data. Defaults to True.
        """
        import pyfdb

        ArchiveProvider.__init__(self, config, pattern, fetch=fetch)
        self.fdb = pyfdb.FDB()
        self.platform = Platform(self.config)

    def check_expver_restrictions(self, expver):
        """Check if user is allowed to archive this expver.

        Args:
            expver (str): FDB experiment identifier

        Raises:
            RuntimeError: If user is not allowed to archive for this expver
        """
        user = os.environ["USER"]
        expver_restrictions = self.config["fdb.expver_restrictions"]

        # Convert to dictionary if necessary
        if hasattr(expver_restrictions, "dict"):
            expver_restrictions = expver_restrictions.dict()

        # Iterate over items in expver_restrictions
        for key, value in expver_restrictions.items():
            if isinstance(value, tuple):
                # Resolve macros for each item in the tuple
                expver_restrictions[key] = [
                    self.platform.substitute(item) for item in value
                ]
            elif isinstance(value, str):
                expver_restrictions[key] = self.platform.substitute(value)
            else:
                raise RuntimeError(
                    f"Unexpected type for expver_restrictions[{key}]: {type(value)}"
                )

        if expver not in expver_restrictions:
            msg = (
                f"The user allowed to archive for expver={expver} is not defined in\n "
                + f"expver_restrictions={expver_restrictions}\n"
                "Please consult the documentation before adding a rule!"
            )
            raise RuntimeError(msg)

        if user not in expver_restrictions[expver]:
            msg = (
                f"The user {user} is not allowed to archive expver={expver}\n"
                + f"according to expver_restrictions={expver_restrictions}\n"
                "Do not alter the rules without consulting the documentation!"
            )
            raise RuntimeError(msg)

    def create_resource(self, resource):
        """Create the resource.

        Args:
            resource (Resource): Resource.

        Raises:
            RuntimeError: If expver not set
        Returns:
            bool: True if success

        """
        rules = self.config.get("fdb.negative_rules", {})
        grib_set = dict(self.config["fdb.grib_set"])
        if "expver" not in grib_set:
            msg = """
            Please set expver in the config section fdb.grib_set before archiving to FDB
            and consult documentation before selecting expver
            """
            logger.error(msg)
            raise RuntimeError(msg)
        self.check_expver_restrictions(grib_set["expver"])

        if self.fetch:
            logger.warning("FDB not yet implemented for {}", resource)
        else:
            logger.info("Archiving {} with pyfdb", resource.identifier)
            filename = os.path.basename(resource.identifier)

            # Create rules-file, temp files to replace $2 and temp.grib
            temp1 = f"{filename}_temp1.grib"
            temp2 = f"{filename}_temp2.grib"
            rules_file = "temp_rules"
            self._write_rules_file(rules_file, rules, neg="!")
            os.system(
                f"grib_filter {rules_file} {resource.identifier} -o {temp1}"  # noqa S605
            )
            set_values = ",".join(
                [f"{key}={value}" for key, value in grib_set.items() if value != ""]
            )
            cmd_for_grib = "grib_set -s " + set_values + f" {temp1} {temp2}"
            logger.debug(cmd_for_grib)
            os.system(cmd_for_grib)  # noqa S605
            with open(temp2, "rb") as infile:
                self.fdb.archive(infile.read())
            self.fdb.flush()

            create_inv = self.config.get("fdb.create_inverse_filter", False)
            if create_inv and bool(rules):
                # Create example files for parameters not archived
                inv_temp1 = f"{filename}_temp1_inv.grib"
                inv_rules_file = "inv_temp_rules"
                self._write_rules_file(inv_rules_file, rules, oper=" || ")
                os.system(
                    f"grib_filter {inv_rules_file} {resource.identifier} -o {inv_temp1}"  # noqa S605
                )
                if os.path.isfile(inv_temp1):
                    logger.info("Created file with non archived fields as {}", inv_temp1)
            else:
                os.remove(temp1)
                os.remove(temp2)
                os.remove(rules_file)

        return True

    def _write_rules_file(self, filename, rules, neg="", oper=" && "):
        if bool(rules):
            logger.info("Applying rules from fdb.negative_rules")
            x = [
                f'{neg}({name} is "{value}")'
                for name, values in rules.items()
                for value in values
            ]
            rule = "if (" + oper.join(x) + ") { append; }\n"
        else:
            logger.info(
                "No parameters excluded in fdb.negative_rules. Archiving all fields."
            )
            rule = "append;\n"
        with open(filename, "w") as outfile:
            outfile.write("set edition = 2;\n")
            outfile.write(rule)


def compute_georef(domain_config):
    """Computes georef from domain_config."""
    lat_center = domain_config["xlatcen"]
    lon_center = domain_config["xloncen"]

    return geohash.encode(longitude=lon_center, latitude=lat_center, precision=6)


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
