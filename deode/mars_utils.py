"""Utility for marsprep."""

import contextlib
import os
import shutil
from dataclasses import dataclass, field, replace
from pathlib import Path
from subprocess import run
from typing import Dict, List, Tuple

from .datetime_utils import as_datetime
from .geo_utils import Projection, Projstring
from .logs import logger
from .toolbox import Platform


def write_retrieve_mars_req(req, name: str, method: str, omode: str = "w"):
    """Write a RETRIEVE/READ request for MARS.

    Args:
       req (BaseRequest):  namelist object to write
       name     (string):  request file name
       method   (string):  selected method, retrieve or read
       omode    (string):  file open mode (w: create, a: append), default="w"
    """
    sep0 = "{:<2}".format("")
    sep1 = " = "
    sep2 = ","

    keys = list(req.request.keys())
    with open(name, omode) as f:
        f.write(str(method.upper()) + ",\n")

        for key in keys:
            row_str = sep0 + key + sep1 + str(req.request[key])
            if key != keys[-1]:
                row_str += sep2

            f.write(row_str + "\n")


def write_compute_mars_req(
    name: str, formula: str, fieldset: str = "", target: str = "", omode: str = "w"
):
    """Write a COMPUTE request for MARS.

    Args:
        name    (string): name of request
        formula (string): formula for computation.
        fieldset (string): fieldset for computation.
        target  (string): target file
        omode   (string): file open mode (w: create, a: append), default="a"
    """
    with open(name, omode) as f:
        f.write("COMPUTE,\n")
        f.write(f"  FORMULA = {formula}{',' if fieldset or target else ''}\n")
        if fieldset:
            f.write(f"  FIELDSET = {fieldset}{',' if target else ''}\n")
        if target:
            f.write(f"  TARGET = {target}\n")


def write_write_mars_req(name: str, fieldset: str, target: str, omode: str = "w"):
    """Write a WRITE request for MARS.

    Args:
        name    (string): name of request
        fieldset (string): fieldset for computation.
        target  (string): target file
        omode   (string): file open mode (w: create, a: append), default="a"
    """
    with open(name, omode) as f:
        f.write("WRITE,\n")
        f.write(f"  FIELDSET = {fieldset},\n")
        f.write(f"  TARGET = {target}\n")


def get_mars_keys(source, key_filter="-w shortName:s=z"):
    """Get the basic MARS settings from a static file, to correct a request."""
    if source is None or not os.path.exists(source.strip("\"'")):
        logger.error("Missing file: {}", source)
    logger.info("Reading MARS config settings from {}", source)
    grib_command = f"grib_get {key_filter} "
    param_list = {
        "CLASS": "marsClass",
        "STREAM": "marsStream",
        "TYPE": "marsType",
        "EXPVER": "experimentVersionNumber",
        "DATE": "dataDate",
        "TIME": "dataTime",
        "STEP": "stepRange",
        "LEVTYPE": "levelType",
        "LEVEL": "level",
    }

    result = {}
    for prm in param_list:
        # TODO: could be faster with only 1 call for all keys
        # All output is then space-separated, so just use .split()
        result[prm] = (
            run(
                grib_command + f"-p {param_list[prm]}:s {source}",
                check=True,
                shell=True,  # noqa
                capture_output=True,
            )
            .stdout.decode()
            .strip("\n")
        )
        logger.info("Mars config - {} = {}", prm, result[prm])
    return result


def get_steps_and_members_to_retrieve(
    steps: List[int],
    path: Path,
    tag: str,
    members: List[int],
    platform: Platform = None,
    basetime=None,
    validtime=None,
) -> Tuple[List[int], Dict[str, List[int | None]], Dict[int, List[int]]]:
    """Check which mars file already exist and returns steps and members which missing.

    Args:
        steps   (List[int]): list of steps
        path (pathlib.Path): path to global files
        tag  (string): name of tag to check
        members (List[int]): members to retrieve
        platform (Platform, optional): Platform to process macro's in tag
        basetime (optional): Base time used in platform.substitute. Defaults to None.
        validtime (optional): Valid time used in platform.substitute. Defaults to None.

    Returns:
        steps   (List[int]): steps to retrieve
        members_dict (Dict[str, List[int | None]]): members to retrieve
        missing_member_steps (Dict[int, List[int]]): dict with missing steps per member
    """
    member_list = []
    step_list = []
    missing_member_steps = {}

    for member in members:
        missing_steps_current_member = []
        for step in steps:
            # Default to member 0 if member is None without adding member to
            # the member_list. This covers the deterministic case with no
            # boundary member nesting.

            if platform is not None and "@" in tag:
                filename = platform.substitute(
                    tag.replace("@BDMEMBER@", str(member or 0)),
                    basetime=basetime,
                    validtime=validtime,
                    bd_index=step,
                )
            else:
                filename = f"{tag}_{member or 0}+{step}"

            filename = path / filename

            if not os.path.exists(filename):
                logger.info("Missing file:{}", filename)
                member_list.append(member)
                step_list.append(step)
                missing_steps_current_member.append(step)
            else:
                logger.info("Found file:{}", filename)

        if missing_steps_current_member:
            missing_member_steps[member] = sorted(set(missing_steps_current_member))

    steps = sorted(set(step_list))
    # Get perturbed members only
    perturbed_members = [member for member in sorted(set(member_list)) if member != 0]
    # Construct dictionary with perturbed members and control depending on the
    # provided members list
    members_dict = {}
    if 0 in members:
        members_dict["control_member"] = [0]

    # Add perturbed members for the cases where
    # - control and perturbed members are requested
    # - only perturbed members are requested
    # - no perturbed members are requested (perturbed_members = [None])
    # The latter case covers the "deterministic" case, where the ensemble only
    # contains one member.
    if perturbed_members:
        members_dict["perturbed_members"] = perturbed_members

    return steps, members_dict, missing_member_steps


def check_data_available(basetime, mars):
    """Check if there is data for basetime for choosen expver.

    Args:
        basetime (str): basetime
        mars    (dict): mars config section
    Raises:
        ValueError: No data for this date.
    """
    start_date = as_datetime(mars["start_date"])
    with contextlib.suppress(KeyError):
        end_date = as_datetime(mars["end_date"])
        if basetime > end_date:
            raise ValueError(
                f"No data for {basetime}! "
                f"The data is available between {start_date} and {end_date}."
            )
    if basetime < start_date:
        raise ValueError(
            f"No data for {basetime}! The data is available after {start_date}"
        )


def get_domain_data(config):
    """Read and return domain data.

    Args:
        config (deode.ParsedConfig): Configuration from which we get the domain data
    Returns:
        String containing the domain info for MARS
    """
    # Get domain specs
    domain_spec = {
        "nlon": config["domain.nimax"],
        "nlat": config["domain.njmax"],
        "latc": config["domain.xlatcen"],
        "lonc": config["domain.xloncen"],
        "lat0": config["domain.xlat0"],
        "lon0": config["domain.xlon0"],
        "gsize": config["domain.xdx"],
    }

    # Get domain properties
    projstring = Projstring()
    projection = Projection(
        projstring.get_projstring(lon0=domain_spec["lon0"], lat0=domain_spec["lat0"])
    )
    domain_properties = projection.get_domain_properties(domain_spec)
    fdomainstr = "/".join(
        [
            str(domain_properties["maxlat"]),
            str(domain_properties["minlon"]),
            str(domain_properties["minlat"]),
            str(domain_properties["maxlon"]),
        ]
    )

    return fdomainstr


def get_value_from_dict(dict_, key_orig):
    """Check value according to key.

    - If a string returns the value itself
    - If key is a date search for the most suitable match in value
    - Else return the value matching the key.

    Args:
        dict_ (str, BaseConfig object): Values to select
        key_orig (str): key for value checking

    Returns:
        value (str): Found value

    Raises:
        ValueError: Exception
    """
    if isinstance(dict_, str):
        return dict_

    try:
        ref_date = as_datetime(key_orig)
        for key, val in sorted(dict_.items(), reverse=True):
            if ref_date >= as_datetime(key):
                return val
    except ValueError:
        key = str(key_orig)
        if key in dict_:
            return dict_[key]

    raise ValueError(f"Value not found for {key_orig} within {dict_}")


def get_steplist(bd_offset, fc_range, bdint):
    """Get the list of steps for Mars request.

    Args:
       bd_offset (timedelta):   first boundary time
       fc_range     (timedelta):   forecast range
       bdint        (timedelta):   frequency of boundary files

    Returns:
         steps      (List[int]): list of steps
    """
    step_int = int(bdint.total_seconds() // 3600)
    first_step = int(bd_offset.total_seconds() // 3600)
    fc_range_int = int(fc_range.total_seconds() // 3600)

    steps = list(range(first_step, first_step + fc_range_int + step_int, step_int))
    return steps


def get_and_remove_data(file_name: str) -> bytes:
    """Read in and subsequently remove binary data.

    Args:
        file_name (str): The name of the file containing the data to be read.

    Returns:
        bytes: The binary data read from the file.
    """
    with open(file_name, "rb") as fp:
        additional_data = fp.read()

    os.remove(file_name)
    return additional_data


def add_additional_file_specific_data(
    additional_data: Dict[str, bytes],
):
    """Add additional file specific data.

    The additional_data dict is expected to have the following structure, where
    the "common_data" key is optional:
    {
        "common_data": b"Common data to add to all files",
        "file1": b"Data to add to file1",
        "file2": b"Data to add to file2",
        ...
    }

    If the "common_data" key is present, the data will be added to a file before
    the file-specific data is added.

    Args:
        additional_data (Dict[str, bytes]): Dictionary containing the data to
            add (value) to a given file (key).

    Raises:
        FileNotFoundError: If a file does not exist when trying to append
            data to it.
    """
    common_data = additional_data.get("common_data", b"")

    for key, data in additional_data.items():
        if key == "common_data":
            continue

        combined_data = common_data + data

        if os.path.exists(key):
            if combined_data:
                logger.debug("Adding additional file specific data to file: {}", key)
                with open(key, "ab") as fp:
                    fp.write(combined_data)
            else:
                raise FileNotFoundError(
                    f"File {key} not found. Trying to append data to non-existing file."
                )


def add_additional_data_to_all(
    tag: str,
    steps: List[str],
    members_dict: Dict[str, List[int]],
    additional_data: Dict[str, bytes],
):
    """Add additional common data to all files defined by tag, step and member.

    The additional_data dict is expected to have the following structure:
    {
        "data_key1": b"Data to add to all files",
        "data_key2": b"Data to add to all files",
        ...
    }

    Args:
        tag (str): Name of tag
        steps (List[int]): Steps used to construct filename.
        members_dict (Dict[str, List[int]]): Dictionary with members used to
            construct filename.
        additional_data (Dict[str, bytes]): Dictionary containing the data to add.

    Raises:
        FileNotFoundError: If a file does not exist when trying to append
            data to it.
    """
    for step in steps:
        for members in members_dict.values():
            for member in members:
                filename = f"{tag}_{member or 0}+{step}"
                if os.path.exists(filename):
                    with open(filename, "ab") as fp:
                        for key, data in additional_data.items():
                            logger.debug(
                                "Adding additional common data ({}) to file: {}",
                                key,
                                filename,
                            )
                            fp.write(data)
                else:
                    raise FileNotFoundError(
                        f"File {filename} not found. "
                        "Trying to append data to non-existing file."
                    )


def move_files(
    tag: str, steps: List[int], members_dict: Dict[str, List[int]], target_dir: Path
):
    """Move files to the final location.

    Args:
        tag (str): Name of tag
        steps (list): steps to process the files
        members_dict (Dict[str, List[int]]): dict with members to procces the files
        target_dir (Path): target directory to move the files to
    """
    for step in steps:
        for members in members_dict.values():
            for member in members:
                # Define target file name. Member defaults to 0 if member is None
                file_name = f"{tag}_{member or 0}+{step}"
                if os.path.exists(file_name):
                    shutil.move(file_name, target_dir / file_name)


def compile_target(
    tag: str, member_type: str, members: int | List[int], step: str | int = "[STEP]"
) -> str:
    """Compiles a target name given member_type, members and (optinally) step.

    Args:
        tag (str): the tag used for this target
        member_type (str): member_type for member, control_member or perturbed_members
        members (int or List[int]): the id of the member(s)
        step (str or int): the id of the step(s)

    Returns:
        Compiled target name

    """
    if member_type == "control_member" or members is None:
        member = "0"
    elif isinstance(members, int):
        # Allow for single member (int) argument in stead of List[int]
        member = f"{members}"
    elif not all(members):
        member = "0"
    elif len(members) > 1:
        member = "[NUMBER]"
    else:
        member = f"{members[0]}"

    # Merge target parts
    member_part = f"_{member}" if member is not None else ""
    step_part = f"+{step}" if step is not None else ""

    target = f'"{tag}{member_part}{step_part}"'

    return target


def mars_write_method(mars_version: int) -> str:
    """Gets the mars write_method depending on mars version.

    Args:
        mars_version (int): version of mars client

    Returns:
        write_method: retrieve or read

    """
    return "retrieve" if mars_version == 6 else "read"


@dataclass(kw_only=True)
class BaseRequest:
    """Represents a structured data request with configurable parameters.

    This class facilitates the creation of data requests by encapsulating
    request attributes and providing methods for modifying and enhancing
    the request structure. The request data is stored in a dictionary
    accessible via the `request` attribute.

    Parameters:
        class_ (str): The class of the request, e.g., "D1".
        data_type (str): The type of data requested (e.g., analysis, forecast).
        expver (str): Experiment version identifier.
        levtype (str): Level type (e.g., surface, pressure levels).
        date (str): The date range for the request in YYYYMMDD format.
        time (str): The time range for the request in HHMM format.
        steps (str): Forecast steps or time intervals.
        param (str): Parameter codes for the requested data.
        target (str): Target file or identifier for the output.
        request (dict): A dictionary representation of the request, initialized
            automatically upon object creation.
    """

    class_: str
    data_type: str
    expver: str
    levtype: str
    date: str
    time: str
    steps: str
    param: str
    target: str
    request: dict = field(init=False)

    def __post_init__(self):
        self.request = {
            "CLASS": self.class_,
            "TYPE": self.data_type,
            "EXPVER": self.expver,
            "LEVTYPE": self.levtype,
            "DATE": self.date,
            "TIME": self.time,
            "STEP": "/".join(map(str, self.steps)),
            "PARAM": self.param,
            "TARGET": self.target,
        }

    def update_request(self, upd: dict):
        """Add or replace keys in a mars request.

        Args:
            upd (dict): a dict with keys and values
        """
        self.request.update(upd)

    def add_levelist(self, levelist: str):
        """Set multilevel if needed.

        Args:
            levelist (string): list of levels
        Returns:
            None
        """
        if self.levtype == "ML" and self.param == "129":
            local_levelist = 1

        elif self.levtype == "ML":
            local_levelist = get_value_from_dict(levelist, self.date)
            logger.info("levelist:{}", local_levelist)

        elif self.levtype == "SOL":
            local_levelist = "1/2/3/4/5"
            logger.info("levelist:{}", local_levelist)

        else:
            return

        # Add levelist to request
        self.request["LEVELIST"] = local_levelist

    def add_database_options(self):
        """Add database options."""
        if self.class_ == "D1":
            self.request.update(
                {
                    "DATASET": "extremes-dt",
                }
            )
            if "latlon" not in self.target:
                self.request.update(
                    {
                        "DATABASE": "fdb",
                    }
                )

    def replace(self, **kwargs):
        """Return new instance with updated values."""
        return replace(self, **kwargs)
