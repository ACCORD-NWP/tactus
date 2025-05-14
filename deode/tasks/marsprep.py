"""Marsprep."""

import ast
import contextlib
import os
import shutil
from functools import cached_property
from pathlib import Path
from typing import Dict, List, Optional

from ..config_parser import ParsedConfig
from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..logs import logger
from ..mars_utils import (
    BaseRequest,
    add_additional_data_to_all,
    add_additional_file_specific_data,
    check_data_available,
    get_and_remove_data,
    get_date_time_info,
    get_domain_data,
    get_steps_and_members_to_retrieve,
    get_value_from_dict,
    move_files,
    write_mars_req,
)
from ..os_utils import deodemakedirs, list_files_join
from ..tasks.batch import BatchJob
from .base import Task


class Marsprep(Task):
    """Marsprep task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        Raises:
            ValueError: No data for this date.
        """
        Task.__init__(self, config, __class__.__name__)

        # Default to [None] to cover the deterministic case with no
        # boundary member nesting.
        self.bdmembers = (
            [
                int(self.platform.substitute(bdmember))
                for bdmember in self.config["boundaries.ifs.bdmembers"]
            ]
            if self.config["boundaries.ifs.bdmembers"]
            else [None]
        )

        # path to sfcdata on disk
        self.sfcdir = Path(self.platform.get_platform_value("global_sfcdir"))
        logger.info(f"sfc dir: {self.sfcdir}")

        # Get the times from config.toml
        self.basetime = as_datetime(self.config["general.times.basetime"])
        forecast_range = as_timedelta(self.config["general.times.forecast_range"])

        # Check if there are data for specific date in mars
        check_data_available(self.basetime, self.mars)

        # Get boundary informations
        bdint = as_timedelta(self.config["boundaries.bdint"])
        bdcycle = as_timedelta(self.mars["ifs_cycle_length"])
        bdcycle_start = as_timedelta(self.mars["ifs_cycle_start"])
        bdshift = as_timedelta(self.config["boundaries.bdshift"])

        # Get date/time/steps info
        self.init_date_str, self.init_hour_str, self.steps = get_date_time_info(
            self.basetime,
            forecast_range,
            bdint,
            bdcycle,
            bdshift,
        )

        if bdshift.total_seconds() % bdcycle.total_seconds() != 0:
            raise ValueError("bdshift needs to be a multiple of bdcycle!")

        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime,
            bdcycle,
            bdcycle_start=bdcycle_start,
            bdshift=-bdshift,
        )
        logger.info("bd_basetime: {}", self.bd_basetime)

        exist_snow = False
        with contextlib.suppress(KeyError):
            self.param_snow = get_value_from_dict(
                self.mars["GG_snow"], self.init_date_str
            )
            exist_snow = True

        start_snow_date = as_datetime(self.mars["start_date"])
        with contextlib.suppress(KeyError):
            start_snow_date = as_datetime(self.mars["start_snow_date"])

        self.exist_snow = exist_snow and self.bd_basetime >= start_snow_date
        # Split mars by bdint
        self.split_mars = self.config["suite_control.split_mars"]
        if self.split_mars:
            self.prep_step = ast.literal_eval(self.config["task.args.prep_step"])

        self.prepdir = Path(
            self.platform.substitute(
                self.config["system.marsdir"],
                basetime=self.bd_basetime,
                validtime=self.basetime,
            )
        )
        self.mars_version = int(
            self.config.get("submission.task_exceptions.Marsprep.mars_version", "6")
        )
        logger.info("MARS data expected in:{}", self.prepdir)

    @staticmethod
    def mars_selection(selection: str, config: ParsedConfig) -> dict:
        """Copy default settings if requested.

        Args:
            selection             (str): The selection to use.
            config (deode.ParsedConfig): Configuration object

        Returns:
             mars                (dict): mars config section

        """
        mars = config[f"mars.{selection}"].dict()
        if "expver" not in mars:
            mars["expver"] = selection

        # Copy default settings if requested
        if "default" in mars:
            default = config[f"mars.{mars['default']}"]
            for k in default:
                if k not in mars:
                    mars[k] = default[k]

        return mars

    @cached_property
    def mars(self) -> dict:
        """Get mars selection."""
        selection = self.platform.substitute(self.config["boundaries.ifs.selection"])
        return self.mars_selection(selection, self.config)

    def update_data_request(
        self,
        request: BaseRequest,
        prefetch: bool,
        specify_domain: bool,
        bdmembers: Optional[List[int]] = None,
        grid: Optional[str] = None,
        source: Optional[str] = None,
    ):
        """Create ECMWF MARS system request.

        Args:
            request:            BaseRequest object to update
            prefetch:           Retrieve or stage
            specify_domain:     Use lat/lon and rotation or use global (default)
            bdmembers:          Boundary members to retrieve in case of eps.
            grid:               Specific grid for some request. Default None.
            source:             Sorce for retrieve data from disk. Defaults None.

        """
        if grid is not None and self.mars_version == 6:
            request.add_grid(grid)

        # Additional request parameters if EPS
        # Only if all members are True like. This makes it possible to distinguish
        # between a deterministic run with no boundary member nesting (members = [None])
        # and an ensemble run with boundary member nesting (e.g. members = [0]
        # for a one member ensemble, members = [0, 1, 2] for a three member ensemble etc.)
        if bdmembers and all(bdmembers):
            request.add_eps_members(bdmembers, prefetch=prefetch)

        if self.mars_version == 6:
            request.add_process()
        request.add_levelist(self.mars["levelist"])

        # Set stream
        stream = get_value_from_dict(self.mars["stream"], request.time)
        request.request["STREAM"] = [stream]

        # Retrieve from already fetched data
        if not prefetch:
            request.update_based_on_target(source)

        request.add_database_options()

        if specify_domain:
            request.request.update(
                {
                    "GRID": [get_value_from_dict(self.mars["grid"], request.date)],
                    "AREA": [get_domain_data(self.config)],
                }
            )

    def execute(self):
        """Run task.

        Define run sequence.

        Raises:
            RuntimeError: If there is an issue with the work folder.
        """
        try:
            if not os.path.exists(self.prepdir):
                deodemakedirs(
                    self.prepdir, unixgroup=self.platform.get_platform_value("unix_group")
                )
        except OSError as e:
            raise RuntimeError(f"Error while preparing the mars folder: {e}") from e

        if self.split_mars and self.prep_step:
            logger.debug("*** Need only latlon data")
        else:
            if self.split_mars:
                self.steps = [self.steps[int(self.config["task.args.bd_index"])]]

            logger.info("Need steps:{}", self.steps)
            self.get_grid_point_surface_data()
            self.get_spectral_harmonic_data()
            self.get_grid_point_upper_air_data()

        self.get_sfx_data()

    def get_grid_point_surface_data(self):
        """Get grid point surface data."""
        tag = "ICMGG"
        steps, members_dict = get_steps_and_members_to_retrieve(
            self.steps, self.prepdir, tag, self.bdmembers
        )
        if steps:
            self.get_gg_data(tag, steps, members_dict)

            exist_soil = False
            with contextlib.suppress(KeyError):
                gg_soil_param = get_value_from_dict(
                    self.mars["GG_soil"], self.init_date_str
                )
                gg1_param = get_value_from_dict(self.mars["GG1"], self.init_date_str)
                exist_soil = True

            additional_data = {}
            if exist_soil:
                additional_data["common_data"] = self.get_gg_soil_data(
                    tag, params={"GG_soil": gg_soil_param, "GG1": gg1_param}
                )

            else:
                self.fmanager.input(f"{self.sfcdir}/sfcfile", "sfcdata")

                additional_data["common_data"] = get_and_remove_data("sfcdata")

            if self.exist_snow:
                additional_data |= self.get_gg_snow_data(
                    tag, steps, members_dict, self.param_snow
                )

                add_additional_file_specific_data(additional_data=additional_data)

                move_files(tag, steps, members_dict, self.prepdir)
            else:
                add_additional_data_to_all(tag, steps, members_dict, additional_data)
                move_files(tag, steps, members_dict, self.prepdir)

    def get_spectral_harmonic_data(self):
        """Get spectral harmonic data."""
        tag = "ICMSH"
        steps, members_dict = get_steps_and_members_to_retrieve(
            self.steps, self.prepdir, tag, self.bdmembers
        )

        if steps:
            self.get_sh_data(
                tag,
                steps,
                members_dict,
            )

            additional_data = {"z": self.get_shz_data(tag)}

            add_additional_data_to_all(tag, steps, members_dict, additional_data)
            move_files(tag, steps, members_dict, self.prepdir)

    def get_grid_point_upper_air_data(self):
        """Get gridpoint upper air data."""
        tag = "ICMUA"
        steps, members_dict = get_steps_and_members_to_retrieve(
            self.steps, self.prepdir, tag, self.bdmembers
        )
        if steps:
            self.get_ua_data(tag, steps, members_dict)
            logger.info(
                "steps {}, members_dict {}",
                steps,
                members_dict,
            )
            move_files(tag, steps, members_dict, self.prepdir)

    def get_sfx_data(self):
        """Get SFX data."""
        # Split the lat/lon part and perform it here
        mars_file_check_list = []
        bddir_sfx = Path(
            self.platform.substitute(
                self.config["system.bddir_sfx"],
                basetime=self.bd_basetime,
                validtime=self.basetime,
            )
        )
        for member in self.bdmembers:
            bdfile_sfx = self.config["system.bdfile_sfx_template"].replace(
                "@BDMEMBER@", str(member)
            )
            prep_filename = self.platform.substitute(
                bdfile_sfx,
                basetime=self.bd_basetime,
                validtime=self.basetime,
            )
            mars_file_check_list.append(bddir_sfx / prep_filename)

        if all(
            os.path.exists(mars_file_check) for mars_file_check in mars_file_check_list
        ):
            logger.info("Prep files already exists as {}", mars_file_check_list)
        elif self.split_mars and not self.prep_step:
            logger.debug("No need Prep file")
        else:
            self.get_lat_lon_data()
            self.get_geopotential_latlon()
            # Get the file list to join
            for member in self.bdmembers:
                bdfile_sfx = self.config["system.bdfile_sfx_template"].replace(
                    "@BDMEMBER@", str(member or 0)
                )
                prep_filename = self.platform.substitute(
                    bdfile_sfx,
                    basetime=self.bd_basetime,
                    validtime=self.basetime,
                )
                # Default to 0 if member is None
                prep_pattern = f"mars_latlon*_{member or 0}"
                logger.info("prep_pattern: {}", prep_pattern)
                filenames = list_files_join(self.wdir, prep_pattern)
                logger.info(filenames)
                with open(prep_filename, "ab") as output_file:
                    for filename in filenames:
                        with open(filename, "rb") as input_file:
                            output_file.write(input_file.read())
                shutil.move(
                    prep_filename,
                    os.path.join(self.prepdir, prep_filename),
                )

    def get_lat_lon_data(self):
        """Get Lat/Lon data."""
        prefetch = False

        param = (
            get_value_from_dict(self.mars["GG"], self.init_date_str)
            + "/"
            + get_value_from_dict(self.mars["GG_sea"], self.init_date_str)
        )

        for member in self.bdmembers:
            data_type = self.mars["type_AN"] if member == 0 else self.mars["type_FC"]
            self._build_and_run_request(
                req_file_name="latlonGG.req",
                data_type=data_type,
                levtype="SFC",
                param=param,
                steps=[self.steps[0]],
                members=[member],
                target=f"mars_latlonGG_{member or 0}",
                prefetch=prefetch,
                specify_domain=True,
                # Default to ICMGG_0+* if member is None
                source=f'"{self.prepdir}/ICMGG_{member or 0}+{self.steps[0]}"',
                write_method="retrieve" if self.mars_version == 6 else "read",
            )

    def get_geopotential_latlon(self):
        """Get geopotential in lat/lon."""
        prefetch = False
        # Retrieve for Surface Geopotential in lat/lon
        param = get_value_from_dict(self.mars["SHZ"], self.init_date_str)
        data_type = get_value_from_dict(self.mars["GGZ_type"], self.init_date_str)
        lev_type = get_value_from_dict(self.mars["Zlev_type"], self.init_date_str)

        first_member = self.bdmembers[0]
        self._build_and_run_request(
            req_file_name="latlonz.req",
            data_type=data_type,
            levtype=lev_type,
            param=param,
            steps=[self.steps[0]],
            members=[first_member],
            target=f"mars_latlonZ_{first_member or 0}",
            prefetch=prefetch,
            specify_domain=True,
            # Default to ICMSH_0+* if member is None
            source=f'"{self.prepdir}/ICMSH_{first_member or 0}+{self.steps[0]}"',
            write_method="retrieve" if self.mars_version == 6 else "read",
        )
        for member in self.bdmembers[1:]:
            self.fmanager.input(
                f"mars_latlonZ_{first_member or 0}", f"mars_latlonZ_{member}"
            )

    def get_sh_data(self, tag: str, steps: List[int], members_dict: Dict[str, List[int]]):
        """Get SH data."""
        logger.info("Retrieving {} data for steps: {}", tag, steps)
        param = get_value_from_dict(self.mars["SH"], self.init_date_str)

        for member_type, members in members_dict.items():
            data_type = (
                self.mars["type_AN"]
                if member_type == "control_member"
                else self.mars["type_FC"]
            )
            self._build_and_run_request(
                req_file_name=f"{member_type}_{tag}.req",
                data_type=data_type,
                levtype="ML",
                param=param,
                steps=steps,
                members=members,
                target=(
                    f'"{tag}_0+[STEP]"'
                    if member_type == "control_member"
                    or not all(members)  # Default to target="*_0+*" if member is None
                    else f'"{tag}+[STEP]"'
                ),
                grid=self.mars["grid_ML"],
            )

    def get_shz_data(self, tag: str):
        """Get geopotential in spectral harmonic."""
        # there is no sh geopotential in LUMI fdb
        if self.mars["class"] == "D1":
            # the spectral orography on Lumi is taken from sfcdir,
            # we use the one with tco=2559_4
            self.fmanager.input(f"{self.sfcdir}/../2559_4/sporog", f"{tag}.Z")

        else:
            param = get_value_from_dict(self.mars["SHZ"], self.init_date_str)
            self._build_and_run_request(
                req_file_name=f"{tag}Z.req",
                data_type=self.mars["SHZ_type"],
                levtype="ML",
                param=param,
                steps=[0],
                target=f'"{tag}.Z"',
                grid=self.mars["grid_ML"],
            )

        return get_and_remove_data(f"{tag}.Z")

    def get_ua_data(self, tag: str, steps: List[int], members_dict: Dict[str, List[int]]):
        """Get upper air data."""
        logger.info("Retrieving {} data for steps: {}", tag, steps)
        param = get_value_from_dict(self.mars["UA"], self.init_date_str)

        for member_type, members in members_dict.items():
            data_type = (
                self.mars["type_AN"]
                if member_type == "control_member"
                else self.mars["type_FC"]
            )
            self._build_and_run_request(
                req_file_name=f"{member_type}_{tag}.req",
                data_type=data_type,
                levtype="ML",
                param=param,
                steps=steps,
                members=members,
                target=(
                    f'"{tag}_0+[STEP]"'
                    if member_type == "control_member"
                    or not all(members)  # Default to target="*_0+*" if member is None
                    else f'"{tag}+[STEP]"'
                ),
                grid=self.mars["grid_ML"],
            )

    def get_gg_data(self, tag: str, steps: List[int], members_dict: Dict[str, List[int]]):
        """Get gridpoint surface data."""
        logger.info("Retrieving {} data for steps: {}", tag, steps)
        param = (
            get_value_from_dict(self.mars["GG"], self.init_date_str)
            + "/"
            + get_value_from_dict(self.mars["GG_sea"], self.init_date_str)
        )

        for member_type, members in members_dict.items():
            data_type = (
                self.mars["type_AN"]
                if member_type == "control_member"
                else self.mars["type_FC"]
            )

            self._build_and_run_request(
                req_file_name=f"{member_type}_{tag}.req",
                data_type=data_type,
                levtype="SFC",
                param=param,
                steps=steps,
                members=members,
                target=(
                    f'"{tag}_0+[STEP]"'
                    if member_type == "control_member"
                    or not all(members)  # Default to target="*_0+*" if member is None
                    else f'"{tag}+[STEP]"'
                ),
            )

    def get_gg_soil_data(self, tag: str, params: dict):
        """Get soil gridpoint data."""
        for param_name, param in params.items():
            target = f"{tag}.{param_name}"
            self._build_and_run_request(
                req_file_name=f"{target}.req",
                data_type=self.mars["type_AN"],
                levtype="SFC",
                param=param,
                steps=[0],
                target=target,
            )
        # Collect and return the data from target files
        # (Read the single-step MARS files first, hence the reversed order)
        data = b""
        for param_name in reversed(params.keys()):
            target = f"{tag}.{param_name}"
            data += get_and_remove_data(target)
        return data

    def get_gg_snow_data(
        self, tag: str, steps: List[int], members_dict: Dict[str, List[int]], param
    ):
        """Get soil gridpoint data."""
        additional_data: Dict[str, bytes] = {}
        for member_type, members in members_dict.items():
            data_type = (
                self.mars["type_AN"]
                if member_type == "control_member"
                else self.mars["type_FC"]
            )
            self._build_and_run_request(
                req_file_name=f"{member_type}_{tag}.snow.req",
                data_type=data_type,
                levtype="SOL",
                param=param,
                steps=steps,
                members=members,
                target=(
                    f'"{tag}.snow_0+[STEP]"'
                    if member_type == "control_member"
                    or not all(members)  # Default to target="*_0+*" if member is None
                    else f'"{tag}.snow+[STEP]"'
                ),
            )

            for step in steps:
                for member in members:
                    # Default to "*_0+{step}" if member is None
                    key = f"{tag}_{member or 0}+{step}"
                    file_snow = f"{tag}.snow_{member or 0}+{step}"
                    additional_data[key] = get_and_remove_data(file_snow)

        return additional_data

    def _build_and_run_request(
        self,
        req_file_name: str,
        data_type: str,
        levtype: str,
        param: str,
        steps: List[int],
        target: str,
        members: Optional[List[int]] = None,
        source: Optional[str] = None,
        prefetch: bool = True,
        specify_domain: bool = False,
        write_method: str = "retrieve",
        grid: Optional[str] = None,
    ) -> None:
        """Build and run request to retrieve data from mars.

        Args:
            req_file_name: Name of the request file.
            data_type: Data type.
            levtype: Level type.
            param: Parameter(s) to retrieve.
            steps: Steps to retrieve.
            target: Name of the target file.
            members: Members to retrieve data from.
            source: Source file to retrieve data from.
            prefetch: Prefetch.
            specify_domain: Whether to specify domain or not.
            write_method: The write method to use when writing the request to file.
            grid: The grid to use.
        """
        request = BaseRequest(
            class_=self.mars["class"],
            data_type=data_type,
            expver=self.mars["expver"],
            levtype=levtype,
            date=self.init_date_str,
            time=self.init_hour_str,
            steps=steps,
            param=param,
            target=target,
        )
        self.update_data_request(
            request,
            prefetch=prefetch,
            specify_domain=specify_domain,
            bdmembers=members,
            source=source,
            grid=grid,
        )
        write_mars_req(request, req_file_name, write_method)
        BatchJob(os.environ, wrapper=self.wrapper).run(
            f"{self.get_binary('mars')} {req_file_name}"
        )
