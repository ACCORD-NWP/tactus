"""Marsprep."""

import ast
import contextlib
import os
from functools import cached_property
from pathlib import Path, PurePosixPath
from typing import Dict, List, Optional

from tactus.boundary_utils import Boundary
from tactus.datetime_utils import as_datetime, as_timedelta
from tactus.eps.eps_setup import get_member_config, infer_members
from tactus.logs import logger
from tactus.mars_utils import (
    BaseRequest,
    add_additional_data_to_all,
    add_additional_file_specific_data,
    check_data_available,
    compile_target,
    fix_snow_layer,
    get_and_remove_data,
    get_domain_data,
    get_mars_keys,
    get_steplist,
    get_steps_and_members_to_retrieve,
    get_value_from_dict,
    mars_selection,
    mars_write_method,
    move_files,
    wait4_file,
    waitfor_files,
    write_compute_mars_req,
    write_retrieve_mars_req,
    write_write_mars_req,
)
from tactus.os_utils import join_files, list_files_join, tactusmakedirs
from tactus.scheduler import EcflowServer
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob


class Marsprep(Task):
    """Marsprep task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (tactus.ParsedConfig): Configuration

        Raises:
            ValueError: No data for this date.
        """
        Task.__init__(self, config, __class__.__name__)

        # Get bdmember(s) from member specific eps setting if member specific
        # mars prep is enabled
        if self.config["suite_control.member_specific_mars_prep"]:
            try:
                member_ = int(os.environ.get("MEMBER"))
            except ValueError as exc:
                raise ValueError(
                    "MEMBER environment variable must be an integer if "
                    "suite_control.member_specific_mars_prep is True"
                ) from exc
            else:
                member_config = get_member_config(self.config, member_)
                bdmember_config_value = member_config["boundaries.ifs.bdmember"]

        # Get bdmember(s) from eps members settings (attempted first) or
        # boundaries.ifs.bdmember.
        else:
            try:
                bdmember_config_value = self.config[
                    "eps.member_settings.boundaries.ifs.bdmember"
                ]
            except KeyError:
                bdmember_config_value = self.config["boundaries.ifs.bdmember"]

        self.bdmember = infer_members(self.platform.substitute(bdmember_config_value))
        # Default to [None] if self.bdmember is empty to cover the deterministic
        # case with no boundary member nesting.
        if not self.bdmember:
            self.bdmember = [None]

        # path to sfcdata on disk
        resolution = self.mars["resolution"]
        self.platform.store_macro("RESOLUTION", resolution)
        self.sfcdir = Path(
            self.platform.substitute(
                str(self.platform.get_platform_value("global_sfcdir"))
            )
        )

        logger.info(f"sfc dir: {self.sfcdir}")

        # Get the times from config.toml
        self.basetime = as_datetime(self.config["general.times.basetime"])
        forecast_range = as_timedelta(self.config["general.times.forecast_range"])

        # Get boundary informations
        self.boundary = Boundary(config)

        # Check if there are data for specific date in mars
        check_data_available(self.boundary.bd_basetime, self.mars)

        self.steps = get_steplist(
            self.boundary.bd_offset, forecast_range, self.boundary.bdint
        )

        self.init_date_str = self.boundary.bd_basetime.strftime("%Y%m%d")
        self.init_hour_str = self.boundary.bd_basetime.strftime("%H")

        exist_snow = False
        with contextlib.suppress(KeyError):
            self.param_snow = get_value_from_dict(
                self.mars["GG_snow"], self.init_date_str
            )
            exist_snow = True

        start_snow_date = as_datetime(self.mars["start_date"])
        with contextlib.suppress(KeyError):
            start_snow_date = as_datetime(self.mars["start_snow_date"])

        self.exist_snow = exist_snow and self.boundary.bd_basetime >= start_snow_date
        # Split mars by bdint
        self.split_mars = self.config["suite_control.split_mars"]
        if self.split_mars:
            self.prep_step = ast.literal_eval(self.config["task.args.prep_step"])

        self.prepdir = Path(
            self.platform.substitute(
                self.config["system.bddir"],
                basetime=self.boundary.bd_basetime,
                validtime=self.basetime,
            )
        )
        logger.info("MARS data expected in:{}", self.prepdir)

        self.mars_version = int(
            self.config.get("submission.task_exceptions.Marsprep.mars_version", "6")
        )

        self.mars_bin = self.get_binary("mars")

        # Use fixed static orography files (spherical harmonics and/or gaussian grid)
        # TODO: check existence of static orography files & read settings
        if "use_static_sh" in self.mars:
            self.use_static_sh_oro = bool(self.mars["use_static_sh"])
        else:
            self.use_static_sh_oro = False
        if self.use_static_sh_oro:
            self.sh_oro_source = self.mars["sh_oro_source"]
            logger.info("Using static SH orography file {}", self.sh_oro_source)

        if "use_static_gg" in self.mars:
            self.use_static_gg_oro = bool(self.mars["use_static_gg"])
        else:
            self.use_static_gg_oro = False
        if self.use_static_gg_oro:
            self.gg_oro_source = self.mars["gg_oro_source"]
            logger.info("Using static GG orography file {}", self.gg_oro_source)

    @cached_property
    def mars(self) -> dict:
        """Get mars selection."""
        selection = self.platform.substitute(self.config["boundaries.ifs.selection"])
        return mars_selection(selection, self.config)

    def update_data_request(
        self,
        request: BaseRequest,
        prefetch: bool,
        specify_domain: bool,
        bdmember: Optional[List[int]] = None,
        grid: Optional[str] = None,
        source: Optional[str] = None,
        fieldset: Optional[str] = None,
    ):
        """Create ECMWF MARS system request.

        Args:
            request:            BaseRequest object to update
            prefetch:           Retrieve or stage
            specify_domain:     Use lat/lon and rotation or use global (default)
            bdmember:          Boundary members to retrieve in case of eps.
            grid:               Specific grid for some request. Default None.
            source:             Sorce for retrieve data from disk. Defaults None.
            fieldset:           Name of fieldset. Defaults None.
        """
        if grid is not None and self.mars_version == 6:
            request.update_request({"GRID": grid})

        # Additional request parameters if EPS
        # Only if all members are True like. This makes it possible to distinguish
        # between a deterministic run with no boundary member nesting (members = [None])
        # and an ensemble run with boundary member nesting (e.g. members = [0]
        # for a one member ensemble, members = [0, 1, 2] for a three member ensemble etc.)
        if bdmember and all(bdmember):
            request.update_request({"NUMBER": "/".join(map(str, bdmember))})

        if self.mars_version == 6:
            request.update_request({"PROCESS": "LOCAL"})
        request.add_levelist(self.mars["levelist"])

        if request.param == "32":
            request.update_request({"LEVELIST": "1"})

        # Set stream
        base_stream = get_value_from_dict(self.mars["stream"], request.time)
        if bdmember == [0]:
            stream = self.mars.get("stream_control", base_stream)
        else:
            stream = base_stream
        request.update_request({"STREAM": stream})

        # Retrieve from already fetched data
        if not prefetch:
            request.update_request({"SOURCE": source})
            # NOTE: the following could be moved to get_geopotential_latlon()
            if "mars_latlonZ" in request.target and (
                self.use_static_gg_oro or self.use_static_sh_oro
            ):
                z_keys = get_mars_keys(source)
                request.update_request(z_keys)

        request.add_database_options()

        if specify_domain:
            request.update_request({
                "GRID": get_value_from_dict(self.mars["grid"], request.date),
                "AREA": get_domain_data(self.config),
            })

        if fieldset:
            request.update_request({"FIELDSET": fieldset})

    def execute(self):
        """Run task.

        Define run sequence.

        Raises:
            RuntimeError: If there is an issue with the work folder.
        """
        try:
            if not os.path.exists(self.prepdir):
                tactusmakedirs(
                    self.prepdir,
                    unixgroup=self.platform.get_platform_value("unix_group"),
                )
        except OSError as e:
            raise RuntimeError(f"Error while preparing the mars folder: {e}") from e

        # Suspend the model task if there is a mirroring
        if self.config["suite_control.mirror_globalDT"]:
            current_path = PurePosixPath(os.environ["ECF_NAME"])
            model_path = current_path.parents[1] / "Mirrors"
            server = EcflowServer(self.config)
            server.suspend(str(model_path))

        if self.split_mars and self.prep_step:
            logger.debug("*** Need only latlon data")
        else:
            if self.split_mars:
                self.steps = [self.steps[int(self.config["task.args.bd_index"])]]

            logger.info("Need steps:{}", self.steps)
            self.get_grid_point_surface_data()
            self.get_spectral_harmonic_data()
            self.get_grid_point_upper_air_data()

            if self.config["suite_control.do_interpolsstsic"]:
                self.get_sst_data()

        if not self.config["boundaries.bd_has_surfex"]:
            self.get_sfx_data()

    def get_grid_point_surface_data(self):
        """Get grid point surface data."""
        tag = "ICMGG"
        steps, waitfor_steps, members_dict, _, _ = get_steps_and_members_to_retrieve(
            self.steps,
            self.prepdir,
            tag,
            self.bdmember,
            platform=self.platform,
            use_lockfile=self.config["boundaries.do_slaf"],
        )
        if steps:
            self.get_gg_data(tag, steps, members_dict)
            if "CY50" in self.config["general.cycle"]:
                fix_snow_layer(tag, steps, members_dict)
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
            else:
                add_additional_data_to_all(tag, steps, members_dict, additional_data)

            move_files(tag, steps, members_dict, self.prepdir)
        if waitfor_steps:
            waitfor_files(tag, waitfor_steps, members_dict, self.prepdir)

    def get_spectral_harmonic_data(self):
        """Get spectral harmonic data."""
        tag = "ICMSH"
        steps, waitfor_steps, members_dict, _, _ = get_steps_and_members_to_retrieve(
            self.steps,
            self.prepdir,
            tag,
            self.bdmember,
            platform=self.platform,
            use_lockfile=self.config["boundaries.do_slaf"],
        )

        if steps:
            self.get_sh_data(
                tag,
                steps,
                members_dict,
            )

            additional_data = {}
            additional_data["common_data"] = self.get_shz_data(tag)

            param_spectral_temperature = None
            with contextlib.suppress(KeyError):
                param_spectral_temperature = get_value_from_dict(
                    self.mars["SH_temperature"], self.init_date_str
                )

            if param_spectral_temperature:
                additional_data |= self.get_sh_temperature_data(
                    tag, steps, members_dict, param_spectral_temperature
                )
                add_additional_file_specific_data(additional_data=additional_data)

            else:
                add_additional_data_to_all(tag, steps, members_dict, additional_data)
            move_files(tag, steps, members_dict, self.prepdir)
        if waitfor_steps:
            waitfor_files(tag, waitfor_steps, members_dict, self.prepdir)

    def get_grid_point_upper_air_data(self):
        """Get gridpoint upper air data."""
        tag = "ICMUA"
        steps, waitfor_steps, members_dict, _, _ = get_steps_and_members_to_retrieve(
            self.steps,
            self.prepdir,
            tag,
            self.bdmember,
            platform=self.platform,
            use_lockfile=self.config["boundaries.do_slaf"],
        )
        if steps:
            self.get_ua_data(tag, steps, members_dict)
            logger.info(
                "steps {}, members_dict {}",
                steps,
                members_dict,
            )
            move_files(tag, steps, members_dict, self.prepdir)
        if waitfor_steps:
            waitfor_files(tag, waitfor_steps, members_dict, self.prepdir)

    def get_bdmember_fetch_list(self, bddir: str, bdfile: str):
        """Builds the list of missing files to fetch from MARS."""
        mars_file_check_list = {}
        prep_dir = Path(
            self.platform.substitute(
                bddir,
                basetime=self.boundary.bd_basetime,
                validtime=self.basetime,
            )
        )

        for member in self.bdmember:
            prep_filename = self.platform.substitute(
                bdfile.replace("@BDMEMBER@", str(member or 0)),
                basetime=self.boundary.bd_basetime,
                validtime=self.basetime,
            )

            mars_file_check_list[member] = prep_dir / prep_filename

        bdmember_fetch_list = [
            bdmember
            for bdmember, mars_file_check in mars_file_check_list.items()
            if not os.path.exists(mars_file_check)
        ]

        if not bdmember_fetch_list:
            logger.info("Prep files already exists as:")
            for bdmember, filename in mars_file_check_list.items():
                logger.info(f" {bdmember}: {filename}")

        elif self.split_mars and not self.prep_step:
            logger.debug("No need Prep file")
            bdmember_fetch_list = []
        else:
            for member in self.bdmember:
                if member not in bdmember_fetch_list:
                    logger.info(
                        "Prep file member={} already exists as {}",
                        member,
                        mars_file_check_list[member],
                    )
                else:
                    logger.info(
                        "Create prep file member={} as {}",
                        member,
                        mars_file_check_list[member],
                    )

        return mars_file_check_list, bdmember_fetch_list

    def get_sfx_data(self):
        """Get SFX data."""
        bddir = self.config["system.bddir_sfx"]

        try:
            if not os.path.exists(bddir):
                tactusmakedirs(
                    bddir, unixgroup=self.platform.get_platform_value("unix_group")
                )
        except OSError as e:
            raise RuntimeError(f"Error while preparing the bddir_sfx folder: {e}") from e

        bdfile = self.config["file_templates.bdfile_sfx.archive"]
        mars_file_check_list, bdmember_fetch_list = self.get_bdmember_fetch_list(
            bddir, bdfile
        )

        # Split the lat/lon part and perform it here
        logger.debug("bdmember_fetch_list: {}", bdmember_fetch_list)
        if bdmember_fetch_list:
            self.get_lat_lon_data(bdmember_fetch_list)
            self.get_geopotential_latlon(self.bdmember[0], bdmember_fetch_list)

        # Get the file list to join
        for bdmember in bdmember_fetch_list:
            prep_pattern = f"mars_latlon*_{bdmember or 0}"
            filenames = list_files_join(self.wdir, prep_pattern)
            prep_filepath = mars_file_check_list[bdmember]
            join_files(filenames, prep_filepath)

    def get_sst_data(self):
        """Get SST data."""
        bddir_sst = Path(
            self.platform.substitute(
                self.config["system.bddir_sst"],
                basetime=self.boundary.bd_basetime,
                validtime=self.basetime,
            )
        )

        try:
            if not os.path.exists(bddir_sst):
                tactusmakedirs(
                    bddir_sst, unixgroup=self.platform.get_platform_value("unix_group")
                )
        except OSError as e:
            raise RuntimeError(f"Error while preparing the bddir_sst folder: {e}") from e

        bdfile = self.config["file_templates.bdfile_sst.model"]

        (
            _,
            _,
            members_dict,
            missing_steps_per_member,
            waitfor_steps_per_member,
        ) = get_steps_and_members_to_retrieve(
            self.steps,
            bddir_sst,
            bdfile,
            self.bdmember,
            platform=self.platform,
            basetime=self.boundary.bd_basetime,
            validtime=self.basetime,
            use_lockfile=self.config["boundaries.do_slaf"],
        )

        if not missing_steps_per_member:
            logger.info("All SST files already exists")
        else:
            # Get lat lon data
            self.get_lat_lon_sst_data(missing_steps_per_member, members_dict)

            # Get the file list to join
            for member in missing_steps_per_member:
                steps = missing_steps_per_member[member]
                logger.info(f"Missing steps for member {member}: {steps}")
                for step in steps:
                    merge_pattern = f"mars_latlonGG*_{member or 0}+{step}"
                    filenames = list_files_join(self.wdir, merge_pattern)
                    sst_filename = self.platform.substitute(
                        bdfile.replace("@BDMEMBER@", str(member or 0)),
                        basetime=self.boundary.bd_basetime,
                        validtime=self.basetime,
                        bd_index=step,
                    )
                    sst_filepath = bddir_sst / sst_filename
                    join_files(filenames, sst_filepath)

        if waitfor_steps_per_member:
            # Wait for the relevant files
            for member in waitfor_steps_per_member:
                steps = waitfor_steps_per_member[member]
                for step in steps:
                    sst_filename = self.platform.substitute(
                        bdfile.replace("@BDMEMBER@", str(member or 0)),
                        basetime=self.boundary.bd_basetime,
                        validtime=self.basetime,
                        bd_index=step,
                    )
                    dest_file = bddir_sst / sst_filename
                    wait4_file(dest_file)
                    logger.info(f"Waitfor_steps found: {dest_file}")

    def get_lat_lon_data(self, bdmember_list: List[int]):
        """Get Lat/Lon data.

        Args:
            bdmember_list (List[int]): Members to fetch data for

        """
        prefetch = False

        param = (
            get_value_from_dict(self.mars["GG"], self.init_date_str)
            + "/"
            + get_value_from_dict(self.mars["GG_sea"], self.init_date_str)
        )
        if "CY50" in self.config["general.cycle"]:
            param = "/".join(x for x in param.split("/") if x != "32")

        for member in bdmember_list:
            data_type = self.mars["type_AN"] if member == 0 else self.mars["type_FC"]
            # Default to ICMGG_0+* if member is None
            source = f'"{self.prepdir}/ICMGG_{member or 0}+{self.steps[0]}"'
            if self.config["boundaries.do_slaf"]:
                wait4_file(ast.literal_eval(source))
            self._build_and_run_retrieve_request(
                req_file_name="latlonGG.req",
                data_type=data_type,
                levtype="SFC",
                param=param,
                steps=[self.steps[0]],
                members=[member],
                target=f"mars_latlonGG_{member or 0}",
                prefetch=prefetch,
                specify_domain=True,
                source=source,
                write_method=mars_write_method(self.mars_version),
            )
            if "CY50" in self.config["general.cycle"]:
                self._build_and_run_retrieve_request(
                    req_file_name="latlonGG.req",
                    data_type=data_type,
                    levtype="SOL",
                    param="32",
                    steps=[self.steps[0]],
                    members=[member],
                    target=f"mars_latlonGG_32_{member or 0}",
                    prefetch=prefetch,
                    specify_domain=True,
                    source=source,
                    write_method=mars_write_method(self.mars_version),
                )

    def get_lat_lon_sst_data(
        self,
        missing_steps_per_member: Dict[int, List[int]],
        members_dict: Dict[str, List[int]],
    ):
        """Get lat/lon data for sst."""
        tag_mask = "ICMGG_maskland"
        tag_sea = "sea_latlon"
        tag_aux = "aux_latlon"

        # JS: MARS 7 doesn't support all the features as supported by MARS 6
        use_verbose_mode = self.mars_version == 7 or self.config.get(
            "submission.task_exceptions.Marsprep.verbose_ICMGG_maskland", False
        )

        if use_verbose_mode:
            logger.info("MARS request for 'ICMGG_maskland' is using verbose mode")

        prefetch = False
        param_sea = get_value_from_dict(self.mars["GG_sea"], self.init_date_str)
        param_lsm = get_value_from_dict(self.mars["GG_lsm"], self.init_date_str)
        param_skt = get_value_from_dict(self.mars["GG_skt"], self.init_date_str)

        target_aux = "mars_latlonGG"
        target_maskland = "mars_latlonGG_maskland"

        fieldset_dict = dict.fromkeys(["sic_sst", "lsm"])
        fieldset_dict["sic_sst"] = {"param": param_sea, "target": target_aux}
        fieldset_dict["lsm"] = {"param": param_lsm, "target": target_aux}

        for member_type, members in members_dict.items():
            data_type = (
                self.mars["type_AN"]
                if member_type == "control_member"
                else self.mars["type_FC"]
            )

            members_to_process = list(set(members) & set(missing_steps_per_member.keys()))
            for member in members_to_process:
                # SST/SIC for updating during run (on lat/lon), must be done in loop as
                # COMPUTE doesn't seem to support multiple steps
                for step in missing_steps_per_member[member]:
                    source = compile_target(
                        f"{self.prepdir}/ICMGG", member_type, member, step
                    )
                    target = compile_target(tag_mask, member_type, member, step)
                    if self.config["boundaries.do_slaf"]:
                        wait4_file(ast.literal_eval(source))

                    if use_verbose_mode:
                        # Make an verbose formula
                        target_striped = target.strip('"')
                        formula = [
                            {"formula": '"lsm > 0.5"', "fieldset": "mask"},
                            {
                                "formula": '"bitmap(sic, mask)"',
                                "fieldset": "sic_masked",
                                "target": f'"sic_{target_striped}"',
                            },
                            {
                                "formula": '"bitmap(sst, mask)"',
                                "fieldset": "sst_masked",
                                "target": f'"sst_{target_striped}"',
                            },
                        ]
                    else:
                        formula = '"bitmap(sic_sst, bitmap(lsm > 0.5, 1))"'

                    # Execute MARS request to mask land
                    self._build_and_run_compute_request(
                        req_file_name=f"{tag_mask}.req",
                        data_type=data_type,
                        levtype="SFC",
                        param=None,  # Params are per fieldset defined in fieldset_dict
                        steps=[step],
                        target=target,
                        fieldset_dict=fieldset_dict,
                        formula=formula,
                        members=[member],
                        source=source,
                        prefetch=prefetch,
                        specify_domain=False,
                        write_method=mars_write_method(self.mars_version),
                    )

                    # Interpolate masked ocean fields
                    self._build_and_run_retrieve_request(
                        req_file_name=f"{tag_sea}.req",
                        data_type=data_type,
                        levtype="SFC",
                        param=param_sea,
                        steps=[step],
                        target=compile_target(
                            target_maskland, member_type, member, step=step
                        ),
                        members=[member],
                        source=target,
                        prefetch=prefetch,
                        specify_domain=True,
                        write_method=mars_write_method(self.mars_version),
                    )

                    # Interpolate lsm & skt
                    self._build_and_run_retrieve_request(
                        req_file_name=f"{tag_aux}.req",
                        data_type=data_type,
                        levtype="SFC",
                        param=param_lsm + "/" + param_skt,
                        steps=[step],
                        target=compile_target(target_aux, member_type, member, step=step),
                        members=[member],
                        source=source,
                        prefetch=prefetch,
                        specify_domain=True,
                        write_method=mars_write_method(self.mars_version),
                    )

    def get_geopotential_latlon(self, first_member, bdmember_list):
        """Get geopotential in lat/lon.

        Args:
            first_member (int): First bdmember in ensemble
            bdmember_list (List[int]): Members to fetch data for

        """
        prefetch = False
        # Retrieve for Surface Geopotential in lat/lon
        param = get_value_from_dict(self.mars["SHZ"], self.init_date_str)
        data_type = get_value_from_dict(self.mars["GGZ_type"], self.init_date_str)
        lev_type = get_value_from_dict(self.mars["Zlev_type"], self.init_date_str)

        if self.use_static_gg_oro:
            self.fmanager.input(self.gg_oro_source, "gg_oro.Z")
            z_source = "gg_oro.Z"
        else:
            # Default to ICMSH_0+* if member is None
            z_source = f'"{self.prepdir}/ICMSH_{first_member or 0}+{self.steps[0]}"'
            if self.config["boundaries.do_slaf"]:
                wait4_file(ast.literal_eval(z_source))
        # NOTE: for z, the step is always 0, while steps[0] may be shifted!

        target = f"mars_latlonZ_{first_member or 0}"
        self._build_and_run_retrieve_request(
            req_file_name="latlonz.req",
            data_type=data_type,
            levtype=lev_type,
            param=param,
            steps=[0],
            members=[first_member],
            target=target,
            prefetch=prefetch,
            specify_domain=True,
            source=z_source,
            write_method=mars_write_method(self.mars_version),
        )
        for member in bdmember_list:
            if member == first_member:
                continue
            self.fmanager.input(target, f"mars_latlonZ_{member}")

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
            self._build_and_run_retrieve_request(
                req_file_name=f"{member_type}_{tag}.req",
                data_type=data_type,
                levtype="ML",
                param=param,
                steps=steps,
                members=members,
                target=compile_target(tag, member_type, members),
                grid=self.mars["grid_ML"],
            )

    def get_sh_temperature_data(
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
            self._build_and_run_retrieve_request(
                req_file_name=f"{member_type}_{tag}.temp.req",
                data_type=data_type,
                levtype="ML",
                param=param,
                steps=steps,
                members=members,
                target=compile_target(f"{tag}.temperature", member_type, members),
            )

            for step in steps:
                for member in members:
                    # Default to "*_0+{step}" if member is None
                    key = f"{tag}_{member or 0}+{step}"
                    file_temperature = f"{tag}.temperature_{member or 0}+{step}"
                    additional_data[key] = get_and_remove_data(file_temperature)

        return additional_data

    def get_shz_data(self, tag: str):
        """Get geopotential in spherical harmonics."""
        if self.use_static_sh_oro:
            self.fmanager.input(self.sh_oro_source, f"{tag}.Z")

        else:
            param = get_value_from_dict(self.mars["SHZ"], self.init_date_str)
            self._build_and_run_retrieve_request(
                req_file_name=f"{tag}Z.req",
                data_type=self.mars["SHZ_type"],
                levtype="ML",
                param=param,
                steps=[0],
                target=f'"{tag}.Z"',
                grid=self.mars["grid_ML"],
                members=[0],
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
            self._build_and_run_retrieve_request(
                req_file_name=f"{member_type}_{tag}.req",
                data_type=data_type,
                levtype="ML",
                param=param,
                steps=steps,
                members=members,
                target=compile_target(tag, member_type, members),
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

            self._build_and_run_retrieve_request(
                req_file_name=f"{member_type}_{tag}.req",
                data_type=data_type,
                levtype="SFC",
                param=param,
                steps=steps,
                members=members,
                target=compile_target(tag, member_type, members),
            )

    def get_gg_soil_data(self, tag: str, params: dict):
        """Get soil gridpoint data."""
        for param_name, param in params.items():
            target = f"{tag}.{param_name}"
            self._build_and_run_retrieve_request(
                req_file_name=f"{target}.req",
                data_type=self.mars["type_AN"],
                levtype="SFC",
                param=param,
                steps=[0],
                target=target,
                members=[0],
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
            self._build_and_run_retrieve_request(
                req_file_name=f"{member_type}_{tag}.snow.req",
                data_type=data_type,
                levtype="SOL",
                param=param,
                steps=steps,
                members=members,
                target=compile_target(f"{tag}.snow", member_type, members),
            )

            for step in steps:
                for member in members:
                    # Default to "*_0+{step}" if member is None
                    key = f"{tag}_{member or 0}+{step}"
                    file_snow = f"{tag}.snow_{member or 0}+{step}"
                    additional_data[key] = get_and_remove_data(file_snow)

        return additional_data

    def _build_retrieve_request(
        self,
        data_type: str,
        levtype: str,
        param: str,
        steps: List[int],
        target: str,
        members: Optional[List[int]] = None,
        source: Optional[str] = None,
        prefetch: bool = True,
        specify_domain: bool = False,
        grid: Optional[str] = None,
        fieldset: Optional[str] = None,
    ) -> BaseRequest:
        """Build request to retrieve data from mars.

        Args:
            data_type: Data type.
            levtype: Level type.
            param: Parameter(s) to retrieve.
            steps: Steps to retrieve.
            target: Name of the target file.
            members: Members to retrieve data from.
            source: Source file to retrieve data from.
            prefetch: Prefetch.
            specify_domain: Whether to specify domain or not.
            grid: The grid to use.
            fieldset: Name of fieldset. Defaults None.

        Returns:
            request: BaseRequest to retrieve data

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
            bdmember=members,
            grid=grid,
            source=source,
            fieldset=fieldset,
        )

        return request

    def _run_request_file(
        self,
        req_file_name: str,
    ) -> None:
        """Run request to retrieve data from mars.

        Args:
            req_file_name: Name of the request file.
        """
        BatchJob(os.environ, wrapper=self.wrapper).run(
            f"{self.get_binary('mars')} {req_file_name}"
        )

    def _build_and_run_retrieve_request(
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
        request = self._build_retrieve_request(
            data_type,
            levtype,
            param,
            steps,
            target,
            members,
            source,
            prefetch,
            specify_domain,
            grid,
        )

        # Write the request to file
        write_retrieve_mars_req(request, req_file_name, write_method)

        # Execute written request
        self._run_request_file(req_file_name)

    def _build_and_run_compute_request(
        self,
        req_file_name: str,
        data_type: str,
        levtype: str,
        param: str,
        steps: List[int],
        target: str,
        fieldset_dict: dict,
        formula: str | List[Dict],
        members: Optional[List[int]] = None,
        source: Optional[str] = None,
        prefetch: bool = True,
        specify_domain: bool = False,
        write_method: str = "retrieve",
        grid: Optional[str] = None,
    ) -> None:
        """Build and run request to retrieve and process data from mars.

        Args:
            req_file_name: Name of the request file.
            data_type: Data type.
            levtype: Level type.
            param: Parameter(s) to retrieve.
            steps: Steps to retrieve.
            target: Name of the target file.
            fieldset_dict: Dict with fieldset and fieldset-specific data
            formula: Formula for compute (or list of dics with formulas and fieldsets).
            members: Members to retrieve data from.
            source: Source file to retrieve data from.
            prefetch: Prefetch.
            specify_domain: Whether to specify domain or not.
            write_method: The write method to use when writing the request to file.
            grid: The grid to use.
        """
        # First request will write to a new file
        omode = "w"

        use_verbose_mode = isinstance(formula, list)

        for fieldset, fieldset_data in fieldset_dict.items():
            fieldset_param = param
            if "param" in fieldset_data:
                fieldset_param = fieldset_data["param"]

            fieldset_target = target
            if "target" in fieldset_data:
                fieldset_target = fieldset_data["target"]

            if use_verbose_mode:
                # MARS version 7 cannot handle reading multiple PARMS to one FIELDSET,
                # so split them into multiple requests
                fieldset_param_list = fieldset_param.split("/")
                fieldset_name_list = fieldset.split("_")
                if len(fieldset_param_list) != len(fieldset_name_list):
                    # Cannot split request into mulpile
                    logger.error(
                        (
                            "Cannot split MARS request; number of PARAMs (="
                            f"{len(fieldset_param_list)}"
                            ") and FIELDSETs (="
                            f"{len(fieldset_name_list)}"
                            ") not equal"
                        )
                    )
            else:
                fieldset_param_list = [fieldset_param]
                fieldset_name_list = [fieldset]

            for single_param, single_fieldset in zip(
                fieldset_param_list, fieldset_name_list
            ):
                request = self._build_retrieve_request(
                    data_type,
                    levtype,
                    single_param,
                    steps,
                    fieldset_target,
                    members,
                    source,
                    prefetch,
                    specify_domain,
                    grid,
                    single_fieldset,
                )

                # Write/append request to file
                write_retrieve_mars_req(request, req_file_name, write_method, omode)

                # All upcomming requests will be appened to the same file
                omode = "a"

        # Write/append compute request to file
        if use_verbose_mode:
            for formula_part in formula:
                if "fieldset" in formula_part:
                    write_compute_mars_req(
                        req_file_name,
                        formula=formula_part["formula"],
                        fieldset=formula_part["fieldset"],
                        omode=omode,
                    )

                if "target" in formula_part:
                    write_write_mars_req(
                        req_file_name,
                        fieldset=formula_part["fieldset"],
                        target=formula_part["target"],
                        omode=omode,
                    )
        else:
            write_compute_mars_req(
                req_file_name, formula=formula, target=target, omode=omode
            )

        # Execute written request
        self._run_request_file(req_file_name)

        if use_verbose_mode:
            # Join different output files
            generated_files = [
                formula_part.get("target").strip('"')
                for formula_part in formula
                if formula_part.get("target") is not None
            ]
            join_files(generated_files, target.strip('"'))
