"""Marsprep."""

import ast
import contextlib
import os
import shutil
from pathlib import Path

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..logs import logger
from ..mars_utils import (
    BaseRequest,
    check_data_available,
    get_date_time_info,
    get_domain_data,
    get_steps_and_members_to_retrieve,
    get_value_from_dict,
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

        # Get MARS selection
        self.mars = self.mars_selection()
        self.members = self.config["boundaries.ifs.bdmember"]

        # path to sfcdata on disk
        self.sfcdir = Path(self.platform.get_platform_value("global_sfcdir"))
        logger.info(f"sfc dir: {self.sfcdir}")

        # Get the times from config.toml
        basetime = as_datetime(self.config["general.times.basetime"])
        forecast_range = as_timedelta(self.config["general.times.forecast_range"])

        # Check if there are data for specific date in mars
        check_data_available(basetime, self.mars)

        # Get boundary informations
        bdint = as_timedelta(self.config["boundaries.bdint"])
        bdcycle = as_timedelta(self.mars["ifs_cycle_length"])
        bdcycle_start = as_timedelta(self.mars["ifs_cycle_start"])
        bdshift = as_timedelta(self.config["boundaries.bdshift"])

        # Get date/time/steps info
        self.init_date_str, self.init_hour_str, self.str_steps = get_date_time_info(
            basetime,
            forecast_range,
            bdint,
            bdcycle,
            bdshift,
        )

        if bdshift.total_seconds() % bdcycle.total_seconds() != 0:
            raise ValueError("bdshift needs to be a multiple of bdcycle!")

        bd_basetime = basetime - cycle_offset(
            basetime,
            bdcycle,
            bdcycle_start=bdcycle_start,
            bdshift=-bdshift,
        )
        logger.info("bd_basetime: {}", bd_basetime)

        # Split mars by bdint
        self.split_mars = self.config["suite_control.split_mars"]
        if self.split_mars:
            self.bd_index = int(self.config["task.args.bd_index"])
            self.prep_step = ast.literal_eval(self.config["task.args.prep_step"])

        self.prepdir = Path(
            self.platform.substitute(
                self.config["system.marsdir"],
                basetime=bd_basetime,
                validtime=basetime,
            )
        )
        self.prep_filename = self.platform.substitute(
            self.config["system.bdfile_sfx_template"],
            basetime=bd_basetime,
            validtime=basetime,
        )
        logger.info("MARS data expected in:{}", self.prepdir)

        self.mars_bin = self.get_binary("mars")
        self.batch = BatchJob(os.environ, wrapper=self.wrapper)

        self.additional_data = {}

    def add_additional_data(self, file_name, member):
        """Add additional data to dict.

        Args:
            file_name (str): The name of the file containing the data.
            member (str): The member to which the data from the file should be added.
                If member is "", than these data need to be add to all members.
        """
        with open(file_name, "rb") as fp:
            data = fp.read()
        if member in self.additional_data:
            self.additional_data[member] += data
        else:
            self.additional_data[member] = data
        os.remove(file_name)

    def mars_selection(self, selection=None):
        """Copy default settings if requested.

        Args:
             selection: default self.config["boundaries.ifs.selection"]

        Returns:
             mars (dict): updated mars config section

        """
        if selection is None:
            selection = self.platform.substitute(self.config["boundaries.ifs.selection"])
        mars = self.config[f"mars.{selection}"].dict()
        if "expver" not in mars:
            mars["expver"] = selection

        # Copy default settings if requested
        if "default" in mars:
            default = self.config[f"mars.{mars['default']}"]
            for k in default:
                if k not in mars:
                    mars[k] = default[k]

        logger.debug("MARS selection config:{}", mars)

        return mars

    def update_data_request(
        self,
        request: BaseRequest,
        prefetch,
        specify_domain,
        members,
        grid=None,
        source=None,
    ):
        """Create ECMWF MARS system request.

        Args:
            request:            BaseRequest object to update
            prefetch:           Retrieve or stage
            specify_domain:     Use lat/lon and rotation or use global (default)
            members:            Members to retrieve in case of eps.
            grid:               Specific grid for some request. Default None.
            source:             Sorce for retrieve data from disk. Defaults None.

        """
        if grid is not None:
            request.add_grid(grid)

        # Additional request parameters if EPS
        if members:
            request.add_eps_members(members, prefetch=prefetch)

        request.add_process()
        request.add_levelist(self.mars["levelist"])

        # Set stream
        stream = get_value_from_dict(self.mars["stream"], request.time)
        request.request["STREAM"] = [stream]

        # Retrieve from fetch data
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
            # Part1
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
                self.str_steps = [self.str_steps[self.bd_index]]

            logger.info("Need steps:{}", self.str_steps)

            # Get Grid point surface data
            tag = "ICMGG"
            steps, members, is_control_member = get_steps_and_members_to_retrieve(
                self.str_steps, self.prepdir, tag, self.members
            )
            if steps:
                self.get_gg_data(tag, steps, members, is_control_member)

                exist_soil = False
                with contextlib.suppress(KeyError):
                    param1 = get_value_from_dict(self.mars["GG_soil"], self.init_date_str)
                    param2 = get_value_from_dict(self.mars["GG1"], self.init_date_str)
                    exist_soil = True

                if exist_soil:
                    self.get_gg_soil_data(tag, param1, param2)

                else:
                    tco_high = self.mars["tco"] + "9_4"
                    tco_low = (
                        tco_high
                        if self.mars["class"] == "D1"
                        else self.mars["tco"] + "l_2"
                    )

                    for param in self.mars["sfc_high_res_params"]:
                        self.fmanager.input(f"{self.sfcdir}/{tco_high}/{param}", param)

                    for param in self.mars["sfc_low_res_params"]:
                        self.fmanager.input(f"{self.sfcdir}/{tco_low}/{param}", param)

                    for data_file in (
                        self.mars["sfc_high_res_params"] + self.mars["sfc_low_res_params"]
                    ):
                        self.add_additional_data(data_file, "")

                self.add_data_and_move_files(steps, tag, members, "")
            # Get spectral harmonic data
            tag = "ICMSH"

            steps, members, is_control_member = get_steps_and_members_to_retrieve(
                self.str_steps, self.prepdir, tag, self.members
            )

            if steps:
                self.get_sh_data(
                    tag,
                    steps,
                    members,
                    is_control_member,
                )

                self.get_shz_data(
                    tag,
                )

                self.add_data_and_move_files(steps, tag, members, "z")
            # Get gridpoint upper air data
            tag = "ICMUA"

            steps, members, is_control_member = get_steps_and_members_to_retrieve(
                self.str_steps, self.prepdir, tag, self.members
            )
            if steps:
                self.get_ua_data(tag, steps, members, is_control_member)
                logger.info(
                    "steps {}, members {}, is_control_member {}",
                    steps,
                    members,
                    is_control_member,
                )
                self.add_data_and_move_files(steps, tag, members)

        #
        # Split the lat/lon part and perform it here
        #

        mars_file_check = self.prepdir / self.prep_filename
        if os.path.exists(mars_file_check):
            logger.debug("Warning: Prep file allready exists")
        elif self.split_mars and not self.prep_step:
            logger.debug("No need Prep file")
        else:
            self.get_lat_lon_data(self.str_steps)
            # Get the file list to join
            if not self.members:
                prep_pattern = "mars_latlon*"
                filenames = list_files_join(self.wdir, prep_pattern)
                logger.info(filenames)
                with open(self.prep_filename, "ab") as output_file:
                    for filename in filenames:
                        with open(filename, "rb") as input_file:
                            output_file.write(input_file.read())
                        os.remove(filename)
                shutil.move(
                    self.prep_filename,
                    os.path.join(self.prepdir, self.prep_filename),
                )
            else:
                for member in self.members:
                    prep_pattern = f"mars_latlon*_{member}"
                    logger.info("prep_pattern: {}", prep_pattern)
                    filenames = list_files_join(self.wdir, prep_pattern)
                    logger.info(filenames)
                    with open(self.prep_filename, "ab") as output_file:
                        for filename in filenames:
                            with open(filename, "rb") as input_file:
                                output_file.write(input_file.read())
                    shutil.move(
                        self.prep_filename,
                        os.path.join(self.prepdir, self.prep_filename + f"_{member}"),
                    )

    def get_lat_lon_data(self, steps):
        """Get Lat/Lon data."""
        prefetch = False

        method = self.mars["latlon_method"]
        str_step = "{}".format(steps[0])
        param = (
            get_value_from_dict(self.mars["GG"], self.init_date_str)
            + "/"
            + get_value_from_dict(self.mars["GG_sea"], self.init_date_str)
        )
        if not self.members:
            request_latlon_gg = BaseRequest(
                class_=self.mars["class"],
                data_type=self.mars["type_FC"],
                expver=self.mars["expver"],
                date=self.init_date_str,
                time=self.init_hour_str,
                steps=str_step,
                levtype="SFC",
                param=param,
                target="mars_latlonGG",
            )
            self.update_data_request(
                request_latlon_gg,
                prefetch=prefetch,
                specify_domain=True,
                members=self.members,
                source=f'"{self.prepdir}/ICMGG+{str_step}"',
            )

            write_mars_req(request_latlon_gg, "latlonGG.req", method)
            self.batch.run(f"{self.mars_bin} latlonGG.req")
        else:
            for member in self.members:
                request_latlon_gg = BaseRequest(
                    class_=self.mars["class"],
                    data_type=self.mars["type_FC"],
                    expver=self.mars["expver"],
                    date=self.init_date_str,
                    time=self.init_hour_str,
                    steps=str_step,
                    levtype="SFC",
                    param=param,
                    target="mars_latlonGG",
                )
                self.update_data_request(
                    request_latlon_gg,
                    prefetch=prefetch,
                    specify_domain=True,
                    members=[member],
                    source=f'"{self.prepdir}/ICMGG_{member}+{str_step}"',
                )

                write_mars_req(request_latlon_gg, "latlonGG.req", method)
                self.batch.run(f"{self.mars_bin} latlonGG.req")

        # Retrieve for Surface Geopotential in lat/lon
        param = get_value_from_dict(self.mars["SHZ"], self.init_date_str)
        d_type = get_value_from_dict(self.mars["GGZ_type"], self.init_date_str)
        lev_type = get_value_from_dict(self.mars["Zlev_type"], self.init_date_str)

        request_latlon_z = BaseRequest(
            class_=self.mars["class"],
            data_type=d_type,
            expver=self.mars["expver"],
            date=self.init_date_str,
            time=self.init_hour_str,
            steps=str_step,
            levtype=lev_type,
            param=param,
            target="mars_latlonZ",
        )
        if not self.members:
            self.update_data_request(
                request_latlon_z,
                prefetch=prefetch,
                members=self.members,
                specify_domain=True,
                source=f'"{self.prepdir}/ICMSH+{str_step}"',
            )
            write_mars_req(request_latlon_z, "latlonz.req", method)
            self.batch.run(f"{self.mars_bin} latlonz.req")
        else:
            first_member = self.members[0]
            self.update_data_request(
                request_latlon_z,
                prefetch=prefetch,
                members=[first_member],
                specify_domain=True,
                source=f'"{self.prepdir}/ICMSH_{first_member}+{str_step}"',
            )
            write_mars_req(request_latlon_z, "latlonz.req", method)
            self.batch.run(f"{self.mars_bin} latlonz.req")
            for member in self.members[1:]:
                self.fmanager.input(
                    f"mars_latlonZ_{first_member}", f"mars_latlonZ_{member}"
                )

    def get_sh_data(self, tag, steps, members, is_control_member):
        """Get SH data."""
        logger.info("Retrieving {} data for steps: {}", tag, steps)
        param = get_value_from_dict(self.mars["SH"], self.init_date_str)

        request_sh = BaseRequest(
            class_=self.mars["class"],
            data_type=self.mars["type_FC"],
            expver=self.mars["expver"],
            levtype="ML",
            date=self.init_date_str,
            time=self.init_hour_str,
            steps="/".join(steps),
            param=param,
            target=f'"{tag}+[STEP]"',
        )
        self.update_data_request(
            request_sh,
            prefetch=True,
            grid=self.mars["grid_ML"],
            specify_domain=False,
            members=members,
        )
        write_mars_req(request_sh, f"{tag}.req", "retrieve")
        self.batch.run(f"{self.mars_bin} {tag}.req")

        if is_control_member:
            request_control_sh = request_sh.replace(
                data_type=self.mars["type_AN"], target=f'"{tag}_0+[STEP]"'
            )

            self.update_data_request(
                request_control_sh,
                prefetch=True,
                grid=self.mars["grid_ML"],
                specify_domain=False,
                members=[],
            )

            write_mars_req(request_control_sh, f"{tag}_0.req", "retrieve")
            self.batch.run(f"{self.mars_bin} {tag}_0.req")

    def get_shz_data(self, tag):
        """Get geopotential in spectral harmonic."""
        # there is no sh geopotential in LUMI fdb
        if self.mars["class"] == "D1":
            tco_high = self.mars["tco"] + "9_4"
            self.fmanager.input(f"{self.sfcdir}/{tco_high}/sporog", f"{tag}.Z")

        else:
            param = get_value_from_dict(self.mars["SHZ"], self.init_date_str)
            request_shz = BaseRequest(
                class_=self.mars["class"],
                data_type=self.mars["SHZ_type"],
                expver=self.mars["expver"],
                levtype="ML",
                date=self.init_date_str,
                time=self.init_hour_str,
                steps="00",
                param=param,
                target=f'"{tag}.Z"',
            )
            self.update_data_request(
                request_shz,
                prefetch=True,
                grid=self.mars["grid_ML"],
                specify_domain=False,
                members=[],
            )
            write_mars_req(request_shz, f"{tag}Z.req", "retrieve")
            self.batch.run(f"{self.mars_bin} {tag}Z.req")

        self.add_additional_data(f"{tag}.Z", "z")

    def get_ua_data(self, tag, steps, members, is_control_member):
        """Get upper air data."""
        logger.info("Retrieving {} data for steps: {}", tag, steps)
        param = get_value_from_dict(self.mars["UA"], self.init_date_str)
        request_ua = BaseRequest(
            class_=self.mars["class"],
            data_type=self.mars["type_FC"],
            expver=self.mars["expver"],
            levtype="ML",
            date=self.init_date_str,
            time=self.init_hour_str,
            steps="/".join(steps),
            param=param,
            target=f'"{tag}+[STEP]"',
        )
        self.update_data_request(
            request_ua,
            prefetch=True,
            grid=self.mars["grid_ML"],
            specify_domain=False,
            members=members,
        )
        write_mars_req(request_ua, f"{tag}.req", "retrieve")
        self.batch.run(f"{self.mars_bin} {tag}.req")

        if is_control_member:
            request_control_ua = BaseRequest(
                class_=self.mars["class"],
                data_type=self.mars["type_AN"],
                expver=self.mars["expver"],
                levtype="ML",
                date=self.init_date_str,
                time=self.init_hour_str,
                steps="/".join(steps),
                param=param,
                target=f'"{tag}_0+[STEP]"',
            )
            self.update_data_request(
                request_control_ua,
                prefetch=True,
                grid=self.mars["grid_ML"],
                specify_domain=False,
                members=[],
            )
            write_mars_req(request_control_ua, f"{tag}_0.req", "retrieve")
            self.batch.run(f"{self.mars_bin} {tag}_0.req")

    def get_gg_data(self, tag, steps, members, is_control_member):
        """Get gridpoint surface data."""
        logger.info("Retrieving {} data for steps: {}", tag, steps)
        param = (
            get_value_from_dict(self.mars["GG"], self.init_date_str)
            + "/"
            + get_value_from_dict(self.mars["GG_sea"], self.init_date_str)
        )
        # Create a new steps request
        request_gg = BaseRequest(
            class_=self.mars["class"],
            data_type=self.mars["type_FC"],
            expver=self.mars["expver"],
            levtype="SFC",
            date=self.init_date_str,
            time=self.init_hour_str,
            steps="/".join(steps),
            param=param,
            target=f'"{tag}+[STEP]"',
        )
        self.update_data_request(
            request_gg,
            prefetch=True,
            specify_domain=False,
            members=members,
        )

        write_mars_req(request_gg, f"{tag}.req", "retrieve")
        self.batch.run(f"{self.mars_bin} {tag}.req")

        if is_control_member:
            request_control_gg = BaseRequest(
                class_=self.mars["class"],
                data_type=self.mars["type_AN"],
                expver=self.mars["expver"],
                levtype="SFC",
                date=self.init_date_str,
                time=self.init_hour_str,
                steps="/".join(steps),
                param=param,
                target=f'"{tag}_0+[STEP]"',
            )
            self.update_data_request(
                request_control_gg,
                prefetch=True,
                specify_domain=False,
                members=[],
            )

            write_mars_req(request_control_gg, f"control_{tag}.req", "retrieve")
            self.batch.run(f"{self.mars_bin} control_{tag}.req")

    def get_gg_soil_data(self, tag, param1, param2):
        """Get soil gridpoint data."""
        request_gg_soil = BaseRequest(
            class_=self.mars["class"],
            data_type=self.mars["type_AN"],
            expver=self.mars["expver"],
            levtype="SFC",
            date=self.init_date_str,
            time=self.init_hour_str,
            steps="00",
            param=param1,
            target=f"{tag}.soil",
        )
        self.update_data_request(
            request_gg_soil,
            prefetch=True,
            members=[],
            specify_domain=False,
        )
        write_mars_req(request_gg_soil, f"{tag}.soil.req", "retrieve")
        self.batch.run(f"{self.mars_bin} {tag}.soil.req")

        request_gg1 = BaseRequest(
            class_=self.mars["class"],
            data_type=self.mars["type_AN"],
            expver=self.mars["expver"],
            levtype="SFC",
            date=self.init_date_str,
            time=self.init_hour_str,
            steps="00",
            param=param2,
            target=tag,
        )

        self.update_data_request(
            request_gg1,
            prefetch=True,
            grid=get_value_from_dict(self.mars["grid_GG1"], self.init_date_str),
            members=[],
            specify_domain=False,
        )

        write_mars_req(request_gg1, f"{tag}1.req", "retrieve")
        self.batch.run(f"{self.mars_bin} {tag}1.req")

        # Read the single-step MARS files first
        self.add_additional_data(f"{tag}", "")
        self.add_additional_data(f"{tag}.soil", "")

    def add_data_and_move_files(self, steps, tag, members, additonal_data_key=None):
        """Add aditional data (same for all steps). Move the files to the final location.

        Args:
            steps     (list): steps to process the files
            tag     (string): Name of tag
            members   (list): members to procces the files
            additonal_data_key (string): key of data to add
        """
        for j in steps:
            i = int(j)
            i_fstring = f"{i:02d}"
            if not members:
                if os.path.exists(f"{tag}+{i}"):
                    if additonal_data_key is not None:
                        with open(f"{tag}+{i}", "ab") as fp:
                            fp.write(self.additional_data[additonal_data_key])
                    shutil.move(
                        f"{tag}+{i}",
                        self.prepdir / f"{tag}+{i_fstring}",
                    )
            else:
                for member in [0, *members]:
                    if os.path.exists(f"{tag}_{member}+{i}"):
                        if additonal_data_key is not None:
                            with open(f"{tag}_{member}+{i}", "ab") as fp:
                                fp.write(self.additional_data[additonal_data_key])
                        shutil.move(
                            f"{tag}_{member}+{i}",
                            self.prepdir / f"{tag}_{member}+{i_fstring}",
                        )
