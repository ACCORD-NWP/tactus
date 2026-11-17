"""Retrieve CAMS data from MARS for the aerosols in the lateral boundaries."""

import os
from functools import cached_property
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from tactus.boundary_utils import Boundary
from tactus.datetime_utils import as_datetime, as_timedelta
from tactus.logs import logger
from tactus.mars_utils import (
    add_additional_file_specific_data,
    check_data_available,
    get_steplist,
    get_steps_and_members_to_retrieve,
    get_value_from_dict,
    mars_selection,
    move_files,
    previous_archived_step,
    relabel_step,
)
from tactus.os_utils import tactusmakedirs
from tactus.tasks.base import Task
from tactus.tasks.marsprep import Marsprep


class MarsprepCams(Marsprep):
    """Retrieve CAMS (class=MC) forecasts for the aerosol boundaries.

    Retrieves a complete c903 input data set on the native CAMS grid (spectral
    fields, surface fields, humidity, aerosols and ozone), so that C903Cams can
    interpolate the aerosols with a single input geometry. Per step the files
    ICMSH_0+STEP, ICMGG_0+STEP and ICMUA_0+STEP are written to nrt_aerosols.bddir.

    Reuses the retrieval methods of Marsprep, but not its setup: CAMS has no
    ensemble members, and SST/SIC, SURFEX and static orography are not needed.
    In addition:
    - Fields archived at a coarser interval (spectral winds, multi-layer snow)
      are taken from the previous archived step with the step relabelled. They
      are not written by the aerosol-only c903, so this does not affect the
      aerosol boundaries.
    """

    def __init__(self, config):
        """Construct object.

        Marsprep.__init__ is not called, as it sets up the IFS boundaries.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.basetime = as_datetime(self.config["general.times.basetime"])
        forecast_range = as_timedelta(self.config["general.times.forecast_range"])

        self.boundary = Boundary(config, cams=True)
        check_data_available(self.boundary.bd_basetime, self.mars)

        self.steps = get_steplist(
            self.boundary.bd_offset, forecast_range, self.boundary.bdint
        )
        self.init_date_str = self.boundary.bd_basetime.strftime("%Y%m%d")
        self.init_hour_str = self.boundary.bd_basetime.strftime("%H")

        self.prepdir = Path(
            self.platform.substitute(
                self.config["nrt_aerosols.bddir"],
                basetime=self.boundary.bd_basetime,
                validtime=self.basetime,
            )
        )
        logger.info("CAMS data expected in: {}", self.prepdir)

        # Settings used by the Marsprep retrieval methods
        self.mars_version = int(
            self.config.get("submission.task_exceptions.MarsprepCams.mars_version", "6")
        )
        self.use_static_sh_oro = False
        self.use_static_gg_oro = False
        # CAMS has no ensemble members
        self.bdmember = [None]

    @cached_property
    def mars(self) -> dict:
        """CAMS mars selection."""
        selection = self.platform.substitute(self.config["nrt_aerosols.selection"])
        return mars_selection(selection, self.config)

    def param(self, key: str) -> str:
        """Get a (possibly date dependent) value from the mars selection.

        Args:
            key (str): Key in the mars selection

        Returns:
            str: Value
        """
        return get_value_from_dict(self.mars[key], self.init_date_str)

    def missing_steps(self, tag: str) -> Tuple[List[int], Dict[str, List]]:
        """Return the steps without a file in prepdir.

        Args:
            tag (str): ICMSH, ICMGG or ICMUA

        Returns:
            Tuple[List[int], Dict[str, List]]: Steps and members_dict to retrieve
        """
        steps, _, members_dict, _, _ = get_steps_and_members_to_retrieve(
            self.steps, self.prepdir, tag, [None]
        )
        return steps, members_dict

    def get_archived_data(
        self,
        tag: str,
        requests: List[Tuple[str, str, Optional[str]]],
        steps: List[int],
        interval_key: str,
    ) -> Dict[str, bytes]:
        """Get fields archived at a coarser interval, relabelled to steps.

        Args:
            tag (str): ICMSH or ICMGG
            requests (List[Tuple[str, str, Optional[str]]]): (levtype, param,
                levelist) of each request
            steps (List[int]): Steps to fill
            interval_key (str): Key of the archiving interval in the mars selection

        Returns:
            Dict[str, bytes]: Data to add, per file
        """
        interval = int(as_timedelta(self.param(interval_key)).total_seconds() // 3600)
        archived = {step: previous_archived_step(step, interval) for step in steps}
        for i, (levtype, param, levelist) in enumerate(requests):
            self._build_and_run_retrieve_request(
                req_file_name=f"{tag}.archived{i}.req",
                data_type=self.mars["type_FC"],
                levtype=levtype,
                param=param,
                steps=sorted(set(archived.values())),
                target=f'"{tag}.archived{i}+[STEP]"',
                levelist=levelist,
            )

        additional_data = {
            f"{tag}_0+{step}": b"".join(
                relabel_step(f"{tag}.archived{i}+{archived[step]}", step)
                for i in range(len(requests))
            )
            for step in steps
        }
        for i in range(len(requests)):
            for step in set(archived.values()):
                os.remove(f"{tag}.archived{i}+{step}")
        return additional_data

    def execute(self):
        """Run task."""
        tactusmakedirs(
            self.prepdir, unixgroup=self.platform.get_platform_value("unix_group")
        )
        levelist = self.param("levelist")

        # Spectral: t/lnsp at every step, orography, winds at their interval
        tag = "ICMSH"
        steps, members_dict = self.missing_steps(tag)
        if steps:
            self.get_sh_data(tag, steps, members_dict)
            additional_data = {"common_data": self.get_shz_data(tag)}
            additional_data |= self.get_archived_data(
                tag, [("ML", self.param("SH_wind"), levelist)], steps, "SH_wind_interval"
            )
            add_additional_file_specific_data(additional_data)
            move_files(tag, steps, members_dict, self.prepdir)

        # Surface: every step, constant fields, snow at its interval
        tag = "ICMGG"
        steps, members_dict = self.missing_steps(tag)
        if steps:
            self.get_gg_data(tag, steps, members_dict)
            additional_data = {
                "common_data": self.get_gg_soil_data(tag, {"GG1": self.param("GG1")})
            }
            additional_data |= self.get_archived_data(
                tag,
                [
                    ("SOL", self.param("GG_snow"), self.param("GG_snow_levelist")),
                    ("SFC", self.param("GG_snow_extra"), None),
                ],
                steps,
                "GG_snow_interval",
            )
            add_additional_file_specific_data(additional_data)
            move_files(tag, steps, members_dict, self.prepdir)

        # Upper air (humidity, aerosols, ozone) at every step
        self.get_grid_point_upper_air_data()
