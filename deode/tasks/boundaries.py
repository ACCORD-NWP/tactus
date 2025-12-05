"""Boundary interpolation."""

import json
import os
from typing import Dict

from deode.boundary_utils import Boundary
from deode.config_parser import ConfigPaths
from deode.datetime_utils import as_datetime
from deode.logs import logger
from deode.namelist import NamelistGenerator
from deode.tasks.base import Task
from deode.tasks.batch import BatchJob


class InterpolateBoundaries(Task):
    """Boundary interpolation task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.boundary = Boundary(config)

        if self.boundary.method == "e927":
            self.namelists = {self.boundary.method: "fort.4"}
            self.logs = ["fort.4", "NODE.001_01"]
            self.outfile = "PF@CNMEXP@000+0000"

        elif self.boundary.method == "c903":
            self.namelists = {
                "c903_main": "fort.4",
                "c903_domain": "namelist_c903_domain",
            }
            self.logs = ["fort.4", "namelist_c903_domain", "NODE.001_01"]
            self.outfile = "ELSCFMARS@FPDOMAIN@+@NNN@"

        self.target = (
            f"{self.platform.get_system_value('intp_bddir')}"
            + "/"
            + f"{self.config['file_templates.interpolated_boundaries.archive']}"
        )

        self.input_definition = f"{self.boundary.method}_input_definition"
        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = self.get_binary("MASTERODB", task_name=self.boundary.method.upper())
        self.name = (
            f"{self.boundary.method}_"
            f"{self.boundary.min_index}-"
            f"{self.boundary.max_index}"
        ).upper()

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Fetch input data
        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value(self.input_definition)
        )

        logger.info("Read input data spec from: {}", input_definition)
        with open(input_definition, "r", encoding="utf-8") as f:
            input_data = json.load(f)

        host_model_files: Dict[str, str | Dict[str, str]] = input_data.pop(
            "host_model_files"
        )

        if self.boundary.method == "c903":
            host_model_static_files: Dict[str, str | Dict[str, str]] = input_data.pop(
                "host_model_static_files"
            )
            path_static = host_model_static_files["path"]
            logger.info("*** host_model_static_files: {}", host_model_static_files)

            for dst, src in host_model_static_files["files"].items():
                # Default to 0 for bdmember if no bdmember specified. This is to
                # be able to reference files created by marsprep, which contains
                # bdmember = 0 even for "deterministic" runs.
                src_local = src
                if not self.config.get(f"boundaries.{self.boundary.bdmodel}.bdmember"):
                    src_local = src_local.replace("@BDMEMBER@", "0")
                self.fmanager.input(
                    f"{path_static}/{src_local}",
                    dst,
                    basetime=self.boundary.bd_basetime,
                    validtime=as_datetime(
                        self.boundary.bd_index_time_dict[self.boundary.min_index]
                    ),
                )
        # Link the static data
        self.fmanager.input_data_iterator(input_data, basetime=self.boundary.bd_basetime)

        # Host model input files
        path = host_model_files["path"]
        logger.info("*** host_model_files: {}", host_model_files)
        for dst, src in host_model_files["files"].items():
            # Default to 0 for bdmember if no bdmember specified. This is to
            # be able to reference files created by marsprep, which contains
            # bdmember = 0 even for "deterministic" runs.
            src_local = src
            if not self.config.get(f"boundaries.{self.boundary.bdmodel}.bdmember"):
                src_local = src_local.replace("@BDMEMBER@", "0")
            for bd_index, bd_time in self.boundary.bd_index_time_dict.items():
                self.fmanager.input(
                    f"{path}/{src_local}",
                    dst.replace("@NNN@", f"{bd_index - self.boundary.min_index}"),
                    basetime=self.boundary.bd_basetime,
                    validtime=as_datetime(bd_time),
                )

        # Generate namelists
        for key, value in self.namelists.items():
            if key == "c903_main":
                self.nlgen.load(key)
                self.nlgen.update(
                    {"NAMCT0": {"NFRPOS": len(self.boundary.bd_index_time_dict)}},
                    "bd_dynamic_update",
                )
                nml = self.nlgen.assemble_namelist(key)
                self.nlgen.write_namelist(nml, value)
            else:
                self.nlgen.generate_namelist(key, value)

        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store result
        for bd_index in self.boundary.bd_index_time_dict:
            self.target = (
                f"{self.platform.get_system_value('intp_bddir')}"
                + "/"
                + f"{self.config['file_templates.interpolated_boundaries.archive']}"
            ).replace("@NNN@", f"{bd_index:03}")

            self.fmanager.output(
                self.outfile.replace("@NNN@", f"{bd_index - self.boundary.min_index:04}"),
                self.target,
            )
        self.archive_logs(self.logs)


class E927(InterpolateBoundaries):
    """E927."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config)


class C903(InterpolateBoundaries):
    """E927."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config)
