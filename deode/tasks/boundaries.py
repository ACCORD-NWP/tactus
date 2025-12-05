"""Boundary interpolation."""

import json
import os
from typing import Dict

from tactus.config_parser import ConfigPaths
from tactus.datetime_utils import as_datetime, as_timedelta, cycle_offset
from tactus.logs import logger
from tactus.namelist import NamelistGenerator
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob
from tactus.tasks.marsprep import Marsprep


class InterpolateBoundaries(Task):
    """Boundary interpolation task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        basetime = self.config["general.times.basetime"]
        self.bd_index = self.config.get("task.args.bd_index", 0)
        self.bd_time = self.config.get("task.args.bd_time", basetime)
        self.forecast_range = self.config["general.times.forecast_range"]

        bdmodel_map = {"lam": "e927", "ifs": "c903"}
        self.bdmodel = self.config["boundaries.bdmodel"]
        method = bdmodel_map[self.bdmodel]

        if method == "e927":
            bdcycle = as_timedelta(self.config["boundaries.lam.bdcycle"])
            bdcycle_start = as_timedelta(self.config["boundaries.lam.bdcycle_start"])
            self.namelists = {method: "fort.4"}
            self.logs = ["fort.4", "NODE.001_01"]
            self.outfile = "PF@CNMEXP@000+0000"

        elif method == "c903":
            mars = Marsprep.mars_selection(
                selection=self.platform.substitute(
                    self.config[f"boundaries.{self.bdmodel}.selection"]
                ),
                config=self.config,
            )
            bdcycle = as_timedelta(mars["ifs_cycle_length"])
            bdcycle_start = as_timedelta(mars["ifs_cycle_start"])
            self.namelists = {
                "c903_main": "fort.4",
                "c903_domain": "namelist_c903_domain",
            }
            self.logs = ["fort.4", "namelist_c903_domain", "NODE.001_01"]
            self.outfile = "ELSCFMARS@FPDOMAIN@+0000"

        self.target = (
            f"{self.platform.get_system_value('intp_bddir')}"
            + "/"
            + f"{self.config['file_templates.interpolated_boundaries.archive']}"
        )

        # Boundary basetime
        self.basetime = as_datetime(basetime)
        bdshift = as_timedelta(config["boundaries.bdshift"])
        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime, bdcycle, bdcycle_start=bdcycle_start, bdshift=bdshift
        )

        self.input_definition = f"{method}_input_definition"
        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = self.get_binary("MASTERODB", task_name=method.upper())
        self.name = f"{method}_{self.bd_index}".upper()

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

        # Link the static data
        self.fmanager.input_data_iterator(
            input_data, basetime=self.bd_basetime, validtime=as_datetime(self.bd_time)
        )

        # Host model input files
        path = host_model_files["path"]
        for dst, src in host_model_files["files"].items():
            # Default to 0 for bdmember if no bdmember specified. This is to
            # be able to reference files created by marsprep, which contains
            # bdmember = 0 even for "deterministic" runs.
            src_local = src
            if not self.config.get(f"boundaries.{self.bdmodel}.bdmember"):
                src_local = src_local.replace("@BDMEMBER@", "0")

            self.fmanager.input(
                f"{path}/{src_local}",
                dst,
                basetime=self.bd_basetime,
                validtime=as_datetime(self.bd_time),
            )

        # Generate namelists
        for key, value in self.namelists.items():
            self.nlgen.generate_namelist(key, value)

        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store result
        self.fmanager.output(self.outfile, self.target)
        self.archive_logs(self.logs)


class E927(InterpolateBoundaries):
    """E927."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config)


class C903(InterpolateBoundaries):
    """E927."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config)
