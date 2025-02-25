"""C903."""

import json
import os

from ..config_parser import ConfigPaths
from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..logs import logger
from ..namelist import NamelistGenerator
from .base import Task
from .batch import BatchJob
from .marsprep import Marsprep


class C903(Task):
    """C903, preform interpolation from IFS to IAL LAM."""

    def __init__(self, config):
        """Construct C903 object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.climdir = self.platform.get_system_value("climdir")
        self.basetime = as_datetime(self.config["general.times.basetime"])

        mars = Marsprep.mars_selection(self)
        bdcycle = as_timedelta(mars["ifs_cycle_length"])
        bdcycle_start = as_timedelta(mars["ifs_cycle_start"])
        bdshift = as_timedelta(self.config["boundaries.bdshift"])
        # Boundary basetime
        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime, bdcycle, bdcycle_start=bdcycle_start, bdshift=-bdshift
        )

        self.bd_index = self.config["task.args.bd_index"]
        self.bd_time = self.config["task.args.bd_time"]
        self.forecast_range = self.config["general.times.forecast_range"]

        self.bdfile_template = self.config["system.bdfile_template"]
        self.intp_bddir = self.platform.get_system_value("intp_bddir")

        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = self.get_binary("MASTERODB")

        self.name = f"{self.name}_{self.bd_index}"

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Fetch input data
        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value("c903_input_definition")
        )

        logger.info("Read input data spec from: {}", input_definition)
        with open(input_definition, "r", encoding="utf-8") as f:
            input_data = json.load(f)

        ifs_files = input_data.pop("IFS_files")

        # Link the static data
        self.fmanager.input_data_iterator(input_data)

        # IFS input files
        path = ifs_files["path"]
        for dst, src in ifs_files["files"].items():
            self.fmanager.input(
                f"{path}/{src}",
                dst,
                basetime=self.bd_basetime,
                validtime=as_datetime(self.bd_time),
            )

        # Namelist
        self.nlgen.generate_namelist("c903_main", "fort.4")
        self.nlgen.generate_namelist("c903_domain", "namelist_c903_domain")

        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store result
        target = f"{self.intp_bddir}/{self.bdfile_template}"
        self.fmanager.output("ELSCFMARS@FPDOMAIN@+0000", target)
        self.archive_logs(["fort.4", "namelist_c903_domain", "NODE.001_01"])
