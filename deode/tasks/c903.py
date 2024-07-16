"""C903."""

import os

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..logs import logger
from ..namelist import NamelistGenerator
from .base import Task
from .batch import BatchJob


class C903(Task):
    """C903."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.climdir = self.platform.get_system_value("climdir")

        self.expdir = self.config["system.marsdir"]
        self.basetime = as_datetime(self.config["general.times.basetime"])
        bdcycle = as_timedelta(self.config["boundaries.bdcycle"])
        bdcycle_start = as_timedelta(self.config["boundaries.bdcycle_start"])
        bdshift = as_timedelta(self.config["boundaries.bdshift"])
        # Boundary basetime
        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime, bdcycle, bdcycle_start=bdcycle_start, bdshift=-bdshift
        )

        self.bdnr = self.config["task.args.bd_nr"]
        self.bd_time = self.config["task.args.bd_time"]
        self.forecast_range = self.config["general.times.forecast_range"]

        self.dom = self.config["domain.name"]

        self.bdfile_template = self.config["system.bdfile_template"]
        self.intp_bddir = self.platform.get_system_value("intp_bddir")
        logger.info("Domain: {}", self.dom)

        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = self.get_binary("MASTERODB")

        self.name = f"{self.name}_{self.bdnr}"

    def remove_links(self, link):
        """Remove link.

        Args:
            link (list) : List of links to remove
        """
        logger.info("clean {}", link)
        for x in link:
            try:
                os.unlink(x)
            except FileNotFoundError:  # noqa: PERF203
                logger.warning("Could not remove file '{}'.", x, exc_info=True)

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Climate files
        mm = self.bd_basetime.strftime("%m")
        self.fmanager.input(f"{self.climdir}/Const.Clim.{mm}", f"{self.dom}_{mm}")

        # Namelist
        self.nlgen.generate_namelist("c903_main", "fort.4")
        self.nlgen.generate_namelist("c903_domain", "namelist_c903_domain")

        # Input files
        self.fmanager.input(
            f"{self.expdir}/ICMSH+@LL@",
            "ICMSHMARSINIT",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMGG+@LL@",
            "ICMGGMARSINIT",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMUA+@LL@",
            "ICMUAMARSINIT",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )

        self.fmanager.input(
            f"{self.expdir}/ICMSH+@LL@",
            "ICMSHMARS",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMGG+@LL@",
            "ICMGGMARS",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMUA+@LL@",
            "ICMUAMARS",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )

        self.fmanager.input(
            f"{self.expdir}/ICMSH+@LL@",
            "ICMSHMARS+000000",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMGG+@LL@",
            "ICMGGMARS+000000",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMUA+@LL@",
            "ICMUAMARS+000000",
            basetime=self.bd_basetime,
            validtime=as_datetime(self.bd_time),
        )

        # Run masterodb

        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        target = f"{self.intp_bddir}/{self.bdfile_template}"

        logger.debug("WRKDIR: {}", self.wrk)
        logger.debug("OUTPUT {}", f"ELSCFMARS{self.dom}+0000")
        self.fmanager.output(f"ELSCFMARS{self.dom}+0000", target)
        self.archive_logs("NODE.001_01")
