"""C903."""

import os

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..logs import logger
from ..namelist import NamelistGenerator
from .base import Task
from .batch import BatchJob


class C903(Task):
    """C903task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.climdir = self.platform.get_system_value("climdir")

        self.expdir = self.config["system.marsdir"]
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        self.bdcycle = as_timedelta(self.config["boundaries.bdcycle"])
        self.bdnr = self.config["task.args.bd_nr"]
        self.bd_time = self.config["task.args.bd_time"]
        self.forecast_range = self.config["general.forecast_range"]

        self.dom = self.config["domain.name"]

        self.expver = self.config["general.mars_expver"]
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
            except FileNotFoundError:
                logger.warning("Could not remove file '{}'.", x, exc_info=True)

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Climate files
        mm = self.basetime.strftime("%m")
        self.fmanager.input(
            f"{self.climdir}/Const.Clim.{mm}", "{}_{}".format(self.dom, mm)
        )

        # Namelist
        self.nlgen.generate_namelist("c903_main", "fort.4")
        self.nlgen.generate_namelist("c903_domain", "namelist_c903_domain")

        # Forecast range
        intbdint = int(self.bdint.total_seconds() // 3600)
        i = int(self.bdnr) * intbdint

        # Fix basetime for PT00H,PT12H only
        basetime = self.basetime
        bd_basetime = self.basetime - cycle_offset(self.basetime, self.bdcycle)
        basetime_hour = int(basetime.strftime("%H"))
        int_bdcycle = int(self.bdcycle.total_seconds()) // 3600

        intlbc = i + basetime_hour % int_bdcycle
        # Input files
        self.fmanager.input(
            f"{self.expdir}/ICMSH+{intlbc:02d}",
            "ICMSHMARSINIT",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMGG+{intlbc:02d}",
            "ICMGGMARSINIT",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMUA+{intlbc:02d}",
            "ICMUAMARSINIT",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )

        self.fmanager.input(
            f"{self.expdir}/ICMSH+{intlbc:02d}",
            "ICMSHMARS",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMGG+{intlbc:02d}",
            "ICMGGMARS",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMUA+{intlbc:02d}",
            "ICMUAMARS",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )

        self.fmanager.input(
            f"{self.expdir}/ICMSH+{intlbc:02d}",
            "ICMSHMARS+000000",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMGG+{intlbc:02d}",
            "ICMGGMARS+000000",
            basetime=bd_basetime,
            validtime=as_datetime(self.bd_time),
        )
        self.fmanager.input(
            f"{self.expdir}/ICMUA+{intlbc:02d}",
            "ICMUAMARS+000000",
            basetime=bd_basetime,
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
