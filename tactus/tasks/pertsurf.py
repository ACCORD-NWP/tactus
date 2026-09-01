"""PertSurf."""

import os

from tactus.datetime_utils import as_datetime
from tactus.initial_conditions import InitialConditions
from tactus.logs import logger
from tactus.namelist import NamelistGenerator
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob


class PertSurf(Task):
    """Perturb surface parameters in SURFEX initial file."""

    def __init__(self, config):
        """Construct PertSurf object.

        Args:
            config (tactus.ParseConfig): Configuration object.
        """
        Task.__init__(self, config, __class__.__name__)

        self.archive = self.platform.get_system_value("archive")
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.nlgen = NamelistGenerator(self.config, "pertsurf")
        self.file_templates = self.config.get_as_dict("file_templates")

        self.binary = self.get_binary("PERTSURF")

    def execute(self):
        """Execute the PertSurf task."""
        ensmbr = self.config["general.member"]

        # Surface initial file
        _, initfile_sfx, _ = InitialConditions(self.config).find_initial_files(
            "Pertsurf", types=["surfex"]
        )

        output = self.platform.substitute(
            f"{self.config['file_templates.pertsurf.archive']}"
        )

        self.fmanager.input(
            initfile_sfx,
            output,
            provider_id="copy",
        )

        # Seed
        m = 1000000
        dtg = int(self.basetime.strftime("%Y%m%d%H"))
        iseed = m * ensmbr + (dtg % m)
        logger.debug(f"Generated seed (ISEED): {iseed}")

        # Build namelist
        self.nlgen.load("pertsurf")
        self.nlgen.update(
            {
                "NAMSFC": {
                    "ISEED": iseed,
                    "CFSFC": output,
                }
            },
            "pertsurf_dynamic_update",
        )
        nml = self.nlgen.assemble_namelist("pertsurf")
        self.nlgen.write_namelist(nml, "nampert")

        # Run PERTSURF
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.binary)

        # Move PERTSURF output to archive
        self.fmanager.output(
            output,
            f"{self.archive}/{output}",
            provider_id="move",
        )
