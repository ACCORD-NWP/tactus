"""PertSurf."""

import os

from tactus.datetime_utils import as_datetime
from tactus.initial_conditions import InitialConditions
from tactus.logs import logger
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

        self.npatch = self.config["pgd.npatch"]
        self.archive = self.platform.get_system_value("archive")
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.pertsurf_param = self.config.get_as_dict("perturbations.pertsurf")
        self.file_templates = self.config.get_as_dict("file_templates")

        self.binary = self.get_binary("PERTSURF")

    @staticmethod
    def fmt_fortran_val(value):
        """Format a Python value for Fortran namelist."""
        if isinstance(value, bool):
            return ".true." if value else ".false."
        if isinstance(value, str):
            return f"'{value}'"
        if isinstance(value, (list, tuple)):
            return ", ".join(f"'{v}'" if isinstance(v, str) else str(v) for v in value)
        return str(value)

    def build_namelist(self, param_dict, iseed, output):
        """Build namelist string for PERTSURF."""
        param_dict.pop("active", None)
        param_dict["ISEED"] = iseed
        param_dict["CFSFC"] = output
        param_dict["IPATCH"] = self.npatch

        namelist_str = "&NAMSFC\n"
        for key, value in param_dict.items():
            formatted_value = self.fmt_fortran_val(value)
            namelist_str += f"{key.upper()} = {formatted_value},\n"
        namelist_str += "/\n"
        return namelist_str

    def execute(self):
        """Execute the PertSurf task."""
        ensmbr = self.config["general.member"]

        # Surface initial file
        _, initfile_sfx, _ = InitialConditions(self.config).find_initial_files(
            "Pertsurf", "surfex"
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
        logger.info(f"Generated seed (ISEED): {iseed}")

        # Build namelist
        namelist_str = self.build_namelist(self.pertsurf_param, iseed, output)

        with open("nampert", "w") as nml:
            nml.write(namelist_str)

        logger.info("Created namelist: {}", "nampert")

        # Run PERTSURF
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.binary)

        # Move PERTSURF output to archive
        self.fmanager.output(
            output,
            f"{self.archive}/{output}",
            provider_id="move",
        )
