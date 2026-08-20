"""Pertana."""

import os

from tactus.initial_conditions import InitialConditions
from tactus.logs import logger
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob


class Pertana(Task):
    """Pertana, add perturbations from the nesting file to the initial file."""

    """pert(mbr) = inifile(mbr) + Z_mult(bd1(mbr) - bd1(ensctl)),
    where bd1 denotes the first boundary file."""

    def __init__(self, config):
        """Construct Pertana object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.binary = self.get_binary("ADDPERT")
        self.z_mult = self.config["perturbations.pertana.z_mult"]
        self.intp_bddir = self.config["system.intp_bddir"]
        self.mbr_str = self.config["general.member_str"]
        self.intp_start_mbr = self.config[
            "file_templates.interpolated_boundaries.archive"
        ]
        self.archive = self.platform.get_system_value("archive")
        logger.info("archive:", self.archive)

    def execute(self):
        """Run task."""
        # Create namelist file (static)
        with open("fort.4", "w") as nl:
            nl.write("&NAMPERT\n")
            for i in range(1, 5):
                nl.write(f"  CLFNAME({i})='FILE{i}',\n")
            nl.write(f"  Z_MULT={self.z_mult}\n")
            nl.write("/\n")
            nl.close()

        """Find initial file"""
        initfile, initfile_sfx, status = InitialConditions(
            self.config
        ).find_initial_files("Pertana", False)
        self.fmanager.input(initfile, "FILE1")

        """Find bd1(mbr)"""
        input_mbr = self.platform.substitute(
            f"{self.intp_bddir}/{self.intp_start_mbr}", bd_index=0
        )

        self.fmanager.input(input_mbr, "FILE2")

        """Find bd1(ensctl)"""
        intp_bddir_cntrl = self.platform.substitute(self.intp_bddir)
        intp_bddir_cntrl = intp_bddir_cntrl.replace(self.mbr_str, "mbr000")
        input_cntl = self.platform.substitute(
            f"{intp_bddir_cntrl}/{self.intp_start_mbr}", bd_index=0
        )

        self.fmanager.input(input_cntl, "FILE3")

        """Prepare the perturbed file"""
        self.fmanager.input("FILE1", "FILE4", provider_id="copy")

        # Run binary
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.binary)

        perturbed_filewpath = f"@ARCHIVE@/{self.config['file_templates.pertana.archive']}"
        self.fmanager.output(
            "FILE4",
            perturbed_filewpath,
        )
