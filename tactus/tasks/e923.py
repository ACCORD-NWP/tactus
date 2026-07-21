"""E923."""

import glob
import json
import os
import shutil
from typing import List, Optional

from tactus.config_parser import ConfigPaths

from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class E923(Task):
    """Methods for the e923 work."""

    def __init__(self, config, taskname=None):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
            taskname (str): Enforced task name
        """
        name = __class__.__name__ if taskname is None else taskname
        Task.__init__(self, config, name)

        self.climdir = self.platform.get_system_value("climdir")
        self.constant_file = f"{self.climdir}/Const.Clim.const"
        self.pgd_prel = self.config["file_templates.pgd_prel.archive"]
        self.months = [f"{mm:02d}" for mm in range(1, 13)]
        self.optional_sections = self.config.get("general.e923_optional_sections", [])

        self.master = self.get_binary("MASTERODB")

        self.nlgen = NamelistGenerator(self.config, "master")

        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value("e923_input_definition")
        )

        logger.info("Read input data spec from: {}", input_definition)
        with open(input_definition, "r", encoding="utf-8") as f:
            self.input_data = json.load(f)

    def myexec(self, cmd, i):
        """Execute binary task.

        Args:
            cmd (str) : Command to run
            i (int) : sequence number
        """
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(cmd)

        log = "NODE.001_01"
        try:
            shutil.copy(log, f"{log}_part_{i}")
        except FileNotFoundError:
            logger.info("No logfile {} produced", log)

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

    def constant_part(self, constant_file):
        """Run the constant part of e923.

        Args:
            constant_file: filename of the resulting file
        """
        logger.info("Create: {}", constant_file)

        # RRTM input
        self.link_data_files(label="rrtm")

        # PGD input
        self.fmanager.input(f"{self.climdir}/{self.pgd_prel}", "Neworog")
        self.fmanager.input(f"{self.climdir}/{self.pgd_prel}", "Newsuborog")

        for part_nr in [0, 1, 2]:
            if part_nr != 1:
                # Part 1 reuses files from part 0
                remove_link_files = self.link_data_files(label=part_nr)

            self.execute_part(part_nr)

            # Cleanup
            if part_nr == 0:
                self.remove_links(["Neworog"])
                os.rename("Const.Clim", "Neworog")
            elif len(remove_link_files) > 0:
                self.remove_links(remove_link_files)

        # Archive namelists and NODE files
        self.archive_logs(glob.glob("NODE.*"), target=self.climdir)
        self.archive_logs(glob.glob("fort.4*"), target=self.climdir)

        self.fmanager.output("Const.Clim", constant_file, provider_id="copy")

    def monthly_part(self, constant_file):
        """Run the monthly part of e923.

        Args:
            constant_file: filename of the input constant file
        """
        # Make sure constant file is in wdir
        if not os.path.exists("Const.Clim"):
            self.fmanager.input(constant_file, "Const.Clim", provider_id="copy")

        # Part 3 expects 12 input files, silly...
        part_nr = 3
        for mm in range(1, 13):
            shutil.copy(constant_file, f"Const.Clim.{mm:02d}")

        # RRTM input
        self.link_data_files(label="rrtm")
        self.link_data_files(label=part_nr)
        self.execute_part(part_nr)

        # Link/copy the constant file groups
        for group in ["constant", "constant_copy"]:
            self.link_data_files(label=group)

        for mm in self.months:
            os.rename(f"Const.Clim.{mm}", "Const.Clim")

            for part_nr in [4, *list(set(self.optional_sections) & {5, 6}), 8, 9]:
                remove_link_files = self.link_data_files(
                    label=part_nr,
                    month=mm,
                    gunzip_flags="-f",
                )
                self.execute_part(part_nr, mm)

                if len(remove_link_files) > 0:
                    self.remove_links(remove_link_files)

            # Finished. Archive output
            self.fmanager.output("Const.Clim", f"Const.Clim.{mm}")

    def execute_part(self, part_nr: int, month: Optional[int] = None):
        """Generates namelist for E923 part and execute binary.

        Args:
            part_nr: the number of the E923 part to execute
            month: (optional) month number
        """
        self.nlgen.generate_namelist(f"e923_part_{part_nr}", "fort.4")
        shutil.copy("fort.4", f"fort.4_{part_nr}")

        if month is None:
            logger.info("Executing PART {}", part_nr)
        else:
            logger.info("Executing PART {}, month {}", part_nr, month)

        self.myexec(self.master, part_nr)

    def link_data_files(
        self,
        label: int | str,
        month: Optional[int] = None,
        gunzip_flags: Optional[str] = "",
    ) -> List[str]:
        """Link/copy input files for E923 part.

        Args:
            label: dataset label in input_data (or integer to get related part)
            month: month number
            gunzip_flags: Flags passed to gunzip when decompressing compressed
                (``*.Z``) files

        Returns:
            List of files where links can be removed for cleanup
        """
        if isinstance(label, int):
            label = f"part{label}"

        data = self.input_data.get(label)

        logger.info("*** {}: {}", label, data)

        path = data.get("path", "")
        sep = "/" if len(path) > 0 else ""
        remove_link_files = []

        remove_links = data.get("remove_links", False)
        provider_id = data.get("provider_id", "symlink")
        params = data.get("param", [])

        apply_param = len(params) > 0
        if not apply_param:
            # Default use one unused param to handle non-parameterized parts
            params = [None]

        files = data["files"]
        if isinstance(files, list):
            files = dict(zip(files, files))

        for x in params:
            for dst, src in files.items():
                src_local = src.replace("@PARAM@", x) if apply_param else src
                dst_local = dst.replace("@PARAM@", x) if apply_param else dst

                if month is not None:
                    src_local = src_local.replace("@MM@", month)

                self.fmanager.input(
                    f"{path}{sep}{src_local}", dst_local, provider_id=provider_id
                )

                # Decompress compressed files
                if dst_local.endswith(".Z"):
                    os.system(f"gunzip {gunzip_flags} {dst_local}")

                if remove_links:
                    remove_link_files.append(dst_local)

        return remove_link_files


class PgdUpdate(Task):
    """PgdUpdate.

    Ensure consistency in orography between the atmosphere and surfex
    Read gridpoint orography from the atmospheric file
    and insert it in the PGD file Const.Clim.sfx.
    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __class__.__name__)

        self.climdir = self.platform.get_system_value("climdir")

        self.gl = self.get_binary("gl")
        self.basetime = config["task.args.basetime"]
        self.outfile = self.platform.substitute(
            self.config["file_templates.pgd.archive"], basetime=self.basetime
        )
        self.pgd_prel = self.platform.substitute(
            self.config["file_templates.pgd_prel.archive"], basetime=self.basetime
        )

    def execute(self):
        """Run task."""
        for ifile in ["Const.Clim.const", self.pgd_prel]:
            self.fmanager.input(f"{self.climdir}/{ifile}", ifile, provider_id="copy")

        self.fmanager.input(
            f"{self.climdir}/{self.pgd_prel}", self.outfile, provider_id="copy"
        )

        with open("namgl", "w") as namelist:
            namelist.write(
                f"""&naminterp
 INPUT_FORMAT='FA',
 OUTPUT_FORMAT='memory',
 INFILE='Const.Clim.const',
 READKEY(1:1)%FANAME='SURFGEOPOTENTIEL',
/
&naminterp
 OUTPUT_FORMAT='FIXZS',
 OUTPUT_TYPE='APPEND'
 INPUT_FORMAT='fa',
 INFILE='{self.pgd_prel}',
 OUTFILE='{self.outfile}',
 USE_SAVED_CADRE=T,
 READKEY%FANAME='SFX.ZS',
/
                  """
            )
            namelist.close()

        # Run gl
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(f"{self.gl} -n namgl")

        self.fmanager.output(self.outfile, f"{self.climdir}/{self.outfile}")


class E923Constant(E923):
    """E923Constant task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        E923.__init__(self, config, "E923Constant")

    def execute(self):
        """Run task.

        Define run sequence.

        """
        tactusmakedirs(self.climdir, unixgroup=self.unix_group)

        logger.debug("Constant file:{}", self.constant_file)

        # Run the constant part
        self.constant_part(self.constant_file)


class E923Monthly(E923):
    """E923Monthly task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (tactus.ParsedConfig): Configuration
        """
        try:
            months = config["task.args.months"].split(",")
            tag = "_" + "_".join(months)
        except KeyError:
            months = [f"{mm:02d}" for mm in range(1, 13)]
            tag = ""

        E923.__init__(self, config, f"E923Monthly{tag}")
        self.months = months
        logger.debug("Create files for month:{}", self.months)

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Run the monthly part
        self.monthly_part(self.constant_file)

        # Store the data
        for mm in self.months:
            source = f"Const.Clim.{mm}"
            target = f"{self.climdir}/Const.Clim.{mm}"
            self.fmanager.output(source, target)

            self.archive_logs(glob.glob("NODE.*"), target=self.climdir)
            self.archive_logs(glob.glob("fort.4*"), target=self.climdir)
