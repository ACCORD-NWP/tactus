"""E923."""

import glob
import os
import shutil

from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import deodemakedirs
from .base import Task
from .batch import BatchJob


class E923(Task):
    """Methods for the e923 work."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.climdir = self.platform.get_system_value("climdir")
        self.constant_file = f"{self.climdir}/Const.Clim.const"
        self.months = [f"{mm:02d}" for mm in range(1, 13)]

        self.master = self.get_binary("MASTERODB")

        self.nlgen = NamelistGenerator(self.config, "master")

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
            except FileNotFoundError:
                logger.warning("Could not remove file '{}'.", x, exc_info=True)

    def print_part(self, part, month=None):
        """Run the constant part of e923.

        Args:
            part : which step
            month : month
        """
        if month is None:
            logger.info("Executing PART {}", part)
        else:
            logger.info("Executing PART {}, month {}", part, month)

    def constant_part(self, constant_file):
        """Run the constant part of e923.

        Args:
            constant_file : filename of the resulting file
        """
        logger.info("Create: {}", constant_file)

        # RTTM input
        for ifile in ["MCICA", "RADSRTM"]:
            self.fmanager.input(f"@RRTM_DIR@/{ifile}", ifile)

        # PGD input

        self.fmanager.input(f"{self.climdir}/PGD_prel.fa", "Neworog")

        # Part 0
        i = 0
        topo_files = [
            "Water_Percentage",
            "Oro_Mean",
            "Sigma",
            "Nb_Peaks",
            "Urbanisation",
            "Dh_over_Dx_Dh_over_Dy",
            "Dh_over_Dx_square",
            "Dh_over_Dy_square",
            "Hmax-HxH-Hmin_ov4",
        ]
        for fname in topo_files:
            self.fmanager.input(f"@E923_DATA@/GTOPT030/{fname}", fname)

        self.nlgen.generate_namelist(f"e923_{i}", "fort.4")
        shutil.copy("fort.4", "fort.4_0")
        self.print_part(0)
        self.myexec(self.master, 0)

        # Cleanup
        self.remove_links(["Neworog"])
        os.rename("Const.Clim", "Neworog")

        # Part 1
        i = 1
        self.nlgen.generate_namelist(f"e923_{i}", "fort.4")
        shutil.copy("fort.4", "fort.4_1")
        self.print_part(1)
        self.myexec(self.master, 1)
        self.remove_links(topo_files)

        # Part 2
        i = 2
        self.nlgen.generate_namelist(f"e923_{i}", "fort.4")
        shutil.copy("fort.4", f"fort.4_{i}")
        ifiles = [
            "itp_GL",
            "alb_GL",
            "emi_GL",
            "dps_GL",
            "arg_GL",
            "sab_GL",
            "vgx_GL",
            "dpr_GL",
        ]
        for fname in ifiles:
            self.fmanager.input(f"@E923_DATA@/SURFACE_G/{fname}", fname)

        self.print_part(2)
        self.myexec(self.master, 2)
        self.remove_links(ifiles)

        files = glob.glob("NODE.*")
        logger.info(files)
        self.archive_logs(files, target=self.climdir)
        self.fmanager.output("Const.Clim", constant_file, provider_id="copy")

    def monthly_part(self, constant_file):
        """Run the monthly part of e923.

        Args:
            constant_file : filename of the input constant file
        """
        # Make sure constant file is in wdir
        if not os.path.exists("Const.Clim"):
            self.fmanager.input(constant_file, "Const.Clim", provider_id="copy")

        for ifile in ["MCICA", "RADSRTM"]:
            self.fmanager.input(f"@RRTM_DIR@/{ifile}", ifile)

        # Part 3 expects 12 input files, silly...
        i = 3
        for mm in range(1, 13):
            shutil.copy(constant_file, f"Const.Clim.{mm:02d}")

        for ifile in ["N108_GL"]:
            self.fmanager.input(f"@E923_DATA@/N108/{ifile}", ifile)

        self.nlgen.generate_namelist(f"e923_{i}", "fort.4")
        shutil.copy("fort.4", f"fort.4_{i}")
        self.print_part(i)
        self.myexec(self.master, i)

        for ifile in ["z0v_GL", "alv_GL", "rsm_GL"]:
            self.fmanager.input(f"@E923_DATA@/SURFACE_G/{ifile}", ifile)

        for ifile in [
            "msk_HR",
            "itp_HR",
            "dpr_HR",
            "rsm_HR",
            "vgx_HR",
            "alv_HR",
            "z0v_HR",
        ]:
            self.fmanager.input(f"@E923_DATA@/SURFACE_L/{ifile}", ifile)

        for ifile in ["rel_GL.Z"]:
            self.fmanager.input(
                f"@E923_DATA@/CLIM_G/v2/{ifile}", ifile, provider_id="copy"
            )
        os.system("gunzip rel_GL.Z")  # noqa

        for mm in self.months:

            os.rename(f"Const.Clim.{mm}", "Const.Clim")

            # PART 4
            files = []
            for vegtype in ["veg", "lai"]:
                source = f"@E923_DATA@/SURFACE_G/{vegtype}_{mm}_GL"
                target = f"{vegtype}_GL"
                files.append(target)
                self.fmanager.input(source, target)

            self.nlgen.generate_namelist("e923_4", "fort.4")
            shutil.copy("fort.4", "fort.4_4")
            self.print_part(4, mm)
            self.myexec(self.master, 4)
            self.remove_links(files)

            # PART 5
            files = []
            for vegtype in ["veg", "lai"]:
                source = f"@E923_DATA@/SURFACE_L/{vegtype}_{mm}_HR"
                target = f"{vegtype}_HR"
                files.append(target)
                self.fmanager.input(source, target)

            self.nlgen.generate_namelist("e923_5", "fort.4")
            shutil.copy("fort.4", "fort.4_5")
            self.print_part(5, mm)
            self.myexec(self.master, 5)
            self.remove_links(files)

            # PART 6
            indata = ["tpl", "wpl", "snl"]
            for x in indata:
                target = f"{x}_GL.Z"
                source = f"@E923_DATA@/CLIM_G/v2/{x}_{mm}_GL.Z"
                self.fmanager.input(source, target, provider_id="copy")

            for x in indata:
                os.system(f"gunzip -f {x}_GL.Z")  # noqa

            self.fmanager.input("tpl_GL", "tsl_GL", provider_id="copy")
            self.fmanager.input("wpl_GL", "wsl_GL", provider_id="copy")

            self.nlgen.generate_namelist("e923_6", "fort.4")
            shutil.copy("fort.4", "fort.4_6")
            self.print_part(6, mm)
            self.myexec(self.master, 6)

            # PART 8
            self.fmanager.input(f"@E923_DATA@/abc_O3/abc_quadra_{mm}", "abc_coef")
            self.nlgen.generate_namelist("e923_8", "fort.4")
            shutil.copy("fort.4", "fort.4_8")
            self.print_part(8, mm)
            self.myexec(self.master, 8)

            # PART 9
            self.fmanager.input(f"@E923_DATA@/aero_tegen/aero.tegen.m{mm}_GL", "aero_GL")

            self.nlgen.generate_namelist("e923_9", "fort.4")
            shutil.copy("fort.4", "fort.4_9")
            self.print_part(9, mm)
            self.myexec(self.master, 9)

            # Finished. Archive output
            self.fmanager.output("Const.Clim", f"Const.Clim.{mm}")


class PgdUpdate(Task):
    """PgdUpdate.

    Ensure consistency in orography between the atmosphere and surfex
    Read gridpoint orography from the atmospheric file
    and insert it in the PGD file Const.Clim.sfx.
    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, "PgdUpdate")

        self.climdir = self.platform.get_system_value("climdir")
        self.gl = self.get_binary("gl")
        self.outfile = "Const.Clim.sfx"

    def execute(self):
        """Run task."""
        for ifile in ["Const.Clim.const", "PGD_prel.fa"]:
            self.fmanager.input(f"{self.climdir}/{ifile}", ifile, provider_id="copy")

        self.fmanager.input(
            f"{self.climdir}/PGD_prel.fa", self.outfile, provider_id="copy"
        )

        with open("namgl", "w") as namelist:
            namelist.write(
                """&naminterp
 INPUT_FORMAT='FA',
 OUTPUT_FORMAT='memory',
 INFILE='Const.Clim.const',
 READKEY(1:1)%FANAME='SURFGEOPOTENTIEL',
/
&naminterp
 OUTPUT_FORMAT='FIXZS',
 OUTPUT_TYPE='APPEND'
 INPUT_FORMAT='fa',
 INFILE='PGD_prel.fa',
 OUTFILE='Const.Clim.sfx',
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
            config (deode.ParsedConfig): Configuration
        """
        E923.__init__(self, config)
        self.name = "E923Constant"

    def execute(self):
        """Run task.

        Define run sequence.

        """
        deodemakedirs(self.climdir, unixgroup=self.unix_group)

        logger.debug("Constant file:{}", self.constant_file)

        # Run the constant part
        self.constant_part(self.constant_file)


class E923Monthly(E923):
    """E923Monthly task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        try:
            months = config["task.args.months"].split(",")
            tag = "_" + "_".join(months)
        except KeyError:
            months = [f"{mm:02d}" for mm in range(1, 13)]
            tag = ""

        E923.__init__(self, config)
        self.name = f"E923Monthly{tag}"
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
