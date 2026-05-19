"""BlendSur task — merges CANARI surface analysis with LBC SST.

Two BLENDSUR steps:
  Step 1: Add LBC SST to CANARI analysis over sea.
  Step 2: Subtract CANARI SST (keep land surface from CANARI, sea from LBC).

Output ``surface.YYYYMMDDRR`` is the blended surface field used as the
first guess for 3D-Var screening and as the Forecast initial conditions.

Ported from: seemhews/scripts/assim/analysis/surface/blendsur.ecf
"""
import os
import shutil

from ..datetime_utils import as_datetime
from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class BlendSur(Task):
    """Blend CANARI surface analysis with LBC SST using the BLENDSUR utility."""

    def __init__(self, config):
        """Construct BlendSur task.

        Args:
            config (tactus.ParsedConfig): Experiment configuration.
        """
        Task.__init__(self, config, __class__.__name__)
        self.basetime = as_datetime(config["general.times.basetime"])
        self.domain = config["domain.name"]
        self.cnmexp = config["general.cnmexp"]
        self.da_scratch = self.platform.substitute(config["da.scratch"])
        # Location of the +0000 LBC file (from InterpolationFamily)
        self.intp_bddir = self.platform.substitute(
            config.get("system.intp_bddir", "")
        )
        logger.debug("Constructed BlendSur task")

    def execute(self):
        """Run two BLENDSUR steps to blend surface fields."""
        yyyy = self.basetime.strftime("%Y")
        mm = self.basetime.strftime("%m")
        dd = self.basetime.strftime("%d")
        rr = self.basetime.strftime("%H")
        ymdrr = f"{yyyy}{mm}{dd}{rr}"

        canari_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, "canari")

        # --- inputs ---
        # LBC +000 file
        lbc_file = os.path.join(
            self.intp_bddir, f"ELSCF{self.cnmexp}ALBC000"
        )
        if not os.path.isfile(lbc_file):
            raise FileNotFoundError(
                f"BlendSur: LBC +0000 not found at {lbc_file}"
            )
        shutil.copy2(lbc_file, "AL_LBC")

        # CANARI analysis
        canari_analysis = os.path.join(canari_dir, "ICMSHANAL+0000")
        if not os.path.isfile(canari_analysis):
            raise FileNotFoundError(
                f"BlendSur: CANARI analysis not found at {canari_analysis}"
            )
        shutil.copy2(canari_analysis, "AL_3D")

        # --- binary ---
        blendsur_bin = self.get_binary("BLENDSUR")

        rte = dict(os.environ)

        # ------------------------------------------------------------------
        # Step 1: Add LBC SST analysis over sea to CANARI OI analysis
        #   CL_FNAME1='AL_3D'  (CANARI)
        #   CL_FNAME2='AL_LBC' (LBC)
        #   CL_FNAME3='AL_2S'  (output)
        #   ZSIGNL=0., ZSIGNS=1.
        # ------------------------------------------------------------------
        shutil.copy2("AL_3D", "AL_2S")
        namel_1 = (
            "&NAMCUMF\n/\n&NAMDYNCORE\n/\n&NAMPAR0\n/\n&NAMSCEN\n/\n"
            "&NEMFPEZO\n/\n&NAMBLENDSUR\n"
            "CL_FNAME1='AL_3D'\nCL_FNAME2='AL_LBC'\nCL_FNAME3='AL_2S'\n"
            "ZSIGNL=0.\nZSIGNS=1.\n/\n"
        )
        with open("fort.4", "w") as fh:
            fh.write(namel_1)

        BatchJob(rte, wrapper="").run(blendsur_bin)
        self._check_node("NODE1")

        # ------------------------------------------------------------------
        # Step 2: Subtract CANARI SST from Step-1 output to keep land fields
        #   CL_FNAME1='AL_2S'  (step 1 output)
        #   CL_FNAME2='AL_3D'  (CANARI)
        #   CL_FNAME3='AL_A'   (final output)
        #   ZSIGNS=-1., ZSIGNL=0.
        # ------------------------------------------------------------------
        shutil.copy2("AL_3D", "AL_A")
        namel_2 = (
            "&NAMBLENDSUR\n"
            "CL_FNAME1='AL_2S',\nCL_FNAME2='AL_3D',\nCL_FNAME3='AL_A',\n"
            "ZSIGNS=-1.,\nZSIGNL=0.,\n/\n"
            "&NAMSCEN\n/\n&NAMCUMF\n/\n&NAMDYNCORE\n/\n&NAMPAR0\n/\n&NEMFPEZO\n/\n"
        )
        with open("fort.4", "w") as fh:
            fh.write(namel_2)

        BatchJob(rte, wrapper="").run(blendsur_bin)
        self._check_node("NODE2")

        surface_out = f"surface.{ymdrr}"
        os.rename("AL_A", surface_out)

        # --- archive ---
        out_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, "blendsur")
        tactusmakedirs(out_dir)
        shutil.copy2(surface_out, os.path.join(out_dir, surface_out))
        logger.info("BlendSur: blended surface field archived to {}", out_dir)
        self.archive_logs(["NODE1", "NODE2"])

    # ------------------------------------------------------------------

    @staticmethod
    def _check_node(logfile):
        """Raise RuntimeError if BLENDSUR wrote an ERROR line to *logfile*."""
        if os.path.isfile(logfile):
            with open(logfile) as fh:
                if "ERROR" in fh.read():
                    raise RuntimeError(
                        f"BlendSur: BLENDSUR failed — see {logfile}."
                    )
