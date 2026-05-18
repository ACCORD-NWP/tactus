"""OopsVar task — OOPS-based 3D-Var (screening + minimization via OOVAR).

Replaces the classic two-step Screen + Minim MASTERODB pair with a single
``OOVAR oops.json`` execution.  Active when ``da.do_upper_air = true`` (the default).

OOPS namelist directory
-----------------------
The task reads namelist *templates* from ``da.oops.namelist_dir``.  Unlike
the MASTERODB-based tasks, OOPS uses many small named files rather than a
single ``fort.4``.  The following files are expected in that directory:

``namelist_oopsminim``
    Main minimization namelist template.  Placeholders ``{NBPROC}``,
    ``{NPRTRV}``, ``{NPRTRW}``, ``{NSTRIN}``, ``{NSTROUT}``,
    ``{NPROMA}``, ``{NPRGPEW}``, ``{NPRGPNS}``, ``{niter}``,
    ``{nsimu}``, ``{rednmc}``, ``{LEJK}``, ``{LSPRT}``, ``{qlsp}``,
    ``{qlgp}``, ``{NSMAXJK}``, ``{ALPHAKT}``, ``{ALPHAKVOR}``,
    ``{ALPHAKDIV}``, ``{ALPHAKQ}``, ``{ALPHAKP}``, ``{PRESINFJK}``,
    ``{PRESUPJK}``, ``{NTRUNCJK}`` are substituted from config.

``oops_3dvar.json``
    OOPS control JSON template.  Placeholders ``{yyyy}``, ``{mm}``,
    ``{dd}``, ``{hh}`` are substituted with the basetime date parts.

``namelist_standard_geometry``, ``namelist_geometry_tENS``
    Geometry namelists.  Placeholder ``{nproma}`` is substituted.

``namelist_bmatrix_3dvar``, ``namelist_observations_tlad``,
``namelist_observations``, ``namelist_traj_model``,
``namelist_linear_model``, ``namelist_nonlinear_model``,
``namelist_oops_write_spec``, ``namelist_write_analysis``,
``namelist_gom_setup_hres``, ``namelist_gom_setup``
    Linked as-is (no substitution) to the names expected by OOVAR.

``iasichannels``
    IASI channel selection file, copied from the OOPS namelist dir.
"""
import datetime
import glob
import os
import shutil
import stat

from ..datetime_utils import as_datetime
from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class OopsVar(Task):
    """Run OOPS-based 3D-Var (screening + minimization in one OOVAR call).

    Reads the merged ECMA ODB from the OdbMerge scratch, the blended
    first-guess from BlendSur, and produces the 3D-Var analysis
    (``MXMINI999+0000``) archived to ``da.analysis_dir``.
    """

    def __init__(self, config):
        """Construct OopsVar task.

        Args:
            config (tactus.ParsedConfig): Experiment configuration.
        """
        Task.__init__(self, config, __class__.__name__)
        self.basetime = as_datetime(config["general.times.basetime"])
        self.da_scratch = self.platform.substitute(config["da.scratch"])
        self.da_const_dir = self.platform.substitute(config["da.const_dir"])
        self.oops_namelist_dir = self.platform.substitute(
            config.get("da.oops.namelist_dir", "")
        )
        self.varbc_dir = self.platform.substitute(config.get("da.varbc_dir", ""))
        self.analysis_dir = self.platform.substitute(config.get("da.analysis_dir", ""))
        self.da_bgcycle = config.get("da.bgcycle", "")
        self.rttov_coef_dir = self.platform.substitute(
            config.get(
                "da.rttov_coef_dir",
                os.path.join(config.get("da.const_dir", ""), "rtcoef"),
            )
        )
        self.nbpool = config.get("da.nbpool", 12)

        # Minimization parameters
        self.niter = config.get("da.oops.niter", 66)
        self.nsimu = config.get("da.oops.nsimu", 69)
        self.rednmc = config.get("da.oops.rednmc", 0.5)

        # Geometry / parallel I/O
        self.nproma = config.get("da.oops.nproma", -32)
        self.nprgpns = config.get("da.oops.nprgpns", 128)
        self.nprgpew = config.get("da.oops.nprgpew", 1)
        self.nprtrv = config.get("da.oops.nprtrv", 1)
        self.nprtrw = config.get("da.oops.nprtrw", 128)
        self.nstrin = config.get("da.oops.nstrin", 2)
        self.nstrout = config.get("da.oops.nstrout", 2)

        # JK transform / covariance parameters (Fortran .TRUE./.FALSE. strings)
        def _f90bool(val):
            return ".TRUE." if val else ".FALSE."

        self.lejk = _f90bool(config.get("da.oops.lejk", False))
        self.lsprt = _f90bool(config.get("da.oops.lsprt", False))
        self.qlgp = _f90bool(config.get("da.oops.qlgp", True))
        self.qlsp = _f90bool(config.get("da.oops.qlsp", False))
        self.nsmaxjk = config.get("da.oops.nsmaxjk", 215)
        self.alphakt = config.get("da.oops.alphakt", 0.70)
        self.alphakvor = config.get("da.oops.alphakvor", 0.80)
        self.alphakdiv = config.get("da.oops.alphakdiv", 0.10)
        self.alphakq = config.get("da.oops.alphakq", 0.04)
        self.alphakp = config.get("da.oops.alphakp", 0.0)
        self.presinfjk = config.get("da.oops.presinfjk", 100500.0)
        self.presupjk = config.get("da.oops.presupjk", 98000.0)
        self.ntruncjk = config.get("da.oops.ntruncjk", 8)

        if not self.oops_namelist_dir:
            raise RuntimeError(
                "OopsVar: da.oops.namelist_dir must be configured when da.do_upper_air = true."
            )
        logger.debug("Constructed OopsVar task")

    # ------------------------------------------------------------------

    def execute(self):
        """Run OOVAR 3D-Var analysis and archive the result."""
        yyyy = self.basetime.strftime("%Y")
        mm = self.basetime.strftime("%m")
        dd = self.basetime.strftime("%d")
        rr = self.basetime.strftime("%H")
        ymdrr = f"{yyyy}{mm}{dd}{rr}"

        nproc = int(os.environ.get("NPROC", "128"))

        odb_dir = os.path.join(
            self.da_scratch, yyyy, mm, dd, rr, "odbmerge_3dvar", "ECMA"
        )
        blendsur_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, "blendsur")
        surface_file = os.path.join(blendsur_dir, f"surface.{ymdrr}")

        # --- binary ---
        oovar_bin = self.get_binary("OOVAR")
        bindir = os.path.dirname(oovar_bin)
        os.symlink(oovar_bin, "OOVAR")

        # ioassign tools — must be executable in the working directory
        for tool in ("ioassign", "create_ioassign"):
            src = os.path.join(bindir, tool)
            if os.path.isfile(src):
                shutil.copy2(src, tool)
                os.chmod(tool, os.stat(tool).st_mode | stat.S_IEXEC | stat.S_IXGRP)

        # --- ECMA ODB ---
        if not os.path.isdir(odb_dir):
            raise FileNotFoundError(
                f"OopsVar: merged ECMA ODB not found at {odb_dir}"
            )
        shutil.copytree(odb_dir, "ECMA", symlinks=True)

        # --- CCMA skeleton via create_ioassign (serial) ---
        os.makedirs("CCMA", exist_ok=True)
        rte_setup = dict(os.environ)
        rte_setup["PATH"] = f"{os.getcwd()}:{rte_setup.get('PATH', '')}"
        BatchJob(rte_setup, wrapper="").run(f"./create_ioassign -lCCMA -n{nproc}")
        # Merge IOASSIGN so OOVAR can read both ECMA and write CCMA
        if os.path.isfile("IOASSIGN") and os.path.isfile("ECMA/IOASSIGN"):
            with open("ECMA/IOASSIGN", "a") as ecma_io, open("IOASSIGN") as ccma_io:
                ecma_io.write(ccma_io.read())
        for dest in ("CCMA/IOASSIGN", "IOASSIGN"):
            if os.path.isfile("ECMA/IOASSIGN") and not os.path.isfile(dest):
                shutil.copy2("ECMA/IOASSIGN", dest)

        # --- first guess from BlendSur ---
        if not os.path.isfile(surface_file):
            raise FileNotFoundError(
                f"OopsVar: blended first-guess not found at {surface_file}"
            )
        shutil.copy2(surface_file, "ICMSHOOPS+0000")
        for link_name in (
            "ICMSHMINIIMIN", "ICMSHMINIINIT", "ICMRFMINI0000",
            "ICMSHOOPSINIT", "MXMINI999+0000", "ICMSHOOPSIMIN",
            "ICMSHSCREINIT",
        ):
            if not os.path.lexists(link_name):
                os.symlink(surface_file, link_name)

        # --- VarBC ---
        dateprev = (self.basetime - datetime.timedelta(days=1)).strftime("%Y/%m/%d")
        varbc_src = os.path.join(self.varbc_dir, dateprev, f"VARBC.cycle_{rr}")
        if os.path.isfile(varbc_src):
            shutil.copy2(varbc_src, "VARBC.cycle")
        elif ymdrr != self.da_bgcycle:
            raise FileNotFoundError(
                f"OopsVar: VARBC file not found at {varbc_src}. "
                "Cold-start (bgcycle) is the only allowed run without VARBC."
            )

        # --- B-matrix (3 files for OOPS: .bal, .cv, .cvt) ---
        for bmat in ("stabal96.bal", "stabal96.cv", "stabal96.cvt"):
            src = os.path.join(self.da_const_dir, bmat)
            if os.path.isfile(src):
                shutil.copy2(src, os.path.basename(bmat))
            else:
                logger.warning("OopsVar: B-matrix file not found: {}", src)

        # --- constant links ---
        const_globs = ["ATLAS_*", "ATLAS*", "errgrib*", "MCICA", "RAD*", "amv_*",
                       "bcor_noaa.dat", "bcor_meto.dat"]
        for pattern in const_globs:
            for src in glob.glob(os.path.join(self.da_const_dir, pattern)):
                link = os.path.basename(src)
                if not os.path.lexists(link):
                    os.symlink(src, link)

        # Optional LHN files (skip silently if absent)
        for lhn_file in ("MAP_INCA_*.txt", "LHN_DUMMY.fa"):
            for src in glob.glob(os.path.join(self.da_const_dir, lhn_file)):
                link = os.path.basename(src)
                if not os.path.lexists(link):
                    os.symlink(src, link)

        # --- OOPS namelists ---
        self._link_oops_namelists(nproc, yyyy, mm, dd, rr)

        # --- ODB environment ---
        rte = dict(os.environ)
        rte.update(
            {
                "EC_MEMINFO": "0",
                "EC_LINUX_TRBK": "1",
                "EC_MPI_ATEXIT": "0",
                "EC_PROFILE_HEAP": "0",
                "OOPS_LOGFILE": "oops.log",
                "OMP_NUM_THREADS": os.environ.get("OMP_NUM_THREADS", "1"),
                "F_UFMTENDIAN": "big",
                "F_RECLUNIT": "BYTE",
                "MKL_DYNAMIC": "FALSE",
                "DR_HOOK": "0",
                "DR_HOOK_SILENT": "1",
                "DR_HOOK_IGNORE_SIGNALS": "-1",
                # ODB
                "ODB_STATIC_LINKING": "1",
                "TO_ODB_ECMWF": "0",
                "ODB_TRACE_FILE": "List_odb",
                "TO_ODB_DEBUG": "0",
                "ODB_TRACE_PROC": "0",
                "ODB_IO_METHOD": "4",
                "ODB_IO_FILESIZE": "128",
                "ODB_IO_GRPSIZE": str(self.nbpool),
                "BATOR_NBSLOT": str(self.config.get("da.bator_nbslot", 1)),
                "ODB_CCMA_CREATE_DIRECT": "1",
                "BATOR_WINDOW_LEN": str(self.config.get("da.bator_window_len", 180)),
                "BATOR_WINDOW_SHIFT": str(self.config.get("da.bator_window_shift", -90)),
                "BATOR_SLOT_LEN": str(self.config.get("da.bator_slot_len", 0)),
                "BATOR_CENTER_LEN": "0",
                "ODB_SRCPATH_RSTBIAS": os.path.join(self.wdir, "ECMA"),
                "ODB_SRCPATH_ECMA": os.path.join(self.wdir, "ECMA"),
                "ODB_DATAPATH_ECMA": os.path.join(self.wdir, "ECMA"),
                "ODB_SRCPATH_CCMA": os.path.join(self.wdir, "CCMA"),
                "ODB_DATAPATH_CCMA": os.path.join(self.wdir, "CCMA"),
                "ODB_CCMA_CREATE_POOLMASK": "1",
                "ODB_CCMA_POOLMASK_FILE": os.path.join(self.wdir, "CCMA", "CCMA.poolmask"),
                "ODB_CMA": "CCMA",
                "IOASSIGN": os.path.join(self.wdir, "IOASSIGN"),
                "RTTOV_COEFDIR": self.rttov_coef_dir,
                "PATH": f"{os.getcwd()}:{rte.get('PATH', '')}",
            }
        )

        # --- run OOVAR ---
        BatchJob(rte, wrapper=self.platform.substitute(self.wrapper)).run(
            "./OOVAR oops.json"
        )

        # --- verify and archive ---
        analysis = "MXMINI999+0000"
        if not os.path.isfile(analysis) or os.path.getsize(analysis) == 0:
            raise RuntimeError("OopsVar: analysis file not produced or empty.")

        if self.analysis_dir:
            out_dir = os.path.join(self.analysis_dir, yyyy, mm, dd)
            tactusmakedirs(out_dir)
            dst = os.path.join(out_dir, f"analysis.{ymdrr}")
            shutil.copy2(analysis, dst)
            logger.info("OopsVar: analysis archived to {}", dst)

        # Archive updated VarBC to varbc_dir for the next cycle to read
        if self.varbc_dir and os.path.isfile("VARBC.cycle"):
            varbc_out = os.path.join(self.varbc_dir, yyyy, mm, dd)
            tactusmakedirs(varbc_out)
            shutil.copy2("VARBC.cycle", os.path.join(varbc_out, f"VARBC.cycle_{rr}"))

        self.archive_logs(["oops.log", "NODE.001_01"])

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _link_oops_namelists(self, nproc, yyyy, mm, dd, rr):
        """Generate / link all OOPS namelist files from da.oops.namelist_dir."""
        nd = self.oops_namelist_dir

        # Substitution dict for the main minimization namelist template
        subst = {
            "{NBPROC}":    str(nproc),
            "{NPRTRV}":    str(self.nprtrv),
            "{NPRTRW}":    str(self.nprtrw),
            "{NSTRIN}":    str(self.nstrin),
            "{NSTROUT}":   str(self.nstrout),
            "{NPROMA}":    str(self.nproma),
            "{NPRGPEW}":   str(self.nprgpew),
            "{NPRGPNS}":   str(self.nprgpns),
            "{niter}":     str(self.niter),
            "{nsimu}":     str(self.nsimu),
            "{rednmc}":    str(self.rednmc),
            "{LEJK}":      self.lejk,
            "{LSPRT}":     self.lsprt,
            "{qlsp}":      self.qlsp,
            "{qlgp}":      self.qlgp,
            "{NSMAXJK}":   str(self.nsmaxjk),
            "{ALPHAKT}":   str(self.alphakt),
            "{ALPHAKVOR}": str(self.alphakvor),
            "{ALPHAKDIV}": str(self.alphakdiv),
            "{ALPHAKQ}":   str(self.alphakq),
            "{ALPHAKP}":   str(self.alphakp),
            "{PRESINFJK}": str(self.presinfjk),
            "{PRESUPJK}":  str(self.presupjk),
            "{NTRUNCJK}":  str(self.ntruncjk),
        }

        # fort.4 from namelist_oopsminim template
        self._fill_template(
            os.path.join(nd, "namelist_oopsminim"), "fort.4", subst
        )

        # Geometry namelists with NPROMA substitution
        geom_subst = {"{nproma}": str(self.nproma)}
        self._fill_template(
            os.path.join(nd, "namelist_standard_geometry"),
            "naml_standard_geometry",
            geom_subst,
        )
        self._fill_template(
            os.path.join(nd, "namelist_geometry_tENS"),
            "naml_standard_geometry_tENS",
            geom_subst,
        )

        # OOPS JSON from oops_3dvar.json template (date substitution)
        json_subst = {
            "{yyyy}": yyyy,
            "{mm}":   mm,
            "{dd}":   dd,
            "{hh}":   rr,
        }
        self._fill_template(
            os.path.join(nd, "oops_3dvar.json"), "oops.json", json_subst
        )

        # Namelists linked as-is (no substitution)
        link_map = {
            "namelist_bmatrix_3dvar":     "naml_bmatrix",
            "namelist_observations_tlad": "naml_observations_tlad",
            "namelist_observations":      "naml_observations",
            "namelist_traj_model":        "naml_traj_model",
            "namelist_linear_model":      "naml_linear_model",
            "namelist_nonlinear_model":   "naml_nonlinear_model",
            "namelist_oops_write_spec":   "naml_oops_write_spec",
            "namelist_write_analysis":    "naml_write_analysis",
            "namelist_gom_setup_hres":    "namelist_gom_setup_hres",
            "namelist_gom_setup":         "namelist_gom_setup",
        }
        for src_name, link_name in link_map.items():
            src = os.path.join(nd, src_name)
            if os.path.isfile(src):
                if not os.path.lexists(link_name):
                    os.symlink(src, link_name)
                    os.chmod(link_name, os.stat(link_name).st_mode | stat.S_IEXEC)
            else:
                logger.warning("OopsVar: OOPS namelist not found: {}", src)

        # iasichannels — copy so OOVAR can read it
        iasi = os.path.join(nd, "iasichannels")
        if os.path.isfile(iasi):
            shutil.copy2(iasi, "iasichannels")

    @staticmethod
    def _fill_template(src_path, dst_path, substitutions):
        """Read *src_path*, apply *substitutions* dict, write to *dst_path*.

        Raises FileNotFoundError if *src_path* does not exist.
        """
        if not os.path.isfile(src_path):
            raise FileNotFoundError(
                f"OopsVar: OOPS template not found: {src_path}"
            )
        text = open(src_path).read()
        for placeholder, value in substitutions.items():
            text = text.replace(placeholder, value)
        with open(dst_path, "w") as fh:
            fh.write(text)
        logger.debug("OopsVar: wrote {} from template {}", dst_path, src_path)
