"""Shared base for ODB observation-ingestion tasks (Bator, Obsconvert)."""

import os
import shutil
import subprocess
from collections.abc import Mapping

import pyproj

from ..datetime_utils import as_datetime
from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class OdbIngestionTask(Task):
    """Abstract base for tasks that run an ODB observation-ingestion binary.

    Subclasses must set the four class attributes below.
    """

    _BINARY_NAME: str = ""
    _PARAM_CFG_NAME: str = ""
    _LOG_TAG: str = ""
    _NLGEN_KEY: str = ""

    def __init__(self, config):
        Task.__init__(self, config, self.__class__.__name__)
        self.basetime = as_datetime(config["general.times.basetime"])
        self.da_scratch = self.platform.substitute(config["da.scratch"])
        self.da_const_dir = self.platform.substitute(config["da.const_dir"])
        self.domain = config["domain.name"]
        self.obstype = config.get("task.args.obstype", "")
        if not self.obstype:
            raise RuntimeError(
                f"{self._LOG_TAG}: task.args.obstype variable is not set. "
            )
        self.family1 = config.get("task.args.da_stream", "3dvar")
        self.nbpool = (
            config.get("da.nbpool", 16)
            if self.family1 == "surface"
            else config.get("da.oops.nbpool", 128)
        )
        self.bator_window_len = config.get("da.bator_window_len", 180)
        self.bator_window_shift = config.get("da.bator_window_shift", -90)
        self.bator_nbslot = config.get("da.bator_nbslot", 1)
        self.bator_slot_len = config.get("da.bator_slot_len", 0)
        self.bator_center_len = config.get("da.bator_center_len", 0)
        obs_provider = config.get("da.obs_provider", "UWC")
        self._provider = config.get("da.providers", {}).get(obs_provider, {})
        self.nlgen = NamelistGenerator(config, self._NLGEN_KEY)
        logger.debug("Constructed {} task for obstype={}", self._LOG_TAG, self.obstype)

    def execute(self):
        """Run the configured obs-ingestion binary for *self.obstype*."""
        yyyy = self.basetime.strftime("%Y")
        mm = self.basetime.strftime("%m")
        dd = self.basetime.strftime("%d")
        rr = self.basetime.strftime("%H")

        obsprep_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, "obsprep")

        # --- binary ---
        # Allow a per-obstype override (e.g. "Bator_synop") so a single
        # obstype can point at a different build than the rest of this task.
        specific_task = f"{self.name}_{self.obstype}"
        task_name = (
            specific_task
            if self.config.get(f"submission.task_exceptions.{specific_task}")
            is not None
            else None
        )
        bin_path = self.get_binary(self._BINARY_NAME, task_name=task_name)
        bindir = os.path.dirname(bin_path)
        for tool in ["create_ioassign", "ioassign"]:
            src = os.path.join(bindir, tool)
            if os.path.isfile(src):
                os.symlink(src, tool)
        os.symlink(bin_path, self._BINARY_NAME)

        # --- namelists and constants ---
        self.nlgen.generate_namelist(self._NLGEN_KEY, "NAMELIST")

        for static_file, link_name in [
            (self._PARAM_CFG_NAME, "param.cfg"),
            ("param_rgb", "namelist_rgb"),
        ]:
            src = os.path.join(self.da_const_dir, static_file)
            if os.path.isfile(src):
                os.symlink(src, link_name)

        self._write_gpssol_list()
        self._write_nam_lamflag()
        bator_lamflag = "1"

        for const in ["LISTE_NOIRE_DIAP", "LISTE_LOC"]:
            src = os.path.join(self.da_const_dir, const)
            if os.path.isfile(src):
                os.symlink(src, const)

        # --- ODB environment ---
        rte = dict(os.environ)
        rte.update(
            {
                "TO_ODB_ECMWF": "0",
                "TO_ODB_SWAPOUT": "0",
                "ODB_DEBUG": "0",
                "ODB_CTX_DEBUG": "0",
                "ODB_REPRODUCIBLE_SEQNO": "2",
                "ODB_STATIC_LINKING": "1",
                "ODB_IO_METHOD": "1",
                "ODB_IO_FILESIZE": "128",
                "ODB_IO_GRPSIZE": str(self.nbpool),
                "EC_PROFILE_HEAP": "0",
                "F_RECLUNIT": "BYTE",
                "F_UFMTENDIAN": "big",
                "ODB_ANALYSIS_DATE": f"{yyyy}{mm}{dd}",
                "ODB_ANALYSIS_TIME": f"{rr}0000",
                "TIME_INIT_YYYYMMDD": f"{yyyy}{mm}{dd}",
                "TIME_INIT_HHMMSS": f"{rr}0000",
                "ODB_FEBINPATH": bindir,
                "ODB_CMA": "ECMA",
                "NBPOOL": str(self.nbpool),
                "BATOR_NBPOOL": str(self.nbpool),
                "BATOR_WINDOW_LEN": str(self.bator_window_len),
                "BATOR_WINDOW_SHIFT": str(self.bator_window_shift),
                "BATOR_SLOT_LEN": str(self.bator_slot_len),
                "BATOR_CENTER_LEN": str(self.bator_center_len),
                "BATOR_NBSLOT": str(self.bator_nbslot),
                "BATOR_BASE": bindir,
                "BATOR_LAMFLAG": bator_lamflag,
                "IOASSIGN": os.path.join(self.wdir, "IOASSIGN"),
                "SWAPP_ODB_IOASSIGN": os.path.join(self.wdir, "IOASSIGN"),
                "ODB_SRCPATH_ECMA": os.path.join(self.wdir, f"ECMA.{self.obstype}"),
                "ODB_SRCPATH_RSTBIAS": os.path.join(self.wdir, "ECMA"),
                "ODB_DATAPATH_ECMA": os.path.join(self.wdir, f"ECMA.{self.obstype}"),
                "ODB_ECMA_CREATE_POOLMASK": "2",
                "ODB_ECMA_POOLMASK_FILE": os.path.join(
                    self.wdir, f"ECMA.{self.obstype}", "ECMA.poolmask"
                ),
                "DR_HOOK_ASSERT_MPI_INITIALIZED": "0",
            }
        )

        # --- stage obs file(s) from ObsPrep output ---
        local_name = self._stage_obs(obsprep_dir)
        if not local_name:
            logger.info(
                "{}: no obs file for obstype '{}' — skipping run.",
                self._LOG_TAG,
                self.obstype,
            )
            return

        # --- create refdata and batormap ---
        self._write_refdata_and_batormap(yyyy, mm, dd, rr, local_name)

        # --- create ECMA output directory ---
        os.makedirs(f"ECMA.{self.obstype}", exist_ok=True)

        # --- create IOASSIGN file ---
        # create_ioassign internally calls the `ioassign` binary so `.` must be on PATH.
        ioassign_env = dict(rte)
        ioassign_env["PATH"] = "." + os.pathsep + ioassign_env.get("PATH", "")
        result = subprocess.run(
            f"./create_ioassign -l{rte['ODB_CMA']} -n{self.nbpool}",
            shell=True,
            env=ioassign_env,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"create_ioassign failed with return code {result.returncode}"
            )
        ioassign_file = os.path.join(self.wdir, "IOASSIGN")
        if not os.path.isfile(ioassign_file):
            raise RuntimeError(
                f"create_ioassign returned 0 but IOASSIGN file not found at {ioassign_file}"
            )
        logger.info("{}: IOASSIGN created at {}", self._LOG_TAG, ioassign_file)

        # --- run obs-ingestion binary ---
        # The platform wrapper (srun) acts as the MPI launcher on SLURM systems;
        # adding a separate mpirun prefix causes double-MPI-init and DR_HOOK abort.
        BatchJob(rte, wrapper=self.wrapper).run(f"./{self._BINARY_NAME}")

        # --- verify output ---
        ecma_out = f"ECMA.{self.obstype}"
        if not os.path.isdir(ecma_out):
            logger.warning(
                "{} did not produce {} — marking as complete with warning.",
                self._LOG_TAG,
                ecma_out,
            )

        # --- archive output ---
        # Use stream-specific subdirectory so surface (16-pool) and upper-air (128-pool)
        # archives don't overwrite each other when both streams process the same obstype.
        out_dir = os.path.join(
            self.da_scratch, yyyy, mm, dd, rr, "odb", self.family1, self.obstype
        )
        tactusmakedirs(out_dir)
        if os.path.isdir(ecma_out):
            dst = os.path.join(out_dir, ecma_out)
            if os.path.exists(dst):
                subprocess.run(["rm", "-rf", dst], check=True)
            shutil.copytree(ecma_out, dst, symlinks=True, dirs_exist_ok=True)
            logger.info("{}: archived {} to {}", self._LOG_TAG, ecma_out, out_dir)

    # ------------------------------------------------------------------
    # Private helpers (shared by all subclasses)
    # ------------------------------------------------------------------

    def _stage_obs(self, obsprep_dir):
        """Link the obs file from ObsPrep into the work dir.

        Returns the local filename on success, None if no file is available.
        """
        # New schema: obstypes nested. Old schema: direct keys under provider.
        obstypes = self._provider.get("obstypes") or self._provider
        spec = obstypes.get(self.obstype)
        if not isinstance(spec, Mapping):
            logger.info(
                "{}: no provider entry for obstype '{}' — skipping.",
                self._LOG_TAG,
                self.obstype,
            )
            return None

        fmt = spec.get("format", "")
        local_name = (
            f"{fmt}.{self.obstype}" if fmt else spec.get("local_name", self.obstype)
        )
        src = os.path.join(obsprep_dir, local_name)
        if not os.path.isfile(src):
            logger.info(
                "{}: no obs file '{}' in obsprep dir for obstype '{}' — skipping.",
                self._LOG_TAG,
                local_name,
                self.obstype,
            )
            return None

        if not os.path.lexists(local_name):
            if local_name.startswith("OBSOUL."):
                self._copy_obsoul_fixed_header(src, local_name)
            else:
                os.symlink(src, local_name)
        logger.debug("{}: staged {} -> {}", self._LOG_TAG, src, local_name)
        return local_name

    def _copy_obsoul_fixed_header(self, src, local_name):
        """Copy OBSOUL file rewriting the header time to 6-digit HHMMSS format.

        Some providers write only a 2-digit HH (e.g. "    20250209          00")
        which causes the ingestion binary to abort with "OBsoul incorrect".
        """
        hhmmss = self.basetime.strftime("%H") + "0000"
        yyyymmdd = self.basetime.strftime("%Y%m%d")
        correct_header = f"    {yyyymmdd}\t{hhmmss}\n"
        with open(src) as fin, open(local_name, "w") as fout:
            fin.readline()  # discard original header
            fout.write(correct_header)
            for line in fin:
                fout.write(line)
        logger.debug(
            "{}: copied {} with fixed OBSOUL header", self._LOG_TAG, local_name
        )

    def _write_refdata_and_batormap(self, yyyy, mm, dd, rr, local_name):
        """Write refdata and batormap files.

        Format is derived from the local_name prefix (e.g. "OBSOUL.synop" → "OBSOUL").
        """
        fmt = local_name.split(".")[0].upper() if "." in local_name else ""
        if not fmt:
            logger.warning(
                "{}: cannot derive format from local_name '{}' — skipping refdata/batormap",
                self._LOG_TAG,
                local_name,
            )
            return

        bator_name = self.obstype
        with open("refdata", "w") as fh:
            fh.write(
                f"{self.obstype:<8} {fmt:<8} {bator_name:<16} {yyyy}{mm}{dd} {rr}\n"
            )
        with open("batormap", "w") as fh:
            fh.write(f"{self.obstype:<8} {self.obstype:<8} {fmt:<8} {bator_name}\n")

    def _write_gpssol_list(self):
        """Write list_gpssol from da.gpssol_stations config (may be empty)."""
        stations = self.config.get("da.gpssol_stations", [])
        with open("list_gpssol", "w") as fh:
            for s in stations:
                fh.write(f"{s}\n")

    def _write_nam_lamflag(self):
        """Generate NAM_lamflag from domain config parameters.

        Computes SW corner via LCC projection so the ingestion binary can
        spatially filter observations to the model domain.
        """
        nlon = self.config["domain.nimax"]
        nlat = self.config["domain.njmax"]
        latc = float(self.config["domain.xlatcen"])
        lonc = float(self.config["domain.xloncen"])
        xdx = float(self.config["domain.xdx"])
        xdy = float(self.config["domain.xdy"])

        lat0_raw = self.config.get("domain.xlat0", "")
        lon0_raw = self.config.get("domain.xlon0", "")
        lat0 = float(lat0_raw) if lat0_raw else latc
        lon0 = float(lon0_raw) if lon0_raw else lonc

        redzone = float(self.config.get("da.bator_redzone", 50.0))
        canzone = float(self.config.get("da.bator_canzone", 1500.0))

        proj = pyproj.CRS.from_string(
            f"+proj=lcc +lat_0={lat0} +lon_0={lon0} "
            f"+lat_1={lat0} +lat_2={lat0} +units=m +no_defs +R=6371000.0"
        )
        wgs84 = pyproj.CRS.from_string("EPSG:4326")
        to_proj = pyproj.Transformer.from_crs(wgs84, proj, always_xy=True)
        to_geo = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True)

        xc, yc = to_proj.transform(lonc, latc)
        x_sw = xc - 0.5 * (nlon - 1) * xdx
        y_sw = yc - 0.5 * (nlat - 1) * xdy
        lon1, lat1 = to_geo.transform(x_sw, y_sw)

        obs_flags = [
            ("LAIREP", True),
            ("LDRIBU", True),
            ("LPAOB", False),
            ("LPILOT", True),
            ("LRADAR", True),
            ("LSATEM", True),
            ("LSATOB", True),
            ("LSCATT", True),
            ("LSLIMB", True),
            ("LSYNOP", True),
            ("LTEMP", True),
        ]

        def _tf(v):
            return ".T." if v else ".F."

        with open("NAM_lamflag", "w") as fh:
            fh.write("&NAMFCNT\n")
            fh.write("  LOBSONLY=.F.,\n")
            fh.write("/\n")
            fh.write("&NAMFGEOM\n")
            fh.write(f"  EFDELX={xdx:.11G},\n")
            fh.write(f"  EFDELY={xdy:.11G},\n")
            fh.write(f"  EFLAT0={lat0:.13G},\n")
            fh.write(f"  EFLAT1={lat1:.13G},\n")
            fh.write(f"  EFLATC={latc:.13G},\n")
            fh.write(f"  EFLON0={lon0:.13G},\n")
            fh.write(f"  EFLON1={lon1:.13G},\n")
            fh.write(f"  EFLONC={lonc:.13G},\n")
            fh.write("  LNEWGEOM=.T.,\n")
            fh.write("  LVAR=.T.,\n")
            fh.write("  NFDGUN=1,\n")
            fh.write(f"  NFDGUX={nlat},\n")
            fh.write("  NFDLUN=1,\n")
            fh.write(f"  NFDLUX={nlon},\n")
            fh.write(f"  REDZONE={redzone:.7G},\n")
            fh.write(f"  Z_CANZONE={canzone:.7G},\n")
            fh.write("/\n")
            fh.write("&NAMFOBS\n")
            for flag, val in obs_flags:
                fh.write(f"  {flag}={_tf(val)},\n")
            fh.write("/\n")
        logger.debug(
            "{}: wrote NAM_lamflag for domain {} ({}x{} @ {}m, SW={:.4f},{:.4f})",
            self._LOG_TAG,
            self.domain,
            nlon,
            nlat,
            xdx,
            lat1,
            lon1,
        )
