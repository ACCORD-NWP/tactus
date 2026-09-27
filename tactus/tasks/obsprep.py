"""Observation preparation task.

Observation provider selection
-----------------------------
``da.obs_provider`` names the active provider convention.  All provider
definitions live in configuration under ``da.providers.<name>``::

e.g. obs_provider = "LACE"

Multi-file merging
------------------
All candidates found in the archive are merged into a single file per obs
type.

- **BUFR / GRIB**: files are concatenated byte-for-byte.
- **OBSOUL**: files are merged via ``obsoul_merge.pl`` (configured via
  ``da.obsoul_merge_script``).
- **NETCDF**: only the first file is used; a warning is issued when more
  than one is found.
- **HDF**: No merging performed - radar files should be linked as site${i}

Temporal windowing
------------------
``obs_step`` in the active provider block (minutes, default 0 = disabled)
activates collection across multiple slots within the assimilation window.
When enabled, ObsPrep computes the slots covered by
``[basetime + window_shift, basetime + window_shift + window_len]`` and
searches each slot's date directory.

Example: basetime 00 UTC, window_shift=-90 min, window_len=180 min →
slots 23, 00, 01 should be searched (three different hours, possibly across
two calendar dates).
"""

import datetime as dt
import os
import shutil
import subprocess
import tempfile
from collections.abc import Mapping

from ..datetime_utils import as_datetime
from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task


class ObsPrep(Task):
    """Observation-preparation task.

    Supported families as defined by da_stream:
    - ``surface`` stages only surface (SYNOP) observations for CANARI.
    - ``3dvar``   stages all configured obs types for 3D-Var.
    """

    DEFAULT_OBS_SURFACE = ["synop"]

    DEFAULT_OBS_3DVAR = [
        "synop",
        "gpssol",
        "amdar",
        "geowind",
        "hrwind",
        "temp",
        "wp",
        "seviri",
        "amsua",
        "amsub",
        "iasi",
        "ascat",
        "radar",
    ]

    def __init__(self, config):
        """Construct ObsPrep task.

        Args:
            config (tactus.ParsedConfig): Experiment configuration.
        """
        Task.__init__(self, config, __class__.__name__)
        self.basetime = as_datetime(config["general.times.basetime"])
        self.obs_dir = self.platform.substitute(config["da.obs_dir"])
        self.da_scratch = self.platform.substitute(config["da.scratch"])
        self.family = config.get("task.args.da_stream,", "surface")

        self.obs_types = (
            config.get("da.obs_types_surface", self.DEFAULT_OBS_SURFACE)
            if self.family == "surface"
            else config.get("da.obs_types_3dvar", self.DEFAULT_OBS_3DVAR)
        )
        logger.info("da_stream:{}", self.family)

        self.obs_provider = config.get("da.obs_provider", "None.")
        all_providers = config.get("da.providers", {})

        if self.obs_provider not in all_providers:
            logger.warning(
                "ObsPrep: obs_provider '{}' not defined in da.providers — "
                "no obs sources will be found; add a [da.providers.{}] block.",
                self.obs_provider,
                self.obs_provider,
            )
            self._provider = {}
        else:
            self._provider = all_providers[self.obs_provider]
        # New schema: obstypes nested under provider. Old schema: direct keys.
        self._obstypes = self._provider.get("obstypes") or self._provider

        self.bator_window_len = config.get("da.bator_window_len", 180)
        self.bator_window_shift = config.get("da.bator_window_shift", -90)
        # obs_step lives in the provider block; fall back to top-level da.obs_step
        # for backward compatibility, then to 0 (windowing disabled).
        self.obs_step = self._provider.get("obs_step", config.get("da.obs_step", 0))

        self.obsoul_merge_script = self.platform.substitute(
            config.get("da.obsoul_merge_script", "")
        )

        logger.debug(
            "Constructed ObsPrep for family={} obs_provider={} obs_step={}min",
            self.family,
            self.obs_provider,
            self.obs_step,
        )

    def execute(self):
        """Stage observation files and write obstypes availability list.

        For each obs type, collects all matching files from all archive slots
        within the assimilation window, merges them, and copies the result
        into the working directory. Writes ``obstypes_YYYYMMDDRR`` with the
        list of successfully staged types — used by the Bator tasks.
        """
        yyyy = self.basetime.strftime("%Y")
        mm = self.basetime.strftime("%m")
        dd = self.basetime.strftime("%d")
        rr = self.basetime.strftime("%H")
        ymdrr = f"{yyyy}{mm}{dd}{rr}"

        available_types = []

        for obstype in self.obs_types:
            staged = self._stage_obstype(obstype, ymdrr)
            if staged:
                available_types.append(obstype)
            else:
                logger.warning(
                    "ObsPrep: obs type '{}' not available for {}", obstype, ymdrr
                )

        if not available_types:
            raise RuntimeError(
                f"ObsPrep: no observation types were available for {ymdrr}. "
                "Cannot proceed with assimilation."
            )

        obstypes_file = f"obstypes_{ymdrr}"
        with open(obstypes_file, "w") as fh:
            fh.write("\n".join(available_types) + "\n")
        logger.info(
            "ObsPrep: available obs types for {}: {} in {}",
            ymdrr,
            available_types,
            self.obs_dir,
        )

        out_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, "obsprep")
        tactusmakedirs(out_dir)
        for f in os.listdir("."):
            src = os.path.join(self.wdir, f)
            dst = os.path.join(out_dir, f)
            if os.path.isfile(src):
                shutil.copy2(src, dst)
        logger.info("ObsPrep: staged files archived to {}", out_dir)

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _window_slots(self, obs_step=None):
        """Return list of datetimes covering the assimilation window.

        When ``obs_step`` is 0 only the basetime itself is returned.
        Otherwise all slot boundaries within the window are returned,
        floored to the nearest ``obs_step`` boundary.

        ``obs_step`` defaults to the provider-level value when not given.
        """
        if obs_step is None:
            obs_step = self.obs_step
        if obs_step <= 0:
            return [self.basetime]

        import calendar

        step_sec = obs_step * 60
        window_start = self.basetime + dt.timedelta(minutes=self.bator_window_shift)
        window_end = self.basetime + dt.timedelta(
            minutes=self.bator_window_shift + self.bator_window_len
        )

        t_epoch = int(calendar.timegm(window_start.timetuple()) // step_sec) * step_sec
        end_epoch = calendar.timegm(window_end.timetuple())

        slots = []
        while t_epoch <= end_epoch:
            slots.append(dt.datetime.utcfromtimestamp(t_epoch))
            t_epoch += step_sec
        return slots

    # Maps tactus obstype names to the OBSOUL type code embedded in temp
    # filenames so that obsoul_merge.pl (which splits on '_' and reads field [1])
    # accepts records of the correct type.  Numeric strings work because
    # obsoul_merge.pl uses numeric != for the per-record type check.
    # Using the numeric code rather than a source-specific name (e.g. "amdar")
    # lets multiple aircraft sub-types (AMDAR, MODES, EHS …) all be accepted.
    _OBSOUL_MERGE_NAMES = {
        "amdar": "2",  # OBSOUL type 2 = aircraft
        "synop": "1",  # OBSOUL type 1 = synop/ship
    }

    def _stage_obstype(self, obstype, ymdrr):
        """Collect and merge all obs files for *obstype* across window slots.

        Returns True when at least one file was found and merged, False
        otherwise.
        """
        spec = self._obstypes.get(obstype)
        if not isinstance(spec, Mapping):
            return False

        candidates = spec.get("candidates", [])
        fmt = spec.get("format", "")
        local_name = f"{fmt}.{obstype}" if fmt else spec.get("local_name", obstype)
        if not candidates:
            return False

        # Per-obstype obs_step overrides the provider-level default.
        # Set obs_step = 0 in the provider spec for geostationary obs
        # (seviri, geowind, hrwind) to collect only the nominal basetime slot.
        obs_step = spec.get("obs_step", self.obs_step)

        collected = []
        for slot in self._window_slots(obs_step):
            syyyy = slot.strftime("%Y")
            smm = slot.strftime("%m")
            sdd = slot.strftime("%d")
            srr = slot.strftime("%H")
            slot_ymdrr = f"{syyyy}{smm}{sdd}{srr}"
            subst = {
                "ymdrr": slot_ymdrr,
                "yyyy": syyyy,
                "mm": smm,
                "dd": sdd,
                "rr": srr,
            }
            slot_date_dir = os.path.join(self.obs_dir, syyyy, smm, sdd)
            src_dir = self.platform.substitute(spec.get("source_dir", slot_date_dir))
            for cand_tpl in candidates:
                fname = cand_tpl.format(**subst)
                path, is_tmp = self._collect_file(os.path.join(src_dir, fname), obstype)
                if path is not None:
                    collected.append((path, is_tmp))
                    logger.debug(
                        "ObsPrep: found {} for type {} slot {}",
                        fname,
                        obstype,
                        slot_ymdrr,
                    )

        if not collected:
            return False

        paths = [p for p, _ in collected]
        try:
            self._merge_files(paths, local_name, obstype)
        finally:
            for path, is_tmp in collected:
                if is_tmp:
                    try:
                        os.unlink(path)
                    except OSError:
                        pass

        return True

    def _collect_file(self, src, obstype=None):
        """Return (path, is_temp) for *src* or *src*.gz.

        Decompresses gz files into a temporary file so the caller always gets
        a plain path.  Returns (None, False) when the source does not exist
        or is empty.

        When *obstype* has a known mapping in ``_OBSOUL_MERGE_NAMES`` the temp
        file is given a prefix of the form ``obsoul_<type>_`` so that
        obsoul_merge.pl can derive the correct OBS type from the filename.
        """
        if os.path.isfile(src) and os.path.getsize(src) > 0:
            return src, False

        src_gz = src + ".gz"
        if os.path.isfile(src_gz) and os.path.getsize(src_gz) > 0:
            import gzip

            merge_name = self._OBSOUL_MERGE_NAMES.get(obstype) if obstype else None
            prefix = f"obsoul_{merge_name}_" if merge_name else None
            tmp = tempfile.NamedTemporaryFile(
                delete=False,
                suffix=".obsprep.tmp",
                **({"prefix": prefix} if prefix else {}),
                dir=".",
            )
            with gzip.open(src_gz, "rb") as f_gz:
                shutil.copyfileobj(f_gz, tmp)
            tmp.close()
            return tmp.name, True

        return None, False

    def _merge_files(self, paths, local_name, obstype):
        """Merge *paths* into *local_name* using the format-appropriate method.

        Format is derived from the ``local_name`` prefix (BUFR, OBSOUL,
        NETCDF, GRIB), which is constructed as ``<format>.<obstype>``.
        Single-file cases bypass merge logic entirely.
        """
        if len(paths) == 1:
            shutil.copy2(paths[0], local_name)
            logger.debug("ObsPrep: staged {} -> {}", paths[0], local_name)
            return

        fmt = local_name.split(".")[0].upper() if "." in local_name else ""

        if fmt in ("BUFR", "GRIB"):
            with open(local_name, "wb") as out:
                for p in paths:
                    with open(p, "rb") as inp:
                        shutil.copyfileobj(inp, out)
            logger.info(
                "ObsPrep: merged {} {} files -> {}", len(paths), fmt, local_name
            )

        elif fmt == "OBSOUL":
            merge_ok = self.obsoul_merge_script and os.path.isfile(
                self.obsoul_merge_script
            )
            if merge_ok:
                list_tmp = tempfile.NamedTemporaryFile(
                    mode="w", suffix=".list", delete=False, dir="."
                )
                try:
                    list_tmp.write("\n".join(paths) + "\n")
                    list_tmp.close()
                    subprocess.run(
                        [
                            "perl",
                            self.obsoul_merge_script,
                            "-o",
                            local_name,
                            "-f",
                            list_tmp.name,
                        ],
                        check=True,
                    )
                finally:
                    try:
                        os.unlink(list_tmp.name)
                    except OSError:
                        pass
                logger.info(
                    "ObsPrep: merged {} OBSOUL files -> {} via obsoul_merge.pl",
                    len(paths),
                    local_name,
                )
            else:
                raise RuntimeError(
                    f"ObsPrep: obsoul_merge.pl not found "
                    f"(da.obsoul_merge_script={self.obsoul_merge_script}). "
                    f"Cannot proceed with assimilation."
                )

        else:
            shutil.copy2(paths[0], local_name)
            if len(paths) > 1:
                logger.warning(
                    "ObsPrep: cannot merge {} files of format '{}' for type '{}'; "
                    "using first file only.",
                    len(paths),
                    fmt,
                    obstype,
                )
