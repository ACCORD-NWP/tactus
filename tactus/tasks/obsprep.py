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

import contextlib
import os
import shutil
import subprocess
import tempfile
from collections.abc import Mapping

from ..config_parser import GeneralConstants
from ..datetime_utils import as_datetime, as_timedelta, split_date
from ..logs import logger
from ..obs_utils import Observations
from ..os_utils import tactusmakedirs
from .base import Task


class ObsPrep(Task):
    """Observation-preparation task.

    Supported families as defined by da_stream:
    - ``surface`` stages only surface (SYNOP) observations for surface assimilation
    - ``upper_air``   stages all configured obs types for upper_air
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
        self.obs_dir = config["platform.obs_dir"]
        self.da_scratch = self.config["da.scratch"]
        self.obs = Observations(config)

        self.obsoul_merge_script = self.platform.substitute(
            config.get(
                "da.obsoul_merge_script",
                GeneralConstants.PACKAGE_DIRECTORY / "aux" / "obsoul_merge.pl",
            )
        )

        logger.debug(
            "Constructed ObsPrep for family={} obs_provider={} obs_step={}min",
            self.obs.family,
            self.obs.obs_provider,
            self.obs.obs_step,
        )

    def execute(self):
        """Stage observation files and write obstypes availability list.

        For each obs type, collects all matching files from all archive slots
        within the assimilation window, merges them, and copies the result
        into the working directory. Writes ``obstypes_YYYYMMDDRR`` with the
        list of successfully staged types — used by the Bator tasks.
        """
        ymdhh = self.basetime.strftime("%Y%m%d%H")

        available_types = []

        for obstype in self.obs.obs_types:
            staged = self._stage_obstype(obstype)
            if staged:
                available_types.append(obstype)
            else:
                logger.warning(
                    "ObsPrep: obs type '{}' not available for {}", obstype, ymdhh
                )

        if not available_types:
            raise RuntimeError(
                f"ObsPrep: no observation types were available for {ymdhh}"
                f"\nin {self.platform.substitute(self.obs_dir)}."
                "\nCannot proceed with assimilation."
            )

        obstypes_file = f"obstypes_{ymdhh}"
        with open(obstypes_file, "w") as fh:
            fh.write("\n".join(available_types) + "\n")
        out_dir = os.path.join(
            self.platform.substitute(self.da_scratch), f"{self.obs.family}/obsprep"
        )
        tactusmakedirs(out_dir)
        logger.info(
            "ObsPrep: available obs types for {}: {} in {}",
            ymdhh,
            available_types,
            out_dir,
        )

        for f in os.listdir("."):
            if f not in ["config.toml"] and os.path.isfile(f):
                self.fmanager.output(f, out_dir, provider_id="copy")

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------
    def _window_slots(self, obs_step=None):
        """Return list of datetimes covering the assimilation window.

        When ``obs_step`` is PT0H only the basetime itself is returned.
        Otherwise all slot boundaries within the window are returned,
        floored to the nearest ``obs_step`` boundary.

        ``obs_step`` defaults to the provider-level value when not given.
        """
        if obs_step is None:
            obs_step = self.obs.obs_step
        if obs_step <= as_timedelta("PT0H"):
            return [self.basetime]

        window_start = (
            self.basetime
            + self.obs.bator_window_shift
            - self.obs.bator_window_shift % obs_step
        )
        window_end = (
            self.basetime + self.obs.bator_window_shift + self.obs.bator_window_len
        )

        slots = []
        while window_start <= window_end:
            slots.append(window_start)
            window_start += obs_step

        return slots

    # Maps tactus obstype names to the OBSOUL type code embedded in temp
    # filenames so that obsoul_merge.pl (which splits on '_' and reads field [1])
    # accepts records of the cohhect type.  Numeric strings work because
    # obsoul_merge.pl uses numeric != for the per-record type check.
    # Using the numeric code rather than a source-specific name (e.g. "amdar")
    # lets multiple aircraft sub-types (AMDAR, MODES, EHS …) all be accepted.
    _OBSOUL_MERGE_NAMES = {
        "amdar": "2",  # OBSOUL type 2 = aircraft
        "synop": "1",  # OBSOUL type 1 = synop/ship
    }

    def _stage_obstype(self, obstype):
        """Collect and merge all obs files for *obstype* across window slots.

        Returns True when at least one file was found and merged, False
        otherwise.
        """
        spec = self.obs.obstypes.get(obstype)
        if not isinstance(spec, Mapping):
            return False

        candidates = spec.get("candidates", [])
        fmt = spec.get("format", "")
        local_name = f"{fmt}.{obstype}" if fmt else spec.get("local_name", obstype)
        if not candidates:
            return False

        # Per-obstype obs_step ovehhides the provider-level default.
        # Set obs_step = 0 in the provider spec for geostationary obs
        # (seviri, geowind, hrwind) to collect only the nominal basetime slot.
        try:
            obs_step = spec["obs_step"]
            obs_step = as_timedelta(obs_step)
        except KeyError:
            obs_step = self.obs.obs_step

        collected = []
        for slot in self._window_slots(obs_step):
            (syyyy, smm, sdd, shh) = split_date(slot)
            slot_ymdhh = f"{syyyy}{smm}{sdd}{shh}"
            subst = {
                "ymdhh": slot_ymdhh,
                "yyyy": syyyy,
                "mm": smm,
                "dd": sdd,
                "hh": shh,
            }
            src_dir = self.platform.substitute(
                spec.get("source_dir", self.obs_dir),
                basetime=slot,
            )
            for cand_tpl in candidates:
                fname = cand_tpl.format(**subst)
                path, is_tmp = self._collect_file(os.path.join(src_dir, fname), obstype)
                if path is not None:
                    collected.append((path, is_tmp))
                    logger.debug(
                        "ObsPrep: found {} for type {} slot {}",
                        fname,
                        obstype,
                        slot_ymdhh,
                    )

        if not collected:
            return False

        paths = [p for p, _ in collected]
        try:
            self._merge_files(paths, local_name, obstype)
        finally:
            for path, is_tmp in collected:
                if is_tmp:
                    with contextlib.suppress(OSError):
                        os.unlink(path)

        return True

    def _collect_file(self, src, obstype=None):
        """Return (path, is_temp) for *src* or *src*.gz.

        Decompresses gz files into a temporary file so the caller always gets
        a plain path.  Returns (None, False) when the source does not exist
        or is empty.

        When *obstype* has a known mapping in ``_OBSOUL_MERGE_NAMES`` and the
        basename does not already start with ``obsoul_<type>_``, that prefix is
        prepended so that obsoul_merge.pl can derive the correct OBS type from
        the filename.
        """
        if os.path.isfile(src) and os.path.getsize(src) > 0:
            return src, False

        src_gz = src + ".gz"
        if os.path.isfile(src_gz) and os.path.getsize(src_gz) > 0:
            import gzip

            base = os.path.basename(src)
            merge_name = self._OBSOUL_MERGE_NAMES.get(obstype) if obstype else None
            if merge_name and not base.startswith(f"obsoul_{merge_name}_"):
                base = f"obsoul_{merge_name}_{base}"
            tmp = tempfile.NamedTemporaryFile(
                delete=False,
                prefix=f"{base}.",
                suffix=".obsprep.tmp",
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
            logger.info("ObsPrep: merged {} {} files -> {}", len(paths), fmt, local_name)

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
                    cmd = [
                        "perl",
                        self.obsoul_merge_script,
                        "-o",
                        local_name,
                        "-f",
                        list_tmp.name,
                    ]
                    subprocess.run(cmd, check=True)
                finally:
                    with contextlib.suppress(OSError):
                        os.unlink(list_tmp.name)
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
