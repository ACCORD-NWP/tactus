"""ODB merge task — merges per-obstype ECMA subbases into a single ECMA database.
Uses the SHUFFLE binary and merge_ioassign.

"""

import datetime
import os
import shlex
import shutil

from ..datetime_utils import as_datetime
from ..logs import logger
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class OdbMerge(Task):
    """Merge all successfully completed BATOR subbases into one ECMA ODB.

    The merged ECMA database is archived to the DA scratch directory so that
    downstream tasks (surface or upper-air analysis) can access it.
    """

    def __init__(self, config):
        """Construct OdbMerge task.

        Args:
            config (tactus.ParsedConfig): Experiment configuration.
        """
        Task.__init__(self, config, __class__.__name__)
        self.basetime = as_datetime(config["general.times.basetime"])
        self.da_scratch = self.platform.substitute(config["da.scratch"])
        # family1 determines output archive subdirectory name
        self.family1 = config.get("task.args.da_stream", "3dvar")
        self.nbpool = (
            config.get("da.nbpool", 16)
            if self.family1 == "surface"
            else config.get("da.oops.nbpool", 128)
        )
        self.bator_window_len = config.get("da.bator_window_len", 180)
        self.bator_window_shift = config.get("da.bator_window_shift", -90)
        # Only merge subbases that belong to this stream's obs type list.
        if self.family1 == "surface":
            self.obs_types = set(
                config.get("da.obs_types_surface", ["synop", "synop_1"])
            )
        else:
            self.obs_types = set(
                config.get(
                    "da.obs_types_3dvar",
                    [
                        "synop",
                        "synop_1",
                        "gpssol",
                        "amdr",
                        "geowind",
                        "hrwind",
                        "temp",
                        "wp",
                        "seviri",
                        "amsua",
                        "amsub",
                        "iasi",
                        "ascat",
                    ],
                )
            )
        logger.debug(
            "Constructed OdbMerge task for family={} obs_types={}",
            self.family1,
            sorted(self.obs_types),
        )

    def execute(self):
        """Merge BATOR subbases and archive merged ECMA ODB."""
        yyyy = self.basetime.strftime("%Y")
        mm = self.basetime.strftime("%m")
        dd = self.basetime.strftime("%d")
        rr = self.basetime.strftime("%H")
        bator_base_dir = os.path.join(
            self.da_scratch, yyyy, mm, dd, rr, "odb", self.family1
        )

        # --- locate binaries ---
        shuffle_bin = self.get_binary("shuffle")
        if not os.access(shuffle_bin, os.X_OK):
            shuffle_bin = self.get_binary("shuffle.x")
        bin_dir = os.path.dirname(shuffle_bin)
        for binary in ["shuffle", "ioassign", "merge_ioassign", "create_ioassign"]:
            src = os.path.join(bin_dir, binary)
            if not os.access(src, os.X_OK):
                src = os.path.join(bin_dir, binary + ".x")
            if os.access(src, os.X_OK) and not os.path.lexists(binary):
                os.symlink(src, binary)

        # --- collect available BATOR subbases for this stream ---
        bases_to_merge = []
        if os.path.isdir(bator_base_dir):
            for obstype in sorted(os.listdir(bator_base_dir)):
                if obstype not in self.obs_types:
                    continue
                ecma_src = os.path.join(bator_base_dir, obstype, f"ECMA.{obstype}")
                if os.path.isdir(ecma_src):
                    # Skip subbases with no observations inside the domain —
                    # ECMA.dd is absent when BATOR produced an empty ODB.
                    if not os.path.isfile(os.path.join(ecma_src, "ECMA.dd")):
                        logger.info(
                            "OdbMerge: skipping empty subbase {} (no ECMA.dd)", obstype
                        )
                        continue
                    if not os.path.isdir(f"ECMA.{obstype}"):
                        shutil.copytree(ecma_src, f"ECMA.{obstype}")
                    bases_to_merge.append(obstype)

        if not bases_to_merge:
            raise RuntimeError(
                f"OdbMerge: no BATOR subbases found under {bator_base_dir}"
            )

        with open("bases2merge", "w") as fh:
            fh.write("\n".join(bases_to_merge) + "\n")
        logger.info("OdbMerge: merging subbases: {}", bases_to_merge)

        # --- ODB environment ---
        odb_reprod_seqno = "2"
        rte = dict(os.environ)
        rte.update(
            {
                "TO_ODB_ECMWF": "0",
                "TO_ODB_SWAPOUT": "0",
                "ODB_DEBUG": "0",
                "ODB_CTX_DEBUG": "0",
                "ODB_REPRODUCIBLE_SEQNO": odb_reprod_seqno,
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
                "ODB_FEBINPATH": bin_dir,
                "ODB_CMA": "ECMA",
                "BATOR_NBSLOT": "1",
                # merge_ioassign writes the merged IOASSIGN into wdir/ECMA/,
                # so ODB_SRCPATH_ECMA must point there (not to wdir).
                "SWAPP_ODB_IOASSIGN": os.path.join(self.wdir, "ECMA", "IOASSIGN"),
                "ODB_SRCPATH_ECMA": os.path.join(self.wdir, "ECMA"),
                "ODB_DATAPATH_ECMA": os.path.join(self.wdir, "ECMA"),
                "ODB_SRCPATH_RSTBIAS": os.path.join(self.wdir, "ECMA"),
                "ODB_ECMA_CREATE_POOLMASK": "1",
                "ODB_ECMA_POOLMASK_FILE": os.path.join(
                    self.wdir, "ECMA", "ECMA.poolmask"
                ),
                "IOASSIGN": os.path.join(self.wdir, "ECMA", "IOASSIGN"),
                "DR_HOOK_ASSERT_MPI_INITIALIZED": "0",
            }
        )

        # --- run merge_ioassign then shuffle ---
        # merge_ioassign: combines IOASSIGN files from all subbases
        liste = " ".join(f"-t {b}" for b in bases_to_merge)
        merge_cmd = f"./merge_ioassign -d {self.wdir} {liste}"
        BatchJob(rte, wrapper="").run(merge_cmd)  # serial, no MPI

        # ficdate: window around basetime using config window parameters
        na = self.nbpool
        bt = self.basetime
        datemin = (bt + datetime.timedelta(minutes=self.bator_window_shift)).strftime(
            "%Y%m%d%H%M"
        ) + "00"
        datemax = (
            bt
            + datetime.timedelta(
                minutes=self.bator_window_shift + self.bator_window_len
            )
        ).strftime("%Y%m%d%H%M") + "00"
        with open("ficdate", "w") as fh:
            fh.write(f"{datemin}\n{datemax}\n")

        # -b1: BATOR always runs as a single process regardless of NPROC.
        # NPROC (= da.oops.nbpool for 3dvar) determines how many shuffle MPI
        # tasks srun launches, which sets the number of output pool files.
        shuffle_bin_cmd = f"./shuffle -iECMA -oECMA -b1 -a{na}"
        with open("env_dump.sh", "w") as fh:
            for key, value in rte.items():
                fh.write(f"export {key}={shlex.quote(value)}\n")
        BatchJob(rte, wrapper=self.platform.substitute(self.wrapper)).run(
            shuffle_bin_cmd
        )

        if not os.path.isdir("ECMA"):
            raise RuntimeError("OdbMerge: shuffle did not produce an ECMA database.")

        # --- archive merged ECMA + subbases to DA scratch ---
        # ECMA.iomap references ../ECMA.{obstype}/ relative to ECMA/, so
        # subbases must be archived alongside ECMA as siblings.
        archive_subdir = f"odbmerge_{self.family1}"
        out_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, archive_subdir)
        tactusmakedirs(out_dir)
        for src_name in ["ECMA"] + [f"ECMA.{b}" for b in bases_to_merge]:
            dst = os.path.join(out_dir, src_name)
            if os.path.exists(dst):
                shutil.rmtree(dst)
            if os.path.isdir(src_name):
                shutil.copytree(src_name, dst, symlinks=True)
        logger.info("OdbMerge: merged ECMA archived to {}", out_dir)
