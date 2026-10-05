"""ODB merge task — merges per-obstype ECMA subbases into a single ECMA database.

Uses the SHUFFLE binary and merge_ioassign.

"""

import os
import shlex
import shutil

from ..datetime_utils import as_datetime, split_date
from ..logs import logger
from ..obs_utils import Observations
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
        self.da_scratch = config["da.scratch"]

        self.obs = Observations(config)
        logger.debug(
            "Constructed OdbMerge task for family={} obs_types={}",
            self.obs.family,
            sorted(self.obs.obs_types),
        )

    def execute(self):
        """Merge BATOR subbases and archive merged ECMA ODB."""
        (yyyy, mm, dd, hh) = split_date(self.basetime)
        bator_base_dir = self.platform.substitute(
            os.path.join(self.da_scratch, self.obs.family, "odb")
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
                if obstype not in self.obs.obs_types:
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
        rte = {
            "TO_ODB_ECMWF": "0",
            "TO_ODB_SWAPOUT": "0",
            "ODB_DEBUG": "0",
            "ODB_CTX_DEBUG": "0",
            "ODB_REPRODUCIBLE_SEQNO": odb_reprod_seqno,
            "ODB_STATIC_LINKING": "1",
            "ODB_IO_METHOD": "1",
            "ODB_IO_FILESIZE": "128",
            "ODB_IO_GRPSIZE": str(self.obs.nbpool),
            "EC_PROFILE_HEAP": "0",
            "F_RECLUNIT": "BYTE",
            "F_UFMTENDIAN": "big",
            "ODB_ANALYSIS_DATE": f"{yyyy}{mm}{dd}",
            "ODB_ANALYSIS_TIME": f"{hh}0000",
            "TIME_INIT_YYYYMMDD": f"{yyyy}{mm}{dd}",
            "TIME_INIT_HHMMSS": f"{hh}0000",
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
            "ODB_ECMA_POOLMASK_FILE": os.path.join(self.wdir, "ECMA", "ECMA.poolmask"),
            "IOASSIGN": os.path.join(self.wdir, "ECMA", "IOASSIGN"),
            "DR_HOOK_ASSERT_MPI_INITIALIZED": "0",
        }
        rte.update(dict(os.environ))

        # --- run merge_ioassign then shuffle ---
        # merge_ioassign: combines IOASSIGN files from all subbases
        liste = " ".join(f"-t {b}" for b in bases_to_merge)
        merge_cmd = f"./merge_ioassign -d {self.wdir} {liste}"
        BatchJob(rte, wrapper="").run(merge_cmd)  # serial, no MPI

        # ficdate: window around basetime using config window parameters
        datemin = (self.basetime + self.obs.bator_window_shift).strftime("%Y%m%d%H%M%S")
        datemax = (
            self.basetime + self.obs.bator_window_shift + self.obs.bator_window_len
        ).strftime("%Y%m%d%H%M%S")
        with open("ficdate", "w") as fh:
            fh.write(f"{datemin}\n{datemax}\n")

        # -b1: BATOR always runs as a single process regardless of NPROC.
        # NPROC (= da.da_stream.nbpool for upper air) determines how many shuffle MPI
        # tasks srun launches, which sets the number of output pool files.
        shuffle_bin_cmd = f"./shuffle -iECMA -oECMA -b1 -a{self.obs.nbpool}"
        with open("env_dump.sh", "w") as fh:
            for key, value in rte.items():
                fh.write(f"export {key}={shlex.quote(value)}\n")
        BatchJob(rte, wrapper=self.platform.substitute(self.wrapper)).run(shuffle_bin_cmd)

        if not os.path.isdir("ECMA"):
            raise RuntimeError("OdbMerge: shuffle did not produce an ECMA database.")

        # --- archive merged ECMA + subbases to DA scratch ---
        # ECMA.iomap references ../ECMA.{obstype}/ relative to ECMA/, so
        # subbases must be archived alongside ECMA as siblings.
        archive_subdir = f"{self.obs.family}/odbmerge"
        out_dir = self.platform.substitute(os.path.join(self.da_scratch, archive_subdir))
        tactusmakedirs(out_dir)
        for src_name in ["ECMA"] + [f"ECMA.{b}" for b in bases_to_merge]:
            dst = os.path.join(out_dir, src_name)
            if os.path.exists(dst):
                shutil.rmtree(dst)
            if os.path.isdir(src_name):
                shutil.copytree(src_name, dst, symlinks=True)
        logger.info("OdbMerge: merged ECMA archived to {}", out_dir)
