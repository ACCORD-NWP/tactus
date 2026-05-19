"""Canari — surface OI analysis using MASTERODB (configuration 701).

Ported from: seemhews/scripts/assim/analysis/surface/canari.ecf
"""
import json
import os
import shutil

from ..config_parser import ConfigPaths
from ..datetime_utils import as_datetime
from ..initial_conditions import InitialConditions
from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import tactusmakedirs
from .base import Task
from .batch import BatchJob


class Canari(Task):
    """Run CANARI surface analysis (OI, MASTERODB conf 701).

    Reads the short-range background (first-guess) field and the merged
    surface ECMA ODB, then produces ``ICMSHCYCL+0000`` — the OI surface
    analysis field — which is archived to the DA scratch directory for
    use by the BlendSur task.
    """

    def __init__(self, config):
        """Construct Canari task.

        Args:
            config (tactus.ParsedConfig): Experiment configuration.
        """
        Task.__init__(self, config, __class__.__name__)
        self.basetime = as_datetime(config["general.times.basetime"])
        self.domain = config["domain.name"]
        self.cnmexp = config["general.cnmexp"]
        self.member_str = config["general.member_str"]
        self.da_scratch = self.platform.substitute(config["da.scratch"])
        # climate files (Const.Clim.MM) live in system.climdir
        self.clim_dir = self.platform.get_system_value("climdir")
        self.nbpool = config.get("da.nbpool", 12)
        self.nlgen = NamelistGenerator(config, "canari")
        logger.debug("Constructed Canari task")

    def execute(self):
        """Run CANARI surface OI analysis."""
        yyyy = self.basetime.strftime("%Y")
        mm = self.basetime.strftime("%m")
        dd = self.basetime.strftime("%d")
        rr = self.basetime.strftime("%H")
        ymdrr = f"{yyyy}{mm}{dd}{rr}"

        # month neighbour for climate interpolation
        day = int(dd)
        m2 = (int(mm) - 1) if day <= 15 else (int(mm) + 1)
        if m2 == 0:
            m2 = 12
        if m2 == 13:
            m2 = 1
        mm2 = f"{m2:02d}"

        odb_archive_dir = os.path.join(
            self.da_scratch, yyyy, mm, dd, rr, "odbmerge_surface"
        )

        # --- binary ---
        masterodb_bin = self.get_binary("MASTERODB")
        if not os.path.lexists("MASTERODB_canari"):
            os.symlink(masterodb_bin, "MASTERODB_canari")

        # --- namelist ---
        # In cold_start mode the atmospheric first-guess is an LBC file that
        # does not contain near-surface diagnostics (T2M, HU2M) required by
        # the SURFEX OI (CANARI_SFX). Disable it so the atmospheric OI still
        # runs and produces ICMSHCYCL+0000.
        mode = self.config.get("suite_control.mode", "cycling")
        self.nlgen.load("canari")
        if mode == "cold_start":
            self.nlgen.update({"NACTEX": {"LAEICS_SX": False}}, "cold_start_sfx_disable")
        nml = self.nlgen.assemble_namelist("canari")
        self.nlgen.write_namelist(nml, "fort.4")

        # --- static input files ---
        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value("assimilation_input_definition")
        )
        with open(input_definition, "r", encoding="utf-8") as f:
            input_data = json.load(f)
        self.fmanager.input_data_iterator(input_data)

        # --- climate files (Const.Clim.MM → ICMSHANALCLIM/ICMSHANALCLI2) ---
        # Names must match CNMEXP set in canari_namelists.yml (currently ANAL).
        for clim_month, clim_link in [
            (mm, "ICMSHANALCLIM"),
            (mm2, "ICMSHANALCLI2"),
        ]:
            clim_src = os.path.join(self.clim_dir, f"Const.Clim.{clim_month}")
            if os.path.isfile(clim_src):
                shutil.copy2(clim_src, clim_link)
            else:
                logger.warning("Canari: climate file not found: {}", clim_src)

        # --- Const.Clim.sfx (mid-month PGD sfx for Surfex, named Const.ClimMM15.sfx) ---
        pgd_sfx_src = os.path.join(self.clim_dir, f"Const.Clim{mm}15.sfx")
        if os.path.isfile(pgd_sfx_src):
            shutil.copy2(pgd_sfx_src, "Const.Clim.sfx")
        else:
            logger.warning("Canari: PGD sfx file not found: {}", pgd_sfx_src)

        # --- first guess (atmosphere FA + surface sfx) ---
        # Delegate to the same logic used by Initialization/FirstGuess so
        # that cold-start, restart, and cycling modes are all handled correctly.
        fg_atm, fg_sfx = InitialConditions(self.config).find_initial_files()
        for link in ["ICMSHCYCLINIT", "ICMGGCYCLINIT", "ELSCFCYCLALBC000", "ELSCFANALALBC000", "ICMSHANALINIT"]:
            if not os.path.lexists(link):
                os.symlink(fg_atm, link)
        logger.info("Canari: atmospheric first guess {}", fg_atm)

        if not os.path.lexists("ICMSHCYCLINIT.sfx"):
            os.symlink(fg_sfx, "ICMSHCYCLINIT.sfx")
        if not os.path.lexists("ICMSHANAL+0000.sfx"):
            os.symlink(fg_sfx, "ICMSHANAL+0000.sfx")
        logger.info("Canari: soil first guess {}", fg_sfx)

        # --- ODB ---
        # Copy all ODB dirs from the archive (ECMA + ECMA.synop etc.).
        # ECMA.iomap references $ODB_SRCPATH_ECMA/../ECMA.{obstype}/ so
        # subbases must be siblings of ECMA in the work directory.
        if not os.path.isdir(odb_archive_dir):
            raise FileNotFoundError(
                f"Canari: merged ECMA ODB archive not found at {odb_archive_dir}"
            )
        for entry in sorted(os.listdir(odb_archive_dir)):
            src = os.path.join(odb_archive_dir, entry)
            if os.path.isdir(src) and not os.path.exists(entry):
                shutil.copytree(src, entry, symlinks=True)
        if not os.path.isdir("ECMA"):
            raise RuntimeError("Canari: ECMA directory missing after ODB copy.")
        # IOASSIGN symlinks required by MASTERODB (ECMA/ECMA.IOASSIGN is the data file)
        if not os.path.lexists("IOASSIGN"):
            os.symlink(os.path.join("ECMA", "ECMA.IOASSIGN"), "IOASSIGN")
        if not os.path.lexists("IOASSIGN.ECMA"):
            os.symlink(os.path.join("ECMA", "ECMA.IOASSIGN"), "IOASSIGN.ECMA")

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
                "ODB_IO_METHOD": "4",
                "ODB_IO_FILESIZE": "128",
                "ODB_IO_GRPSIZE": str(self.nbpool),
                "EC_PROFILE_HEAP": "0",
                "TRACEBK": "0",
                "ODB_ANALYSIS_DATE": f"{yyyy}{mm}{dd}",
                "ODB_ANALYSIS_TIME": f"{rr}0000",
                "TIME_INIT_YYYYMMDD": f"{yyyy}{mm}{dd}",
                "TIME_INIT_HHMMSS": f"{rr}0000",
                "ODB_FEBINPATH": os.path.dirname(masterodb_bin),
                "ODB_CMA": "ECMA",
                "ODB_SRCPATH_ECMA": os.path.join(self.wdir, "ECMA"),
                "ODB_DATAPATH_ECMA": os.path.join(self.wdir, "ECMA"),
                "ODB_MERGEODB_DIRECT": "0",
                "BASETIME": ymdrr,
                "CNMEXPB": "CYCL",
                "F_RECLUNIT": "BYTE",
                "F_UFMTENDIAN": "big:10,33,50,54,81",
                "ODB_ECMA_CREATE_POOLMASK": "1",
                "ODB_ECMA_POOLMASK_FILE": os.path.join(
                    self.wdir, "ECMA", "ECMA.poolmask"
                ),
                "IOASSIGN": "IOASSIGN",
            }
        )

        # --- run MASTERODB (CANARI conf 701) ---
        BatchJob(rte, wrapper=self.platform.substitute(self.wrapper)).run(
            "./MASTERODB_canari"
        )

        # CANARI with CNMEXP=ANAL produces ICMSHANAL+0000
        output_file = "ICMSHANAL+0000"
        if not os.path.isfile(output_file) or os.path.getsize(output_file) == 0:
            raise RuntimeError(f"Canari: {output_file} not produced or empty.")

        # --- archive output ---
        out_dir = os.path.join(self.da_scratch, yyyy, mm, dd, rr, "canari")
        tactusmakedirs(out_dir)
        shutil.copy2(output_file, os.path.join(out_dir, output_file))
        if os.path.isfile("ICMSHANAL+0000.sfx"):
            shutil.copy2(
                "ICMSHANAL+0000.sfx", os.path.join(out_dir, "ICMSHANAL+0000.sfx")
            )
        logger.info("Canari: analysis archived to {}", out_dir)
        self.archive_logs(["NODE.001_01", "fort.4"])
