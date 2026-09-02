"""Data assimilation suite components.

Provides composable EcflowSuiteFamily subclasses that implement the full
CANARI (surface OI) + 3D-Var upper-air DA cycle.

"""

from typing import List

from ..submission import TaskSettings
from .base import EcflowSuiteFamily, EcflowSuiteTask

# ---------------------------------------------------------------------------
# Default obs-type lists
# ---------------------------------------------------------------------------

_DEFAULT_OBS_SURFACE: List[str] = ["synop"]

_DEFAULT_OBS_3DVAR: List[str] = [
    "synop",
    "gpssol",
    "amdr",
    "geowind",
    "temp",
    "seviri",
    "amsua",
    "amsub",
    "iasi",
    "ascat",
    "radar",
]

# ---------------------------------------------------------------------------
# OdbFamily — parallel BATOR tasks + OdbMerge
# ---------------------------------------------------------------------------


class OdbFamily(EcflowSuiteFamily):
    """ecFlow family that runs BATOR per obs type.

    Each obs type gets its own sub-family (``Bator_<obstype>``) containing a
    single ``Bator`` task.  The ``OdbMerge`` task triggers when all Bator
    sub-families are complete.
    """

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        obs_types: List[str],
        task_class: str = "Bator",
        family_name: str = "Odb",
        trigger=None,
        ecf_files_remotely=None,
    ):
        """Construct OdbFamily.

        Args:
            parent: Parent ecFlow node.
            config: Experiment config.
            task_settings: Submission configuration / task settings.
            input_template: ecFlow job template.
            ecf_files: Local path prefix for ecf scripts.
            obs_types: List of observation type names to process.
            task_class: Task class to use for each obs type (``"Bator"`` or
                ``"Obsconvert"``).  Both archive their output to the same
                ``odb/`` directory so OdbMerge works with either.
            family_name: Name of this family node (default ``Odb``).
            trigger: Optional trigger for the whole family.
            ecf_files_remotely: Remote path prefix for ecf scripts.
        """
        super().__init__(
            family_name,
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        bator_tasks = []
        for obstype in obs_types:
            task = EcflowSuiteTask(
                f"{task_class}_{obstype}",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables={"OBSTYPE": obstype, "TACTUS_TASK": task_class},
                ecf_files_remotely=ecf_files_remotely,
            )
            bator_tasks.append(task)

        EcflowSuiteTask(
            "OdbMerge",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=bator_tasks,
            ecf_files_remotely=ecf_files_remotely,
        )


# ---------------------------------------------------------------------------
# SurfaceAnalysisFamily — CANARI surface OI chain
# ---------------------------------------------------------------------------


class SurfaceAnalysisFamily(EcflowSuiteFamily):
    """ecFlow family for the CANARI surface OI assimilation chain.

    Tasks within this family (in dependency order):
    1. ``ObsPrep``  – stage surface observations (synop).
    2. ``Odb``      – build surface ODB (Bator + OdbMerge).
    3. ``Canari``   – run CANARI surface analysis (MASTERODB conf 701).
    4. ``BlendSur`` – blend CANARI with LBC SST (BLENDSUR executable).
    """

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        trigger=None,
        ecf_files_remotely=None,
    ):
        """Construct SurfaceAnalysisFamily.

        Args:
            parent: Parent ecFlow node.
            config: Experiment config.
            task_settings: Submission configuration.
            input_template: ecflow job template.
            ecf_files: Local ecf script path prefix.
            trigger: Optional trigger for the whole Surface family.
            ecf_files_remotely: Remote ecf script path prefix.
        """
        super().__init__(
            "Surface",
            parent,
            ecf_files,
            trigger=trigger,
            variables={"DA_STREAM": "surface"},
            ecf_files_remotely=ecf_files_remotely,
        )

        obs_types_surface = config.get("da.obs_types_surface", _DEFAULT_OBS_SURFACE)
        odb_task_surface = config.get("da.odb_task_surface", "Obsconvert")

        obsprep = EcflowSuiteTask(
            "ObsPrep",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            ecf_files_remotely=ecf_files_remotely,
        )

        odb_family = OdbFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            obs_types=obs_types_surface,
            task_class=odb_task_surface,
            family_name="Odb",
            trigger=obsprep,
            ecf_files_remotely=ecf_files_remotely,
        )

        canari = EcflowSuiteTask(
            "Canari",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=odb_family,
            ecf_files_remotely=ecf_files_remotely,
        )

        self.blendsur = EcflowSuiteTask(
            "BlendSur",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=canari,
            ecf_files_remotely=ecf_files_remotely,
        )


# ---------------------------------------------------------------------------
# VariationalFamily — 3D-Var upper-air analysis chain
# ---------------------------------------------------------------------------


class VariationalFamily(EcflowSuiteFamily):
    """ecFlow family for the 3D-Var upper-air assimilation chain.

    Tasks within this family (in dependency order):
    1. ``ObsPrep`` – stage all upper-air observation types.
    2. ``Odb``     – build 3D-Var ODB (Bator or Obsconvert + OdbMerge).
    3. ``OopsVar`` – OOPS-based screening + minimisation; triggered by both
                     ``Odb`` and ``Surface/BlendSur`` (blended first guess).
    """

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        blendsur_node,
        trigger=None,
        ecf_files_remotely=None,
    ):
        """Construct VariationalFamily.

        Args:
            parent: Parent ecFlow node.
            config: Experiment config.
            task_settings: Submission configuration.
            input_template: ecFlow job template.
            ecf_files: Local ecf script path prefix.
            blendsur_node: The ``BlendSur`` EcflowSuiteTask from the sibling
                ``SurfaceAnalysisFamily``.  OopsVar will wait for it.
            trigger: Optional trigger for the whole UpperAir family.
            ecf_files_remotely: Remote ecf script path prefix.
        """
        super().__init__(
            "UpperAir",
            parent,
            ecf_files,
            trigger=trigger,
            variables={"DA_STREAM": "3dvar"},
            ecf_files_remotely=ecf_files_remotely,
        )

        obs_types_3dvar = config.get("da.obs_types_3dvar", _DEFAULT_OBS_3DVAR)
        odb_task_3dvar = config.get("da.odb_task_3dvar", "Bator")

        obsprep = EcflowSuiteTask(
            "ObsPrep",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            ecf_files_remotely=ecf_files_remotely,
        )

        odb_family = OdbFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            obs_types=obs_types_3dvar,
            task_class=odb_task_3dvar,
            family_name="Odb",
            trigger=obsprep,
            ecf_files_remotely=ecf_files_remotely,
        )

        # OOPS Var: a single OOVAR call handles screening + minimization.
        EcflowSuiteTask(
            "OopsVar",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=[odb_family, blendsur_node],
            ecf_files_remotely=ecf_files_remotely,
        )


# ---------------------------------------------------------------------------
# AssimilationFamily — top-level DA family
# ---------------------------------------------------------------------------


class AssimilationFamily(EcflowSuiteFamily):
    """Top-level DA family containing the surface OI and optionally 3D-Var."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        trigger=None,
        ecf_files_remotely=None,
        cycle_basetime=None,
    ):
        """Construct AssimilationFamily.

        Args:
            parent: Parent ecFlow node (typically ``CycleFamily``).
            config: Experiment config.
            task_settings: Submission configuration.
            input_template: ecFlow job template.
            ecf_files: Local ecf script path prefix.
            trigger: Trigger to start the DA chain (typically
                ``InitializationFamily`` so that FirstGuess is ready).
            ecf_files_remotely: Remote ecf script path prefix.
            cycle_basetime: ISO basetime string for this specific cycle,
                used to set IS_BGCYCLE at generation time.
        """
        super().__init__(
            "Assimilation",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        # Cold-start skip: set IS_BGCYCLE=1 on the bgcycle, 0 otherwise.
        # The ecFlow complete expression fires only when IS_BGCYCLE == 1,
        # bypassing all DA tasks for that slot (forecast uses first-guess).
        # ecFlow string comparison (eq) is unsupported; integer IS_BGCYCLE
        # avoids that limitation while keeping the expression runtime-visible.
        # NOTE: for repeat YMD, IS_BGCYCLE would need to be a derived variable
        # (computed from the repeat counter) rather than a generation-time constant.
        bgcycle = config.get("da.bgcycle", "")
        if bgcycle and len(bgcycle) == 10:
            bgcycle_iso = (
                f"{bgcycle[:4]}-{bgcycle[4:6]}-{bgcycle[6:8]}T{bgcycle[8:10]}:00:00Z"
            )
            is_bgcycle = 1 if cycle_basetime == bgcycle_iso else 0
            self.ecf_node.add_variable("IS_BGCYCLE", is_bgcycle)
            self.ecf_node.add_complete("Assimilation:IS_BGCYCLE == 1")

        # Surface OI chain — always present
        surface_family = SurfaceAnalysisFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            ecf_files_remotely=ecf_files_remotely,
        )

        # Upper-air 3D-Var chain — optional (da.do_upper_air, default true)
        if config.get("da.do_upper_air", True):
            VariationalFamily(
                self,
                config,
                task_settings,
                input_template,
                ecf_files,
                blendsur_node=surface_family.blendsur,
                ecf_files_remotely=ecf_files_remotely,
            )
