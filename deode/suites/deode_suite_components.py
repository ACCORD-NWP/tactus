"""Module to create the different parts of the DEODE ecFlow suite."""

from datetime import datetime, timedelta
from typing import Generator, Optional

from deode.suites.suite_utils import Cycle, Cycles, lbc_times_generator

from ..datetime_utils import (
    as_datetime,
    as_timedelta,
    get_decadal_list,
    get_decade,
    get_month_list,
)
from ..logs import logger
from ..submission import ProcessorLayout, TaskSettings
from ..tasks.impacts import ImpactModels
from .base import (
    EcflowSuiteFamily,
    EcflowSuiteLimit,
    EcflowSuiteTask,
    EcflowSuiteTrigger,
    EcflowSuiteTriggers,
)


class PgdInputFamily(EcflowSuiteFamily):
    """Class for creating the PGD input ecFlow family."""

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
        """Class initialization."""
        super().__init__(
            "PgdInput",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        EcflowSuiteTask(
            "Gmted",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
            ecf_files_remotely=ecf_files_remotely,
        )

        if config["suite_control.do_soil"]:
            EcflowSuiteTask(
                "Soil",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables=None,
            )


class E923MonthlyFamily(EcflowSuiteFamily):
    """Class for creating the E923Monthly ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        trigger=None,
        ecf_files_remotely=None,
        dry_run: bool = False,
        limit=None,
    ):
        """Class initialization."""
        super().__init__(
            "E923Monthly",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        seasons = {
            "Q1": "01,02,03",
            "Q2": "04,05,06",
            "Q3": "07,08,09",
            "Q4": "10,11,12",
        }

        if config["pgd.one_decade"]:
            month_list = get_month_list(
                config["general.times.start"], config["general.times.end"]
            )
            last_month = month_list[0] - 1
            if last_month == 0:
                last_month = 12
            next_month = month_list[-1] + 1
            if next_month == 13:
                next_month = 1

            month_list.insert(0, last_month)
            month_list.append(next_month)

            seasons = {}
            for mm in sorted(month_list):
                seasons[f"m{mm:02d}"] = f"{mm:02d}"

        for season, months in seasons.items():
            month_family = EcflowSuiteFamily(
                season,
                self,
                ecf_files,
                ecf_files_remotely=ecf_files_remotely,
                limit=limit if not dry_run else None,
            )

            EcflowSuiteTask(
                "E923Monthly",
                month_family,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables={"ARGS": f"months={months}"},
                ecf_files_remotely=ecf_files_remotely,
            )


class PgdNode(EcflowSuiteFamily, EcflowSuiteTask):
    """Class for creating the PGD ecFlow family and task."""

    def __init__(
        self,
        node_name,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        trigger=None,
        ecf_files_remotely=None,
        limit=None,
    ):
        """Class initialization."""
        if config["pgd.one_decade"]:
            EcflowSuiteFamily.__init__(
                self,
                node_name,
                parent,
                ecf_files,
                trigger=trigger,
                ecf_files_remotely=ecf_files_remotely,
            )
            decade_dates = get_decadal_list(
                as_datetime(config["general.times.start"]),
                as_datetime(config["general.times.end"]),
            )

            for dec_date in decade_dates:
                decade_pgd_family = EcflowSuiteFamily(
                    f"decade_{get_decade(dec_date)}",
                    self,
                    ecf_files,
                    ecf_files_remotely=ecf_files_remotely,
                    limit=limit,
                )

                EcflowSuiteTask(
                    node_name,
                    decade_pgd_family,
                    config,
                    task_settings,
                    ecf_files,
                    input_template=input_template,
                    variables={"ARGS": f"basetime={dec_date}"},
                    ecf_files_remotely=ecf_files_remotely,
                )

        else:
            basetime = as_datetime(config["general.times.basetime"])
            EcflowSuiteTask.__init__(
                self,
                node_name,
                parent,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables={"ARGS": f"basetime={basetime}"},
                trigger=trigger,
                ecf_files_remotely=ecf_files_remotely,
            )


class StaticDataFamily(EcflowSuiteFamily):
    """Class for creating the StaticData ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        trigger=None,
        ecf_files_remotely=None,
        dry_run: bool = False,
    ):
        """Class initialization."""
        super().__init__(
            name="StaticData",
            parent=parent,
            ecf_files=ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        if config["suite_control.member_specific_static_data"]:
            # Create the StaticData member generator
            static_data_member_generator = StaticDataMemberGenerator(
                parent=self,
                config=config,
                task_settings=task_settings,
                input_template=input_template,
                ecf_files=ecf_files,
                ecf_files_remotely=ecf_files_remotely,
                dry_run=dry_run,
            )
            # Iterate through the StaticData member generator to create all member
            # families
            self.static_data_members = {}
            for member, static_data_member in static_data_member_generator:
                self.static_data_members[member] = static_data_member

        else:
            static_data_limit = None
            if not dry_run:
                static_data_max = config["suite_control.max_static_data_tasks"]
                static_data_limit = EcflowSuiteLimit("static_data_limit", static_data_max)
                self.ecf_node.add_limit(
                    static_data_limit.limit_name, static_data_limit.max_jobs
                )

            StaticDataTasks(
                parent=self,
                config=config,
                task_settings=task_settings,
                input_template=input_template,
                ecf_files=ecf_files,
                ecf_files_remotely=ecf_files_remotely,
                limit=static_data_limit,
            )


class StaticDataMemberGenerator:
    """Class for creating the StaticData ecFlow family generator."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        ecf_files_remotely,
        dry_run,
    ):
        """Class initialization."""
        self.parent = parent
        self.config = config
        self.task_settings = task_settings
        self.input_template = input_template
        self.ecf_files = ecf_files
        self.ecf_files_remotely = ecf_files_remotely
        self.dry_run = dry_run

    def get_member_family(self, member: int):
        """Get member specific family with tasks to produce static data.

        Args:
            member (int): The member number.

        Returns:
            EcflowSuiteFamily: The member family that contains tasks to
                                produce static data for the given member.
        """
        member_family = EcflowSuiteFamily(
            f"mbr{member:03d}",
            self.parent,
            self.ecf_files,
            variables={"MEMBER": member},
            ecf_files_remotely=self.ecf_files_remotely,
        )
        static_data_limit = None
        if not self.dry_run:
            static_data_max = self.config["suite_control.max_static_data_tasks"]
            static_data_limit = EcflowSuiteLimit("static_data_limit", static_data_max)
            member_family.ecf_node.add_limit(
                static_data_limit.limit_name, static_data_limit.max_jobs
            )

        StaticDataTasks(
            member_family,
            self.config,
            self.task_settings,
            self.input_template,
            self.ecf_files,
            ecf_files_remotely=self.ecf_files_remotely,
            limit=static_data_limit,
        )

        return member_family

    def __iter__(self):
        if self.config["suite_control.member_specific_static_data"]:
            for member in self.config["eps.general.members"]:
                yield member, self.get_member_family(member)
        else:
            yield 0, self.get_member_family(0)


class StaticDataTasks:
    """Class for creating the StaticData ecFlow tasks."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        ecf_files_remotely,
        limit: Optional[EcflowSuiteLimit] = None,
    ):
        """Class initialization."""
        pgd_node = None
        if config["suite_control.do_pgd"]:
            pgd_input = PgdInputFamily(
                parent,
                config,
                task_settings,
                input_template,
                ecf_files,
                ecf_files_remotely=ecf_files_remotely,
            )

            pgd_node = PgdNode(
                "Pgd",
                parent,
                config,
                task_settings,
                input_template,
                ecf_files,
                trigger=pgd_input,
                ecf_files_remotely=ecf_files_remotely,
            )

        e923constant = EcflowSuiteTask(
            "E923Constant",
            parent,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
            trigger=pgd_node,
            ecf_files_remotely=ecf_files_remotely,
        )

        e923_monthly_family = E923MonthlyFamily(
            parent,
            config,
            task_settings,
            input_template,
            ecf_files,
            trigger=e923constant,
            ecf_files_remotely=ecf_files_remotely,
            limit=limit,
        )

        archive_static_member_trigger = e923_monthly_family

        pgd_update = None
        if config["suite_control.do_pgd"]:
            pgd_update = PgdNode(
                "PgdUpdate",
                parent,
                config,
                task_settings,
                input_template,
                ecf_files,
                trigger=e923constant,
                ecf_files_remotely=ecf_files_remotely,
                limit=limit,
            )
            archive_static_member_trigger = pgd_update

        if (
            config["suite_control.do_archiving"]
            and config["suite_control.member_specific_static_data"]
        ):
            EcflowSuiteTask(
                "ArchiveStaticMember",
                parent,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables=None,
                trigger=archive_static_member_trigger,
            )

        if config["suite_control.do_creategrib_static"]:
            PgdNode(
                "CreateGribStatic",
                self,
                config,
                task_settings,
                input_template,
                ecf_files,
                trigger=pgd_update,
                ecf_files_remotely=ecf_files_remotely,
            )


class InputDataFamily(EcflowSuiteFamily):
    """Class for creating the InputDataFamily ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        trigger=None,
        ecf_files_remotely=None,
        external_marsprep_trigger_node=None,
    ):
        """Class initialization."""
        super().__init__(
            "InputData",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        prepare_cycle = EcflowSuiteTask(
            "PrepareCycle",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
        )

        marsprep_trigger_nodes = [prepare_cycle]
        if external_marsprep_trigger_node is not None:
            marsprep_trigger_nodes.extend(external_marsprep_trigger_node)

        if (
            config["suite_control.do_marsprep"]
            and config["suite_control.interpolate_boundaries"]
            and not config["suite_control.split_mars"]
        ):
            EcflowSuiteTask(
                "Marsprep",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=marsprep_trigger_nodes,
                ecf_files_remotely=ecf_files_remotely,
            )


class PrepFamily(EcflowSuiteFamily):
    """Class for creating the PrepFamily ecFlow family."""

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
        """Class initialization."""
        super().__init__(
            "Prep",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        bd_step_index = 0
        mode = config["suite_control.mode"]
        if mode == "restart":
            bd_step_index = 1

        split_mars_task = None
        if config["suite_control.split_mars"]:
            args = f"bd_index={bd_step_index};prep_step=True"
            variables = {"ARGS": args}
            split_mars_task = EcflowSuiteTask(
                "marsprep",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables=variables,
                ecf_files_remotely=ecf_files_remotely,
            )

        EcflowSuiteTask(
            "Prep",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=split_mars_task,
            ecf_files_remotely=ecf_files_remotely,
        )


class LBCSubFamilyGenerator(EcflowSuiteFamily):
    """Class for creating the ecFlow LBCSubFamilyGenerator.

    When iterating over this class, it will create a new LBC family for each
    time in the lbc_time_generator.
    """

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        bdint: timedelta,
        lbc_time_generator: Generator[datetime, None, None],
        trigger=None,
        ecf_files_remotely=None,
        do_prep: bool = True,
        limit: Optional[EcflowSuiteLimit] = None,
    ):
        """Class initialization."""
        self.parent = parent
        self.config = config
        self.task_settings = task_settings
        self.input_template = input_template
        self.ecf_files = ecf_files
        self.trigger = trigger
        self.ecf_files_remotely = ecf_files_remotely
        self.do_prep = do_prep
        self.limit = limit
        self.bdint = bdint
        self.lbc_time_generator = lbc_time_generator

    def __iter__(self):
        for bd_index, lbc_time in enumerate(self.lbc_time_generator):
            date_string = lbc_time.isoformat(sep="T").replace("+00:00", "Z")
            args = f"bd_time={date_string};bd_index={bd_index};prep_step=False"
            variables = {"ARGS": args}

            lbc_family_name = date_string.replace("-", "").replace(":", "_")

            super().__init__(
                lbc_family_name,
                self.parent,
                self.ecf_files,
                trigger=self.trigger,
                variables=None,
                ecf_files_remotely=self.ecf_files_remotely,
                limit=self.limit,
            )

            split_mars_task = None
            if self.config["suite_control.split_mars"]:
                split_mars_task = EcflowSuiteTask(
                    "Marsprep",
                    self,
                    self.config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=self.input_template,
                    variables=variables,
                    trigger=None,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

            interpolation_task_name = (
                "C903" if self.config["suite_control.do_marsprep"] else "E927"
            )
            EcflowSuiteTask(
                interpolation_task_name,
                self,
                self.config,
                self.task_settings,
                self.ecf_files,
                input_template=self.input_template,
                variables=variables,
                trigger=split_mars_task,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            yield self


class LBCFamily(EcflowSuiteFamily):
    """Class for creating the ecFlow LBCFamily."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        cycle: Cycle,
        trigger=None,
        lbc_family_trigger=None,
        ecf_files_remotely=None,
        do_prep: bool = True,
        dry_run: bool = False,
    ):
        """Class initialization."""
        super().__init__(
            "LBCs",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        if not dry_run:
            bdmax = config["boundaries.max_interpolation_tasks"]
            lbc_limit = EcflowSuiteLimit("lbc_limit", bdmax)
            self.ecf_node.add_limit(lbc_limit.limit_name, lbc_limit.max_jobs)

        basetime = as_datetime(cycle.basetime)
        forecast_range = as_timedelta(config["general.times.forecast_range"])
        endtime = basetime + forecast_range
        bdint = as_timedelta(config["boundaries.bdint"])
        lbc_times_generator_instance = lbc_times_generator(
            basetime,
            endtime,
            bdint,
            mode=config["suite_control.mode"],
            do_prep=do_prep,
        )
        lbc_family_generator_instance = LBCSubFamilyGenerator(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            bdint,
            lbc_times_generator_instance,
            trigger=lbc_family_trigger,
            ecf_files_remotely=ecf_files_remotely,
            do_prep=do_prep,
            limit=lbc_limit if not dry_run else None,
        )

        # Iterate through the LBC family generator to create the next
        # LBC families
        for _ in lbc_family_generator_instance:
            pass


class InterpolationFamily(EcflowSuiteFamily):
    """Class for creating the Interpolation ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        cycles: Cycles,
        trigger=None,
        ecf_files_remotely=None,
        do_prep: bool = True,
        dry_run: bool = False,
    ):
        """Class initialization."""
        super().__init__(
            "Interpolation",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        e923_update_task = None
        csc = config["general.csc"]

        if do_prep:
            prep_fam = PrepFamily(
                self,
                config,
                task_settings,
                input_template,
                ecf_files,
                ecf_files_remotely=ecf_files_remotely,
            )

            if csc == "ALARO":
                e923_update_task = EcflowSuiteTask(
                    "E923Update",
                    self,
                    config,
                    task_settings,
                    ecf_files,
                    trigger=prep_fam,
                    input_template=input_template,
                    ecf_files_remotely=ecf_files_remotely,
                )

            if config["suite_control.mode"] != "cold_start" or csc == "ALARO":
                do_prep = False

        if csc == "ALARO" and cycles.end_of_month:
            do_prep = True

        LBCFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            cycles.current_cycle,
            lbc_family_trigger=e923_update_task,
            ecf_files_remotely=ecf_files_remotely,
            do_prep=do_prep,
            dry_run=dry_run,
        )


class InitializationFamily(EcflowSuiteFamily):
    """Class for creating the Initialization ecFlow family."""

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
        """Class initialization."""
        super().__init__(
            "Initialization",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        EcflowSuiteTask(
            "FirstGuess",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            ecf_files_remotely=ecf_files_remotely,
        )


class MergeIOFamily(EcflowSuiteFamily):
    """Class for creating the MergeIO ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        n_io_merge: int,
        nproc_io: int,
        trigger=None,
        ecf_files_remotely=None,
    ):
        """Class initialization."""
        # these tasks should trigger when the Forecast is *active* or *complete*
        iomerge_trigger = EcflowSuiteTriggers(
            [
                EcflowSuiteTrigger(trigger, "active"),
                EcflowSuiteTrigger(trigger, "complete"),
            ],
            mode="OR",
        )
        super().__init__(
            "MergeIO",
            parent,
            ecf_files,
            trigger=iomerge_trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        for ionr in range(n_io_merge):
            iomerge_sub = EcflowSuiteFamily(
                f"IO_{ionr:02}",
                self,
                ecf_files,
                ecf_files_remotely=ecf_files_remotely,
            )
            args = f"ionr={ionr};nproc_io={nproc_io}"
            EcflowSuiteTask(
                "IOmerge",
                iomerge_sub,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                variables={"ARGS": args},
                ecf_files_remotely=ecf_files_remotely,
            )


class ForecastFamily(EcflowSuiteFamily):
    """Class for creating the Forecast ecFlow family."""

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
        """Class initialization."""
        super().__init__(
            "Forecasting",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        # Get the number of I/O processors
        settings = task_settings.get_settings("Forecast")
        procs = ProcessorLayout(settings)
        nproc_io = procs.nproc_io if procs.nproc_io is not None else 0
        n_io_merge = config["suite_control.n_io_merge"] if nproc_io > 0 else 0

        logger.debug(task_settings.get_task_settings("Forecast"))

        forecast_task = EcflowSuiteTask(
            "Forecast",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            ecf_files_remotely=ecf_files_remotely,
        )

        add_calc_fields_trigger = forecast_task
        creategrib_trigger = forecast_task

        if n_io_merge > 0:
            io_merge = MergeIOFamily(
                self,
                config,
                task_settings,
                input_template,
                ecf_files,
                n_io_merge,
                nproc_io,
                trigger=forecast_task,
                ecf_files_remotely=ecf_files_remotely,
            )
            add_calc_fields_trigger = io_merge
            creategrib_trigger = io_merge

        if len(config.get("creategrib.CreateGrib.conversions", [])) > 0:
            add_calc_fields_trigger = EcflowSuiteTask(
                "CreateGrib",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=creategrib_trigger,
                ecf_files_remotely=ecf_files_remotely,
            )

        add_calc_fields_task = EcflowSuiteTask(
            "AddCalculatedFields",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=add_calc_fields_trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        fdb_sel = config.get("archiving.FDB.fdb", {})
        fdb_archiving_active = [v["active"] for v in fdb_sel.values()]
        if any(fdb_archiving_active):
            EcflowSuiteTask(
                "ArchiveFDB",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=add_calc_fields_task,
                ecf_files_remotely=ecf_files_remotely,
            )

        if config["suite_control.do_extractsqlite"]:
            EcflowSuiteTask(
                "ExtractSQLite",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=add_calc_fields_task,
            )

        if ImpactModels(config, "StartImpactModels").is_active:
            EcflowSuiteTask(
                "StartImpactModels",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=add_calc_fields_task,
            )


class CycleFamily(EcflowSuiteFamily):
    """Class for creating the Cycle ecFlow family."""

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
        """Class initialization."""
        super().__init__(
            "Cycle",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        initialization_family = InitializationFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            ecf_files_remotely=ecf_files_remotely,
        )

        ForecastFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            trigger=initialization_family,
            ecf_files_remotely=ecf_files_remotely,
        )


class PostCycleFamily(EcflowSuiteFamily):
    """Class for creating the PostCycle ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        cycle: Cycle,
        ecf_out,
        suite_name,
        trigger=None,
        external_cycle_cleaning_trigger=None,
        ecf_files_remotely=None,
    ):
        """Class initialization."""
        super().__init__(
            "PostCycle",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        collectlogs_triggers = []
        cleaning_triggers = []
        if config["suite_control.do_archiving"]:
            archive_hour = EcflowSuiteTask(
                "ArchiveHour",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                ecf_files_remotely=ecf_files_remotely,
            )
            cleaning_triggers.append(archive_hour)
            collectlogs_triggers.append(archive_hour)

        if (
            config["suite_control.do_cleaning"]
            and external_cycle_cleaning_trigger is not None
        ):
            cleaning_triggers.append(external_cycle_cleaning_trigger)

            cleaning_task = EcflowSuiteTask(
                "CycleCleaning",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=cleaning_triggers,
                ecf_files_remotely=ecf_files_remotely,
            )
            cleaning_triggers.append(cleaning_task)
            collectlogs_triggers.append(cleaning_task)

        task_logs = config["system.wrk"]
        args = ";".join(
            [
                (f"joboutdir={ecf_out}/{suite_name}" + f"/{cycle.day}/{cycle.time}"),
                f"tarname={cycle.day}_{cycle.time}",
                f"task_logs={task_logs}",
                "config_label=hourlogs",
            ]
        )
        EcflowSuiteTask(
            "CollectLogs",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            variables={"ARGS": args},
            trigger=collectlogs_triggers,
            ecf_files_remotely=ecf_files_remotely,
        )


class TimeDependentFamily(EcflowSuiteFamily):
    """Class for creating the time dependent part of a DW suite."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        ecf_out,
        suite_name,
        trigger: Optional[EcflowSuiteTask | StaticDataFamily] = None,
        ecf_files_remotely=None,
        do_prep: bool = True,
        dry_run: bool = False,
    ):
        """Class initialization."""
        cycles = Cycles(
            first_cycle=config["general.times.start"],
            last_cycle=config["general.times.end"],
            cycle_length=config["general.times.cycle_length"],
        )

        prev_cycle_triggers = {}
        prev_interpolation_triggers = {}
        postcycle_families = {}
        unique_days = set()
        day_family: EcflowSuiteFamily

        for cycle in cycles:
            if cycle.day not in unique_days:
                day_family = EcflowSuiteFamily(
                    cycle.day,
                    parent,
                    ecf_files,
                    ecf_files_remotely=ecf_files_remotely,
                )
                unique_days.add(cycle.day)

            time_variables = {
                "BASETIME": cycle.basetime,
                "VALIDTIME": cycle.validtime,
            }
            self._last_node = time_family = EcflowSuiteFamily(
                cycle.time,
                day_family,
                ecf_files,
                variables=time_variables,
                ecf_files_remotely=ecf_files_remotely,
            )

            if not config["suite_control.member_specific_mars_prep"]:
                external_marsprep_trigger_nodes = [
                    prev_interpolation_triggers.get(member)
                    for member in config["eps.general.members"]
                ]
                external_marsprep_trigger_nodes = (
                    external_marsprep_trigger_nodes
                    if all(external_marsprep_trigger_nodes)
                    else None
                )
                inputdata = InputDataFamily(
                    time_family,
                    config,
                    task_settings,
                    input_template,
                    ecf_files,
                    trigger=trigger,
                    ecf_files_remotely=ecf_files_remotely,
                    external_marsprep_trigger_node=external_marsprep_trigger_nodes,
                )
                ready_for_cycle = [inputdata]

            for member in config["eps.general.members"]:
                member_family = EcflowSuiteFamily(
                    f"mbr{member:03d}",
                    time_family,
                    ecf_files,
                    variables={"MEMBER": member},
                    ecf_files_remotely=ecf_files_remotely,
                )

                mbr_trigger = trigger
                if config["suite_control.member_specific_static_data"]:
                    # If trigger has static_data_members, then let each member family
                    # trigger on the corresponding static_data_member
                    try:
                        mbr_trigger = trigger.static_data_members[member]
                    except KeyError:
                        logger.warning(
                            f"Static data member for member {member} not found "
                            f"in trigger. Using trigger {trigger}"
                        )

                if config["suite_control.member_specific_mars_prep"]:
                    external_marsprep_trigger_nodes = [
                        prev_interpolation_triggers.get(member)
                    ]
                    external_marsprep_trigger_nodes = (
                        external_marsprep_trigger_nodes
                        if all(external_marsprep_trigger_nodes)
                        else None
                    )
                    inputdata = InputDataFamily(
                        member_family,
                        config,
                        task_settings,
                        input_template,
                        ecf_files,
                        trigger=mbr_trigger,
                        ecf_files_remotely=ecf_files_remotely,
                        external_marsprep_trigger_node=external_marsprep_trigger_nodes,
                    )
                    ready_for_cycle = [inputdata]

                if config["suite_control.interpolate_boundaries"]:
                    int_family = InterpolationFamily(
                        member_family,
                        config,
                        task_settings,
                        input_template,
                        ecf_files,
                        cycles,
                        trigger=inputdata,
                        ecf_files_remotely=ecf_files_remotely,
                        do_prep=do_prep,
                        dry_run=dry_run,
                    )

                    ready_for_cycle = prev_interpolation_triggers[member] = int_family

                prev_cycle_trigger = prev_cycle_triggers.get(member)
                if prev_cycle_trigger:
                    ready_for_cycle = [ready_for_cycle, *prev_cycle_trigger]

                cycle_family = CycleFamily(
                    member_family,
                    config,
                    task_settings,
                    input_template,
                    ecf_files,
                    trigger=ready_for_cycle,
                    ecf_files_remotely=ecf_files_remotely,
                )
                prev_cycle_triggers[member] = [cycle_family]

                postcycle_families[member] = PostCycleFamily(
                    member_family,
                    config,
                    task_settings,
                    input_template,
                    ecf_files,
                    cycle,
                    ecf_out=ecf_out,
                    suite_name=suite_name,
                    trigger=cycle_family,
                    external_cycle_cleaning_trigger=postcycle_families.get(member),
                    ecf_files_remotely=ecf_files_remotely,
                )

    @property
    def last_node(self):
        """Return the last family node of self."""
        return self._last_node
