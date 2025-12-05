"""Module to create the different parts of the DEODE ecFlow suite."""

from datetime import datetime, timedelta
from typing import Generator, List, Optional, Tuple

from deode.boundary_utils import Boundary
from tactus.suites.suite_utils import Cycles, lbc_times_generator

from tactus.datetime_utils import (
    as_datetime,
    as_timedelta,
    get_decadal_list,
    get_decade,
    get_month_list,
)
from deode.host_actions import SelectHost
from deode.logs import logger
from deode.submission import ProcessorLayout, TaskSettings
from deode.suites.base import (
    EcflowSuiteFamily,
    EcflowSuiteLimit,
    EcflowSuiteTask,
    EcflowSuiteTrigger,
    EcflowSuiteTriggers,
)
from deode.suites.suite_utils import Cycles, lbc_times_generator
from deode.tasks.impacts import get_impact
from deode.toolbox import Platform


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
            "Topography",
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
                if static_data_max > 0:
                    static_data_limit = EcflowSuiteLimit(
                        "static_data_limit", static_data_max
                    )
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
            if static_data_max > 0:
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

        if config["general.csc"] == "ALARO" and config["general.surfex"]:
            pgd_filter_town_frac = PgdNode(
                "PgdFilterTownFrac",
                parent,
                config,
                task_settings,
                input_template,
                ecf_files,
                trigger=pgd_update,
                ecf_files_remotely=ecf_files_remotely,
                limit=limit,
            )
            archive_static_member_trigger = pgd_filter_town_frac

        if config["general.windfarm"] and config["json2tab.enabled"]:
            generate_wfp_tabfile = EcflowSuiteTask(
                "GenerateWfpTabFile",
                parent,
                config,
                task_settings,
                ecf_files,
                input_template,
                trigger=None,
                ecf_files_remotely=ecf_files_remotely,
            )
            archive_static_member_trigger = generate_wfp_tabfile

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
                parent,
                config,
                task_settings,
                input_template,
                ecf_files,
                trigger=archive_static_member_trigger,
                ecf_files_remotely=ecf_files_remotely,
            )


class MirrorFamily(EcflowSuiteFamily):
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
        cycle_valid=None,
    ):
        """Class initialization."""
        super().__init__(
            "Mirrors",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        platform = Platform(config)

        if config["suite_control.mirror_globalDT"]:
            EcflowSuiteTask(
                config["scheduler.mirror_globalDT"]["remote_path"].split("/")[-1],
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=[trigger],
                mirror=True,
                mirror_config=config["scheduler.mirror_globalDT"],
                ecf_files_remotely=ecf_files_remotely,
            )

        if config["suite_control.mirror_host_case"]:
            mirror_config = config["scheduler.mirror_host_case"].dict()
            remote_host = mirror_config["remote_host"]
            remote_host = platform.substitute(remote_host)
            mirror_config["remote_host"] = platform.evaluate(
                remote_host, object_=SelectHost
            )

            bd_basetime = Boundary(config).bd_basetime
            mirror_config["remote_path"] = platform.substitute(
                mirror_config["remote_path"], basetime=bd_basetime, validtime=cycle_valid
            )
            EcflowSuiteTask(
                config["scheduler.mirror_host_case"]["remote_path"].split("/")[-1],
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=[trigger],
                mirror=True,
                mirror_config=mirror_config,
                ecf_files_remotely=ecf_files_remotely,
            )

        if config["suite_control.mirror_offline"]:
            mirror_config = config["scheduler.mirror_offline"].dict()
            mirror_config["remote_path"] = platform.substitute(
                mirror_config["remote_path"], validtime=cycle_valid
            )
            EcflowSuiteTask(
                config["scheduler.mirror_offline"]["remote_path"].split("/")[-1],
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=[trigger],
                mirror=True,
                mirror_config=mirror_config,
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
        add_var_trigger=None,
        remote_path=None,
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
                add_var_trigger=add_var_trigger,
                remote_path=remote_path,
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
        lbc_time_generator: Generator[Tuple[List[int], List[datetime]], None, None],
        trigger=None,
        ecf_files_remotely=None,
        is_first_cycle: bool = True,
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
        self.is_first_cycle = is_first_cycle
        self.limit = limit
        self.bdint = bdint
        self.lbc_time_generator = lbc_time_generator

    def __iter__(self):
        for bd_index_time_dict in self.lbc_time_generator:
            args = f"bd_index_time_dict={bd_index_time_dict};prep_step=False"
            variables = {"ARGS": args}

            min_time, max_time = (
                bd_index_time_dict[k]
                for k in (min(bd_index_time_dict), max(bd_index_time_dict))
            )

            def format_time(t):
                return t.replace("+00:00", "Z").replace("-", "").replace(":", "_")

            lbc_family_name = f"{format_time(min_time)}to{format_time(max_time)}"

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

            # do we need to run E927/C903 boundary interpolation, or just SST/SIC
            if (
                self.config["suite_control.mode"] == "restart"
                or (
                    self.config["suite_control.mode"] == "start"
                    and not self.is_first_cycle
                )
            ) and self.config["suite_control.do_interpolsstsic"]:
                do_intp_task = False
            else:
                do_intp_task = True

            interpolation_task_name = (
                "C903" if self.config["suite_control.do_marsprep"] else "E927"
            )
            if do_intp_task:
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

            if (
                self.config["suite_control.do_interpolsstsic"]
                and interpolation_task_name == "C903"
            ):
                EcflowSuiteTask(
                    "InterpolSstSic",
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
        cycles: Cycles,
        trigger=None,
        lbc_family_trigger=None,
        ecf_files_remotely=None,
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

        lbc_limit = None
        if not dry_run:
            bdmax = config["boundaries.max_interpolation_tasks"]
            if bdmax > 0:
                lbc_limit = EcflowSuiteLimit("lbc_limit", bdmax)
                self.ecf_node.add_limit(lbc_limit.limit_name, lbc_limit.max_jobs)

        basetime = as_datetime(cycles.current_cycle.basetime)
        forecast_range = as_timedelta(config["general.times.forecast_range"])
        endtime = basetime + forecast_range
        bdint = as_timedelta(config["boundaries.bdint"])
        is_first_cycle = cycles.current_index == 0
        lbc_times_generator_instance = lbc_times_generator(
            basetime,
            endtime,
            bdint,
            mode=config["suite_control.mode"],
            is_first_cycle=is_first_cycle,
            do_interpolsstsic=config["suite_control.do_interpolsstsic"],
            lbc_per_task=int(config["boundaries.lbc_per_task"]),
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
            is_first_cycle=is_first_cycle,
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
        add_var_trigger=None,
        remote_path=None,
    ):
        """Class initialization."""
        super().__init__(
            "Interpolation",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
            add_var_trigger=add_var_trigger,
            remote_path=remote_path,
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

            if csc == "ALARO" and not config["general.surfex"]:
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

        if csc == "ALARO" and not config["general.surfex"] and cycles.end_of_month:
            do_prep = True

        LBCFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            cycles,
            lbc_family_trigger=e923_update_task,
            ecf_files_remotely=ecf_files_remotely,
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


class SubTaskFamily(EcflowSuiteFamily):
    """Class for creating a subtask ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        input_template,
        ecf_files,
        taskname,
        ntasks: int,
        extra_args=None,
        trigger=None,
        ecf_files_remotely=None,
    ):
        """Class initialization."""
        super().__init__(
            f"{taskname}Tasks",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )

        for tasknr in range(ntasks):
            subfamily = EcflowSuiteFamily(
                f"Subtask_{tasknr:02}",
                self,
                ecf_files,
                ecf_files_remotely=ecf_files_remotely,
            )
            args = f"tasknr={tasknr};ntasks={ntasks}"
            if extra_args is not None:
                args += f";{extra_args}"
            EcflowSuiteTask(
                taskname,
                subfamily,
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
            iomerge_trigger = EcflowSuiteTriggers(
                [
                    EcflowSuiteTrigger(forecast_task, "active"),
                    EcflowSuiteTrigger(forecast_task, "complete"),
                ],
                mode="OR",
            )
            io_merge = SubTaskFamily(
                self,
                config,
                task_settings,
                input_template,
                ecf_files,
                "IOmerge",
                n_io_merge,
                extra_args=f"nproc_io={nproc_io}",
                trigger=iomerge_trigger,
                ecf_files_remotely=ecf_files_remotely,
            )
            add_calc_fields_trigger = io_merge
            creategrib_trigger = io_merge

        if len(config.get("creategrib.CreateGrib.conversions", [])) > 0:
            create_grib_family = SubTaskFamily(
                self,
                config,
                task_settings,
                input_template,
                ecf_files,
                "CreateGrib",
                config.get("suite_control.n_creategrib", 1),
                trigger=creategrib_trigger,
                ecf_files_remotely=ecf_files_remotely,
            )
            add_calc_fields_trigger = create_grib_family

        add_calc_fields_family = SubTaskFamily(
            self,
            config,
            task_settings,
            input_template,
            ecf_files,
            "AddCalculatedFields",
            config.get("suite_control.n_addcalculatedfields", 1),
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
                trigger=add_calc_fields_family,
                ecf_files_remotely=ecf_files_remotely,
            )

        databridge_sel = config.get("archiving.DataBridge.fdb", {})
        databridge_archiving_active = [v["active"] for v in databridge_sel.values()]
        if any(databridge_archiving_active):
            EcflowSuiteTask(
                "ArchiveDataBridge",
                self,
                config,
                task_settings,
                ecf_files,
                input_template=input_template,
                trigger=add_calc_fields_family,
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
                trigger=add_calc_fields_family,
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

        EcflowSuiteTask(
            "CollectLogsHour",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
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

            check_globaldt_date = check_offline_date = path_globaldt = path_offline = None

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

                if self.has_mirror_family(config):
                    MirrorFamily(
                        time_family,
                        config,
                        task_settings,
                        input_template,
                        ecf_files,
                        ecf_files_remotely=ecf_files_remotely,
                        cycle_valid=cycle.validtime,
                    )

                    if self.active_mirror(config, "globalDT"):
                        path_globaldt = "{0}/{1}/{2}/Mirrors/{3}".format(
                            parent.path,
                            cycle.day,
                            cycle.time,
                            config["scheduler.mirror_globalDT"]["remote_path"].split("/")[
                                -1
                            ],
                        )

                        if config["scheduler.mirror_globalDT"]["check_var"]:
                            check_globaldt_date = {
                                config["scheduler.mirror_globalDT"][
                                    "check_var"
                                ]: cycle.day
                            }

                    if self.active_mirror(config, "offline"):
                        path_offline = "{0}/{1}/{2}/Mirrors/{3}".format(
                            parent.path,
                            cycle.day,
                            cycle.time,
                            config["scheduler.mirror_offline"]["remote_path"].split("/")[
                                -1
                            ],
                        )
                        if config["scheduler.mirror_offline"]["check_var"]:
                            check_offline_date = {
                                config["scheduler.mirror_offline"][
                                    "check_var"
                                ]: "{0}/{1}".format(cycle.day, cycle.time)
                            }

                    if self.active_mirror(config, "host_case"):
                        path_offline = "{0}/{1}/{2}/Mirrors/{3}".format(
                            parent.path,
                            cycle.day,
                            cycle.time,
                            config["scheduler.mirror_host_case"]["remote_path"].split(
                                "/"
                            )[-1],
                        )
                        logger.info(config["scheduler.mirror_host_case"]["check_var"])
                        if config["scheduler.mirror_host_case"]["check_var"]:
                            check_offline_date = {
                                config["scheduler.mirror_host_case"][
                                    "check_var"
                                ]: "{0}/{1}".format(cycle.day, "0000")
                            }

                inputdata = InputDataFamily(
                    time_family,
                    config,
                    task_settings,
                    input_template,
                    ecf_files,
                    trigger=trigger,
                    ecf_files_remotely=ecf_files_remotely,
                    external_marsprep_trigger_node=external_marsprep_trigger_nodes,
                    add_var_trigger=check_globaldt_date,
                    remote_path=path_globaldt,
                )
                ready_for_cycle = inputdata

            member_families: List[EcflowSuiteFamily] = []
            member_cycle_families: List[EcflowSuiteFamily] = []
            for member in config["eps.general.members"]:
                member_family = EcflowSuiteFamily(
                    f"mbr{member:03d}",
                    time_family,
                    ecf_files,
                    variables={"MEMBER": member},
                    ecf_files_remotely=ecf_files_remotely,
                )
                member_families.append(member_family)

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
                    ready_for_cycle = inputdata

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
                        add_var_trigger=check_offline_date,
                        remote_path=path_offline,
                    )

                    ready_for_cycle = prev_interpolation_triggers[member] = int_family

                prev_cycle_trigger = prev_cycle_triggers.get(member)
                if prev_cycle_trigger:
                    ready_for_cycle = (
                        [*ready_for_cycle, *prev_cycle_trigger]
                        if isinstance(ready_for_cycle, list)
                        else [ready_for_cycle, *prev_cycle_trigger]
                    )

                cycle_family = CycleFamily(
                    member_family,
                    config,
                    task_settings,
                    input_template,
                    ecf_files,
                    trigger=ready_for_cycle,
                    ecf_files_remotely=ecf_files_remotely,
                )
                member_cycle_families.append(cycle_family)
                prev_cycle_triggers[member] = [cycle_family]

                postcycle_families[member] = PostCycleFamily(
                    member_family,
                    config,
                    task_settings,
                    input_template,
                    ecf_files,
                    trigger=cycle_family,
                    external_cycle_cleaning_trigger=postcycle_families.get(member),
                    ecf_files_remotely=ecf_files_remotely,
                )

            if (
                config["suite_control.do_extractsqlite"]
                and config["suite_control.do_mergesqlite"]
                and len(config["eps.general.members"]) > 1
            ):
                MergeSQLitesFamily(
                    time_family,
                    config,
                    task_settings,
                    ecf_files,
                    trigger=member_families,
                    input_template=input_template,
                    ecf_files_remotely=ecf_files_remotely,
                )

            if len(get_impact(config, "StartImpactModels")) > 0:
                EcflowSuiteTask(
                    "StartImpactModels",
                    time_family,
                    config,
                    task_settings,
                    ecf_files,
                    input_template=input_template,
                    trigger=member_cycle_families,
                    ecf_files_remotely=ecf_files_remotely,
                )

    @property
    def last_node(self):
        """Return the last family node of self."""
        return self._last_node

    def active_mirror(self, config, tag):
        """Detects active mirror from a given tag."""
        return (
            config[f"suite_control.mirror_{tag}"] and f"scheduler.mirror_{tag}" in config
        )

    def has_mirror_family(self, config):
        """Detects if we have any active mirrors."""
        mirror_options = ("globalDT", "offline", "host_case")
        return any(
            (config[f"suite_control.mirror_{x}"] and f"scheduler.mirror_{x}" in config)
            for x in mirror_options
        )


class MergeSQLitesFamily(EcflowSuiteFamily):
    """Class for creating the MergeSQLites ecFlow family."""

    def __init__(
        self,
        parent,
        config,
        task_settings: TaskSettings,
        ecf_files,
        trigger=None,
        input_template=None,
        ecf_files_remotely=None,
    ):
        """Class initialization."""
        super().__init__(
            "MergeSQLites",
            parent,
            ecf_files,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )
        merge_sqlites = EcflowSuiteTask(
            "MergeSQLites",
            self,
            config,
            task_settings,
            ecf_files,
            input_template=input_template,
            trigger=trigger,
            ecf_files_remotely=ecf_files_remotely,
        )
        EcflowSuiteTask(
            "ArchiveMergedSQLites",
            self,
            config,
            task_settings,
            ecf_files,
            trigger=merge_sqlites,
            input_template=input_template,
            ecf_files_remotely=ecf_files_remotely,
        )
