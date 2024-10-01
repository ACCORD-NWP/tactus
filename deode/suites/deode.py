"""Ecflow suites."""

from pathlib import Path

from ..datetime_utils import (
    as_datetime,
    as_timedelta,
    get_decadal_list,
    get_decade,
    get_month_list,
)
from ..logs import logger
from ..os_utils import deodemakedirs
from ..submission import ProcessorLayout
from .base import (
    EcflowSuiteFamily,
    EcflowSuiteTask,
    EcflowSuiteTrigger,
    EcflowSuiteTriggers,
    SuiteDefinition,
)


class DeodeSuiteDefinition(SuiteDefinition):
    """Definition of suite."""

    def __init__(
        self,
        config,
        dry_run=False,
    ):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the definition.

        Args:
            config (deode.ParsedConfig): Configuration file
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.

        Raises:
            ModuleNotFoundError: If ecflow is not loaded and not dry_run

        """
        SuiteDefinition.__init__(self, config, dry_run=dry_run)

        self.config = config

        self.csc = config["general.csc"]

        self.do_cleaning = config["suite_control.do_cleaning"]
        self.create_static_data = config["suite_control.create_static_data"]
        self.do_soil = config["suite_control.do_soil"]
        self.do_pgd = config["suite_control.do_pgd"]
        self.one_decade = config["pgd.one_decade"]
        self.create_time_dependent_suite = config[
            "suite_control.create_time_dependent_suite"
        ]
        self.interpolate_boundaries = config["suite_control.interpolate_boundaries"]
        self.do_marsprep = config["suite_control.do_marsprep"]

        settings = self.task_settings.get_settings("Forecast")
        procs = ProcessorLayout(settings).get_proc_dict()
        self.nproc_io = procs["nproc_io"] if procs["nproc_io"] is not None else 0
        if self.nproc_io > 0:
            self.n_io_merge = config["suite_control.n_io_merge"]
        else:
            self.n_io_merge = 0
        self.do_extractsqlite = config["suite_control.do_extractsqlite"]
        self.do_archiving = config["suite_control.do_archiving"]
        self.surfex = config["general.surfex"]
        self.suite_name = config["general.case"]
        self.mode = config["suite_control.mode"]
        self.split_mars = config["suite_control.split_mars"]

        self.creategrib = len(config.get("task.CreateGrib.conversions", [])) > 0

        unix_group = self.platform.get_platform_value("unix_group")
        deodemakedirs(self.joboutdir, unixgroup=unix_group)

        input_template = (
            Path(__file__).parent.resolve() / "../templates/ecflow/default.py"
        )
        input_template = input_template.as_posix()

        # set max_ecf_tasks from config
        max_ecf_tasks = -1
        try:
            max_ecf_tasks = self.config["submission.max_ecf_tasks"]
        except KeyError:
            max_ecf_tasks = -1

        if max_ecf_tasks > 0 and self.suite.ecf_node is not None:
            self.suite.ecf_node.add_limit("max_ecf_tasks", max_ecf_tasks)
            self.suite.ecf_node.add_inlimit(
                "max_ecf_tasks", f"/{self.suite_name}", max_ecf_tasks
            )

        self.do_prep = True
        if self.mode == "restart":
            self.do_prep = False
            self.create_static_data = False

        final_cleaning_trigger = [None]
        if self.do_cleaning:
            initial_cleaning = EcflowSuiteTask(
                "PreCleaning",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            final_cleaning_trigger = [EcflowSuiteTrigger(initial_cleaning)]

        static_data = final_cleaning_trigger
        if self.create_static_data:
            input_trigger = EcflowSuiteTriggers(final_cleaning_trigger)
            static_data = self.static_suite_part(config, input_template, input_trigger)
            task_logs = config["system.climdir"]
            args = ";".join(
                [
                    f"joboutdir={self.ecf_out}/{self.suite_name}/StaticData",
                    "tarname=StaticData",
                    f"task_logs={task_logs}",
                ]
            )
            variables = {"ARGS": args}

            collect_logs = EcflowSuiteTask(
                "CollectLogs",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=EcflowSuiteTriggers(static_data),
                variables=variables,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            final_cleaning_trigger = static_data

            if self.do_archiving:
                archiving_static_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(collect_logs)]
                )
                archive_static = EcflowSuiteTask(
                    "ArchiveStatic",
                    self.suite,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables=None,
                    trigger=archiving_static_trigger,
                )
                final_cleaning_trigger = [EcflowSuiteTrigger(archive_static)]
            else:
                archive_static = None

        time_dependent_part = []
        if self.create_time_dependent_suite:
            time_dependent_part = self.time_dependent_suite_part(
                config, input_template, static_data
            )

        if self.do_cleaning:
            if time_dependent_part is not None:
                final_cleaning_trigger += time_dependent_part
            trigger = EcflowSuiteTriggers(final_cleaning_trigger)

            EcflowSuiteTask(
                "PostMortem",
                self.suite,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )

    def time_dependent_suite_part(self, config, input_template, static_data):
        """Create the time dependent part of the suite.

        Args:
            config (deode.ParsedConfig): Configuration settings
            input_template (str): Default task template
            static_data(EcflowFamily): EcflowFamily object used for triggering

        Returns:
            prev_cycle_trigger (list) : List of triggers to be used by subsequent tasks

        """
        first_cycle = as_datetime(config["general.times.start"])
        last_cycle = as_datetime(config["general.times.end"])
        cycle_length = as_timedelta(config["general.times.cycle_length"])
        cycles = {}
        cycle_time = first_cycle
        i = 0

        while cycle_time <= last_cycle:
            logger.debug("cycle_time {}", cycle_time)
            cycles.update(
                {
                    str(i): {
                        "day": cycle_time.strftime("%Y%m%d"),
                        "time": cycle_time.strftime("%H%M"),
                        "validtime": cycle_time.strftime("%Y-%m-%dT%H:%M:%SZ"),
                        "basetime": cycle_time.strftime("%Y-%m-%dT%H:%M:%SZ"),
                    }
                }
            )
            i = i + 1
            cycle_time = cycle_time + cycle_length

        days = []
        prev_cycle_trigger = None
        prev_interpolation_trigger = None
        cycle_cleaning = None

        for i, cycle in cycles.items():
            cycle_day = cycle["day"]
            inputdata_trigger = EcflowSuiteTriggers(static_data)

            if cycle_day not in days:
                day_family = EcflowSuiteFamily(
                    cycle["day"],
                    self.suite,
                    self.ecf_files,
                    trigger=inputdata_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )
                days.append(cycle_day)

            time_variables = {
                "BASETIME": cycle["basetime"],
                "VALIDTIME": cycle["validtime"],
            }
            time_family = EcflowSuiteFamily(
                cycle["time"],
                day_family,
                self.ecf_files,
                trigger=inputdata_trigger,
                variables=time_variables,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            inputdata = EcflowSuiteFamily(
                "InputData",
                time_family,
                self.ecf_files,
                trigger=inputdata_trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            inputdata_done = EcflowSuiteTriggers([EcflowSuiteTrigger(inputdata)])
            prepare_cycle = EcflowSuiteTask(
                "PrepareCycle",
                inputdata,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
            )
            triggers = [EcflowSuiteTrigger(prepare_cycle)]

            if prev_interpolation_trigger is not None:
                triggers = triggers + prev_interpolation_trigger
            ready_for_marsprep = EcflowSuiteTriggers(triggers)
            if self.do_marsprep and self.interpolate_boundaries and not self.split_mars:
                EcflowSuiteTask(
                    "Marsprep",
                    inputdata,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables=None,
                    trigger=ready_for_marsprep,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

            if self.interpolate_boundaries:
                int_fam = EcflowSuiteFamily(
                    f'{"Interpolation"}',
                    time_family,
                    self.ecf_files,
                    trigger=inputdata_done,
                    variables=None,
                    ecf_files_remotely=self.ecf_files_remotely,
                )
                prev_interpolation_trigger = [EcflowSuiteTrigger(int_fam)]

                basetime = as_datetime(cycle["basetime"])
                forecast_range = as_timedelta(config["general.times.forecast_range"])
                endtime = basetime + forecast_range
                bdint = as_timedelta(config["boundaries.bdint"])
                bdmax = config["boundaries.bdtasks_per_batch"]

                intnr = 1
                args = ""
                int_trig = inputdata_done

                inthourbdint = int(bdint.total_seconds() // 3600)
                inthourbdintx = 1 if inthourbdint == 0 else inthourbdint
                intminbdint = int(bdint.total_seconds() % 3600 // 60)
                intsecbdint = int(bdint.total_seconds() % 60)

                # we don't need LBC000 if this is not first cycle or mode != cold_start
                if self.mode == "restart" or (self.mode == "start" and not self.do_prep):
                    bdtime = basetime + bdint
                    bdnr = inthourbdint
                    bd_nr = 1
                    subbdnr = intminbdint if (intminbdint or intsecbdint) else None
                    subminbdnr = intsecbdint if intsecbdint else None

                else:
                    bdtime = basetime
                    bdnr = 0
                    bd_nr = 0
                    subbdnr = 0 if (intminbdint or intsecbdint) else None
                    subminbdnr = 0 if intsecbdint else None

                e923_update_done = None

                split_mars_done = None

                if self.do_prep:
                    prep_fam = EcflowSuiteFamily(
                        "Prep",
                        int_fam,
                        self.ecf_files,
                        trigger=int_trig,
                        variables=None,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )
                    if self.split_mars:
                        args = f"bd_nr={bdnr};prep_step=True"
                        variables = {"ARGS": args}
                        split_mars_task = EcflowSuiteTask(
                            "marsprep",
                            prep_fam,
                            config,
                            self.task_settings,
                            self.ecf_files,
                            input_template=input_template,
                            variables=variables,
                            trigger=None,
                            ecf_files_remotely=self.ecf_files_remotely,
                        )

                        split_mars_done = EcflowSuiteTriggers(
                            [EcflowSuiteTrigger(split_mars_task)]
                        )

                    prep_task = EcflowSuiteTask(
                        "Prep",
                        prep_fam,
                        config,
                        self.task_settings,
                        self.ecf_files,
                        input_template=input_template,
                        trigger=split_mars_done,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )
                    prep_done = EcflowSuiteTriggers([EcflowSuiteTrigger(prep_task)])

                    if self.csc == "ALARO":
                        e923_update_task = EcflowSuiteTask(
                            "E923Update",
                            int_fam,
                            config,
                            self.task_settings,
                            self.ecf_files,
                            trigger=prep_done,
                            input_template=input_template,
                            ecf_files_remotely=self.ecf_files_remotely,
                        )
                        e923_update_done = EcflowSuiteTriggers(
                            [EcflowSuiteTrigger(e923_update_task)]
                        )

                    if self.mode != "cold_start" or self.csc == "ALARO":
                        self.do_prep = False

                if self.csc == "ALARO":
                    try:
                        next_cycle = cycles[str(int(i) + 1)]
                        if as_datetime(cycle["day"]).strftime("%m") != as_datetime(
                            next_cycle["day"]
                        ).strftime("%m"):
                            self.do_prep = True
                    except KeyError:
                        logger.debug("It is last cycle")

                while bdtime <= endtime:
                    bch_fam = EcflowSuiteFamily(
                        f"Batch{intnr:02}",
                        int_fam,
                        self.ecf_files,
                        trigger=int_trig,
                        variables=None,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )
                    while bdtime <= endtime:
                        date_string = bdtime.isoformat(sep="T").replace("+00:00", "Z")
                        args = f"bd_time={date_string};bd_nr={bd_nr};prep_step=False"
                        variables = {"ARGS": args}
                        lbc_fam_name = (
                            f"LBC{bdnr*inthourbdintx:02}{subbdnr:02}"
                            + (f"{subminbdnr:02}" if subminbdnr is not None else "")
                            if subbdnr is not None
                            else f"LBC{bdnr*inthourbdintx:02}"
                        )
                        lbc_fam = EcflowSuiteFamily(
                            lbc_fam_name,
                            bch_fam,
                            self.ecf_files,
                            trigger=e923_update_done,
                            variables=None,
                            ecf_files_remotely=self.ecf_files_remotely,
                        )

                        interpolation_task = "C903" if self.do_marsprep else "E927"

                        if self.split_mars:
                            split_mars_task = EcflowSuiteTask(
                                "Marsprep",
                                lbc_fam,
                                config,
                                self.task_settings,
                                self.ecf_files,
                                input_template=input_template,
                                variables=variables,
                                trigger=None,
                                ecf_files_remotely=self.ecf_files_remotely,
                            )

                            split_mars_done = EcflowSuiteTriggers(
                                [EcflowSuiteTrigger(split_mars_task)]
                            )

                        EcflowSuiteTask(
                            interpolation_task,
                            lbc_fam,
                            config,
                            self.task_settings,
                            self.ecf_files,
                            input_template=input_template,
                            variables=variables,
                            trigger=split_mars_done,
                            ecf_files_remotely=self.ecf_files_remotely,
                        )
                        if subbdnr is not None:
                            subbdnr += intminbdint
                            if subminbdnr is not None:
                                subminbdnr += intsecbdint
                                if subminbdnr >= 60:
                                    subbdnr += 1
                                    subminbdnr -= 60
                            if subbdnr >= 60:
                                bdnr += 1
                                subbdnr -= 60

                        bdnr += inthourbdint
                        bdtime += bdint
                        bd_nr += 1
                        if bd_nr % bdmax == 0:
                            intnr += 1
                            int_trig = EcflowSuiteTriggers([EcflowSuiteTrigger(bch_fam)])
                            break

                int_trig = EcflowSuiteTriggers([EcflowSuiteTrigger(int_fam)])
            else:
                int_trig = inputdata_done

            cycle_fam = EcflowSuiteFamily(
                "Cycle",
                time_family,
                self.ecf_files,
                trigger=int_trig,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            triggers = [EcflowSuiteTrigger(inputdata)]
            if prev_cycle_trigger is not None:
                triggers = triggers + prev_cycle_trigger
            ready_for_cycle = EcflowSuiteTriggers(triggers)
            prev_cycle_trigger = [EcflowSuiteTrigger(cycle_fam)]
            initialization = EcflowSuiteFamily(
                "Initialization",
                cycle_fam,
                self.ecf_files,
                trigger=ready_for_cycle,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            EcflowSuiteTask(
                "FirstGuess",
                initialization,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=None,
                variables=None,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            forecast_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(initialization)])
            forecasting = EcflowSuiteFamily(
                "Forecasting",
                cycle_fam,
                self.ecf_files,
                trigger=forecast_trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            logger.debug(self.task_settings.get_task_settings("Forecast"))

            forecast_task = EcflowSuiteTask(
                "Forecast",
                forecasting,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            if self.n_io_merge > 0:
                # these tasks should trigger when the Forecast is *running*
                iomerge_trigger = EcflowSuiteTriggers(
                    [
                        EcflowSuiteTrigger(forecast_task, "active"),
                        EcflowSuiteTrigger(forecast_task, "complete"),
                    ],
                    mode="OR",
                )
                iomerge_family = EcflowSuiteFamily(
                    "Merge_IO",
                    forecasting,
                    self.ecf_files,
                    ecf_files_remotely=self.ecf_files_remotely,
                    trigger=iomerge_trigger,
                    variables=None,
                )
                for ionr in range(self.n_io_merge):
                    iomerge_sub = EcflowSuiteFamily(
                        f"IO_{ionr:02}",
                        iomerge_family,
                        self.ecf_files,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )
                    args = f"ionr={ionr};nproc_io={self.nproc_io}"
                    EcflowSuiteTask(
                        "IOmerge",
                        iomerge_sub,
                        config,
                        self.task_settings,
                        self.ecf_files,
                        input_template=input_template,
                        variables={"ARGS": args},
                        ecf_files_remotely=self.ecf_files_remotely,
                    )

            if self.creategrib:
                creategrib_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(forecast_task)]
                )
                EcflowSuiteTask(
                    "CreateGrib",
                    forecasting,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    trigger=creategrib_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

            if self.do_extractsqlite:
                extractsqlite_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(forecast_task)]
                )
                EcflowSuiteTask(
                    "ExtractSQLite",
                    forecasting,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    trigger=extractsqlite_trigger,
                )

            postcycle_family = EcflowSuiteFamily(
                "PostCycle",
                time_family,
                self.ecf_files,
                trigger=EcflowSuiteTriggers([EcflowSuiteTrigger(cycle_fam)]),
                ecf_files_remotely=self.ecf_files_remotely,
            )

            cday = cycle["day"]
            ctime = cycle["time"]
            task_logs = config["system.wrk"]
            args = ";".join(
                [
                    f"joboutdir={self.ecf_out}/{self.suite_name}/{cday}/{ctime}",
                    f"tarname={cday}_{ctime}",
                    f"task_logs={task_logs}",
                ]
            )
            variables = {"ARGS": args}
            collect_logs_hour = EcflowSuiteTask(
                "CollectLogs",
                postcycle_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=variables,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            if self.do_archiving:
                archiving_hour_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(collect_logs_hour)]
                )

                archive_hour = EcflowSuiteTask(
                    "ArchiveHour",
                    postcycle_family,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    trigger=archiving_hour_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

            if self.do_cleaning:
                cleaning_triggers = triggers
                if self.do_archiving:
                    cleaning_triggers.append(EcflowSuiteTrigger(archive_hour))
                if cycle_cleaning is not None:
                    cleaning_triggers.append(EcflowSuiteTrigger(cycle_cleaning))
                cleaning_trigger = EcflowSuiteTriggers(cleaning_triggers)

                cycle_cleaning = EcflowSuiteTask(
                    "CycleCleaning",
                    postcycle_family,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    trigger=cleaning_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

        prev_cycle_trigger.append(EcflowSuiteTrigger(cycle_cleaning))
        return prev_cycle_trigger

    def static_suite_part(self, config, input_template, input_trigger):
        """Create the time dependent part of the suite.

        Args:
            config (deode.ParsedConfig): Configuration settings
            input_template (str): Default task template
            input_trigger (list): List of trigger objects for the static family

        Returns:
            static_data (list) : List of trigger objects to be used by subsequent tasks

        """
        static_data = EcflowSuiteFamily(
            "StaticData",
            self.suite,
            self.ecf_files,
            trigger=input_trigger,
            ecf_files_remotely=self.ecf_files_remotely,
        )

        if self.do_pgd:
            pgd_input = EcflowSuiteFamily(
                "PgdInput",
                static_data,
                self.ecf_files,
                ecf_files_remotely=self.ecf_files_remotely,
            )
            EcflowSuiteTask(
                "Gmted",
                pgd_input,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            if self.do_soil:
                EcflowSuiteTask(
                    "Soil",
                    pgd_input,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables=None,
                )

            pgd_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(pgd_input)])

            if self.one_decade:
                pgd_family = EcflowSuiteFamily(
                    "Pgd",
                    static_data,
                    self.ecf_files,
                    trigger=pgd_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )
                decade_dates = get_decadal_list(
                    as_datetime(config["general.times.start"]),
                    as_datetime(config["general.times.end"]),
                )

                for dec_date in decade_dates:
                    decade_pgd_family = EcflowSuiteFamily(
                        f"decade_{get_decade(dec_date)}",
                        pgd_family,
                        self.ecf_files,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )

                    pgd = EcflowSuiteTask(
                        "Pgd",
                        decade_pgd_family,
                        config,
                        self.task_settings,
                        self.ecf_files,
                        input_template=input_template,
                        variables={"ARGS": f"basetime={dec_date}"},
                        ecf_files_remotely=self.ecf_files_remotely,
                    )

                e923_constant_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(pgd_family)]
                )
            else:
                basetime = as_datetime(self.config["general.times.basetime"])
                pgd = EcflowSuiteTask(
                    "Pgd",
                    static_data,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables={"ARGS": f"basetime={basetime}"},
                    trigger=pgd_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )
                e923_constant_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(pgd)])
        else:
            e923_constant_trigger = None

        e923constant = EcflowSuiteTask(
            "E923Constant",
            static_data,
            config,
            self.task_settings,
            self.ecf_files,
            input_template=input_template,
            variables=None,
            trigger=e923_constant_trigger,
            ecf_files_remotely=self.ecf_files_remotely,
        )

        e923_monthly_family_trigger = EcflowSuiteTriggers(
            [EcflowSuiteTrigger(e923constant)]
        )
        e923_monthly_family = EcflowSuiteFamily(
            "E923Monthly",
            static_data,
            self.ecf_files,
            trigger=e923_monthly_family_trigger,
            ecf_files_remotely=self.ecf_files_remotely,
        )

        seasons = {
            "Q1": "01,02,03",
            "Q2": "04,05,06",
            "Q3": "07,08,09",
            "Q4": "10,11,12",
        }

        if self.one_decade:
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
                e923_monthly_family,
                self.ecf_files,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            EcflowSuiteTask(
                "E923Monthly",
                month_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables={"ARGS": f"months={months}"},
                ecf_files_remotely=self.ecf_files_remotely,
            )

        if self.do_pgd:
            pgd_update_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(e923constant)])

            if self.one_decade:
                pgd_update_family = EcflowSuiteFamily(
                    "PgdUpdate",
                    static_data,
                    self.ecf_files,
                    trigger=pgd_update_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

                decade_dates = get_decadal_list(
                    as_datetime(config["general.times.start"]),
                    as_datetime(config["general.times.end"]),
                )

                for dec_date in decade_dates:
                    decade_pgdupdate_family = EcflowSuiteFamily(
                        f"decade_{get_decade(dec_date)}",
                        pgd_update_family,
                        self.ecf_files,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )

                    EcflowSuiteTask(
                        "PgdUpdate",
                        decade_pgdupdate_family,
                        config,
                        self.task_settings,
                        self.ecf_files,
                        input_template=input_template,
                        variables={"ARGS": f"basetime={dec_date}"},
                        trigger=pgd_update_trigger,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )

            else:
                basetime = as_datetime(self.config["general.times.basetime"])
                EcflowSuiteTask(
                    "PgdUpdate",
                    static_data,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables={"ARGS": f"basetime={basetime}"},
                    trigger=pgd_update_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

        return [EcflowSuiteTrigger(static_data)]

    def save_as_defs(self, def_file):
        """Save definition file.

        Args:
            def_file (str): Name of definition file
        """
        self.suite.save_as_defs(def_file)
