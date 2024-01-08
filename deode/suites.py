"""Ecflow suites."""

import os
from pathlib import Path

from .datetime_utils import as_datetime, as_timedelta
from .logs import LogDefaults, logger
from .os_utils import deodemakedirs
from .toolbox import Platform

try:
    import ecflow
except ImportError:
    ecflow = None


class SuiteDefinition(object):
    """Definition of suite."""

    def __init__(
        self,
        suite_name,
        config,
        task_settings,
        ecf_home=None,
        ecf_files_remotely=None,
        ecf_include=None,
        ecf_out=None,
        ecf_jobout=None,
        ecf_micro="%",
        dry_run=False,
    ):
        # TODO: Document the variables that right now only are described as "?"
        """Construct the definition.

        Args:
            suite_name (str): Name of suite
            joboutdir (str): Path to jobfiles
            ecf_files (str): Path to ecflow containers
            task_settings (TaskSettings): Submission configuration
            config (deode.ParsedConfig): Configuration file
            ecf_home (str, optional): ECF_HOME. Defaults to None.
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server
            ecf_include (str, optional): ECF_INCLUDE.
                                         Defaults to None which uses ecf_files.
            ecf_out (str, optional): ECF_OUT. Defaults to None.
            ecf_jobout (str, optional): ECF_JOBOUT. Defaults to None.
            ecf_micro (str, optional): ECF_MICRO. Defaults to %
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.

        Raises:
            ModuleNotFoundError: If ecflow is not loaded and not dry_run

        """
        if ecflow is None and not dry_run:
            raise ModuleNotFoundError("Ecflow not found")

        self.platform = Platform(config)
        self.config = config

        self.create_static_data = config["suite_control.create_static_data"]
        self.do_soil = config["suite_control.do_soil"]
        self.do_pgd = config["suite_control.do_pgd"]
        self.one_decade = config["pgd.one_decade"]
        self.create_time_dependent_suite = config[
            "suite_control.create_time_dependent_suite"
        ]
        self.interpolate_boundaries = config["suite_control.interpolate_boundaries"]
        self.do_prep = config["suite_control.do_prep"]
        self.do_marsprep = config["suite_control.do_marsprep"]
        self.do_extractsqlite = config["suite_control.do_extractsqlite"]
        self.do_archiving = config["suite_control.do_archiving"]
        self.surfex = config["general.surfex"]
        self.suite_name = suite_name
        self.mode = config["suite_control.mode"]

        ecf_files = self.config["scheduler.ecfvars.ecf_files"]
        joboutdir = self.config["scheduler.ecfvars.ecf_jobout"]

        self.creategrib = bool("task.creategrib" in config)

        name = suite_name
        self.joboutdir = joboutdir
        if ecf_include is None:
            ecf_include = ecf_files
        self.ecf_include = ecf_include
        self.ecf_files = ecf_files
        if ecf_home is None:
            ecf_home = joboutdir
        self.ecf_home = ecf_home
        if ecf_out is None:
            ecf_out = joboutdir
        self.ecf_out = ecf_out
        self.ecf_micro = ecf_micro
        if ecf_jobout is None:
            ecf_jobout = (
                joboutdir
                + f"/{ecf_micro}ECF_NAME{ecf_micro}.{ecf_micro}ECF_TRYNO{ecf_micro}"
            )
        self.ecf_jobout = ecf_jobout
        self.ecf_files_remotely = ecf_files_remotely
        if ecf_files_remotely is None:
            self.ecf_files_remotely = self.ecf_files
        self.task_settings = task_settings

        # Commands started from the scheduler does not have full environment
        ecf_job_cmd = (
            f"{ecf_micro}TROIKA{ecf_micro} "
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} submit "
            f"-o {ecf_micro}ECF_JOBOUT{ecf_micro} "
            f"{ecf_micro}SCHOST{ecf_micro} "
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        )
        # %ECF_JOB%"
        self.ecf_job_cmd = ecf_job_cmd
        ecf_status_cmd = (
            f"{ecf_micro}TROIKA{ecf_micro} "
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} monitor "
            f"{ecf_micro}SCHOST{ecf_micro} "
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        )
        self.ecf_status_cmd = ecf_status_cmd
        ecf_kill_cmd = (
            f"{ecf_micro}TROIKA{ecf_micro} "
            f"-c {ecf_micro}TROIKA_CONFIG{ecf_micro} kill "
            f"{ecf_micro}SCHOST{ecf_micro} "
            f"{ecf_micro}ECF_JOB{ecf_micro}"
        )
        self.ecf_kill_cmd = ecf_kill_cmd

        troika = "troika"

        platform = Platform(config)
        troika_config = platform.get_value("troika.config_file")
        config_file = config.metadata["source_file_path"]
        first_cycle = as_datetime(config["general.times.start"])
        deode_home = platform.get_platform_value("DEODE_HOME")

        unix_group = platform.get_platform_value("unix_group")
        deodemakedirs(self.joboutdir, unixgroup=unix_group)

        keep_workdirs = "1" if config["general.keep_workdirs"] else "0"
        loglevel = config.get("general.loglevel", LogDefaults.LEVEL).upper()
        variables = {
            "ECF_EXTN": ".py",
            "ECF_FILES": self.ecf_files_remotely,
            "ECF_INCLUDE": self.ecf_include,
            "ECF_TRIES": 1,
            "ECF_HOME": self.ecf_home,
            "ECF_KILL_CMD": self.ecf_kill_cmd,
            "ECF_JOB_CMD": self.ecf_job_cmd,
            "ECF_STATUS_CMD": self.ecf_status_cmd,
            "ECF_OUT": self.ecf_out,
            "ECF_JOBOUT": self.ecf_jobout,
            "ECF_TIMEOUT": 20,
            "ARGS": "",
            "LOGLEVEL": loglevel,
            "CONFIG": str(config_file),
            "TROIKA": troika,
            "TROIKA_CONFIG": troika_config,
            "BASETIME": first_cycle.isoformat(timespec="seconds"),
            "VALIDTIME": first_cycle.isoformat(timespec="seconds"),
            "DEODE_HOME": deode_home,
            "NPROC": "",
            "NPROC_IO": "",
            "NPROCX": "",
            "NPROCY": "",
            "KEEP_WORKDIRS": keep_workdirs,
        }

        input_template = Path(__file__).parent.resolve() / "templates/ecflow/default.py"
        input_template = input_template.as_posix()
        self.suite = EcflowSuite(
            name,
            ecf_files,
            variables=variables,
            dry_run=dry_run,
            ecf_files_remotely=self.ecf_files_remotely,
        )

        if self.mode == "restart":
            self.do_prep = False
            self.create_static_data = False

        if self.create_static_data:
            static_data = self.static_suite_part(config, input_template)
            task_logs = config["system.climdir"]
            args = ";".join(
                [
                    f"joboutdir={self.joboutdir}/{self.suite_name}/StaticData",
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
                trigger=EcflowSuiteTriggers([EcflowSuiteTrigger(static_data)]),
                variables=variables,
                ecf_files_remotely=self.ecf_files_remotely,
            )

            if self.do_archiving:
                archiving_static_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(collect_logs)]
                )
                EcflowSuiteTask(
                    "ArchiveStatic",
                    self.suite,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    variables=None,
                    trigger=archiving_static_trigger,
                )
        else:
            static_data = None

        if self.create_time_dependent_suite:
            self.time_dependent_suite_part(config, input_template, static_data)

    def time_dependent_suite_part(self, config, input_template, static_data):
        """Create the time dependent part of the suite.

        Args:
            config (deode.ParsedConfig): Configuration settings
            input_template (str): Default task template
            static_data(EcflowFamily): EcflowFamily object used for triggering

        """
        first_cycle = as_datetime(config["general.times.start"])
        last_cycle = as_datetime(config["general.times.end"])
        cycle_length = as_timedelta(config["general.times.cycle_length"])
        cycles = {}
        cycle_time = first_cycle
        i = 0
        if self.mode == "restart":
            self.do_prep = False

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

        for cycle in cycles.values():
            cycle_day = cycle["day"]
            if self.create_static_data:
                inputdata_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(static_data)])
            else:
                inputdata_trigger = None

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
            if self.do_marsprep:
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

            if not self.surfex:
                self.do_prep = False

            if self.interpolate_boundaries or self.do_prep:
                int_fam = EcflowSuiteFamily(
                    f'{"Interpolation"}',
                    time_family,
                    self.ecf_files,
                    trigger=inputdata_done,
                    variables=None,
                    ecf_files_remotely=self.ecf_files_remotely,
                )
                prev_interpolation_trigger = [EcflowSuiteTrigger(int_fam)]

                if self.do_prep:
                    EcflowSuiteTask(
                        "Prep",
                        int_fam,
                        config,
                        self.task_settings,
                        self.ecf_files,
                        input_template=input_template,
                        ecf_files_remotely=self.ecf_files_remotely,
                    )

                    if self.mode != "cold_start":
                        self.do_prep = False

                if self.interpolate_boundaries:
                    basetime = as_datetime(cycle["basetime"])
                    forecast_range = as_timedelta(config["general.times.forecast_range"])
                    endtime = basetime + forecast_range
                    bdint = as_timedelta(config["boundaries.bdint"])
                    bdmax = config["boundaries.bdtasks_per_batch"]

                    bdnr = 0
                    intnr = 1
                    args = ""
                    int_trig = inputdata_done
                    bdtime = basetime
                    intbdint = int(bdint.total_seconds() // 3600)
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
                            args = f"bd_time={date_string};bd_nr={bdnr}"
                            variables = {"ARGS": args}
                            lbc_fam = EcflowSuiteFamily(
                                f"LBC{bdnr*intbdint:02}",
                                bch_fam,
                                self.ecf_files,
                                trigger=None,
                                variables=None,
                                ecf_files_remotely=self.ecf_files_remotely,
                            )

                            interpolation_task = "c903" if self.do_marsprep else "e927"

                            EcflowSuiteTask(
                                interpolation_task,
                                lbc_fam,
                                config,
                                self.task_settings,
                                self.ecf_files,
                                input_template=input_template,
                                variables=variables,
                                trigger=None,
                                ecf_files_remotely=self.ecf_files_remotely,
                            )

                            bdnr += 1
                            bdtime += bdint
                            if bdnr % bdmax == 0:
                                intnr += 1
                                int_trig = EcflowSuiteTriggers(
                                    [EcflowSuiteTrigger(bch_fam)]
                                )
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

            cday = cycle["day"]
            ctime = cycle["time"]
            task_logs = config["system.wrk"]
            args = ";".join(
                [
                    f"joboutdir={self.joboutdir}/{self.suite_name}/{cday}/{ctime}",
                    f"tarname={cday}_{ctime}",
                    f"task_logs={task_logs}",
                ]
            )
            variables = {"ARGS": args}
            collect_logs_hour = EcflowSuiteTask(
                "CollectLogs",
                time_family,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                trigger=EcflowSuiteTriggers([EcflowSuiteTrigger(cycle_fam)]),
                variables=variables,
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

            if self.do_archiving:
                archiving_hour_trigger = EcflowSuiteTriggers(
                    [EcflowSuiteTrigger(collect_logs_hour)]
                )

                EcflowSuiteTask(
                    "ArchiveHour",
                    time_family,
                    config,
                    self.task_settings,
                    self.ecf_files,
                    input_template=input_template,
                    trigger=archiving_hour_trigger,
                    ecf_files_remotely=self.ecf_files_remotely,
                )

    def static_suite_part(self, config, input_template):
        """Create the time dependent part of the suite.

        Args:
            config (deode.ParsedConfig): Configuration settings
            input_template (str): Default task template

        Returns:
            static_data: EcflowFamily object used for triggering

        """
        static_data = EcflowSuiteFamily(
            "StaticData",
            self.suite,
            self.ecf_files,
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

            pgd = EcflowSuiteTask(
                "Pgd",
                static_data,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
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
            basetime = as_datetime(config["general.times.start"])
            basetime_month = int(basetime.month)
            last_month = basetime_month - 1
            if last_month == 0:
                last_month = 12
            next_month = basetime_month + 1
            if next_month == 13:
                next_month = 1
            seasons = {
                f"m{last_month:02d}": f"{last_month:02d}",
                f"m{basetime_month:02d}": f"{basetime_month:02d}",
                f"m{next_month:02d}": f"{next_month:02d}",
            }

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
            pgd_update = EcflowSuiteTask(
                "PgdUpdate",
                static_data,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
                trigger=pgd_update_trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )

        if self.do_archiving and self.do_pgd:
            archive_static_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(pgd_update)])
            EcflowSuiteTask(
                "ArchiveStatic",
                static_data,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
                trigger=archive_static_trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )
        elif self.do_archiving and not (self.do_pgd):
            archive_static_trigger = EcflowSuiteTriggers(
                [EcflowSuiteTrigger(month_family)]
            )
            EcflowSuiteTask(
                "ArchiveStatic",
                static_data,
                config,
                self.task_settings,
                self.ecf_files,
                input_template=input_template,
                variables=None,
                trigger=archive_static_trigger,
                ecf_files_remotely=self.ecf_files_remotely,
            )
        return static_data

    def save_as_defs(self, def_file):
        """Save definition file.

        Args:
            def_file (str): Name of definition file
        """
        self.suite.save_as_defs(def_file)


class EcflowNode:
    """A Node class is the abstract base class for Suite, Family and Task.

    Every Node instance has a name, and a path relative to a suite
    """

    def __init__(
        self,
        name,
        node_type,
        parent,
        ecf_files,
        variables=None,
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
    ):
        """Construct the EcflowNode.

        Args:
            name (str): Name of node
            node_type (str): Node type
            parent (EcflowNode): Parent node
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): Remote file prefix

        Raises:
            NotImplementedError: Node type not implemented
            TypeError: "Triggers must be an EcflowSuiteTriggers object"
            TypeError: "defstatus must be either str or an ecflow.Defstatus object"

        """
        self.name = name
        self.node_type = node_type

        has_node = True
        if parent is None:
            has_node = False
        if (
            has_node
            and node_type != "suite"
            and hasattr(parent, "ecf_node")
            and parent.ecf_node is None
        ):
            has_node = False
        if not has_node:
            self.ecf_node = None
            path = ""
        else:
            if self.node_type == "family":
                self.ecf_node = parent.ecf_node.add_family(self.name)
            elif self.node_type == "task":
                self.ecf_node = parent.ecf_node.add_task(self.name)
            elif self.node_type == "suite":
                self.ecf_node = parent.add_suite(self.name)
            else:
                raise NotImplementedError

            path = self.ecf_node.get_abs_node_path()

        self.path = path
        logger.debug("path={} ecf_files_remotely={}", self.path, ecf_files_remotely)
        if ecf_files_remotely is None:
            ecf_files_remotely = ecf_files
        self.ecf_local_container_path = ecf_files + self.path
        self.ecf_remote_container_path = ecf_files_remotely + self.path
        logger.debug(
            "path={} local_container={} remote_container={}",
            self.path,
            self.ecf_local_container_path,
            self.ecf_remote_container_path,
        )
        if variables is not None:
            for key, value in variables.items():
                logger.debug("key={} value={}", key, value)
                if self.ecf_node is not None:
                    self.ecf_node.add_variable(key, value)

        if trigger is not None:
            if isinstance(trigger, EcflowSuiteTriggers):
                if trigger.trigger_string is not None:
                    if self.ecf_node is not None:
                        self.ecf_node.add_trigger(trigger.trigger_string)
                else:
                    logger.warning("Empty trigger")
            else:
                raise TypeError("Triggers must be an EcflowSuiteTriggers object")
        self.trigger = trigger

        if def_status is not None and self.ecf_node is not None:
            if isinstance(def_status, str):
                self.ecf_node.add_defstatus(ecflow.Defstatus(def_status))
            elif isinstance(def_status, ecflow.Defstatus):
                self.ecf_node.add_defstatus(def_status)
            else:
                raise TypeError(
                    "defstatus must be either str or an ecflow.Defstatus object"
                )


class EcflowNodeContainer(EcflowNode):
    """Ecflow node container."""

    def __init__(
        self,
        name,
        node_type,
        parent,
        ecf_files,
        variables=None,
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
    ):
        """Construct EcflowNodeContainer.

        Args:
            name (str): Name of the node container.
            node_type (str): What kind of node.
            parent (EcflowNode): Parent to this node.
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server

        """
        EcflowNode.__init__(
            self,
            name,
            node_type,
            parent,
            variables=variables,
            ecf_files=ecf_files,
            trigger=trigger,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
        )


class EcflowSuite(EcflowNodeContainer):
    """EcflowSuite."""

    def __init__(
        self,
        name,
        ecf_files,
        variables=None,
        dry_run=False,
        def_status=None,
        ecf_files_remotely=None,
    ):
        """Construct the Ecflow suite.

        Args:
            name (str): Name of suite
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server

        """
        if dry_run:
            self.defs = None
        else:
            self.defs = ecflow.Defs({})

        EcflowNodeContainer.__init__(
            self,
            name,
            "suite",
            self.defs,
            ecf_files,
            variables=variables,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
        )

    def save_as_defs(self, def_file):
        """Save defintion file.

        Args:
            def_file (str): Name of the definition file.
        """
        if self.defs is not None:
            self.defs.save_as_defs(def_file)
        logger.info("def file saved to {}", def_file)


class EcflowSuiteFamily(EcflowNodeContainer):
    """A family in ecflow."""

    def __init__(
        self,
        name,
        parent,
        ecf_files,
        variables=None,
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
    ):
        """Construct the family.

        Args:
                    name (str): Name of the family.
                    parent (EcflowNodeContainer): Parent node.
                    ecf_files (str): Location of ecf files
                    variables (dict, optional): Variables to map. Defaults to None
                    trigger (EcflowSuiteTriggers): Trigger. Defaults to None
                    def_status (str, ecflow.Defstatus): Def status. Defaults to None
                    ecf_files_remotely(str, optional): ECF_FILES on ecflow server
        f
        """
        EcflowNodeContainer.__init__(
            self,
            name,
            "family",
            parent,
            ecf_files,
            variables=variables,
            trigger=trigger,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
        )
        logger.debug(self.ecf_remote_container_path)
        if self.ecf_node is not None:
            self.ecf_node.add_variable("ECF_FILES", self.ecf_remote_container_path)


class EcflowSuiteTask(EcflowNode):
    """A task in an ecflow suite/family."""

    def __init__(
        self,
        name,
        parent,
        config,
        task_settings,
        ecf_files,
        input_template=None,
        parse=True,
        variables=None,
        ecf_micro="%",
        trigger=None,
        def_status=None,
        ecf_files_remotely=None,
    ):
        """Constuct the EcflowSuiteTask.

        Args:
            name (str): Name of task
            parent (EcflowNode): Parent node.
            ecf_files (str): Path to ecflow containers
            task_settings (TaskSettings): Submission configuration
            config (deode.ParsedConfig): Configuration file
            task_settings (deode.TaskSettings): Task settings
            input_template(str, optional): Input template
            parse (bool, optional): To parse template file or not
            variables (dict, optional): Variables to map. Defaults to None
            ecf_micro (str, optional): ECF_MICRO. Defaults to %
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None
            ecf_files_remotely(str, optional): ECF_FILES on ecflow server

        Raises:
            ValueError: If input template is to be parsed but it is not passed.
            FileNotFoundError: If the task container is not found.

        """
        EcflowNode.__init__(
            self,
            name,
            "task",
            parent,
            ecf_files,
            variables=variables,
            trigger=trigger,
            def_status=def_status,
            ecf_files_remotely=ecf_files_remotely,
        )

        logger.debug(parent.path)
        logger.debug(parent.ecf_local_container_path)
        task_container = parent.ecf_local_container_path + "/" + name + ".py"
        if parse:
            if input_template is None:
                raise ValueError("Must pass input template if it is to be parsed")

            variables = task_settings.get_settings(name)
            logger.debug("vars {}", variables)
            for var, value_ in variables.items():
                value = value_
                if isinstance(value, int):
                    value = str(value)
                logger.debug("var={} value={}", var, value)
                if self.ecf_node is not None:
                    self.ecf_node.add_variable(var, value)
            task_settings.parse_job(
                task=name,
                config=config,
                input_template_job=input_template,
                task_job=task_container,
                variables=variables,
                ecf_micro=ecf_micro,
            )
        elif not os.path.exists(task_container):
            raise FileNotFoundError(f"Container {task_container} is missing!")


class EcflowSuiteTriggers:
    """Triggers to an ecflow suite."""

    def __init__(self, triggers, mode="AND"):
        """Construct EcflowSuiteTriggers.

        Args:
            triggers (list): List of EcflowSuiteTrigger objects.
            mode (str, optional): Cat mode. Defaults to "AND".

        """
        trigger_string = self.create_string(triggers, mode)
        self.trigger_string = trigger_string

    @staticmethod
    def create_string(triggers, mode):
        """Create the trigger string.

        Args:
            triggers (list): List of trigger objects
            mode (str): Concatenation type.

        Raises:
            ValueError: If there are no triggers to be processed
            TypeError: If trigger is not an EcflowSuiteTrigger object

        Returns:
            str: The trigger string based on trigger objects.

        """
        if not isinstance(triggers, list):
            triggers = [triggers]

        if len(triggers) == 0:
            raise ValueError("No triggers to be processed")

        trigger_string = "("
        first = True
        for trigger in triggers:
            if trigger is not None:
                cat = ""
                if not first:
                    cat = " " + mode + " "
                if isinstance(trigger, EcflowSuiteTriggers):
                    trigger_string = trigger_string + cat + trigger.trigger_string
                elif isinstance(trigger, EcflowSuiteTrigger):
                    trigger_string = (
                        trigger_string + cat + trigger.node.path + " == " + trigger.mode
                    )
                else:
                    raise TypeError("Trigger must be an EcflowSuiteTrigger object")
                first = False
        trigger_string = trigger_string + ")"
        # If no triggers were found/set
        if first:
            trigger_string = None
        return trigger_string

    def add_triggers(self, triggers, mode="AND"):
        """Add triggers.

        Args:
            triggers (EcflowSuiteTriggers): The triggers
            mode (str, optional): Cat mode. Defaults to "AND".

        """
        cat_string = " " + mode + " "
        trigger_string = self.create_string(triggers, mode)
        if trigger_string is not None:
            self.trigger_string = self.trigger_string + cat_string + trigger_string


class EcflowSuiteTrigger:
    """EcFlow Trigger in a suite."""

    def __init__(self, node, mode="complete"):
        """Create a EcFlow trigger object.

        Args:
            node (scheduler.EcflowNode): The node to trigger on
            mode (str, optional): Trigger type. Defaults to "complete".

        """
        self.node = node
        self.mode = mode
