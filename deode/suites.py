"""Ecflow suites."""

import os
from pathlib import Path

from .datetime_utils import as_datetime, as_timedelta
from .logs import get_logger, get_logger_from_config
from .toolbox import Platform

try:
    import ecflow
except ImportError:
    ecflow = None

logger = get_logger(__name__, "DEBUG")


class SuiteDefinition(object):
    """Definition of suite."""

    def __init__(
        self,
        suite_name,
        joboutdir,
        ecf_files,
        config,
        task_settings,
        loglevel,
        ecf_home=None,
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
            task_settings (deode.TaskSettings): Task settings
            loglevel (str): Loglevel
            ecf_home (str, optional): ECF_HOME. Defaults to None.
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

        self.logger = get_logger_from_config(config)
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

        troika = "/opt/troika/bin/troika"

        platform = Platform(config)
        troika_config = platform.get_value("troika.config_file")
        config_file = config.get_value("metadata.source_file_path")
        first_cycle = as_datetime(config.get_value("general.times.start"))
        deode_home = platform.get_platform_value("DEODE_HOME")
        keep_workdirs = "1" if config.get_value("general.keep_workdirs") else "0"
        variables = {
            "ECF_EXTN": ".py",
            "ECF_FILES": self.ecf_files,
            "ECF_INCLUDE": self.ecf_include,
            "ECF_TRIES": 1,
            "ECF_HOME": self.ecf_home,
            "ECF_KILL_CMD": self.ecf_kill_cmd,
            "ECF_JOB_CMD": self.ecf_job_cmd,
            "ECF_STATUS_CMD": self.ecf_status_cmd,
            "ECF_OUT": self.ecf_out,
            "ECF_JOBOUT": self.ecf_jobout,
            "ECF_TIMEOUT": 20,
            "LOGLEVEL": loglevel,
            "CONFIG": str(config_file),
            "TROIKA": troika,
            "TROIKA_CONFIG": troika_config,
            "BASETIME": first_cycle.strftime("%Y%m%d%H%M"),
            "VALIDTIME": first_cycle.strftime("%Y%m%d%H%M"),
            "ARGS": 0,
            "DEODE_HOME": deode_home,
            "KEEP_WORKDIRS": keep_workdirs,
        }

        input_template = Path(__file__).parent.resolve() / "templates/ecflow/default.py"
        input_template = input_template.as_posix()
        self.suite = EcflowSuite(name, ecf_files, variables=variables, dry_run=dry_run)

        static_data = EcflowSuiteFamily("StaticData", self.suite, ecf_files)

        pgd_input = EcflowSuiteFamily("PgdInput", static_data, ecf_files)
        EcflowSuiteTask(
            "Gmted",
            pgd_input,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
        )

        EcflowSuiteTask(
            "Soil",
            pgd_input,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
        )
        # 2nd level Family
        # StaticData >> Pgd
        pgd_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(pgd_input)])
        pgd = EcflowSuiteTask(
            "Pgd",
            static_data,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
            trigger=pgd_trigger,
        )
        e923_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(pgd)])
        e923 = EcflowSuiteTask(
            "e923",
            static_data,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
            trigger=e923_trigger,
        )
        pgd_update_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(e923)])
        pgdupdate = EcflowSuiteTask(
            "PgdUpdate",
            static_data,
            config,
            self.task_settings,
            ecf_files,
            input_template=input_template,
            variables=None,
            trigger=pgd_update_trigger,
        )

        first_cycle = as_datetime(config.get_value("general.times.start"))
        last_cycle = as_datetime(config.get_value("general.times.end"))
        cycle_length = as_timedelta(config.get_value("general.times.cycle_length"))
        cycles = {}
        cycle_time = first_cycle
        i = 0
        while cycle_time <= last_cycle:
            logger.debug("cycle_time %s", cycle_time)
            cycles.update(
                {
                    str(i): {
                        "day": cycle_time.strftime("%Y%m%d"),
                        "time": cycle_time.strftime("%H%M"),
                        "validtime": cycle_time.strftime("%Y%m%d%H%M"),
                        "basetime": cycle_time.strftime("%Y%m%d%H%M"),
                    }
                }
            )
            i = i + 1
            cycle_time = cycle_time + cycle_length

        do_prep = True
        days = []
        prev_cycle_trigger = None
        for __, cycle in cycles.items():
            cycle_day = cycle["day"]
            inputdata_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(pgdupdate)])

            if cycle_day not in days:
                day_family = EcflowSuiteFamily(
                    cycle["day"], self.suite, ecf_files, trigger=inputdata_trigger
                )
                days.append(cycle_day)

            time_variables = {
                "BASETIME": cycle["basetime"],
                "VALIDTIME": cycle["validtime"],
            }
            time_family = EcflowSuiteFamily(
                cycle["time"],
                day_family,
                ecf_files,
                trigger=inputdata_trigger,
                variables=time_variables,
            )
            # 3rd level Family
            # YYYYMMDD >> HHHH >> InputData
            inputdata = EcflowSuiteFamily(
                "InputData", time_family, ecf_files, trigger=inputdata_trigger
            )
            prepare_cycle = EcflowSuiteTask(
                "PrepareCycle",
                inputdata,
                config,
                self.task_settings,
                ecf_files,
                input_template=input_template,
                variables=None,
            )

            prepare_cycle_done = EcflowSuiteTriggers([EcflowSuiteTrigger(prepare_cycle)])

            basetime = as_datetime(config.get_value("general.times.basetime"))
            forecast_range = as_timedelta(config.get_value("general.forecast_range"))
            endtime = basetime + forecast_range
            bdint = as_timedelta(config.get_value("general.bdint"))
            bdmax = config.get_value("general.bdmax")

            int_fam = EcflowSuiteFamily(
                f'{"Interpolation"}',
                time_family,
                ecf_files,
                trigger=prepare_cycle_done,
                variables=None,
            )

            bdnr = 0
            intnr = 1
            args = ""
            int_trig = prepare_cycle_done
            bdtime = basetime
            while bdtime <= endtime:
                bch_fam = EcflowSuiteFamily(
                    f"Batch{intnr:02}",
                    int_fam,
                    ecf_files,
                    trigger=int_trig,
                    variables=None,
                )
                while bdtime <= endtime:
                    date_string = bdtime.isoformat(sep="T").replace("+00:00", "Z")
                    args = f"bd_time={date_string};bd_nr={bdnr}"
                    variables = {"ARGS": args}

                    lbc_fam = EcflowSuiteFamily(
                        f"LBC{bdnr:02}",
                        bch_fam,
                        ecf_files,
                        trigger=None,
                        variables=None,
                    )
                    EcflowSuiteTask(
                        f'{"e927"}',
                        lbc_fam,
                        config,
                        self.task_settings,
                        ecf_files,
                        input_template=input_template,
                        variables=variables,
                        trigger=None,
                    )
                    bdnr += 1
                    bdtime += bdint
                    if bdnr % bdmax == 0:
                        intnr += 1
                        int_trig = EcflowSuiteTriggers([EcflowSuiteTrigger(bch_fam)])
                        break
            int_trig = EcflowSuiteTriggers([EcflowSuiteTrigger(int_fam)])

            # 3rd level Family
            # YYYYMMDD >> HHHH >> Cycle
            cycle = EcflowSuiteFamily("Cycle", time_family, ecf_files, trigger=int_trig)
            triggers = [EcflowSuiteTrigger(inputdata)]
            if prev_cycle_trigger is not None:
                triggers = triggers + prev_cycle_trigger
            ready_for_cycle = EcflowSuiteTriggers(triggers)
            prev_cycle_trigger = [EcflowSuiteTrigger(cycle)]
            initialization = EcflowSuiteFamily(
                "Initialization", cycle, ecf_files, trigger=ready_for_cycle
            )
            prep_trigger = None
            if do_prep:
                prep = EcflowSuiteTask(
                    "Prep",
                    initialization,
                    config,
                    self.task_settings,
                    ecf_files,
                    input_template=input_template,
                    variables=None,
                )
                prep_trigger = EcflowSuiteTrigger(prep)
                do_prep = False

            fg_trigger = None
            if prep_trigger is not None:
                fg_trigger = EcflowSuiteTriggers([prep_trigger])
            EcflowSuiteTask(
                "FirstGuess",
                initialization,
                config,
                self.task_settings,
                ecf_files,
                input_template=input_template,
                trigger=fg_trigger,
                variables=None,
            )

            forecast_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(initialization)])
            forecasting = EcflowSuiteFamily(
                "Forecasting", cycle, ecf_files, trigger=forecast_trigger
            )
            self.logger.debug(self.task_settings.get_task_settings("Forecast"))

            variables = {"ECF_TIMEOUT": 5}
            forecast = EcflowSuiteTask(
                "Forecast",
                forecasting,
                config,
                self.task_settings,
                ecf_files,
                input_template=input_template,
                variables=variables,
            )

            creategrib_trigger = EcflowSuiteTriggers([EcflowSuiteTrigger(forecast)])

            EcflowSuiteTask(
                "CreateGrib",
                forecasting,
                config,
                self.task_settings,
                ecf_files,
                input_template=input_template,
                trigger=creategrib_trigger,
            )

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
        if has_node and node_type != "suite":
            if hasattr(parent, "ecf_node"):
                if parent.ecf_node is None:
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
        self.ecf_container_path = ecf_files + self.path
        if variables is not None:
            for key, value in variables.items():
                logger.debug("key=%s value=%s", key, value)
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

        if def_status is not None:
            if self.ecf_node is not None:
                if isinstance(def_status, str):
                    self.ecf_node.add_defstatus(ecflow.Defstatus(def_status))
                elif isinstance(def_status, ecflow.Defstatus):
                    self.ecf_node.add_defstatus(def_status)
                else:
                    raise TypeError(
                        "defstatus must be either str or an ecflow.Defstatus object"
                    )


class EcflowNodeContainer(EcflowNode):
    """Ecflow node container.

    Args:
        EcflowNode (EcflowNode): Parent class.
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
        )


class EcflowSuite(EcflowNodeContainer):
    """EcflowSuite.

    Args:
        EcflowNodeContainer
        (EcflowNodeContainer): A child of the EcflowNodeContainer class.
    """

    def __init__(self, name, ecf_files, variables=None, dry_run=False, def_status=None):
        """Construct the Ecflow suite.

        Args:
            name (str): Name of suite
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            dry_run (bool, optional): Dry run not using ecflow. Defaults to False.
            def_status (str, ecflow.Defstatus): Def status. Defaults to None

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
        )

    def save_as_defs(self, def_file):
        """Save defintion file.

        Args:
            def_file (str): Name of the definition file.
        """
        if self.defs is not None:
            self.defs.save_as_defs(def_file)
        logger.info("def file saved to %s", def_file)


class EcflowSuiteFamily(EcflowNodeContainer):
    """A family in ecflow.

    Args:
        EcflowNodeContainer (_type_): _description_
    """

    def __init__(
        self, name, parent, ecf_files, variables=None, trigger=None, def_status=None
    ):
        """Construct the family.

        Args:
            name (str): Name of the family.
            parent (EcflowNodeContainer): Parent node.
            ecf_files (str): Location of ecf files
            variables (dict, optional): Variables to map. Defaults to None
            trigger (EcflowSuiteTriggers): Trigger. Defaults to None
            def_status (str, ecflow.Defstatus): Def status. Defaults to None

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
        )
        logger.debug(self.ecf_container_path)
        if self.ecf_node is not None:
            self.ecf_node.add_variable("ECF_FILES", self.ecf_container_path)


class EcflowSuiteTask(EcflowNode):
    """A task in an ecflow suite/family.

    Args:
        EcflowNode (EcflowNodeContainer): The node container.
    """

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
        )

        logger.debug(parent.path)
        logger.debug(parent.ecf_container_path)
        task_container = parent.ecf_container_path + "/" + name + ".py"
        if parse:
            if input_template is None:
                raise ValueError("Must pass input template if it is to be parsed")

            variables = task_settings.get_settings(name)
            logger.debug("vars %s", variables)
            for var, value in variables.items():
                logger.debug("var=%s value=%s", var, value)
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
        else:
            if not os.path.exists(task_container):
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
                else:
                    if isinstance(trigger, EcflowSuiteTrigger):
                        trigger_string = (
                            trigger_string
                            + cat
                            + trigger.node.path
                            + " == "
                            + trigger.mode
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
