#!/usr/bin/env python3
"""Unit tests for the ``tactus.suites`` package."""

import os
from contextlib import suppress
from datetime import datetime, timedelta

import pytest

from tactus.config_parser import ParsedConfig
from tactus.derived_variables import set_times
from tactus.submission import TaskSettings
from tactus.suites.base import (
    EcflowSuite,
    EcflowSuiteLimit,
    EcflowSuiteTrigger,
    EcflowSuiteTriggers,
    SuiteDefinition,
    _get_name,
)
from tactus.suites.compilation import CompilationSuiteDefinition
from tactus.suites.discover_suite import get_suite
from tactus.suites.suite_utils import (
    Cycles,
    combine_triggers,
    lbc_times_generator,
    slaf_planner,
)
from tactus.suites.tactus import TactusSuiteDefinition
from tactus.suites.tactus_suite_components import (
    MirrorFamily,
    PrepFamily,
    StaticDataMemberGenerator,
)

_INPUT_TEMPLATE = f"{os.path.dirname(__file__)}/../../tactus/templates/ecflow/default.py"

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def _module_mockers(module_mocker):
    """Suppress the RuntimeError that TaskSettings.parse_job raises in tests.

    Needed by any test that builds a suite (which calls parse_job for every
    task).
    """
    original_submission_task_settings_parse_job = TaskSettings.parse_job

    def new_submission_task_settings_parse_job(self, **kwargs):
        with suppress(RuntimeError):
            original_submission_task_settings_parse_job(self, **kwargs)

    module_mocker.patch(
        "tactus.submission.TaskSettings.parse_job",
        new=new_submission_task_settings_parse_job,
    )


class _FakeEcflowNode:
    """Minimal stand-in for an ``ecflow`` node used by ``EcflowNode``."""

    def __init__(self, name, node_type, path):
        self.name = name
        self.node_type = node_type
        self.path = path
        self.variables = {}
        self.triggers = []
        self.crons = []
        self.inlimits = []
        self.limits = []
        self.defstatus = None
        self.mirror = None
        self.children = []

    def add_family(self, name):
        child = _FakeEcflowNode(name, "family", f"{self.path}/{name}")
        self.children.append(child)
        return child

    def add_task(self, name):
        child = _FakeEcflowNode(name, "task", f"{self.path}/{name}")
        self.children.append(child)
        return child

    def add_variable(self, key, value):
        self.variables[key] = value

    def add_trigger(self, trigger_string):
        self.triggers.append(trigger_string)

    def add_complete(self, complete_string):
        pass

    def add_cron(self, cron):
        self.crons.append(cron)

    def add_inlimit(self, name, _path=None):
        self.inlimits.append(name)

    def add_limit(self, name, max_jobs):
        self.limits.append((name, max_jobs))

    def add_defstatus(self, defstatus):
        self.defstatus = defstatus

    def add_mirror(self, mirror_attr):
        self.mirror = mirror_attr

    def get_abs_node_path(self):
        return self.path


class _FakeEcflowDefs:
    """Minimal stand-in for ``ecflow.Defs``."""

    def __init__(self, *_args, **_kwargs):
        self.suites = []

    def add_suite(self, name):
        node = _FakeEcflowNode(name, "suite", f"/{name}")
        self.suites.append(node)
        return node

    def save_as_defs(self, def_file):
        with open(def_file, mode="w", encoding="utf8") as fh:
            fh.write("fake defs\n")


class _FakeDefstatus:
    def __init__(self, status):
        self.status = status


class _FakeCron:
    def __init__(self, time_str, days_of_week=None):
        self.time_str = time_str
        self.days_of_week = days_of_week


class _FakeMirrorAttr:
    def __init__(self, *args):
        self.args = args


class _FakeEcflowModule:
    Defs = _FakeEcflowDefs
    Defstatus = _FakeDefstatus
    Cron = _FakeCron
    MirrorAttr = _FakeMirrorAttr


@pytest.fixture
def fake_ecflow(mocker):
    """Patch ``tactus.suites.base.ecflow`` with a lightweight fake module.

    This lets ``SuiteDefinition``/``EcflowNode`` be built with
    ``dry_run=False``, giving every node a non-``None`` ``ecf_node`` and thus
    exercising the variable/trigger/limit/cron code paths that are otherwise
    skipped whenever a suite is built with ``dry_run=True``.
    """
    return mocker.patch("tactus.suites.base.ecflow", _FakeEcflowModule)


# ---------------------------------------------------------------------------
# TactusSuiteDefinition end-to-end generation
# ---------------------------------------------------------------------------


@pytest.mark.usefixtures("_module_mockers")
class TestSuite:
    def test_config_can_be_instantiated(self, default_config):
        assert isinstance(default_config, ParsedConfig)

    @pytest.mark.parametrize(
        "param",
        [
            {
                "suite_control": {
                    "do_marsprep": False,
                    "interpolate_boundaries": False,
                    "create_static_data": False,
                },
                "general": {
                    "times": {
                        "end": "2022-05-03T00:00:00Z",
                        "start": "2022-05-02T00:00:00Z",
                    },
                },
                "eps": {
                    "general": {"members": [0, 1]},
                },
            },
            {"boundaries": {"bdmax": 1}},
            {"suite_control": {"create_static_data": False}},
            {"suite_control": {"create_time_dependent_suite": False, "do_soil": False}},
            {
                "suite_control": {
                    "do_archiving": False,
                    "do_cleaning": False,
                    "do_extractsqlite": True,
                    "do_marsprep": True,
                    "do_pgd": False,
                    "do_soil": False,
                    "interpolate_boundaries": False,
                    "cold_start": False,
                    "split_mars_by_step": True,
                }
            },
            {
                "suite_control": {
                    "interpolate_boundaries": False,
                    "do_addcalculatedfields": False,
                }
            },
            {"submission": {"max_ecf_tasks": 2}},
            # SLAF perturbations in InputDataFamily/SLAFpartFamily.
            {
                "eps": {
                    "general": {"members": [0, 1]},
                    "members": {
                        "0": {"boundaries": {"slaflag": "PT3H", "slafdiff": "PT1H"}},
                    },
                },
            },
            # Member-specific static data and marsprep.
            {
                "suite_control": {
                    "member_specific_static_data": True,
                    "member_specific_mars_prep": True,
                },
                "eps": {"general": {"members": [0, 1]}},
            },
            # ALARO + surfex: PgdFilterTownFrac branch in StaticDataTasks.
            {"general": {"csc": "ALARO", "surfex": True}},
            # ALARO without surfex: E923Update branch in InterpolationFamily.
            {"general": {"csc": "ALARO", "surfex": False}},
            # Windfarm + json2tab: GenerateWfpTabFile branch in StaticDataTasks.
            {"general": {"windfarm": True}},
            # CreateGribStatic branch in StaticDataTasks.
            {"suite_control": {"do_creategrib_static": True}},
            # Non-decadal PGD branch.
            {"pgd": {"one_decade": False}},
            # IOmerge/CreateGrib/ArchiveFDB branches in ForecastFamily.
            {
                "creategrib": {"CreateGrib": {"conversions": ["sfx"]}},
                "archiving": {"FDB": {"fdb": {"main": {"active": True}}}},
                "submission": {"task_exceptions": {"Forecast": {"NPROC_IO": 4}}},
            },
            # MergeSQLitesFamily branch (needs more than one member).
            {"eps": {"general": {"members": [0, 1]}}},
            # Per-LBC-family Marsprep call and split_mars_by_step trigger wiring.
            {"suite_control": {"split_mars_by_step": True}},
            # E923MonthlyFamily month-wraparound: January end (last_month -> 12).
            {
                "general": {
                    "times": {
                        "start": "2022-01-10T00:00:00Z",
                        "end": "2022-01-12T00:00:00Z",
                    },
                },
            },
            # E923MonthlyFamily month-wraparound: December end (next_month -> 1).
            {
                "general": {
                    "times": {
                        "start": "2022-12-10T00:00:00Z",
                        "end": "2022-12-12T00:00:00Z",
                    },
                },
            },
            # Data assimilation: surface OI chain only (da.upper_air defaults False).
            {"suite_control": {"do_assimilation": True}},
            # Data assimilation: surface OI chain + 3D-Var upper-air chain.
            {
                "suite_control": {"do_assimilation": True},
                "da": {"upper_air": True},
            },
            # Data assimilation: 3D-Var upper-air chain only, no surface OI chain.
            {
                "suite_control": {"do_assimilation": True},
                "da": {"surface": False, "upper_air": True},
            },
            # Data assimilation across multiple cycles (end > start), so the
            # AssimilationFamily chain is built repeatedly with cross-cycle
            # triggers (prev_cycle_triggers, prev_interpolation_triggers).
            {
                "suite_control": {"do_assimilation": True},
                "da": {"upper_air": True},
                "general": {
                    "times": {
                        "start": "2022-05-02T00:00:00Z",
                        "end": "2022-05-02T09:00:00Z",
                    },
                },
            },
        ],
    )
    def test_suite(self, default_config, param, tmp_directory):
        config = default_config
        suite_name = "test_suite"
        config = config.copy(
            update={
                "general": {"case": suite_name},
                "scheduler": {
                    "ecfvars": {
                        "ecf_files": f"{tmp_directory}/ecf_files",
                        "ecf_jobout": f"{tmp_directory}/jobout",
                    }
                },
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                    "mirrorglobalDT": {"remote_path": "@YYYY@/@HH@mm@"},
                },
            }
        )

        config = config.copy(update=set_times(config))
        config = config.copy(update=param)
        defs = TactusSuiteDefinition(
            config,
            dry_run=True,
        )
        def_file = f"{tmp_directory}/{suite_name}.def"
        defs.save_as_defs(def_file)


class TestCombineTriggers:
    def test_empty_list(self):
        assert combine_triggers([]) == []

    def test_flat_list_no_none(self):
        assert combine_triggers(["a", "b", "c"]) == ["a", "b", "c"]

    def test_flat_list_with_none(self):
        assert combine_triggers(["a", None, "b"]) == ["a", "b"]

    def test_nested_list_flattened(self):
        assert combine_triggers([["a", "b"], "c"]) == ["a", "b", "c"]

    def test_nested_tuple_flattened(self):
        assert combine_triggers([("a", "b"), "c"]) == ["a", "b", "c"]

    def test_nested_list_with_none_removed(self):
        assert combine_triggers([["a", None, "b"], None, "c"]) == ["a", "b", "c"]

    def test_all_none(self):
        assert combine_triggers([None, None]) == []


# ---------------------------------------------------------------------------
# suite_utils.Cycles
# ---------------------------------------------------------------------------


class TestCycles:
    def test_generates_expected_cycles(self):
        cycles = Cycles(
            first_cycle="2022-01-01T00:00:00Z",
            last_cycle="2022-01-01T12:00:00Z",
            cycle_length="PT6H",
        )
        times = [cycle.time for cycle in cycles]
        assert times == ["0000", "0600", "1200"]
        assert all(cycle.day == "20220101" for cycle in cycles)

    def test_current_and_next_cycle(self):
        cycles = Cycles(
            first_cycle="2022-01-01T00:00:00Z",
            last_cycle="2022-01-01T12:00:00Z",
            cycle_length="PT6H",
        )
        assert cycles.current_index == 0
        assert cycles.current_cycle.time == "0000"
        assert cycles.next_cycle.time == "0600"

        # advance manually and check current/next again
        for _ in cycles:
            pass
        # iterating leaves current_index at len(cycles) after exhaustion... but
        # __iter__ resets to 0 on new iteration; instead advance by hand:
        cycles._current_index = 2
        assert cycles.current_cycle.time == "1200"
        with pytest.raises(StopIteration):
            _ = cycles.next_cycle

    def test_end_of_month_true_at_boundary(self):
        cycles = Cycles(
            first_cycle="2022-01-31T00:00:00Z",
            last_cycle="2022-02-01T00:00:00Z",
            cycle_length="P1D",
        )
        assert cycles.end_of_month is True

    def test_end_of_month_false_within_month(self):
        cycles = Cycles(
            first_cycle="2022-01-01T00:00:00Z",
            last_cycle="2022-01-03T00:00:00Z",
            cycle_length="P1D",
        )
        assert cycles.end_of_month is False

    def test_end_of_month_false_on_last_cycle(self):
        cycles = Cycles(
            first_cycle="2022-01-31T00:00:00Z",
            last_cycle="2022-02-01T00:00:00Z",
            cycle_length="P1D",
        )
        cycles._current_index = 1
        assert cycles.end_of_month is False

    def test_iteration_resets_index(self):
        cycles = Cycles(
            first_cycle="2022-01-01T00:00:00Z",
            last_cycle="2022-01-01T12:00:00Z",
            cycle_length="PT6H",
        )
        first_pass = [cycle.time for cycle in cycles]
        second_pass = [cycle.time for cycle in cycles]
        assert first_pass == second_pass == ["0000", "0600", "1200"]


# ---------------------------------------------------------------------------
# suite_utils.lbc_times_generator
# ---------------------------------------------------------------------------


class TestLbcTimesGenerator:
    def test_first_cycle_start_mode_no_shift(self):
        result = list(
            lbc_times_generator(
                basetime=datetime(2022, 1, 1, 0),
                endtime=datetime(2022, 1, 1, 6),
                step=timedelta(hours=3),
                mode="start",
                is_first_cycle=True,
            )
        )
        assert result == [
            {0: "2022-01-01T00:00:00"},
            {1: "2022-01-01T03:00:00"},
            {2: "2022-01-01T06:00:00"},
        ]

    def test_non_first_cycle_start_mode_shifts_basetime(self):
        result = list(
            lbc_times_generator(
                basetime=datetime(2022, 1, 1, 0),
                endtime=datetime(2022, 1, 1, 6),
                step=timedelta(hours=3),
                mode="start",
                is_first_cycle=False,
            )
        )
        assert result == [
            {1: "2022-01-01T03:00:00"},
            {2: "2022-01-01T06:00:00"},
        ]

    def test_restart_mode_always_shifts(self):
        result = list(
            lbc_times_generator(
                basetime=datetime(2022, 1, 1, 0),
                endtime=datetime(2022, 1, 1, 6),
                step=timedelta(hours=3),
                mode="restart",
                is_first_cycle=True,
            )
        )
        assert result == [
            {1: "2022-01-01T03:00:00"},
            {2: "2022-01-01T06:00:00"},
        ]

    def test_interpolsstsic_suppresses_shift(self):
        result = list(
            lbc_times_generator(
                basetime=datetime(2022, 1, 1, 0),
                endtime=datetime(2022, 1, 1, 6),
                step=timedelta(hours=3),
                mode="restart",
                is_first_cycle=True,
                do_interpolsstsic=True,
            )
        )
        assert result == [
            {0: "2022-01-01T00:00:00"},
            {1: "2022-01-01T03:00:00"},
            {2: "2022-01-01T06:00:00"},
        ]

    def test_lbc_per_task_groups_batches(self):
        result = list(
            lbc_times_generator(
                basetime=datetime(2022, 1, 1, 0),
                endtime=datetime(2022, 1, 1, 9),
                step=timedelta(hours=3),
                mode="start",
                is_first_cycle=True,
                lbc_per_task=2,
            )
        )
        assert result == [
            {0: "2022-01-01T00:00:00", 1: "2022-01-01T03:00:00"},
            {2: "2022-01-01T06:00:00", 3: "2022-01-01T09:00:00"},
        ]

    def test_lbc_per_task_partial_last_batch(self):
        result = list(
            lbc_times_generator(
                basetime=datetime(2022, 1, 1, 0),
                endtime=datetime(2022, 1, 1, 0),
                step=timedelta(hours=3),
                mode="start",
                is_first_cycle=True,
                lbc_per_task=2,
            )
        )
        assert result == [{0: "2022-01-01T00:00:00"}]


# ---------------------------------------------------------------------------
# suite_utils.slaf_planner
# ---------------------------------------------------------------------------


class _FakeConfig:
    """Minimal dotted-key config stand-in for slaf_planner."""

    def __init__(self, data):
        self._data = data

    def _lookup(self, key):
        node = self._data
        for part in key.split("."):
            node = node[part]
        return node

    def __getitem__(self, key):
        return self._lookup(key)

    def get(self, key, default=None):
        try:
            return self._lookup(key)
        except KeyError:
            return default


class TestSlafPlanner:
    def test_distributes_files_across_members(self):
        config = _FakeConfig({
            "boundaries": {"bdint": "PT3H"},
            "eps": {
                "general": {"members": [0, 1]},
                "members": {
                    "0": {"boundaries": {}},
                    "1": {"boundaries": {}},
                },
            },
        })

        def lbc_gen():
            yield {0: "2022-01-01T00:00:00+00:00"}
            yield {0: "2022-01-01T03:00:00+00:00"}

        doer = slaf_planner(config, lbc_gen(), me=0)

        assert set(doer.keys()) == {
            "2022-01-01T00:00:00Z;0",
            "2022-01-01T03:00:00Z;0",
        }
        members_used = set()
        for value in doer.values():
            member, part = value.split(":")
            assert part == "0"
            members_used.add(member)
        # both files should not necessarily go to the same member
        assert members_used <= {"0", "1"}

    def test_future_boundary_raises_runtime_error(self):
        config = _FakeConfig({
            "boundaries": {"bdint": "PT3H"},
            "eps": {
                "general": {"members": [0]},
                "members": {
                    "0": {"boundaries": {"slaflag": "PT1H", "slafdiff": "PT4H"}},
                },
            },
        })

        def lbc_gen():
            yield {0: "2022-01-01T00:00:00+00:00"}

        with pytest.raises(RuntimeError):
            slaf_planner(config, lbc_gen(), me=0)


# ---------------------------------------------------------------------------
# base._get_name / SuiteDefinition / EcflowSuiteTriggers / EcflowSuiteLimit
# ---------------------------------------------------------------------------


class TestGetName:
    def test_uses_explicit_plugin_name(self):
        class Foo:
            __plugin_name__ = "custom_name"

        assert _get_name("Foo", Foo) == "custom_name"

    def test_falls_back_to_lowercase_class_name(self):
        class MySuiteDefinition:
            pass

        assert _get_name("MySuiteDefinition", MySuiteDefinition) == "mysuitedefinition"


class TestSuiteDefinitionRequiresEcflow:
    def test_raises_without_ecflow_when_not_dry_run(self, mocker):
        mocker.patch("tactus.suites.base.ecflow", None)
        with pytest.raises(ModuleNotFoundError):
            SuiteDefinition(config={}, dry_run=False)


class _PathNode:
    """Stand-in for an EcflowNode, exposing only the ``path`` attribute."""

    def __init__(self, path):
        self.path = path


class TestEcflowSuiteTriggers:
    def test_single_trigger_string(self):
        node = EcflowSuiteTrigger(_PathNode("/suite/fam/task"))
        triggers = EcflowSuiteTriggers([node])
        assert triggers.trigger_string == "(/suite/fam/task == complete)"

    def test_non_list_trigger_is_wrapped(self):
        node = EcflowSuiteTrigger(_PathNode("/suite/fam/task"))
        triggers = EcflowSuiteTriggers(node)
        assert triggers.trigger_string == "(/suite/fam/task == complete)"

    def test_multiple_triggers_joined_with_mode(self):
        node_a = EcflowSuiteTrigger(_PathNode("/suite/fam/a"))
        node_b = EcflowSuiteTrigger(_PathNode("/suite/fam/b"))
        triggers = EcflowSuiteTriggers([node_a, node_b], mode="OR")
        assert triggers.trigger_string == (
            "(/suite/fam/a == complete OR /suite/fam/b == complete)"
        )

    def test_nested_ecflow_suite_triggers(self):
        node_a = EcflowSuiteTrigger(_PathNode("/suite/fam/a"))
        inner = EcflowSuiteTriggers([node_a])
        node_b = EcflowSuiteTrigger(_PathNode("/suite/fam/b"))
        outer = EcflowSuiteTriggers([inner, node_b])
        assert outer.trigger_string == (
            "((/suite/fam/a == complete) AND /suite/fam/b == complete)"
        )

    def test_empty_trigger_list_raises_value_error(self):
        with pytest.raises(ValueError, match="No triggers to be processed"):
            EcflowSuiteTriggers([])

    def test_invalid_trigger_type_raises_type_error(self):
        with pytest.raises(TypeError):
            EcflowSuiteTriggers(["not-a-trigger"])

    def test_add_triggers_appends_with_mode(self):
        node_a = EcflowSuiteTrigger(_PathNode("/suite/fam/a"))
        node_b = EcflowSuiteTrigger(_PathNode("/suite/fam/b"))
        triggers = EcflowSuiteTriggers([node_a])
        triggers.add_triggers([node_b], mode="OR")
        assert triggers.trigger_string == (
            "(/suite/fam/a == complete) OR (/suite/fam/b == complete)"
        )


class TestEcflowSuiteLimit:
    def test_stores_name_and_max_jobs(self):
        limit = EcflowSuiteLimit("my_limit", 4)
        assert limit.limit_name == "my_limit"
        assert limit.max_jobs == 4


# ---------------------------------------------------------------------------
# discover_suite
# ---------------------------------------------------------------------------


def _write_suite_module(path, class_name):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, mode="w", encoding="utf8") as fh:
        fh.write(
            "from tactus.suites.base import SuiteDefinition\n"
            f"class {class_name}(SuiteDefinition):\n"
            "    def __init__(self, config):\n"
            "        self.config = config\n"
        )


def _write_broken_suite_module(path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, mode="w", encoding="utf8") as fh:
        fh.write("import this_module_does_not_exist\n")


class TestGetSuite:
    def test_returns_instance_of_discovered_suite(self, default_config, tmp_directory):
        plugin_root = f"{tmp_directory}/get_suite_success"
        _write_suite_module(
            f"{plugin_root}/get_suite_success/suites/mymodule.py",
            "MyPluginSuite",
        )
        config = default_config.copy(
            update={
                "general": {
                    "plugin_registry": {"plugins": {"get_suite_success": plugin_root}},
                }
            }
        )
        obj = get_suite("MyPluginSuite", config)
        assert isinstance(obj, SuiteDefinition)
        assert obj.config is config

    def test_unknown_suite_raises_not_implemented(self, default_config):
        with pytest.raises(NotImplementedError):
            get_suite("ThisSuiteDoesNotExist", default_config)


class TestAvailableSuitesSkipsImportErrors:
    def test_broken_module_is_skipped_but_others_are_found(self, tmp_directory):
        from pathlib import Path

        from tactus.plugin import TactusPlugin, TactusPluginRegistry
        from tactus.suites.discover_suite import available_suites

        plugin_root = f"{tmp_directory}/discover_broken"
        _write_broken_suite_module(
            f"{plugin_root}/discover_broken/suites/broken_module.py"
        )
        _write_suite_module(
            f"{plugin_root}/discover_broken/suites/good_module.py",
            "GoodDiscoverSuite",
        )

        reg = TactusPluginRegistry()
        plg = TactusPlugin("discover_broken", Path(plugin_root))
        reg.register_plugin(plg)

        known_types = available_suites(reg)
        assert "gooddiscoversuite" in known_types


# ---------------------------------------------------------------------------
# compilation.CompilationSuiteDefinition
# ---------------------------------------------------------------------------


@pytest.mark.usefixtures("_module_mockers")
class TestCompilationSuiteDefinition:
    @pytest.mark.parametrize("do_cleaning", [True, False])
    def test_builds_def_file(self, default_config, tmp_directory, do_cleaning):
        suite_name = f"test_compilation_{do_cleaning}"
        config = default_config.copy(
            update={
                "general": {"case": suite_name},
                "scheduler": {
                    "ecfvars": {
                        "ecf_files": f"{tmp_directory}/ecf_files",
                        "ecf_jobout": f"{tmp_directory}/jobout",
                    }
                },
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                    "mirrorglobalDT": {"remote_path": "@YYYY@/@HH@mm@"},
                },
                "suite_control": {"do_cleaning": do_cleaning},
            }
        )
        defs = CompilationSuiteDefinition(config, dry_run=True)
        def_file = f"{tmp_directory}/{suite_name}.def"
        defs.save_as_defs(def_file)


# ---------------------------------------------------------------------------
# Full suite generation with a real (fake) ecf_node, to exercise the
# branches in base.py / tactus.py / tactus_suite_components.py /
# da_components.py that are only reached when ecf_node is not None.
# ---------------------------------------------------------------------------


@pytest.mark.usefixtures("_module_mockers")
class TestSuiteWithEcflowNode:
    def test_full_tree_with_fake_ecflow(self, default_config, tmp_directory, fake_ecflow):
        assert fake_ecflow is not None

        suite_name = "test_suite_fake_ecflow"
        config = default_config.copy(
            update={
                "general": {"case": suite_name},
                "scheduler": {
                    "ecfvars": {
                        "ecf_files": f"{tmp_directory}/ecf_files_fake",
                        "ecf_jobout": f"{tmp_directory}/jobout_fake",
                    }
                },
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                    "mirrorglobalDT": {"remote_path": "@YYYY@/@HH@mm@"},
                },
                "suite_control": {
                    "do_marsprep": False,
                    "interpolate_boundaries": False,
                    "do_assimilation": True,
                },
                "da": {"upper_air": True, "bgcycle": "2022050200"},
                "eps": {"general": {"members": [0, 1]}},
                "submission": {"max_ecf_tasks": 2},
                "reference_checker": {"check": True},
            }
        )
        config = config.copy(update=set_times(config))

        defs = TactusSuiteDefinition(config, dry_run=False)

        # The suite-level ecf_node is real (fake) since dry_run=False, so the
        # max_ecf_tasks limit branch (tactus.py) must have run.
        assert defs.suite.ecf_node is not None
        assert ("max_ecf_tasks", 2) in defs.suite.ecf_node.limits

        def_file = f"{tmp_directory}/{suite_name}_fake.def"
        defs.save_as_defs(def_file)

    def test_mirror_globaldt_and_offline(
        self, default_config, tmp_directory, fake_ecflow
    ):
        """Exercise MirrorFamily/MirrorSuite and the member-specific static data limit.

        Neither is reached by a plain dry_run=True build (the former needs a
        real ecf_node, the latter is only visited when ecf_node is not
        None).
        """
        assert fake_ecflow is not None

        suite_name = "test_suite_mirror"
        mirror_settings = {
            "mirror_name": "mirror",
            "remote_path": "/SUITE/Path/mirror",
            "remote_host": "localhost",
            "remote_port": "3141",
            "remote_polling": "60",
            "remote_ssl": False,
            "remote_auth": "",
            "check_var": "YMD",
        }
        config = default_config.copy(
            update={
                "general": {"case": suite_name},
                "scheduler": {
                    "ecfvars": {
                        "ecf_files": f"{tmp_directory}/ecf_files_mirror",
                        "ecf_jobout": f"{tmp_directory}/jobout_mirror",
                    },
                    "mirror_globalDT": mirror_settings,
                    "mirror_offline": mirror_settings,
                    "mirror_suite": {
                        **mirror_settings,
                        "mirror_name": "@HOST_SUITE@",
                    },
                },
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                },
                "suite_control": {
                    "mirror_globalDT": True,
                    "mirror_offline": True,
                    "mirror_suite": True,
                    "member_specific_static_data": True,
                },
                "eps": {"general": {"members": [0, 1]}},
            }
        )
        config = config.copy(update=set_times(config))

        defs = TactusSuiteDefinition(config, dry_run=False)

        def_file = f"{tmp_directory}/{suite_name}.def"
        defs.save_as_defs(def_file)
        assert os.path.exists(def_file)


@pytest.mark.usefixtures("_module_mockers")
class TestPrepFamilyDirect:
    def test_restart_mode_sets_bd_step_index(self, default_config, tmp_directory):
        """Cover the ``mode == "restart"`` branch inside PrepFamily.

        InterpolationFamily never calls PrepFamily in restart mode (it skips
        the whole family in that case), so this branch can only be reached
        by instantiating PrepFamily directly.
        """
        ecf_files = f"{tmp_directory}/prep_family_ecf"
        config = default_config.copy(
            update={
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                },
                "suite_control": {"mode": "restart", "split_mars_by_step": True},
            }
        )
        config = config.copy(update=set_times(config))
        task_settings = TaskSettings(config)
        root = EcflowSuite("PrepRoot", ecf_files, dry_run=True)

        fam = PrepFamily(root, config, task_settings, _INPUT_TEMPLATE, ecf_files)

        assert fam.name == "Prep"


@pytest.mark.usefixtures("_module_mockers")
class TestStaticDataMemberGeneratorDirect:
    def test_non_member_specific_iteration_yields_member_zero(
        self, default_config, tmp_directory
    ):
        """Cover the non-member-specific branch of ``__iter__``.

        Only reached when a StaticDataMemberGenerator is iterated while
        ``member_specific_static_data`` is False; StaticDataFamily itself
        never does this (it only builds one when the flag is True), so this
        is exercised directly.
        """
        ecf_files = f"{tmp_directory}/static_data_member_gen_ecf"
        config = default_config.copy(
            update={
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                },
                "suite_control": {"member_specific_static_data": False},
            }
        )
        config = config.copy(update=set_times(config))
        task_settings = TaskSettings(config)
        root = EcflowSuite("StaticDataRoot", ecf_files, dry_run=True)

        generator = StaticDataMemberGenerator(
            parent=root,
            config=config,
            task_settings=task_settings,
            input_template=_INPUT_TEMPLATE,
            ecf_files=ecf_files,
            ecf_files_remotely=None,
            dry_run=True,
        )

        members = list(generator)
        assert [member for member, _family in members] == [0]


@pytest.mark.usefixtures("_module_mockers")
class TestMirrorFamilyDirect:
    def test_all_mirror_types_build_mirror_tasks(
        self, default_config, tmp_directory, fake_ecflow
    ):
        """Build MirrorFamily directly with all three mirror kinds enabled.

        Avoids threading suite_control/scheduler mirror settings through the
        whole TactusSuiteDefinition/TimeDependentFamily machinery.
        """
        assert fake_ecflow is not None

        ecf_files = f"{tmp_directory}/mirror_family_ecf"
        mirror_settings = {
            "mirror_name": "mirror",
            "remote_path": "/SUITE/Path/mirror",
            "remote_host": "localhost",
            "remote_port": "3141",
            "remote_polling": "60",
            "remote_ssl": False,
            "remote_auth": "",
            "check_var": "",
        }
        config = default_config.copy(
            update={
                "platform": {
                    "tactus_home": f"{os.path.dirname(__file__)}/../..",
                    "unix_group": "",
                },
                "scheduler": {
                    "mirror_globalDT": mirror_settings,
                    "mirror_host_case": mirror_settings,
                    "mirror_offline": mirror_settings,
                },
                "suite_control": {
                    "mirror_globalDT": True,
                    "mirror_host_case": True,
                    "mirror_offline": True,
                },
            }
        )
        config = config.copy(update=set_times(config))
        task_settings = TaskSettings(config)
        root = EcflowSuite("MirrorRoot", ecf_files, dry_run=False)

        fam = MirrorFamily(
            root,
            config,
            task_settings,
            _INPUT_TEMPLATE,
            ecf_files,
            cycle_valid="2022-01-01T00:00:00Z",
        )

        assert fam.ecf_node is not None
        assert len(fam.ecf_node.children) == 3
