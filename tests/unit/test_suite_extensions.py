#!/usr/bin/env python3
"""Unit tests for ``tactus.suites.suite_extensions``."""

import os
from contextlib import suppress

import pytest

from tactus.derived_variables import set_times
from tactus.submission import TaskSettings
from tactus.suites import suite_extensions
from tactus.suites.base import EcflowSuiteFamily, EcflowSuiteTask
from tactus.suites.suite_extensions import (
    SuiteComponent,
    TaskComponent,
    get_components,
    register_component,
)
from tactus.suites.tactus import TactusSuiteDefinition
from tactus.suites.tactus_suite_components import (
    CycleFamily,
    ForecastFamily,
    TimeDependentFamily,
)


@pytest.fixture(autouse=True)
def _isolated_registry(monkeypatch):
    """Let each test register components without affecting other tests."""
    monkeypatch.setattr(suite_extensions, "_components", {})


class _Family(EcflowSuiteFamily):
    """Family with the arguments of the tactus suite families.

    The components get these arguments, so they are unused here.
    """

    def __init__(
        self,
        parent,
        config,  # ruff:ignore[unused-method-argument]
        task_settings,  # ruff:ignore[unused-method-argument]
        input_template,  # ruff:ignore[unused-method-argument]
        ecf_files,
        ecf_files_remotely=None,  # ruff:ignore[unused-method-argument]
    ):
        super().__init__("Family", parent, ecf_files)
        EcflowSuiteFamily("Child", self, ecf_files)


class _SubFamily(_Family):
    """Family extending another family class."""

    def __init__(self, parent, config, task_settings, input_template, ecf_files):
        super().__init__(parent, config, task_settings, input_template, ecf_files)
        EcflowSuiteFamily("SubChild", self, ecf_files)


class _Recorder(SuiteComponent):
    """Component recording its calls instead of adding nodes."""

    family = _Family
    active = True
    calls = []

    def is_active(self, config):  # ruff:ignore[unused-method-argument]
        return self.active

    def add_nodes(self, parent, ctx):
        trigger = [node.name for node in ctx.trigger or []]
        self.calls.append((self.name, parent.name, ctx.config, trigger))
        return self.name


class TestRegistry:
    def test_register_requires_name(self):
        class NoName(SuiteComponent):
            family = _Family

        with pytest.raises(ValueError, match="must set name"):
            register_component(NoName)

    @pytest.mark.parametrize(
        "family", [None, "ForecastFamily", EcflowSuiteTask, EcflowSuiteFamily]
    )
    def test_register_rejects_non_family_subclass(self, family):
        class BadFamily(SuiteComponent):
            name = "BadFamily"

        BadFamily.family = family
        with pytest.raises(ValueError, match="must be a subclass of EcflowSuite"):
            register_component(BadFamily)

    def test_register_returns_class(self):
        class Comp(_Recorder):
            name = "Comp"

        assert register_component(Comp) is Comp
        assert [c.name for c in get_components(_Family)] == ["Comp"]

    def test_reregister_replaces(self):
        @register_component
        class First(_Recorder):
            name = "Same"

        @register_component
        class Second(_Recorder):
            name = "Same"

        components = get_components(_Family)
        assert len(components) == 1
        assert isinstance(components[0], Second)

    def test_parent_class_components_included(self):
        @register_component
        class OfParent(_Recorder):
            name = "OfParent"

        @register_component
        class OfSub(_Recorder):
            name = "OfSub"
            family = _SubFamily

        assert [c.name for c in get_components(_SubFamily)] == ["OfSub", "OfParent"]
        assert [c.name for c in get_components(_Family)] == ["OfParent"]


class TestAddedAtEndOfFamily:
    def test_active_components_added_after_children(self):
        _Recorder.calls = []

        @register_component
        class A(_Recorder):
            name = "A"

        @register_component
        class Inactive(_Recorder):
            name = "Inactive"
            active = False

        _Family(None, "config", None, "template", "ecf_files")

        assert _Recorder.calls == [("A", "Family", "config", ["Child"])]

    def test_added_once_after_subclass_init(self):
        _Recorder.calls = []

        @register_component
        class A(_Recorder):
            name = "A"

        _SubFamily(None, "config", None, "template", "ecf_files")

        assert _Recorder.calls == [("A", "Family", "config", ["Child", "SubChild"])]

    def test_component_nodes_are_children(self):
        @register_component
        class AddFamily(SuiteComponent):
            name = "AddFamily"
            family = _Family

            def add_nodes(self, parent, ctx):
                return EcflowSuiteFamily(self.name, parent, ctx.ecf_files)

        family = _Family(None, "config", None, "template", "ecf_files")
        assert [child.name for child in family.children] == ["Child", "AddFamily"]

    def test_not_added_to_class_that_is_no_family_node(self, monkeypatch):
        _Recorder.calls = []
        warnings = []
        monkeypatch.setattr(
            suite_extensions.logger, "warning", lambda *args: warnings.append(args)
        )

        @register_component
        class A(_Recorder):
            name = "A"

        family = _Family.__new__(_Family)
        suite_extensions.add_family_components(family, _Family.__init__, (), {})

        assert _Recorder.calls == []
        assert len(warnings) == 1

    def test_base_add_nodes_not_implemented(self):
        with pytest.raises(NotImplementedError):
            SuiteComponent().add_nodes("parent", None)


@pytest.fixture
def _no_parse_job_errors(monkeypatch):
    original = TaskSettings.parse_job

    def parse_job(self, **kwargs):
        with suppress(RuntimeError):
            original(self, **kwargs)

    monkeypatch.setattr(TaskSettings, "parse_job", parse_job)


@pytest.mark.usefixtures("_no_parse_job_errors")
def test_components_in_suite(default_config, tmp_directory, monkeypatch):
    """Registered task components are added at the end of their families."""
    created = []
    original_add_nodes = TaskComponent.add_nodes

    def add_nodes(self, parent, ctx):
        created.append((
            self.name,
            parent.name,
            sorted(node.name for node in ctx.trigger),
        ))
        task = original_add_nodes(self, parent, ctx)
        assert parent.children[-1] is task
        return task

    monkeypatch.setattr(TaskComponent, "add_nodes", add_nodes)

    @register_component
    class ForecastTask(TaskComponent):
        name = "ForecastTask"
        family = ForecastFamily

    @register_component
    class CycleTask(TaskComponent):
        name = "CycleTask"
        family = CycleFamily

    @register_component
    class InactiveTask(TaskComponent):
        name = "InactiveTask"
        family = ForecastFamily

        def is_active(self, config):  # ruff:ignore[unused-method-argument]
            return False

    @register_component
    class TimeDependentTask(TaskComponent):
        name = "TimeDependentTask"
        family = TimeDependentFamily

    config = default_config.copy(
        update={
            "general": {
                "case": "test_suite",
                "times": {
                    "start": "2022-05-02T00:00:00Z",
                    "end": "2022-05-02T03:00:00Z",
                },
            },
            "scheduler": {
                "ecfvars": {
                    "ecf_files": f"{tmp_directory}/ecf_files",
                    "ecf_jobout": f"{tmp_directory}/jobout",
                }
            },
            "platform": {
                "tactus_home": f"{os.path.dirname(__file__)}/../..",
                "unix_group": "",
            },
            "eps": {"general": {"members": [0, 1]}},
        }
    )
    config = config.copy(update=set_times(config))
    TactusSuiteDefinition(config, dry_run=True)

    # Two cycles with two members each
    forecast = [c for c in created if c[0] == "ForecastTask"]
    assert len(forecast) == 4
    assert all(c[1] == "Forecasting" for c in forecast)
    assert all("Forecast" in c[2] for c in forecast)
    assert [c for c in created if c[0] == "CycleTask"] == [
        ("CycleTask", "Cycle", ["Forecasting"])
    ] * 4
    assert not [c for c in created if c[0] in ("InactiveTask", "TimeDependentTask")]
