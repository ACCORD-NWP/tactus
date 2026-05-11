"""Unit tests for tactus.suites.base."""

from types import SimpleNamespace

from tactus.suites.base import EcflowNode


def _node(path):
    """Return a minimal stand-in for EcflowNode with the given path."""
    return SimpleNamespace(path=path)


def make_relative(path, trigger_string):
    """Call EcflowNode.make_relative without constructing a full node."""
    return EcflowNode.make_relative(_node(path), trigger_string)


class TestMakeRelative:
    """Tests for EcflowNode.make_relative."""

    def test_sibling_task(self):
        """Path within the same family becomes a bare name."""
        result = make_relative(
            "/suite/fam/task",
            "(/suite/fam/other == complete)",
        )
        assert result == "(other == complete)"

    def test_different_family(self):
        """Path in a sibling family gets one level of '..'."""
        result = make_relative(
            "/suite/fam1/task",
            "(/suite/fam2/other == complete)",
        )
        assert result == "(../fam2/other == complete)"

    def test_multiple_levels_up(self):
        """Path that requires traversing two levels up."""
        result = make_relative(
            "/suite/fam/subfam/task",
            "(/suite/other/task == complete)",
        )
        assert result == "(../../other/task == complete)"

    def test_multiple_triggers_with_and(self):
        """All absolute paths in an AND expression are made relative."""
        result = make_relative(
            "/suite/fam/task",
            "(/suite/fam/a == complete AND /suite/fam/b == complete)",
        )
        assert result == "(a == complete AND b == complete)"

    def test_multiple_triggers_with_or(self):
        """All absolute paths in an OR expression are made relative."""
        result = make_relative(
            "/suite/fam/task",
            "(/suite/fam/a == complete OR /suite/fam/b == complete)",
        )
        assert result == "(a == complete OR b == complete)"

    def test_nested_parentheses(self):
        """Paths inside nested parentheses are converted correctly."""
        result = make_relative(
            "/suite/fam/task",
            "(/suite/fam/a == complete AND (/suite/fam/b == complete OR /suite/fam/c == complete))",
        )
        assert result == "(a == complete AND (b == complete OR c == complete))"

    def test_no_absolute_paths(self):
        """Trigger strings without absolute paths are returned unchanged."""
        trigger = "(some_var == active)"
        assert make_relative("/suite/fam/task", trigger) == trigger

    def test_mixed_families_and_levels(self):
        """Trigger combining paths from different depths."""
        result = make_relative(
            "/suite/fam1/subfam/task",
            "(/suite/fam1/other == complete AND /suite/fam2/task == complete)",
        )
        assert result == "(../other == complete AND ../../fam2/task == complete)"
