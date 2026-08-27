"""Tests for the tactus compile command."""

import argparse
from pathlib import Path

from tactus.argparse_wrapper import get_args_parser
from tactus.commands_functions import create_compile_exp
from tactus.tasks.compilation import TactusBundleBuild


def test_compile_parser_defaults():
    """Check default arguments for the compile subcommand."""
    parser = get_args_parser()
    args = parser.parse_args(["compile"])

    assert args.run_command is create_compile_exp
    assert args.output_file is None
    assert args.start_suite is True
    assert args.case is None
    assert args.ial_tag is None
    assert args.ial_repo is None


def test_compile_parser_accepts_expected_options():
    """Check explicitly provided compile command options."""
    parser = get_args_parser()
    args = parser.parse_args([
        "compile",
        "--ial-tag",
        "CY50T2",
        "--ial-repo",
        "foo",
        "--output",
        "compile_config.toml",
        "--case-name",
        "my_compile_case",
        "--keep-def-file",
        "--expand-config",
    ])

    assert args.ial_tag == "CY50T2"
    assert args.ial_repo == "foo"
    assert args.output_file == "compile_config.toml"
    assert args.case == "my_compile_case"
    assert args.start_suite is True
    assert args.keep_def_file is True
    assert args.expand_config is True


def test_compile_parser_short_options():
    """Check short aliases for compile command options."""
    parser = get_args_parser()
    args = parser.parse_args([
        "compile",
        "-o",
        "compile_config.toml",
    ])

    assert args.output_file == "compile_config.toml"
    assert args.start_suite is True


def test_create_compile_exp_sets_ial_tag_and_forced_modifications(
    default_config, monkeypatch
):
    """Check compile command prepares config and modification files before creating case."""
    captured = {}

    def fake_create_exp(args, config):
        captured["args"] = args
        captured["config"] = config

    monkeypatch.setattr("tactus.commands_functions.create_exp", fake_create_exp)

    args = argparse.Namespace(
        ial_repo="foo",
        ial_tag="feature/test-branch",
        config_mods=["user_supplied_modification.toml"],
        output_file=None,
        start_suite=False,
        case=None,
        keep_def_file=False,
        expand_config=False,
    )

    create_compile_exp(args, default_config)

    assert captured["args"] is args
    assert captured["config"]["compile.ial_git_branch"] == "feature/test-branch"

    assert args.config_mods == [
        "tactus/data/config_files/modifications/@HOST@.toml",
        "tactus/data/config_files/modifications/compile_suite.toml",
    ]


def test_create_compile_exp_uses_default_ial_tag_from_config(default_config, monkeypatch):
    """Check compile command uses the config default IAL tag when none is provided."""
    captured = {}

    def fake_create_exp(args, config):
        captured["args"] = args
        captured["config"] = config

    monkeypatch.setattr("tactus.commands_functions.create_exp", fake_create_exp)

    parser = get_args_parser()
    args = parser.parse_args(["compile"])

    create_compile_exp(args, default_config)

    assert captured["config"]["compile.ial_git_branch"] == "develop"


def _make_bundle_build_task(bundle_dir, arch):
    """Build a TactusBundleBuild instance without running its __init__."""
    task = TactusBundleBuild.__new__(TactusBundleBuild)
    task.bundle_dir = str(bundle_dir)
    task.arch = arch
    return task


def test_get_install_subpath_without_default_symlink(tmp_path):
    """Check subpath is empty when no `default` symlink exists under arch dir."""
    task = _make_bundle_build_task(tmp_path, "myarch")

    assert task.get_install_subpath() == Path(".")


def test_get_install_subpath_resolves_default_symlink(tmp_path):
    """Check subpath is derived from the resolved `default` symlink target."""
    arch_dir = tmp_path / "myarch"
    build_target = arch_dir / "gnu" / "opt"
    build_target.mkdir(parents=True)
    (arch_dir / "default").symlink_to(build_target, target_is_directory=True)

    task = _make_bundle_build_task(tmp_path, "myarch")

    assert task.get_install_subpath() == Path("gnu/opt")
