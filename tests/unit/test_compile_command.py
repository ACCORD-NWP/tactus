"""Tests for the tactus compile command."""

import argparse

from tactus.argparse_wrapper import get_args_parser
from tactus.commands_functions import create_compile_exp


def test_compile_parser_defaults():
    """Check default arguments for the compile subcommand."""
    parser = get_args_parser()
    args = parser.parse_args(["compile"])

    assert args.run_command is create_compile_exp
    assert args.output_file is None
    assert args.start_suite is True
    assert args.case is None
    assert args.ial_tag == "develop"


def test_compile_parser_accepts_expected_options():
    """Check explicitly provided compile command options."""
    parser = get_args_parser()
    args = parser.parse_args([
        "compile",
        "--ial-tag",
        "CY50T2",
        "--output",
        "compile_config.toml",
        "--case-name",
        "my_compile_case",
        "--keep-def-file",
        "--expand-config",
    ])

    assert args.ial_tag == "CY50T2"
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


def test_create_compile_exp_uses_default_ial_tag_from_parser(default_config, monkeypatch):
    """Check compile command uses the parser default IAL tag when none is provided."""
    captured = {}

    def fake_create_exp(args, config):
        captured["args"] = args
        captured["config"] = config

    monkeypatch.setattr("tactus.commands_functions.create_exp", fake_create_exp)

    parser = get_args_parser()
    args = parser.parse_args(["compile"])

    create_compile_exp(args, default_config)

    assert captured["config"]["compile.ial_git_branch"] == "develop"
