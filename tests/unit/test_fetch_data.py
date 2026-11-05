"""Tests for the IAL-data fetching task, suite and fetch-data command."""

import argparse
import os

import pytest

from tactus.argparse_wrapper import get_args_parser
from tactus.commands_functions import create_fetch_data_exp
from tactus.tasks.fetch_data import IALDataFetch

# ---------------------------------------------------------------------------
# fetch-data command
# ---------------------------------------------------------------------------


def test_fetch_data_parser_defaults():
    """Check default arguments for the fetch-data subcommand."""
    parser = get_args_parser()
    args = parser.parse_args(["fetch-data"])

    assert args.run_command is create_fetch_data_exp
    assert args.data_dir is None
    assert args.ial_data_version is None
    assert args.ial_data_repo is None
    assert args.dvc_targets is None
    assert args.output_file is None
    assert args.start_suite is True
    assert args.case is None


def test_fetch_data_parser_accepts_expected_options():
    """Check explicitly provided fetch-data command options."""
    parser = get_args_parser()
    args = parser.parse_args([
        "fetch-data",
        "--data-dir",
        "/some/dir",
        "--ial-data-version",
        "v1.0",
        "--ial-data-repo",
        "foo",
        "--target",
        "data/ncdir",
        "--target",
        "data/rrtm",
        "-o",
        "fetch.toml",
        "--case-name",
        "my_fetch",
        "-d",
        "-k",
        "-e",
    ])

    assert args.data_dir == "/some/dir"
    assert args.ial_data_version == "v1.0"
    assert args.ial_data_repo == "foo"
    assert args.dvc_targets == ["data/ncdir", "data/rrtm"]
    assert args.output_file == "fetch.toml"
    assert args.case == "my_fetch"
    assert args.start_suite is False
    assert args.keep_def_file is True
    assert args.expand_config is True


@pytest.fixture
def captured_create_exp(monkeypatch):
    """Replace create_exp and capture its arguments."""
    captured = {}

    def fake_create_exp(args, config):
        captured["args"] = args
        captured["config"] = config

    monkeypatch.setattr("tactus.commands_functions.create_exp", fake_create_exp)
    return captured


def test_create_fetch_data_exp_sets_config_and_modifications(
    default_config, captured_create_exp, tmp_path, monkeypatch
):
    """Check command line arguments end up in the fetch_data config section."""
    monkeypatch.chdir(tmp_path)
    parser = get_args_parser()
    args = parser.parse_args([
        "fetch-data",
        "--data-dir",
        "relative/dir",
        "--ial-data-version",
        "v1.0",
        "--ial-data-repo",
        "foo",
        "--target",
        "data/ncdir",
    ])

    create_fetch_data_exp(args, default_config)

    config = captured_create_exp["config"]
    assert config["fetch_data.dir"] == str((tmp_path / "relative/dir").resolve())
    assert config["fetch_data.ial_data_git_version"] == "v1.0"
    assert config["fetch_data.ial_data_git_repo"] == "foo"
    assert list(config["fetch_data.dvc_targets"]) == ["data/ncdir"]
    assert args.config_mods == [
        "tactus/data/config_files/modifications/@HOST@.toml",
        "tactus/data/config_files/modifications/fetch_data_suite.toml",
    ]


def test_create_fetch_data_exp_keeps_config_defaults(default_config, captured_create_exp):
    """Check config values are kept when no arguments are given."""
    parser = get_args_parser()
    args = parser.parse_args(["fetch-data", "--data-dir", "@SCRATCH@/IAL-data"])

    create_fetch_data_exp(args, default_config)

    config = captured_create_exp["config"]
    # Macros are not resolved by the command, but by the task
    assert config["fetch_data.dir"] == "@SCRATCH@/IAL-data"
    assert (
        config["fetch_data.ial_data_git_version"]
        == default_config["fetch_data.ial_data_git_version"]
    )
    assert (
        config["fetch_data.ial_data_git_repo"]
        == default_config["fetch_data.ial_data_git_repo"]
    )


# ---------------------------------------------------------------------------
# IALDataFetch task
# ---------------------------------------------------------------------------


@pytest.fixture
def fetch_task(default_config, tmp_path):
    """Return a factory creating an IALDataFetch task."""

    def _create(**fetch_data):
        settings = {
            "dir": str(tmp_path / "IAL-data"),
            "ial_data_git_repo": "git@example.com:foo/IAL-data.git",
            "ial_data_git_version": "v1.0",
            "dvc_bin": "/path/to/dvc",
        }
        settings.update(fetch_data)
        config = default_config.copy(
            update={
                "fetch_data": settings,
                "system": {"wrk": str(tmp_path / "wrk")},
                "platform": {"unix_group": ""},
            }
        )
        return IALDataFetch(config)

    return _create


def test_task_clones_when_dir_does_not_exist(fetch_task, tmp_path):
    task = fetch_task()
    data_dir = tmp_path / "IAL-data"

    assert task.git_commands() == [
        f"git clone git@example.com:foo/IAL-data.git {data_dir}",
        f"cd {data_dir}; git checkout v1.0",
        f"cd {data_dir}; "
        "if git symbolic-ref -q HEAD > /dev/null; then git pull --ff-only; fi",
    ]
    assert task.dvc_command() == f"cd {data_dir}; /path/to/dvc pull"


def test_task_updates_existing_clone(fetch_task, tmp_path):
    (tmp_path / "IAL-data" / ".git").mkdir(parents=True)
    task = fetch_task()

    cmds = task.git_commands()
    assert cmds[0] == f"cd {tmp_path / 'IAL-data'}; git fetch --tags origin"
    assert not any("git clone" in cmd for cmd in cmds)


def test_task_refuses_non_empty_non_git_dir(fetch_task, tmp_path):
    (tmp_path / "IAL-data").mkdir()
    (tmp_path / "IAL-data" / "something").touch()
    task = fetch_task()

    with pytest.raises(RuntimeError, match="is not a git repository"):
        task.git_commands()


def test_task_dvc_targets_and_jobs(fetch_task, tmp_path):
    task = fetch_task(dvc_targets=["data/ncdir", "data/rrtm/@CYCLE@"], dvc_jobs=4)
    cycle = task.platform.substitute("@CYCLE@")

    assert task.dvc_command() == (
        f"cd {tmp_path / 'IAL-data'}; /path/to/dvc pull --jobs 4 "
        f"data/ncdir data/rrtm/{cycle}"
    )


def test_task_expands_directory_targets(fetch_task, tmp_path):
    coef = tmp_path / "IAL-data" / "data" / "satellite" / "coef"
    coef.mkdir(parents=True)
    (coef / "b.dat.dvc").touch()
    (coef / "a.dat.dvc").touch()
    (coef / "a.dat").touch()
    (coef / ".gitignore").touch()
    (tmp_path / "IAL-data" / "data" / "empty").mkdir()

    task = fetch_task(dvc_targets=["data/satellite", "data/ncdir/file.nc.dvc"])
    assert task.expand_targets() == [
        "data/satellite/coef/a.dat.dvc",
        "data/satellite/coef/b.dat.dvc",
        "data/ncdir/file.nc.dvc",
    ]

    task = fetch_task(dvc_targets=["data/empty"])
    with pytest.raises(FileNotFoundError, match="No dvc tracked data"):
        task.expand_targets()


def test_task_execute_runs_git_then_dvc(fetch_task, monkeypatch):
    task = fetch_task()
    ran = []
    monkeypatch.setattr(
        "tactus.tasks.fetch_data.BatchJob.run", lambda _self, cmd: ran.append(cmd)
    )

    task.execute()

    assert ran == [*task.git_commands(), task.dvc_command()]


def test_get_dvc_bin(monkeypatch, tmp_path):
    assert IALDataFetch.get_dvc_bin("/my/dvc") == "/my/dvc"

    python = tmp_path / "bin" / "python"
    python.parent.mkdir()
    monkeypatch.setattr("sys.executable", str(python))
    monkeypatch.setattr("shutil.which", lambda _name: None)
    assert IALDataFetch.get_dvc_bin() == "dvc"

    (python.parent / "dvc").touch()
    assert IALDataFetch.get_dvc_bin() == os.fspath(python.parent / "dvc")


def test_command_namespace_without_optional_arguments(
    default_config, captured_create_exp
):
    """Check a minimal Namespace (as used by other callers) is accepted."""
    args = argparse.Namespace(
        data_dir=None, ial_data_version=None, ial_data_repo=None, dvc_targets=None
    )
    create_fetch_data_exp(args, default_config)
    assert captured_create_exp["config"]["fetch_data.dir"] == "@CASEDIR@/IAL-data"
