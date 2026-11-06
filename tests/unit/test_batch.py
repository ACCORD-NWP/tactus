#!/usr/bin/env python3
"""Unit tests tasks/batch.py."""

import os

import pytest

from tactus.tasks.batch import BatchJob


def test_run():
    # Test simple run command
    BatchJob(os.environ, wrapper="").run("echo 'foo'")


def test_run_with_logfile_as_str(tmp_directory):
    # Test failure if logfile is str
    logfile = f"{tmp_directory}/test_as_str.log"
    BatchJob(os.environ, wrapper="").run("echo 'foo'", logfile=logfile)
    assert os.path.isfile(logfile)


def test_run_with_logfile_not_open(tmp_directory):
    # Test failure if logfile is not open
    logfile = f"{tmp_directory}/test_not_open.log"
    log_handle = open(logfile, "w")
    log_handle.close()
    with pytest.raises(ValueError, match=f"{log_handle.name} is not open"):
        BatchJob(os.environ, wrapper="").run("echo 'foo'", logfile=log_handle)


def test_run_with_logfile(tmp_directory):
    # Test that we capture the STDOUT output
    logfile = f"{tmp_directory}/test_check_msg.log"
    msg = "foo"
    BatchJob(os.environ, wrapper="").run(f"echo {msg}", logfile=logfile)
    with open(logfile, "r") as f:
        first_line = f.readline().strip()

    assert first_line == msg
