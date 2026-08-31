#!/usr/bin/env python3
"""Unit tests tasks/batch.py."""

import os

import pytest

from tactus.tasks.batch import BatchJob


def test_run():
    # Test simple run command
    BatchJob(os.environ, wrapper="").run("echo 'foo'")


def test_run_with_logfile_as_str():
    # Test failure if logfile is str
    with pytest.raises(TypeError, match="logfile must be a io.IOBase file object"):
        BatchJob(os.environ, wrapper="").run("echo 'foo'", logfile="test.log")


def test_run_with_logfile_not_open():
    # Test failure if logfile is not open
    logfile = open("test.log", "w")
    logfile.close()
    with pytest.raises(ValueError, match=f"{logfile.name} is not open"):
        BatchJob(os.environ, wrapper="").run("echo 'foo'", logfile=logfile)


def test_run_with_logfile():
    # Test that we capture the STDOUT output
    logfile = open("test.log", "w")
    msg = "foo"
    BatchJob(os.environ, wrapper="").run(f"echo {msg}", logfile=logfile)
    logfile.close()
    with open("test.log", "r") as logfile:
        first_line = logfile.readline().strip()

    assert first_line == msg
