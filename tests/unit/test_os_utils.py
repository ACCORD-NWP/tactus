"""Unit tests for os_utils."""

import os
import shutil
import tempfile
from pathlib import Path
from unittest import mock

from tactus.os_utils import Search, deodemakedirs, ping, strip_off_mount_path


class TestSearch:
    """Test the Search class."""

    def test_find_files_return_type(self):
        """Test that the return type of find_files is a list."""
        files = Search.find_files(
            directory="/",
            prefix="",
            postfix="",
            recursive=False,
            onlyfiles=True,
            fullpath=True,
            olderthan=None,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory="/",
            prefix="",
            postfix="",
            pattern="(.*)",
            recursive=False,
            onlyfiles=True,
            fullpath=True,
            olderthan=None,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory="/",
            prefix="",
            postfix="",
            recursive=False,
            onlyfiles=True,
            fullpath=False,
            olderthan=None,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory="/",
            prefix="",
            postfix="",
            recursive=False,
            onlyfiles=False,
            fullpath=True,
            olderthan=None,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory="/",
            prefix="",
            postfix="",
            recursive=False,
            onlyfiles=False,
            fullpath=False,
            olderthan=400.0,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory=".",
            prefix="",
            postfix="",
            recursive=True,
            onlyfiles=False,
            fullpath=False,
            olderthan=None,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory=".",
            prefix="",
            postfix=".py",
            recursive=True,
            onlyfiles=True,
            fullpath=False,
            olderthan=None,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory="/",
            prefix="",
            postfix="",
            recursive=False,
            onlyfiles=False,
            fullpath=True,
            olderthan=0,
            inorder=False,
        )
        assert isinstance(files, list)

        files = Search.find_files(
            directory=".",
            prefix="",
            postfix="",
            recursive=False,
            onlyfiles=False,
            fullpath=True,
            olderthan=0,
            inorder=True,
        )
        assert isinstance(files, list)

    def test_search_constructor_return_type(self):
        """Test that the return type of the Search constructor is a Search object."""
        search = Search()
        assert isinstance(search, Search)


def test_deodemakedirs():
    """Test that creation of directories and change of unix_group works."""
    path = tempfile.mkdtemp()
    if os.path.exists(path):
        shutil.rmtree(path)
    grpids = os.getgroups()
    if len(grpids) > 1:
        newgrp = grpids[1]
        deodemakedirs(f"{path}/wrkdir", unixgroup=newgrp)
        assert os.stat(path).st_gid == newgrp

    path = tempfile.mkdtemp()
    if os.path.exists(path):
        shutil.rmtree(path)
    deodemakedirs(f"{path}/wrkdir")
    assert os.stat(path).st_gid in grpids


def test_ping():
    """Test the ping function."""
    hostname = "localhost"

    assert ping(hostname) is True
    assert ping("foo") is False


def with_mock_user(func):
    """Decorator to set the USER environment variable to a mock user."""
    mock_user = "testuser1234"

    def wrapper(self, *args, **kwargs):
        with mock.patch.dict(os.environ, {"USER": mock_user}):
            return func(self, mock_user, *args, **kwargs)

    return wrapper


def with_no_user_var(func):
    """Decorator to unset the USER environment variable."""

    def wrapper(self, *args, **kwargs):
        original_user = os.environ.get("USER")
        # Only unset the USER environment variable if it is set
        if original_user:
            os.environ.pop("USER")

        func(self, *args, **kwargs)
        # Restore the original USER environment variable
        if original_user:
            os.environ["USER"] = original_user

    return wrapper


class TestStripOffMountPath:
    """Test the strip_off_mount_path function."""

    @with_no_user_var
    def test_no_user_in_path(self):
        """Test that the function returns input path, when no user in path."""
        test_path = Path("/foo/bar")
        assert strip_off_mount_path(test_path) == test_path

    @with_mock_user
    def test_user_in_path(self, mock_user):
        """Test the function when user in path, but nothing to strip."""
        test_path = Path(f"/home/{mock_user}/foo/bar")
        assert strip_off_mount_path(test_path) == test_path

    @with_mock_user
    def test_user_in_path_with_mount(self, mock_user):
        """Test the function when user in path and mount path to strip."""
        test_path = Path(f"/etc/ecmwf/nfs/dh1_home_b/{mock_user}/foo/bar")
        expected_result = Path(f"/home/{mock_user}/foo/bar")

        assert strip_off_mount_path(test_path) == expected_result

    @with_mock_user
    def test_user_in_path_with_mount_double_underscore(self, mock_user):
        """Test the function when user in path and double underscore mount path to strip."""
        test_path = Path(f"/etc/ecmwf/nfs/dh1_10_perm_b/{mock_user}/foo/bar")
        expected_result = Path(f"/perm/{mock_user}/foo/bar")

        assert strip_off_mount_path(test_path) == expected_result
