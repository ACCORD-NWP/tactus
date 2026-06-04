"""Unit tests for os_utils."""

import os
import shutil
import sys
import tempfile
from pathlib import Path
from unittest import mock

import pytest

from tactus.os_utils import (
    Search,
    ping,
    resolve_path_relative_to_package,
    strip_off_mount_path,
    tactusmakedirs,
)


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


def test_tactusmakedirs():
    """Test that creation of directories and change of unix_group works."""
    path = tempfile.mkdtemp()
    if os.path.exists(path):
        shutil.rmtree(path)
    grpids = os.getgroups()
    if len(grpids) > 1:
        newgrp = grpids[1]
        tactusmakedirs(f"{path}/wrkdir", unixgroup=newgrp)
        assert os.stat(path).st_gid == newgrp

    path = tempfile.mkdtemp()
    if os.path.exists(path):
        shutil.rmtree(path)
    tactusmakedirs(f"{path}/wrkdir")
    assert os.stat(path).st_gid in grpids


def test_lockfile_basic():
    """Test the FileLock class."""
    from tactus.os_utils import FileLock

    path = tempfile.mkdtemp()
    filepath = f"{path}/testfile"
    lockfile_path = f"{filepath}.lock"

    lockfile = Path(lockfile_path)
    lockfile.touch(exist_ok=True)

    assert os.path.exists(lockfile_path)
    with (
        pytest.raises(TimeoutError),
        FileLock(filepath, timeout=0.1, check_interval=0.1, delete_existing=False),
    ):
        assert os.path.exists(lockfile_path)
    assert os.path.exists(lockfile_path)
    with FileLock(filepath, delete_existing=True):
        assert os.path.exists(lockfile_path)
    assert not os.path.exists(lockfile_path)


def test_lockfile_timeout():
    """Test the FileLock class."""
    from tactus.os_utils import FileLock

    path = tempfile.mkdtemp()
    filepath = f"{path}/testfile"
    lockfile_path = f"{filepath}.lock"

    lockfile = Path(lockfile_path)
    lockfile.touch(exist_ok=True)

    assert os.path.exists(lockfile_path)
    with (
        pytest.raises(TimeoutError),
        FileLock(filepath, timeout=0.1, check_interval=0.1, delete_existing=False),
    ):
        assert os.path.exists(lockfile_path)
    assert os.path.exists(lockfile_path)


def test_lockfile_thread():
    """Test the FileLock class."""
    from threading import Thread
    from time import sleep

    from tactus.os_utils import FileLock

    def get_lock(filepath):
        with FileLock(filepath):
            sleep(0.5)

    path = tempfile.mkdtemp()
    filepath = f"{path}/testfile"
    thread = Thread(target=get_lock, args=(filepath,))
    thread2 = Thread(target=get_lock, args=(filepath,))
    thread.start()
    thread2.start()
    thread.join()


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


class TestResolvePathRelativeToPackage:
    """Test the resolve_path_relative_to_package function."""

    def test_existing_path_returned_directly(self, tmp_path):
        """A path that already exists on disk is returned as-is."""
        f = tmp_path / "file.txt"
        f.write_text("content")
        assert resolve_path_relative_to_package(f) == f

    def test_resolved_under_sys_path_entry(self, tmp_path):
        """Path resolved to an existing file found under a different sys.path entry."""
        real_base = tmp_path / "real_base"
        (real_base / "pkg" / "data").mkdir(parents=True)
        target = real_base / "pkg" / "data" / "file.txt"
        target.write_text("content")

        fake_base = tmp_path / "fake_base"
        fake_path = fake_base / "pkg" / "data" / "file.txt"

        with mock.patch.object(sys, "path", [str(fake_base), str(real_base)]):
            result = resolve_path_relative_to_package(fake_path)

        assert result == target

    def test_not_found_raises_file_not_found(self, tmp_path):
        """FileNotFoundError when the file is not found under any sys.path entry."""
        fake_base = tmp_path / "fake_base"
        real_base = tmp_path / "real_base"
        real_base.mkdir()
        fake_path = fake_base / "pkg" / "missing.txt"

        with (
            mock.patch.object(sys, "path", [str(fake_base), str(real_base)]),
            pytest.raises(FileNotFoundError),
        ):
            resolve_path_relative_to_package(fake_path)

    def test_not_found_ignore_errors_returns_path(self, tmp_path):
        """Returns the resolved path unchanged when not found and ignore_errors=True."""
        fake_base = tmp_path / "fake_base"
        fake_path = fake_base / "pkg" / "missing.txt"

        with mock.patch.object(sys, "path", [str(fake_base)]):
            result = resolve_path_relative_to_package(fake_path, ignore_errors=True)

        assert result == fake_path.resolve()

    def test_multiple_candidates_raises_value_error(self, tmp_path):
        """ValueError when the relative path resolves under more than one sys.path entry."""
        fake_base = tmp_path / "fake_base"
        fake_path = fake_base / "pkg" / "data" / "file.txt"

        for base_name in ("real_base_a", "real_base_b"):
            base = tmp_path / base_name
            (base / "pkg" / "data").mkdir(parents=True)
            (base / "pkg" / "data" / "file.txt").write_text(base_name)

        with (
            mock.patch.object(
                sys,
                "path",
                [
                    str(fake_base),
                    str(tmp_path / "real_base_a"),
                    str(tmp_path / "real_base_b"),
                ],
            ),
            pytest.raises(ValueError, match="Ambiguous path resolution for"),
        ):
            resolve_path_relative_to_package(fake_path)

    def test_no_matching_sys_path_prefix_raises_file_not_found(self):
        """FileNotFoundError when no sys.path entry is a prefix of the given path."""
        path = Path("/totally/nonexistent/path/file.txt")
        with (
            mock.patch.object(sys, "path", ["/some/unrelated/path"]),
            pytest.raises(FileNotFoundError),
        ):
            resolve_path_relative_to_package(path)

    def test_no_matching_sys_path_prefix_ignore_errors(self):
        """Returns the resolved path unchanged when no prefix matches and ignore_errors=True."""
        path = Path("/totally/nonexistent/path/file.txt")
        with mock.patch.object(sys, "path", ["/some/unrelated/path"]):
            result = resolve_path_relative_to_package(path, ignore_errors=True)

        assert result == path.resolve()

    def test_empty_sys_path_entries_are_skipped(self, tmp_path):
        """Empty strings in sys.path are safely skipped."""
        fake_base = tmp_path / "fake_base"
        fake_path = fake_base / "file.txt"

        with (
            mock.patch.object(sys, "path", ["", str(fake_base), ""]),
            pytest.raises(FileNotFoundError),
        ):
            resolve_path_relative_to_package(fake_path)
