"""Utilities for simple tasks on OS level."""
import os
import re
import shutil
import time
from pathlib import Path


class Search:
    """Search class."""

    def __init__(self):
        """Construct search class."""
        return

    @staticmethod
    def find_files(
        directory,
        prefix="",
        postfix="",
        pattern="",
        recursive=True,
        onlyfiles=True,
        fullpath=False,
        olderthan=None,
        inorder=False,
    ) -> list:
        """Find files in a directory.

        Args:
            directory (str): Directory to search in.
            prefix (str, optional): Only find files with this prefix. Defaults to "".
            postfix (str, optional): Only find files with the postfix. Defaults to "".
            pattern (str, optional): Only find files with matching pattern. Defaults to "".
            recursive (bool, optional): Go into directories recursively. Defaults to True.
            onlyfiles (bool, optional): Show only files. Defaults to True.
            fullpath (bool, optional): Give full path. Defaults to False. If recursive=True, fullpath is given automatically.
            olderthan (int, optional): Match only files older than X seconds from now. Defaults to None.
            inorder (bool, optional): Return sorted list of filenames. Defaults to False.

        Returns:
            list: List containing file names that matches criterias

        Examples:
            >>> files = find_files('/foo/', prefix="", postfix="", recursive=False, onlyfiles=True, fullpath=True, olderthan=86400*100)
        """
        if recursive:
            fullpath = False
            files = []
            for r, _d, f in os.walk(directory):  # r=root, d=directories, f=files
                for file in f:
                    if file.startswith(prefix) and file.endswith(postfix):
                        files.append(os.path.join(r, file))

        elif not recursive:

            if onlyfiles:
                files = [
                    f
                    for f in os.listdir(directory)
                    if f.endswith(postfix)
                    and f.startswith(prefix)
                    and os.path.isfile(os.path.join(directory, f))
                ]

            elif not onlyfiles:
                files = [
                    f
                    for f in os.listdir(directory)
                    if f.endswith(postfix) and f.startswith(prefix)
                ]

        if pattern != "":
            files = [f for f in files if re.search(pattern, f)]

        if fullpath:
            files = [os.path.join(directory, f) for f in files]

        if olderthan is not None:
            now = time.time()
            tfiles = []
            for f in files:
                try:
                    if not fullpath:
                        if os.path.getmtime(os.path.join(directory, f)) < (
                            now - olderthan
                        ):
                            tfiles.append(f)
                    else:
                        if os.path.getmtime(f) < (now - olderthan):
                            tfiles.append(f)
                except FileNotFoundError:
                    continue

            files = tfiles

        if inorder:
            files = sorted(files)

        return files


def filepath_iterator(paths, filename_pattern="*"):
    """Return iterator of paths to files given a list of file or directory paths.

    Given a path or list of paths, yield Path objects corresponding to them.
    If a path points to a directory, then the directory is searched recursively
    and the paths to the files found in this process will be yielded.

    Args:
        paths (typing.Union[pathlib.Path, List[pathlib.Path], str, List[str]]):
            A single path or a collection of paths.
        filename_pattern (str, optional): Pattern used in the recursive glob in
            order to select the names of the files to be yielded.
            Defaults to "*".

    Yields:
        pathlib.Path: Path to located files.
    """
    if isinstance(paths, (str, Path)):
        paths = [paths]

    for path in paths:
        path = Path(path).expanduser().resolve()
        if path.is_dir():
            for subpath in path.rglob(filename_pattern):
                if subpath.is_file():
                    yield subpath
        else:
            yield path


def deodemakedirs(path, unixgroup="", exist_ok=True):
    """Create directories and change unix group as required.

    For a given path the top directory that does not yet exist is searched for, created
    and unix group is set, if required. Permissions are set such that all subdirectories
    and new files inherit the unix group.

    Args:
        path (str): directory path that should be created if it doesn't already exist.
        unixgroup (str, optional): unix group the newly created directories should belong to.
        exist_ok (boolean, optional): Define whether directories may already exist or whether
            an error should be raised.

    Raises:
        OSError  # noqa DAR401

    Returns: Nothing
    """
    p = Path(path).resolve()

    if p.parents[0].is_dir():
        try:
            os.makedirs(path, mode=0o2750, exist_ok=exist_ok)
            if unixgroup and (str(Path(path).group()) != str(unixgroup)):
                shutil.chown(path, group=unixgroup)
                os.chmod(path, mode=0o2750)
        except OSError:
            raise OSError(f"Cannot create {path} properly")
    else:
        # check directory tree for top dir that has to be created
        try:
            idx = 0

            while not p.parents[idx + 1].is_dir():
                idx += 1

            os.makedirs(p.parents[idx], mode=0o2750, exist_ok=exist_ok)
            if unixgroup and str(p.parents[idx].group()) != str(unixgroup):
                shutil.chown(p.parents[idx], group=unixgroup)
                os.chmod(p.parents[idx], mode=0o2750)
            os.makedirs(path)
        except OSError:
            raise OSError(f"Cannot create {path} properly")
