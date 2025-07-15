"""Utilities for simple tasks on OS level."""

import contextlib
import glob
import os
import pathlib
import re
import shutil
import subprocess
import time
from pathlib import Path
from typing import List, Union

from . import GeneralConstants
from .logs import logger


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
            pattern (str, optional): Only find files with matching pattern.
                Defaults to "".
            recursive (bool, optional): Go into directories recursively.
                Defaults to True.
            onlyfiles (bool, optional): Show only files. Defaults to True.
            fullpath (bool, optional): Give full path. Defaults to False. If
                recursive=True, fullpath is given automatically.
            olderthan (int, optional): Match only files older than X seconds from now.
                Defaults to None.
            inorder (bool, optional): Return sorted list of filenames. Defaults to False.

        Returns:
            list: List containing file names that matches criterias

        Examples:
            >>> files = find_files(
                            '/foo/', prefix="", postfix="", recursive=False,
                            onlyfiles=True, fullpath=True, olderthan=86400*100
                        )
        """
        if recursive:
            fullpath = False
            files = []
            for r, _d, f in os.walk(directory):  # r=root, d=directories, f=files
                for file in f:
                    if file.startswith(prefix) and file.endswith(postfix):
                        files.append(os.path.join(r, file))  # noqa: PERF401

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
                with contextlib.suppress(FileNotFoundError):
                    if not fullpath:
                        if os.path.getmtime(os.path.join(directory, f)) < (
                            now - olderthan
                        ):
                            tfiles.append(f)
                    elif os.path.getmtime(f) < (now - olderthan):
                        tfiles.append(f)

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

    for path_ in paths:
        path = Path(path_).expanduser().resolve()
        if path.is_dir():
            for subpath in path.rglob(filename_pattern):
                if subpath.is_file():
                    yield subpath
        else:
            yield path


def deodemakedirs(path: str | Path, unixgroup="", exist_ok=True, def_dir_mode=0o755):
    """Create directories and change unix group as required.

    For a given path the top directory that does not yet exist is searched for, created
    and unix group is set, if required. Permissions are set such that all subdirectories
    and new files inherit the unix group.

    Args:
        path (str | Path): directory path that should be created if it doesn't
            already exist.
        unixgroup (str, optional): unix group the newly created dirs should belong to.
        exist_ok (boolean, optional): Define whether directories may already exist
            or whether an error should be raised.
        def_dir_mode(int, optional): Default directory persmission mode. Defaults to 0o755

    Raises:
        OSError: If cannot create the directory.

    """
    p = Path(path).resolve()

    dir_mode = def_dir_mode
    if unixgroup != "":
        dir_mode = 0o2750

    if p.parents[0].is_dir():
        try:
            os.makedirs(path, mode=dir_mode, exist_ok=exist_ok)
            if unixgroup and (str(Path(path).group()) != str(unixgroup)):
                shutil.chown(path, group=unixgroup)
                # TODO: Check if we really need this permissive mask
                os.chmod(path, mode=dir_mode)
        except OSError as err:
            raise OSError(f"Cannot create {path} properly") from err
    else:
        # check directory tree for top dir that has to be created
        try:
            idx = 0

            while not p.parents[idx + 1].is_dir():
                idx += 1

            os.makedirs(p.parents[idx], mode=0o2750, exist_ok=exist_ok)
            if unixgroup and str(p.parents[idx].group()) != str(unixgroup):
                shutil.chown(p.parents[idx], group=unixgroup)
                # TODO: Check if we really need this permissive mask
                os.chmod(p.parents[idx], mode=0o2750)  # noqa S103
            os.makedirs(path)
        except OSError as err:
            raise OSError(f"Cannot create {path} properly") from err


def remove_empty_dirs(src, dry_run=False):
    """Remove directories.

    Recursively and permanently removes the specified directory,
    and all of its empty subdirectories.

    Args:
        src (str or Path): Top search directory
        dry_run (boolean): Flag for execution of cleaning or not

    Returns:
        found_files (boolean): True if any files found
    """
    cwd = os.getcwd()
    src_dir = Path(src)
    found_files = False
    if not src_dir.exists():
        return found_files

    for path in src_dir.iterdir():
        realpath = os.path.realpath(path)
        if path.is_file() or realpath == cwd:
            found_files = True
            continue
        found_files = remove_empty_dirs(path) or found_files
    if found_files:
        logger.debug(f"Keep:{src_dir}")
    else:
        logger.info(f"Remove:{src_dir}")
        if not dry_run:
            src_dir.rmdir()

    return found_files


def ping(host):
    """Ping host.

    Args:
        host(str): Host to ping

    Returns:
        (boolean): True if host responded

    """
    cmd = ["ping", "-c", "1", host]
    try:
        subprocess.check_output(cmd, stderr=subprocess.STDOUT)  # noqa S603
        return True
    except subprocess.CalledProcessError:
        return False


def strip_off_mount_path(path: Union[str, Path]) -> Path:
    """Strip off the mount path from a given path.

    Assumptions:
        - the path contains the user name as a directory.
        - the parent of the user directory is of the format "<new-dir-name>" or
          "*_<new-dir-name>_*", where "*" contains no underscore(s) and
          where the <new-dir-name> will be used as the new parent directory name
          relative to the user directory.

    Args:
        path (Union[str, Path]): Path to strip off the mount path from.

    Returns:
        path: Path with the mount path stripped off.

    Raises:
        ValueError: If the parent of the user directory does not contain 0
                    or 2 underscores.

    Example:
        >>> strip_off_mount_path("/etc/ecmwf/nfs/dh1_home_b/$USER/Deode-Workflow/deode")
        Path("/home/$USER/Deode-Workflow/deode")
    """
    file_parts = Path(path).parts
    user = os.environ.get("USER")
    if user is None:
        return Path(path)

    index_of_user = file_parts.index(user)
    parent_of_user = file_parts[max(0, index_of_user - 1)]
    # Get number of underscores in parent_of_user
    n_underscores = parent_of_user.count("_")
    if n_underscores not in [0, 2]:
        raise ValueError(
            "Parent of user directory must contain 0 or 2 underscores, "
            + f"but found {n_underscores}. Path: {path}"
        )
    # Get middle part of parent_of_user if it contains 2 underscores
    if n_underscores == 2:
        parent_of_user_parts = parent_of_user.split("_")
        parent_of_user = parent_of_user_parts[1]

    return Path(pathlib.os.sep, parent_of_user, *file_parts[index_of_user:])


def resolve_path_relative_to_package(path: Path, ignore_errors: bool = False) -> Path:
    """Resolve path relative to package directory.

    If the path exists as is, return it. If not, check if it exists in the
    package directory and return path relative to package

    Args:
        path (Path): Path to resolve.
        ignore_errors (bool, optional): Option to ignore errors.
            Defaults to False.

    Raises:
        FileNotFoundError: If it was impossible to determine path relative to package.
        FileNotFoundError: If file does not exist locally or in the package directory.

    Returns:
        Path: Original path (if exists locally), or resolved path relative to
            package directory.

    """
    path = path.resolve()
    # First check if path exists as is
    if not os.path.exists(path):
        # Get path relative to package. Needed when Deode-Workflow is installed as
        # a site-package e.g. when creating plugins.
        if GeneralConstants.PACKAGE_NAME in path.parts:
            # Find last occurence of package name in path
            package_index_in_path = (
                len(path.parts)
                - path.parts[::-1].index(GeneralConstants.PACKAGE_NAME)
                - 1
            )
            # Stick together the path in the package directory
            config_parts = path.parts[package_index_in_path:]
            path = GeneralConstants.PACKAGE_DIRECTORY.parent / Path(*config_parts)
        elif not ignore_errors:
            raise FileNotFoundError(
                f"Could not determine path {path} relative to the "
                + f"{GeneralConstants.PACKAGE_NAME} package."
            )

        # If not, check if it exists in the package directory (used when
        # deode is installed as package)
        if not os.path.exists(path) and not ignore_errors:
            raise FileNotFoundError(
                f"Config file {path} not found locally or in package directory"
            )
        return path

    return path


def list_files_join(folder, f_pattern):
    """Read and return file names based on given pattern.

    Args:
        folder: path with file location
        f_pattern: glob pattern

    Returns:
        list of files that should be joined
    """
    pattern_list = os.path.join(folder, f_pattern)
    filenames = glob.glob(pattern_list)

    return filenames


def join_files(input_files: List[str], output_filepath: str):
    """Joins multiple files into a single file.

    Args:
        input_files (List[str]): List of files to be joined/concatenated
        output_filepath (str):   Output file
    """
    output_filename = os.path.basename(output_filepath)
    with open(output_filename, "wb") as output_file:
        for filename in input_files:
            with open(filename, "rb") as input_file:
                output_file.write(input_file.read())
    shutil.move(output_filename, output_filepath)
    logger.info(f"Created {output_filepath} out of files '{input_files}'")
