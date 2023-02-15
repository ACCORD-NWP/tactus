"""Utilities for simple tasks on OS level."""
import os
import time


class Search:
    """Search class."""

    def __init__(self):
        """Construct search class."""
        return

    @staticmethod
    def find_files(directory, prefix="", postfix="", recursive=True, onlyfiles=True,
                   fullpath=False, olderthan=None, inorder=False) -> list:
        """Find files in a directory.

        Args:
            directory (str): Directory to search in.
            prefix (str, optional): Only remove files with this prefix. Defaults to "".
            postfix (str, optional): Only remove files with the postfix. Defaults to "".
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
                files = [f for f in os.listdir(directory) if
                         f.endswith(postfix) and f.startswith(prefix)
                         and os.path.isfile(directory + f)]

            elif not onlyfiles:
                files = [f for f in os.listdir(directory) if
                         f.endswith(postfix) and f.startswith(prefix)]

        if fullpath:
            files = [directory + f for f in files]

        if olderthan is not None:
            now = time.time()
            tfiles = []
            for f in files:
                try:
                    if not fullpath:
                        if os.path.getmtime(os.path.join(directory, f)) < (now - olderthan):
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
