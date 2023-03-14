"""Unit tests for os_utils."""
from deode.os_utils import Search


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
