#!/usr/bin/env python3
"""Smoke."""
import shutil

import pytest

from deode import PACKAGE_NAME


class TestMain:
    # pylint: disable=no-self-use

    def test_package_executable_is_in_path(self):
        assert shutil.which(PACKAGE_NAME)


if __name__ == "__main__":
    pytest.main()
