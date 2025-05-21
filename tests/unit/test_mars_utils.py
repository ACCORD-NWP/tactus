"""Unit tests for the mars_utils module."""

from pathlib import Path
from typing import Generator

import pytest

from deode.mars_utils import (
    add_additional_file_specific_data,
    get_steps_and_members_to_retrieve,
)


@pytest.fixture(name="tmp_file")
def fixture_tmp_file(tmp_path: Path) -> Generator[Path, None, None]:
    """Fixture that creates a new temporary file and yields its path."""
    temp_file = tmp_path / "test_file"
    temp_file.write_bytes(b"")
    yield temp_file
    temp_file.unlink()


class TestAddAdditionalFileSpecificData:
    """Unit tests for the add_additional_file_specific_data function."""

    def test_add_no_common_data(self, tmp_file: Path):
        """Test adding data without common data."""
        additional_data = {}
        test_data = b"001"
        additional_data[str(tmp_file)] = test_data

        # Run the method that should read the file and add the data to the dictionary
        add_additional_file_specific_data(additional_data)

        # Assert that the correct data has been added to the tmp file
        with open(tmp_file, "rb") as f:
            assert f.read() == test_data

    def test_add_common_data(self, tmp_file: Path):
        """Test adding data with common data."""
        additional_data = {}
        test_data = b"001"
        additional_data["common_data"] = b"common"
        additional_data[str(tmp_file)] = test_data

        # Run the method that should read the file and add the data to the dictionary
        add_additional_file_specific_data(additional_data)

        # Assert that the correct data has been added to the tmp file
        with open(tmp_file, "rb") as f:
            assert f.read() == b"common" + test_data


class TestGetStepsAndMembersToRetrieve:
    """Unit tests for the get_steps_and_members_to_retrieve function."""

    def test_control_member_only(self, tmp_path: Path):
        """Test with control member only."""
        steps = [1, 2, 3]
        members = [0]
        file_name = "test_file"

        # Create existing files for step 1 and 2
        (tmp_path / f"{file_name}_0+1").touch()
        (tmp_path / f"{file_name}_0+2").touch()

        # Run the function
        missing_steps, members_dict = get_steps_and_members_to_retrieve(
            steps, tmp_path, file_name, members
        )

        # Assert results
        assert missing_steps == [3]
        assert members_dict == {"control_member": [0]}

    def test_perturbed_members_only(self, tmp_path: Path):
        """Test with perturbed members only."""
        steps = [1, 2, 3]
        members = [1, 2]
        file_name = "test_file"

        # Create existing files for step 1 and member 1
        (tmp_path / f"{file_name}_1+1").touch()
        (tmp_path / f"{file_name}_2+1").touch()

        # Run the function
        missing_steps, members_dict = get_steps_and_members_to_retrieve(
            steps, tmp_path, file_name, members
        )

        # Assert results
        assert missing_steps == [2, 3]
        assert members_dict == {"perturbed_members": [1, 2]}

    def test_control_and_perturbed_members(self, tmp_path: Path):
        """Test with both control and perturbed members."""
        steps = [1, 2, 3]
        members = [0, 1]
        file_name = "test_file"

        # Create existing files for step 1 and member 0, and step 2 and member 1
        (tmp_path / f"{file_name}_0+1").touch()
        (tmp_path / f"{file_name}_1+2").touch()

        # Run the function
        missing_steps, members_dict = get_steps_and_members_to_retrieve(
            steps, tmp_path, file_name, members
        )

        # Assert results
        assert missing_steps == [1, 2, 3]
        assert members_dict == {
            "control_member": [0],
            "perturbed_members": [1],
        }
