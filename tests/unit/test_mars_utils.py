"""Unit tests for the mars_utils module."""

from pathlib import Path
from typing import Generator

import pytest

from deode.mars_utils import (
    add_additional_file_specific_data,
    compile_target,
    get_steps_and_members_to_retrieve,
    mars_write_method,
)


@pytest.fixture(name="tmp_file")
def fixture_tmp_file(tmp_path: Path) -> Generator[Path, None, None]:
    """Fixture that creates a new temporary file and yields its path."""
    temp_file = tmp_path / "test_file"
    temp_file.write_bytes(b"")
    yield temp_file
    temp_file.unlink()


def split_target(target: str):
    """Splits a compiled target into the three separate fields.

    Args:
        target (str) : the target to split

    Returns:
            The tag, member (or None) and step (or None)

    """
    target = target.strip('"')

    if "+" in target:
        tag_member, step = target.rsplit("+", 1)

        # Revert split with step if step isn't numeric or [STEP]
        if not (step.isnumeric() or step.upper() == "[STEP]"):
            tag_member = f"{tag_member}+{step}"
            step = None
    else:
        tag_member = target
        step = None

    if "_" in tag_member:
        tag, member = tag_member.rsplit("_", 1)

        # Revert split with member if member isn't numeric, [NUMBER] or None
        if not (member.isnumeric() or member.upper() == "[NUMBER]"):
            tag = f"{tag}_{member}"
            member = None
    else:
        tag = tag_member
        member = None

    return (tag, member, step)


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
        (
            missing_steps,
            members_dict,
            missing_member_steps,
        ) = get_steps_and_members_to_retrieve(steps, tmp_path, file_name, members)

        # Assert results
        assert missing_steps == [3]
        assert members_dict == {"control_member": [0]}

        assert len(missing_member_steps) == 1
        assert len(missing_member_steps[0]) == 1
        assert missing_member_steps[0] == [3]

    def test_perturbed_members_only(self, tmp_path: Path):
        """Test with perturbed members only."""
        steps = [1, 2, 3]
        members = [1, 2]
        file_name = "test_file"

        # Create existing files for step 1 and member 1
        (tmp_path / f"{file_name}_1+1").touch()
        (tmp_path / f"{file_name}_2+1").touch()

        # Run the function
        (
            missing_steps,
            members_dict,
            missing_member_steps,
        ) = get_steps_and_members_to_retrieve(steps, tmp_path, file_name, members)

        # Assert results
        assert missing_steps == [2, 3]
        assert members_dict == {"perturbed_members": [1, 2]}

        assert len(missing_member_steps) == 2
        assert missing_member_steps[1] == [2, 3]
        assert missing_member_steps[2] == [2, 3]

    def test_perturbed_members_only_unequal(self, tmp_path: Path):
        """Test with perturbed members only with unequal missing steps."""
        steps = [1, 2, 3]
        members = [1, 2]
        file_name = "test_file"

        # Create existing files for step 1 and member 1
        (tmp_path / f"{file_name}_1+1").touch()
        (tmp_path / f"{file_name}_1+2").touch()
        (tmp_path / f"{file_name}_2+1").touch()

        # Run the function
        (
            missing_steps,
            members_dict,
            missing_member_steps,
        ) = get_steps_and_members_to_retrieve(steps, tmp_path, file_name, members)

        # Assert results
        assert missing_steps == [2, 3]
        assert members_dict == {"perturbed_members": [1, 2]}

        assert len(missing_member_steps) == 2
        assert missing_member_steps[1] == [3]
        assert missing_member_steps[2] == [2, 3]

    def test_control_and_perturbed_members(self, tmp_path: Path):
        """Test with both control and perturbed members."""
        steps = [1, 2, 3]
        members = [0, 1]
        file_name = "test_file"

        # Create existing files for step 1 and member 0, and step 2 and member 1
        (tmp_path / f"{file_name}_0+1").touch()
        (tmp_path / f"{file_name}_1+2").touch()

        # Run the function
        (
            missing_steps,
            members_dict,
            missing_member_steps,
        ) = get_steps_and_members_to_retrieve(steps, tmp_path, file_name, members)

        # Assert results
        assert missing_steps == [1, 2, 3]
        assert members_dict == {
            "control_member": [0],
            "perturbed_members": [1],
        }

        assert len(missing_member_steps) == 2
        assert missing_member_steps[0] == [2, 3]
        assert missing_member_steps[1] == [1, 3]


class TestMarsWriteMethod:
    """Unit tests for the mars_write_method function."""

    def test_version6(self):
        """Test for mars version 6."""
        assert mars_write_method(6) == "retrieve"

    def test_version7(self):
        """Test for mars version 7."""
        assert mars_write_method(7) == "read"


class TestCompileSplitTarget:
    """Unit tests for the compile_target and split_target function."""

    def test_unity(self):
        """Test for consistency of compile_target and split_target."""
        # Setup test arguments
        missing = "%MISSING%"
        tags = ("testTag", "tag_with_underscore", "tag+with+plus")
        member_types = ("control_member", "perturbed_members")
        members = ([0], [None], None, [2, 3], 2)
        steps = (missing, 1)

        for tag in tags:
            for member_type in member_types:
                for member in members:
                    for step in steps:
                        if step == missing:
                            target = compile_target(tag, member_type, member)
                        else:
                            target = compile_target(tag, member_type, member, step)

                        tag2, member2, step2 = split_target(target)

                        if tag2 != tag:
                            pytest.fail(f"ERROR: given tag={tag} results in tag={tag2}")

                        if member2 != member and not (
                            member is None
                            and member2 == "0"
                            or member == [None]
                            and member2 == "0"
                            or isinstance(member, int)
                            and str(member) == member2
                            or member_type == "control_member"
                            and member2 == "0"
                            or len(member) > 1
                            and member2 == "[NUMBER]"
                            or len(member) == 1
                            and member2 == str(member[0])
                        ):
                            pytest.fail(
                                f"ERROR: given member={member} results in member={member2}"
                            )

                        if step2 != str(step) and not (
                            step == missing and step2 == "[STEP]"
                        ):
                            pytest.fail(
                                f"ERROR: given step={step} results in step={step2}"
                            )
