#!/usr/bin/env python3
"""Unit tests for reference_checker.py."""

import datetime
import json
import os
import shutil
from pathlib import Path

import pytest
import tomlkit

from tactus.derived_variables import set_times
from tactus.reference_checker import (
    CheckDefinition,
    CheckItem,
    CheckSummary,
    CheckSummaryAnalysis,
    CheckSummaryCiboulai,
    CheckSummaryJson,
    CheckSummaryTxt,
    NamelistChecker,
    NormsChecker,
    PublishToCiboulai,
    ReferenceChecker,
    ReferenceCheckManager,
    XToolChecker,
)
from tactus.toolbox import FileManager


@pytest.fixture(scope="module")
def basic_config(tmp_directory, default_config):
    scratch = tmp_directory
    config = default_config
    config = config.copy(update=set_times(config))
    return config.copy(
        update={
            "general": {
                "cycle": "CY49t2",
                "case": "@CASE_PREFIX@@CYCLE@_@CSC@_nwp_@DOMAIN@_@YMD_START@",
            },
            "platform": {"scratch": scratch},
        }
    )


def configure_for_check_and_generate(
    config,
    domain_name,
    check,
    generate,
    tolerance_xtool,
    tolerance_normschecker,
    suppress_exception,
):
    tactus_home = os.path.dirname(__file__)
    binary_path = f"{tactus_home}/data/reference_checker/xtool_mockup.sh"
    patch_file = f"{tactus_home}/data/reference_checker/test_reference_checker.toml"
    patch = tomlkit.loads(Path(patch_file).read_text())
    patch["reference_checker"]["check"] = check
    patch["reference_checker"]["generate"] = generate
    patch["reference_checker"]["suppress_exception"] = suppress_exception
    patch["reference_checker"]["methods"]["utForecastNorms"]["tolerance"] = (
        tolerance_normschecker
    )
    patch["reference_checker"]["methods"]["utFullPosFields"]["tolerance"] = (
        tolerance_xtool
    )
    patch["reference_checker"]["methods"]["utFullPosFields"]["binary"] = binary_path
    patch["reference_checker"]["methods"]["utHistoryFields"]["tolerance"] = (
        tolerance_xtool
    )
    patch["reference_checker"]["methods"]["utHistoryFields"]["binary"] = binary_path
    patch["platform"]["references_folder"] = (
        f"{tactus_home}/data/reference_checker/reference"
    )
    patch["domain"]["name"] = domain_name

    return config.copy(update=patch)


def file_contains(filepath, text, count=1):
    """Helper function to check if a file contains specific text."""
    if not os.path.exists(filepath):
        return False
    with open(filepath, "r") as f:
        content = f.read()
        return content_contains(content, text, count)
    return False


def content_contains(content, text, count=1):
    return content.count(text) == count


class TestReferenceChecker:
    """Tests for the ReferenceChecker base class."""

    def test_reference_checker_method_not_implemented(self):
        """Test that compare returns error message."""
        checker = ReferenceChecker(tool="unknown_tool")
        result = checker.compare("test.txt", "reference.txt")
        assert result == "ERROR - Compare not implemented"

    def test_reference_checker_create_various(self, mocker):
        """Test factory method creating NormsChecker."""
        config = {
            "methods": {
                "test_method": {
                    "tool": "norms_checker",
                    "which": "all",
                    "tolerance": 5,
                    "mode": "get_worst",
                    "export_complete_check": False,
                },
                "xtool_method": {
                    "tool": "xtool",
                    "binary": "/path/to/xtool",
                    "args_template": "-test",
                    "file_format": "GRIB",
                    "tolerance": 10,
                },
                "unknown_method": {
                    "tool": "unknown_tool",
                },
            }
        }
        checker = ReferenceChecker.create_reference_checker("test_method", config)
        assert isinstance(checker, NormsChecker)
        checker = ReferenceChecker.create_reference_checker("xtool_method", config)
        assert isinstance(checker, XToolChecker)
        mock_logger = mocker.patch("tactus.reference_checker.logger")
        checker = ReferenceChecker.create_reference_checker("unknown_method", config)
        assert checker is None
        mock_logger.warning.assert_called_once()


class TestNamelistChecker:
    """Tests for the NamelistChecker class."""

    NAMELIST_A = """\
    &NAMDIM
    NFLEVG = 65,
    NPROMA = 32,
    /
    &NAMCT0
    LNHDYN = .TRUE.,
    NCONF  = 1,
    /
    """

    NAMELIST_A_WHITESPACE = """\
    &NAMDIM
        NFLEVG   = 65,
        NPROMA=32,

    /
    &NAMCT0
        LNHDYN = .TRUE.,
        NCONF  = 1,
    /
    """

    NAMELIST_A_CASE = """\
    &namdim
    nflevg = 65,
    nproma = 32,
    /
    &namct0
    lnhdyn = .true.,
    nconf  = 1,
    /
    """

    NAMELIST_B = """\
    &NAMDIM
    NFLEVG = 90,
    NPROMA = 32,
    /
    &NAMCT0
    LNHDYN = .TRUE.,
    NCONF  = 1,
    /
    """

    @staticmethod
    def _write(tmp_path: Path, name: str, content: str) -> str:
        path = tmp_path / name
        path.write_text(content)
        return str(path)

    def test_identical_files_bit_identical(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_A)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker()
        result = checker.compare(test, ref, out)

        assert "SUCCESS" in result
        assert "bit identical" in result
        assert os.path.exists(out)

    def test_whitespace_differences_ignored(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_A_WHITESPACE)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker(
            ignore_case=True, ignore_blank_lines=True, ignore_whitespace=True
        )
        result = checker.compare(test, ref, out)

        assert "SUCCESS" in result

    def test_case_differences_ignored(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_A_CASE)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker(ignore_case=True)
        result = checker.compare(test, ref, out)

        assert "SUCCESS" in result

    def test_case_differences_detected_when_not_ignored(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_A_CASE)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker(
            ignore_case=False, ignore_blank_lines=False, ignore_whitespace=False
        )
        result = checker.compare(test, ref, out)

        assert "FAILURE" in result

    def test_real_differences_detected(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_B)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker()
        result = checker.compare(test, ref, out)

        assert "FAILURE" in result
        # the diff itself must be captured in out_file
        content = Path(out).read_text()
        assert "NFLEVG" in content

    def test_missing_test_file(self, tmp_path):
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_A)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker()
        result = checker.compare(str(tmp_path / "missing.nam"), ref, out)

        assert "ERROR" in result
        assert "Test file" in result

    def test_missing_reference_file(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker()
        result = checker.compare(test, str(tmp_path / "missing.nam"), out)

        assert "ERROR" in result
        assert "Reference file" in result

    def test_out_file_contains_summary(self, tmp_path):
        test = self._write(tmp_path, "test.nam", TestNamelistChecker.NAMELIST_A)
        ref = self._write(tmp_path, "ref.nam", TestNamelistChecker.NAMELIST_B)
        out = str(tmp_path / "diff.out")

        checker = NamelistChecker()
        checker.compare(test, ref, out)

        content = Path(out).read_text()
        assert "FAILURE" in content


class TestNormsChecker:
    """Tests for the NormsChecker class."""

    @staticmethod
    def _compare_normsets(test_file, reference_file):
        with open(test_file, "r") as f:
            test_content = f.read()
        with open(reference_file, "r") as f:
            reference_content = f.read()

        test_value = float(test_content)
        reference_value = float(reference_content)
        if test_value == reference_value:
            return 0

        return 1

    @staticmethod
    def _create_test_files(tmp_path, create_reference="norms", create_test="norms"):
        test_file = os.path.join(tmp_path, "NODE.001_01")
        reference_file = os.path.join(tmp_path, "NODE.001_01.ref")
        out_file = os.path.join(tmp_path, "output.log")
        source = os.path.join(
            os.path.dirname(__file__), "data/reference_checker/reference", "NODE.001_01"
        )
        if create_test:
            if create_test == "dummy":
                with open(test_file, "w") as f:
                    f.write("dummy")
            else:
                shutil.copyfile(source, test_file)

        if create_reference:
            if create_reference == "dummy":
                with open(reference_file, "w") as f:
                    f.write("dummy")
            else:
                shutil.copyfile(source, reference_file)

        return test_file, reference_file, out_file

    def test_norms_checker_success(self, tmp_path):
        """Test successful comparison."""
        test_file, reference_file, out_file = TestNormsChecker._create_test_files(
            tmp_path
        )
        # worst digit below tolerance
        checker = NormsChecker(which="all", tolerance=5, mode="get_worst")
        result = checker.compare(test_file, reference_file, out_file)

        test_string = "SUCCESS - Worst digit is 0 <= tol = 5 (mode=get_worst, which=all)"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_norms_checker_difference_failure(self, tmp_path):
        """Test comparison when differences are found."""
        test_file, reference_file, out_file = TestNormsChecker._create_test_files(
            tmp_path
        )
        # worst digit below tolerance
        checker = NormsChecker(which="all", tolerance=-1, mode="get_worst")
        result = checker.compare(test_file, reference_file, out_file)

        test_string = "FAILURE - Worst digit is 0 > tol = -1"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_norms_checker_reference_file_no_norms_found_failure(self, tmp_path):
        """Test comparison when no norms are found in reference."""
        test_file, reference_file, out_file = TestNormsChecker._create_test_files(
            tmp_path, create_reference="dummy"
        )

        checker = NormsChecker(which="all", tolerance=5, mode="get_worst")

        result = checker.compare(test_file, reference_file, out_file)

        test_string = f"ERROR - No norms found in {reference_file}"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_norms_checker_test_file_no_norms_found_failure(self, tmp_path):
        """Test comparison when no norms are found in test."""
        test_file, reference_file, out_file = TestNormsChecker._create_test_files(
            tmp_path, create_test="dummy"
        )

        checker = NormsChecker(which="all", tolerance=5, mode="get_worst")

        result = checker.compare(test_file, reference_file, out_file)

        test_string = f"ERROR - No norms found in {test_file}"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_norms_checker_no_reference_file_found_failure(self, tmp_path):
        """Test comparison when reference_file is missing."""
        test_file, reference_file, out_file = TestNormsChecker._create_test_files(
            tmp_path, create_reference=None
        )
        checker = NormsChecker(which="all", tolerance=5, mode="get_worst")

        result = checker.compare(test_file, reference_file, out_file)

        test_string = f"ERROR - Reference log {reference_file} not found"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_norms_checker_no_test_file_found_failure(self, tmp_path):
        """Test comparison when test_file is missing."""
        test_file, reference_file, out_file = TestNormsChecker._create_test_files(
            tmp_path, create_test=None
        )
        checker = NormsChecker(which="all", tolerance=5, mode="get_worst")
        result = checker.compare(test_file, reference_file, out_file)

        test_string = f"ERROR - Test log {test_file} not found"
        assert result == test_string
        assert file_contains(out_file, test_string)


class TestXToolChecker:
    """Tests for the XToolChecker class."""

    @staticmethod
    def _create_test_files(tmp_path, create_reference=True, create_test=True):
        test_file = os.path.join(tmp_path, "test_file")
        reference_file = os.path.join(tmp_path, "reference_file")
        out_file = os.path.join(tmp_path, "output.log")

        if create_test:
            with open(test_file, "w") as f:
                f.write("test")
        if create_reference:
            with open(reference_file, "w") as f:
                f.write("reference")

        return test_file, reference_file, out_file

    @staticmethod
    def _get_xtool_binary_mockup():
        # Return path to a mock xtool binary for testing
        return os.path.join(
            os.path.dirname(__file__), "data/reference_checker", "xtool_mockup.sh"
        )

    @staticmethod
    def _get_xtool_binary_mockup_with_internal_failure(tmp_path):
        # Return path to a mock xtool binary that simulates failure for testing
        path = os.path.join(tmp_path, "xtool_mockup_failure.sh")
        with open(path, "w") as f:
            f.write(
                'echo "This file has no shebang line and is not executable to simulate failure"'
            )
        return path

    def test_xtool_checker_success(self, tmp_path):
        """Test argument substitution in compare."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path
        )
        binary = TestXToolChecker._get_xtool_binary_mockup()
        args_template = "-f1 {test_file} -f2 {reference_file} -f {file_format} -s -of SCREEN -de -to {tolerance}"

        for file_format in ["GRIB", "FA"]:
            checker = XToolChecker(binary, file_format, 10, args_template)
            result = checker.compare(test_file, reference_file, out_file)
            test_string = "SUCCESS"
            assert test_string in result
            assert file_contains(out_file, test_string)

    def test_xtool_checker_difference_found_failure(self, tmp_path):
        """Test argument substitution in compare."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path
        )
        binary = TestXToolChecker._get_xtool_binary_mockup()
        args_template = "invalid"
        # tolerance is set to 11 to trigger the difference found case in the mockup binary
        checker = XToolChecker(binary, "GRIB", 11, args_template)
        result = checker.compare(test_file, reference_file, out_file)
        test_string = "ERROR - executing XtoolChecker"
        assert test_string in result
        assert file_contains(out_file, test_string)

    def test_xtool_checker_wrong_argsfailure(self, tmp_path):
        """Test argument substitution in compare."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path
        )
        binary = TestXToolChecker._get_xtool_binary_mockup()
        args_template = "invalid"
        checker = XToolChecker(binary, "GRIB", 10, args_template)
        result = checker.compare(test_file, reference_file, out_file)
        test_string = "ERROR - executing XtoolChecker"
        assert test_string in result
        assert file_contains(out_file, test_string)

    def test_xtool_checker_no_reference_file_failure(self, tmp_path):
        """Test comparison failure when no reference file is available."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path, create_reference=False
        )
        binary = TestXToolChecker._get_xtool_binary_mockup()
        args_template = "-f1 {test_file} -f2 {reference_file} -f {file_format} -s -of SCREEN -de -to {tolerance}"
        checker = XToolChecker(binary, "GRIB", 10, args_template)

        result = checker.compare(test_file, reference_file, out_file)
        test_string = f"ERROR - Reference file {reference_file} not found"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_xtool_checker_no_test_file_failure(self, tmp_path):
        """Test comparison failure when no test file is available."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path, create_test=False
        )
        binary = TestXToolChecker._get_xtool_binary_mockup()
        args_template = "-f1 {test_file} -f2 {reference_file} -f {file_format} -s -of SCREEN -de -to {tolerance}"
        checker = XToolChecker(binary, "GRIB", 10, args_template)

        result = checker.compare(test_file, reference_file, out_file)
        test_string = f"ERROR - Test file {test_file} not found"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_xtool_checker_no_xtool_failure(self, tmp_path):
        """Test comparison failure when no xtool is available."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path
        )
        binary = "noxtool"
        args_template = "-f1 {test_file} -f2 {reference_file} -f {file_format} -s -of SCREEN -de -to {tolerance}"
        checker = XToolChecker(binary, "GRIB", 10, args_template)

        result = checker.compare(test_file, reference_file, out_file)
        test_string = f"ERROR - {binary} not found"
        assert result == test_string
        assert file_contains(out_file, test_string)

    def test_xtool_checker_xtool_internal_failure(self, tmp_path):
        """Test comparison failure when no xtool is available."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path
        )
        binary = TestXToolChecker._get_xtool_binary_mockup_with_internal_failure(tmp_path)
        args_template = "-f1 {test_file} -f2 {reference_file} -f {file_format} -s -of SCREEN -de -to {tolerance}"
        checker = XToolChecker(binary, "GRIB", 10, args_template)

        with pytest.raises(PermissionError):
            checker.compare(test_file, reference_file, out_file)

        assert os.path.exists(out_file)
        test_string = "ERROR - executing XtoolChecker"
        assert file_contains(out_file, test_string)
        assert file_contains(out_file, "[Errno 13] Permission denied")

    def test_xtool_checker_unsupported_format_failure(self, tmp_path):
        """Test comparison failure when an unsupported file format is provided."""
        test_file, reference_file, out_file = TestXToolChecker._create_test_files(
            tmp_path
        )
        binary = TestXToolChecker._get_xtool_binary_mockup()
        args_template = "-f1 {test_file} -f2 {reference_file} -f {file_format} -s -of SCREEN -de -to {tolerance}"
        file_format = "UNSUPPORTED_FORMAT"
        checker = XToolChecker(binary, file_format, 10, args_template)

        result = checker.compare(test_file, reference_file, out_file)
        test_string = f"ERROR - Unsupported file format {file_format}"
        assert result == test_string
        assert file_contains(out_file, test_string)


class TestCheckSummary:
    """Tests for the CheckSummary class."""

    def test_check_summary_create_summary_list(self, basic_config):
        """Test create_summary_list static method."""
        summaries = CheckSummary.create_summary_list(basic_config["reference_checker"])
        assert len(summaries) == 2
        assert isinstance(summaries[1], CheckSummaryJson)
        assert summaries[1].fileformat == "json"
        assert summaries[1].filename == "@CASEDIR@/checks/summary.json"

        assert isinstance(summaries[0], CheckSummaryTxt)
        assert summaries[0].fileformat == "txt"
        assert summaries[0].filename == "@CASEDIR@/checks/summary.txt"


def make_ciboulai_definition(taskname, rulename, results, empty=False):
    """Create a check definition with given task name and item results.

    Args:
        taskname: the name of the task
        rulename: the name of the rule
        results: the list of result strings for the items
        empty: if True, the definition gets no items (missing files)

    Returns:
        a CheckDefinition ready to be used by CheckSummaryCiboulai
    """
    definition = CheckDefinition(
        taskname,
        rulename,
        ".ut",
        "utMethod",
        "@WRK@/inpath",
        "PATTERN*",
        "@REFERENCES_FOLDER@/ref",
        "@WRK@/checks",
        "@WRK@/gen",
    )
    definition.uniquename = f"{taskname}.0001"
    definition.files_pattern = "PATTERN*"
    definition.inpath = "/some/inpath"
    definition.items = []
    if not empty:
        for index, result in enumerate(results):
            definition.items.append(
                CheckItem(
                    f"/test/file{index}",
                    f"/ref/file{index}",
                    "N/A",
                    f"/res/file{index}.diff",
                    result,
                )
            )
    return definition


class TestCheckSummaryCiboulai:
    """Tests for the CheckSummaryCiboulai class."""

    @staticmethod
    def _load(summary, filename):
        with open(os.path.join(summary.fullpath, filename), "r") as file:
            return json.load(file)

    @pytest.fixture
    def ciboulai_summary(self, basic_config, tmp_path):
        """A CheckSummaryCiboulai initialized on a temporary stack directory."""
        fmanager = FileManager(basic_config)
        summary = CheckSummaryCiboulai("ciboulai", str(tmp_path / "summaries_stack"))
        summary.init_full_path(fmanager.platform)
        summary.create()
        return summary

    def test_create_summary_list_accepts_ciboulai_format(self, basic_config):
        """Test that the ciboulai format is instantiated from the configuration."""
        config = configure_for_check_and_generate(
            basic_config, "ciboulai_cfg", True, False, 12, 5, False
        )
        config = config.copy(
            update={
                "reference_checker": {
                    "summary_active": ["uttxt", "utjson", "utciboulai"],
                }
            }
        )
        summaries = CheckSummary.create_summary_list(config["reference_checker"])
        assert len(summaries) == 3
        assert isinstance(summaries[2], CheckSummaryCiboulai)
        assert summaries[2].fileformat == "ciboulai"
        assert summaries[2].filename == "@WRK@/checks/summaries_stack"
        # the ciboulai section of the configuration is passed to the ciboulai summary
        assert summaries[2].ciboulai_config is not None
        assert summaries[2].ciboulai_config["enabled"] is False

    def test_init_full_path(self, basic_config):
        """Test the path resolution of the stack directory."""
        fmanager = FileManager(basic_config)
        wrk = fmanager.platform.get_system_value("wrk")
        summary = CheckSummaryCiboulai("ciboulai", "@WRK@/checks/summaries_stack")
        summary.init_full_path(fmanager.platform)
        assert summary.fullpath == os.path.join(wrk, "checks/summaries_stack")
        assert summary.rundir == wrk

    def test_append_success_writes_both_files(self, ciboulai_summary, tmp_path):
        """Test that successful checks produce E/Ended and OK verdicts."""
        result_file = tmp_path / "a.diff"
        result_file.write_text("detailed comparison result\n")
        definition = make_ciboulai_definition(
            "Forecast", "utlog", ["SUCCESS - Files are bit identical"]
        )
        definition.items[0].result_file = str(result_file)

        ciboulai_summary.append([definition])

        itself = self._load(ciboulai_summary, "Forecast.0001.itself.json")
        assert itself["Status"] == CheckSummaryCiboulai.STATUS_ENDED
        assert itself["Context"]["jobname"] == "Forecast"
        assert itself["Context"]["rundir"] == ciboulai_summary.rundir
        parsed_date = datetime.datetime.fromisoformat(itself["Updated"])
        assert parsed_date.strftime("%Y-%m-%dT%H:%M:%S") == itself["Updated"]
        assert itself["Listing"] == {"Task listing uri(s)": [str(result_file)]}

        consistency = self._load(ciboulai_summary, "Forecast.0001.consistency.json")
        assert set(consistency) == {"comparisonStatus"}
        assert consistency["comparisonStatus"]["symbol"] == "OK"

    def test_append_failure_is_inconsistent(self, ciboulai_summary):
        """Test that a failed comparison produces the IC verdict."""
        definition = make_ciboulai_definition(
            "Forecast",
            "utlog",
            ["SUCCESS - Files are bit identical", "FAILURE - Differences found"],
        )
        ciboulai_summary.append([definition])
        consistency = self._load(ciboulai_summary, "Forecast.0001.consistency.json")
        assert consistency["comparisonStatus"]["symbol"] == "IC"
        assert consistency["comparisonStatus"]["short"] == "Inconsistent"

    def test_append_missing_files_is_no_ref(self, ciboulai_summary):
        """Test that definitions without items produce the 0 verdict."""
        definition = make_ciboulai_definition("Forecast", "utlog", [], empty=True)
        ciboulai_summary.append([definition])
        consistency = self._load(ciboulai_summary, "Forecast.0001.consistency.json")
        status = consistency["comparisonStatus"]
        assert status["symbol"] == "0"
        assert status["text"] == "No file found using PATTERN* at /some/inpath"

    def test_append_generate_only_is_no_ref(self, ciboulai_summary):
        """Test that unperformed comparisons (result N/A) produce the 0 verdict."""
        definition = make_ciboulai_definition("Forecast", "utlog", ["N/A"])
        ciboulai_summary.append([definition])
        consistency = self._load(ciboulai_summary, "Forecast.0001.consistency.json")
        status = consistency["comparisonStatus"]
        assert status["symbol"] == "0"
        assert status["text"] == "No reference to be compared to"

    def test_append_groups_rules_of_same_task(self, ciboulai_summary):
        """Test that all rules of a task end up in a single pair of files."""
        definitions = [
            make_ciboulai_definition("Forecast", "utlog", ["SUCCESS - Files identical"]),
            make_ciboulai_definition("Forecast", "uthistory", [], empty=True),
        ]
        ciboulai_summary.append(definitions)

        files = sorted(os.listdir(ciboulai_summary.fullpath))
        assert files == ["Forecast.0001.consistency.json", "Forecast.0001.itself.json"]

        status = self._load(ciboulai_summary, "Forecast.0001.consistency.json")[
            "comparisonStatus"
        ]
        assert status["symbol"] == "OK"
        assert "No file found using PATTERN* at /some/inpath" in status["text"]

    def test_compute_analysis(self, ciboulai_summary):
        """Test the analysis computed from the consistency files."""
        ciboulai_summary.append([
            make_ciboulai_definition("Forecast", "utlog", ["SUCCESS - identical"]),
            make_ciboulai_definition("Interpol", "uthistory", ["FAILURE - differ"]),
        ])

        analysis = ciboulai_summary.compute_and_append_analysis(True)
        assert analysis.success_count == 1
        assert analysis.error_count == 1
        assert analysis.missing_count == 0
        assert not analysis.success()

        analysis = ciboulai_summary.compute_and_append_analysis(False)
        assert analysis.success_count == 0
        assert analysis.error_count == 0
        assert analysis.success()

    def test_contains_summary_analysis_is_false(self, ciboulai_summary):
        """Test that ciboulai summaries never report an embedded analysis."""
        ciboulai_summary.append([make_ciboulai_definition("Forecast", "utlog", ["N/A"])])
        assert ciboulai_summary.contains_summary_analysis() is False

    def test_delete_removes_the_stack(self, ciboulai_summary):
        """Test that delete removes the whole stack directory."""
        ciboulai_summary.append([make_ciboulai_definition("Forecast", "utlog", ["N/A"])])
        ciboulai_summary.delete()
        assert not os.path.exists(ciboulai_summary.fullpath)

    def test_reference_check_manager_writes_ciboulai_stack(self, basic_config):
        """Run a complete check with ciboulai activated and validate the stack."""
        config = configure_for_check_and_generate(
            basic_config, "check_identical", True, False, 12, 5, False
        )
        config = config.copy(
            update={
                "reference_checker": {
                    "summary_active": ["uttxt", "utjson", "utciboulai"],
                }
            }
        )
        TestReferenceCheckManager._simulate_forecast_execution(config, True)
        fmanager = FileManager(config)
        wrk = fmanager.platform.get_system_value("wrk")
        stack = os.path.join(wrk, "checks/summaries_stack")

        for task in [
            "Prep",
            "Forecast",
            "Interpol",
            "Marsprep",
            "Creategrib",
            "PostMortem",
        ]:
            manager = ReferenceCheckManager.create_reference_check_manager(config, task)
            assert manager
            manager.execute(fmanager)

        expected_tasks = ["Forecast", "Interpol", "Marsprep", "Creategrib"]
        files = sorted(f for f in os.listdir(stack) if f.endswith(".json"))
        assert len(files) == 2 * len(expected_tasks)

        for task in expected_tasks:
            stems = [f for f in files if f.startswith(f"{task}.")]
            assert len(stems) == 2
            for stem in stems:
                with open(os.path.join(stack, stem), "r") as file:
                    content = json.load(file)
                if stem.endswith(".itself.json"):
                    assert content["Status"]["symbol"] == "E"
                    assert content["Context"]["jobname"] == task
                    assert content["Context"]["rundir"] == wrk
                else:
                    assert content["comparisonStatus"]["symbol"] == "OK"


class FakeCiboulaiServer:
    """Stands in for the Ciboulai api, recording the posts of the client."""

    def __init__(self, answer=None):
        """Create the fake server, answering returnCode 0 unless specified."""
        self.posts = []
        self.answer = (
            answer
            if answer is not None
            else json.dumps({"returnCode": 0, "message": "Data received"})
        )

    def post(self, url, form):
        """Record one form post and return the canned answer."""
        self.posts.append((url, dict(form)))
        return 200, self.answer

    def types(self):
        """Return the list of posted types, in order."""
        return [form["type"] for _, form in self.posts]

    def json_data(self, index=0):
        """Return the parsed jsonData of one post."""
        return json.loads(self.posts[index][1]["jsonData"])


@pytest.fixture
def ciboulai_server(monkeypatch):
    """Patch the HTTP layer of PublishToCiboulai with a recording fake."""
    server = FakeCiboulaiServer()
    monkeypatch.setattr(
        PublishToCiboulai,
        "_http_post_form",
        lambda _self, url, form: server.post(url, form),
    )
    monkeypatch.delenv("CIBOULAI_TOKEN", raising=False)
    monkeypatch.setenv("USER", "cvap")
    return server


@pytest.fixture
def ciboulai_platform(basic_config):
    """A platform to substitute the paths of the ciboulai configuration."""
    return FileManager(basic_config).platform


def make_ciboulai_config(tmp_path, **overrides):
    """Build a [reference_checker.ciboulai] configuration in a temp directory.

    A token file is created unless the tests overrides token_file.
    """
    (tmp_path / "token.txt").write_text("s3cr3t\n")
    config = {
        "server": "https://ciboulai.test/davai/",
        "token_file": str(tmp_path / "token.txt"),
        "name_file": str(tmp_path / "checks" / "ciboulai.cfg"),
        "counter_file": str(tmp_path / "shared" / "ciboulai_counter"),
        "experiment_name": "ttr-{counter}-{machine}@{user}",
        "tries": 1,
        "sleep": 0,
        "timeout": 5,
    }
    config.update(overrides)
    return config


def make_publisher(tmp_path, platform, **overrides):
    """Build a PublishToCiboulai on temporary files."""
    stack_dir = str(tmp_path / "checks" / "summaries_stack")
    os.makedirs(stack_dir, exist_ok=True)
    return PublishToCiboulai(
        make_ciboulai_config(tmp_path, **overrides), platform, stack_dir
    )


class TestPublishToCiboulai:
    """Tests for the PublishToCiboulai class, with a fake HTTP layer."""

    def test_api_url(self, ciboulai_platform, tmp_path):
        """Test the construction of the api url from the server url."""
        publisher = make_publisher(tmp_path, ciboulai_platform)
        assert publisher.api_url == "https://ciboulai.test/davai/api/"

    def test_new_experiment_posts_xpinfo(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that a first run increments the counter and initialises the experiment."""
        publisher = make_publisher(tmp_path, ciboulai_platform)
        assert publisher.ensure_experiment(reset=True) is True

        name = f"ttr-0001-{publisher.machine}@cvap"
        assert publisher.xpid == name
        with open(publisher.name_file, "r") as file:
            assert file.read().strip() == name
        with open(publisher.counter_file, "r") as file:
            assert file.read().strip() == "1"

        assert ciboulai_server.types() == ["xpinfo"]
        _, form = ciboulai_server.posts[0]
        assert form["xpid"] == name
        assert form["token"] == "s3cr3t"  # noqa: S105
        xpinfo = ciboulai_server.json_data(0)["xpinfo"]
        assert xpinfo["xpid"] == name
        assert xpinfo["user"] == "cvap"

    def test_counter_is_read_before_being_incremented(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that the counter file value provides the next experiment number."""
        os.makedirs(
            os.path.dirname(make_ciboulai_config(tmp_path)["counter_file"]), exist_ok=True
        )
        publisher = make_publisher(tmp_path, ciboulai_platform)
        with open(publisher.counter_file, "w") as file:
            file.write("5\n")

        assert publisher.ensure_experiment(reset=True) is True
        assert publisher.xpid == f"ttr-0006-{publisher.machine}@cvap"
        with open(publisher.counter_file, "r") as file:
            assert file.read().strip() == "6"
        assert ciboulai_server.types() == ["xpinfo"]

    def test_existing_name_is_reused_without_increment(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that a stored experiment name survives a rerun unchanged."""
        os.makedirs(os.path.join(tmp_path, "checks"), exist_ok=True)
        with open(os.path.join(tmp_path, "checks", "ciboulai.cfg"), "w") as file:
            file.write("ttr-0042-previous@user\n")
        publisher = make_publisher(tmp_path, ciboulai_platform)

        assert publisher.ensure_experiment(reset=True) is True
        assert publisher.xpid == "ttr-0042-previous@user"
        assert not os.path.exists(publisher.counter_file)
        assert ciboulai_server.posts[0][1]["xpid"] == "ttr-0042-previous@user"

    def test_reset_false_does_not_post_xpinfo(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that tasks other than the creator do not reset the experiment."""
        os.makedirs(os.path.join(tmp_path, "checks"), exist_ok=True)
        with open(os.path.join(tmp_path, "checks", "ciboulai.cfg"), "w") as file:
            file.write("ttr-0042-previous@user\n")
        publisher = make_publisher(tmp_path, ciboulai_platform)

        assert publisher.ensure_experiment(reset=False) is True
        assert ciboulai_server.posts == []

    def test_publish_wraps_the_summary(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test the payload shape of a task summary publication."""
        publisher = make_publisher(tmp_path, ciboulai_platform)
        publisher.ensure_experiment(reset=True)
        summary = {"comparisonStatus": {"symbol": "OK"}}

        assert publisher.publish("Forecast.0001", "consistency", summary) is True
        assert ciboulai_server.types() == ["xpinfo", "taskinfo"]
        _, form = ciboulai_server.posts[1]
        assert form["xpid"] == publisher.xpid
        assert ciboulai_server.json_data(1) == {"Forecast.0001": {"consistency": summary}}

    def test_publish_resolves_a_missing_name(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that publishing without a prior ensure_experiment still works."""
        publisher = make_publisher(tmp_path, ciboulai_platform)
        assert publisher.publish("Forecast.0001", "itself", {"Status": {}}) is True
        assert publisher.xpid == f"ttr-0001-{publisher.machine}@cvap"
        assert ciboulai_server.types() == ["xpinfo", "taskinfo"]

    def test_disabled_publisher_is_inert(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that enabled = false prevents any file creation or post."""
        publisher = make_publisher(tmp_path, ciboulai_platform, enabled=False)
        assert publisher.ensure_experiment(reset=True) is False
        assert publisher.publish("Forecast.0001", "itself", {}) is False
        assert ciboulai_server.posts == []
        assert not os.path.exists(publisher.name_file)
        assert not os.path.exists(publisher.counter_file)

    def test_missing_token_skips_publication(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test that no token means no attempt and no counter increment."""
        publisher = make_publisher(
            tmp_path, ciboulai_platform, token_file=str(tmp_path / "absent.txt")
        )
        assert publisher.ensure_experiment(reset=True) is False
        assert publisher.publish("Forecast.0001", "itself", {}) is False
        assert ciboulai_server.posts == []
        assert not os.path.exists(publisher.counter_file)

    def test_token_from_environment(
        self, ciboulai_server, ciboulai_platform, tmp_path, monkeypatch
    ):
        """Test that the CIBOULAI_TOKEN environment variable takes precedence."""
        monkeypatch.setenv("CIBOULAI_TOKEN", "envtoken")
        publisher = make_publisher(
            tmp_path, ciboulai_platform, token_file=str(tmp_path / "absent.txt")
        )
        assert publisher.ensure_experiment(reset=True) is True
        assert ciboulai_server.posts[0][1]["token"] == "envtoken"  # noqa: S105

    def test_server_error_is_not_fatal(self, ciboulai_platform, tmp_path, monkeypatch):
        """Test that a rejected payload is reported but never raises."""
        server = FakeCiboulaiServer(
            json.dumps({"returnCode": 1, "message": "Problem with data"})
        )
        monkeypatch.setattr(
            PublishToCiboulai,
            "_http_post_form",
            lambda _self, url, form: server.post(url, form),
        )
        monkeypatch.delenv("CIBOULAI_TOKEN", raising=False)
        publisher = make_publisher(tmp_path, ciboulai_platform)
        assert publisher.ensure_experiment(reset=True) is True
        assert publisher.publish("Forecast.0001", "itself", {}) is False
        # the publication of the next file is still attempted
        assert publisher.publish("Forecast.0001", "consistency", {}) is False
        assert server.types() == ["xpinfo", "taskinfo", "taskinfo"]

    def test_wrong_token_stops_further_attempts(
        self, ciboulai_platform, tmp_path, monkeypatch
    ):
        """Test that an invalid token disables the publisher."""
        server = FakeCiboulaiServer(
            json.dumps({"returnCode": 1, "message": "Token is not present or wrong"})
        )
        monkeypatch.setattr(
            PublishToCiboulai,
            "_http_post_form",
            lambda _self, url, form: server.post(url, form),
        )
        monkeypatch.delenv("CIBOULAI_TOKEN", raising=False)
        publisher = make_publisher(tmp_path, ciboulai_platform)
        assert publisher.ensure_experiment(reset=True) is True
        assert publisher.publish("Forecast.0001", "itself", {}) is False
        assert server.types() == ["xpinfo"]

    def test_connection_error_is_not_fatal(
        self, ciboulai_platform, tmp_path, monkeypatch
    ):
        """Test that a network failure during retries is reported, not raised."""
        monkeypatch.delenv("CIBOULAI_TOKEN", raising=False)

        def raise_oserror(self, url, form):
            raise OSError("network is down")

        monkeypatch.setattr(PublishToCiboulai, "_http_post_form", raise_oserror)
        publisher = make_publisher(tmp_path, ciboulai_platform)
        assert publisher.ensure_experiment(reset=True) is True
        assert publisher.publish("Forecast.0001", "itself", {}) is False
        # the experiment name is still resolved and stored
        assert publisher.xpid == f"ttr-0001-{publisher.machine}@{publisher.user}"

    def test_ciboulai_summary_publishes_written_files(
        self, ciboulai_server, ciboulai_platform, tmp_path
    ):
        """Test the integration: append writes the files and publishes them."""
        config = make_ciboulai_config(tmp_path)
        stack_dir = str(tmp_path / "checks" / "summaries_stack")
        os.makedirs(stack_dir, exist_ok=True)
        summary = CheckSummaryCiboulai("ciboulai", stack_dir, config)
        summary.init_full_path(ciboulai_platform)
        assert summary.publisher is not None

        summary.create()
        summary.append([
            make_ciboulai_definition("Forecast", "utlog", ["SUCCESS - identical"])
        ])

        # one xpinfo to initialise, then itself before consistency
        assert ciboulai_server.types() == ["xpinfo", "taskinfo", "taskinfo"]
        scope_order = [
            next(iter(payload["Forecast.0001"]))
            for payload in (
                ciboulai_server.json_data(1),
                ciboulai_server.json_data(2),
            )
        ]
        assert scope_order == ["itself", "consistency"]
        published = ciboulai_server.json_data(2)["Forecast.0001"]["consistency"]
        assert published == TestCheckSummaryCiboulai._load(
            summary, "Forecast.0001.consistency.json"
        )

    def test_ciboulai_summary_disabled_has_no_publisher(
        self, ciboulai_platform, tmp_path
    ):
        """Test that an inactive ciboulai section leaves the stack offline."""
        config = make_ciboulai_config(tmp_path, enabled=False)
        stack_dir = str(tmp_path / "checks" / "summaries_stack")
        os.makedirs(stack_dir, exist_ok=True)
        summary = CheckSummaryCiboulai("ciboulai", stack_dir, config)
        summary.init_full_path(ciboulai_platform)
        assert summary.publisher is None


class TestReferenceCheckManager:
    """Tests for the ReferenceCheckManager class."""

    @staticmethod
    def _simulate_forecast_execution(config, identical):
        """Simulate forecast execution by creating expected output files."""
        file_manager = FileManager(config)
        workdir = file_manager.platform.get_system_value("wrk")
        if os.path.exists(workdir):
            shutil.rmtree(workdir)
        result_dir = os.path.join(workdir, "forecast_results")
        log_dir = os.path.join(workdir, "logs/Forecast")
        os.makedirs(result_dir, exist_ok=True)
        os.makedirs(log_dir, exist_ok=True)

        with open(os.path.join(result_dir, "GRIB_00000"), "w") as f:
            if identical:
                f.write("ExpectedValue in GRIB_00000\n")
            else:
                f.write("DifferentValue in GRIB_00000\n")

        for i in range(2):
            with open(os.path.join(result_dir, f"ICMSH_0000{i}"), "w") as f:
                if identical:
                    f.write(f"ExpectedValue in ICMSH_0000{i}\n")
                else:
                    f.write(f"DifferentValue in ICMSH_0000{i}\n")

        test_file = os.path.join(log_dir, "NODE.001_01")
        source = os.path.join(
            os.path.dirname(__file__), "data/reference_checker/reference", "NODE.001_01"
        )
        shutil.copyfile(source, test_file)

    def _prepare_expected_values(self, test_combination):
        """Depending on test_combination, we define the settings of the test and the expected values.

        Args:
            test_combination: a string defining the combination to test
        """
        # Define the main vaparameters of the test
        self.check = "check" in test_combination
        self.generate = "generate" in test_combination
        self.nofile = "nofile" in test_combination
        self.identical = "identical" in test_combination
        self.smalldiff = "smalldiff" in test_combination
        self.tolerance_xtool = 5 if self.smalldiff else 12
        self.tolerance_normschecker = 5 if self.smalldiff or self.identical else -1
        self.suppress_exception = "suppress_exception" in test_combination

        # Define reference values regarding rules
        self.rule_count = {}
        self.rule_count["Prep"] = 0
        self.rule_count["PostMortem"] = 0
        self.rule_count["Forecast"] = 3
        self.rule_count["Interpol"] = 1
        self.rule_count["Marsprep"] = 1
        self.rule_count["Creategrib"] = 1
        self.rule_count["TaskToBeSkipped"] = 0

        self.total_rule_count = 0
        for task in TestReferenceCheckManager._task_list():
            self.total_rule_count = self.total_rule_count + self.rule_count[task]

        # Define expected number of files in each category
        self.missing_file_count = 0
        self.generated_file_count = 0
        self.generated_file_na_count = 0
        self.detailed_result_count = 0

        self.count = {}

        for method in TestReferenceCheckManager._method_list():
            self.count[method] = {}

            for counter in [
                "smalldiff",
                "largediff",
                "identical",
                "success",
                "failure",
                "tested",
            ]:
                self.count[method][counter] = 0

        # We have defined 3 rules for xtool, we will provide 2 files per rule
        self.count["xtool"]["tested"] = 6

        # We have 2 rules for normschecker, we will provide 1 file per rule
        self.count["normschecker"]["tested"] = 2

        self.total_test_file_count = 0

        for method in TestReferenceCheckManager._method_list():
            self.total_test_file_count = (
                self.total_test_file_count + self.count[method]["tested"]
            )

        self.expected_success = True
        if self.nofile:
            self.missing_file_count = self.total_rule_count
            self.expected_success = False
        else:
            if self.generate:
                self.generated_file_count = self.total_test_file_count
            else:
                self.generated_file_na_count = self.total_test_file_count

            if self.check:
                for method in TestReferenceCheckManager._method_list():
                    if self.smalldiff:
                        self.count[method]["smalldiff"] = self.count[method]["tested"]
                    if self.identical:
                        self.count[method]["identical"] = self.count[method]["tested"]
                    if not self.smalldiff and not self.identical:
                        self.count[method]["largediff"] = self.count[method]["tested"]
                        self.expected_success = False

            self.detailed_result_count = self.total_test_file_count

        self.total_success_count = 0
        self.total_failure_count = 0

        for method in TestReferenceCheckManager._method_list():
            self.count[method]["success"] = (
                self.count[method]["smalldiff"] + self.count[method]["identical"]
            )
            self.count[method]["failure"] = self.count[method]["largediff"]

            self.total_success_count = (
                self.total_success_count + self.count[method]["success"]
            )
            self.total_failure_count = (
                self.total_failure_count + self.count[method]["failure"]
            )
        if not self.check:
            result_message = "MISSING - check is disabled"
            if self.missing_file_count > 0:
                result_message = f"{result_message}. {self.missing_file_count} missing(s)"
            self.analysis_result = result_message
        else:
            result_message = "SUCCESS" if self.expected_success else "FAILURE"
            self.analysis_result = f"{result_message} - {self.total_failure_count} error(s), {self.total_success_count} success(es), {self.missing_file_count} missing(s)"

    def _validate_summary_messages(self, summary_path, file_format):
        """Validation that the summay file contains the expected messages.

        Args:
           summary_path: the file path to the summary
           file_format: the file format of the summary (json, txt)
        """
        with open(summary_path, "r") as f:
            content = f.read()

        assert content_contains(
            content, "No file found using", count=self.missing_file_count
        )
        assert content_contains(
            content,
            f"SUCCESS - Worst digit is 0 <= tol = {self.tolerance_normschecker} (mode=get_worst, which=first_and_last_spectral",
            self.count["normschecker"]["success"],
        )

        assert content_contains(
            content,
            "SUCCESS - Files are bit identical",
            self.count["xtool"]["identical"],
        )

        assert content_contains(
            content,
            f"SUCCESS - All results within Tolerance Level = {self.tolerance_xtool}",
            self.count["xtool"]["smalldiff"],
        )

        assert content_contains(
            content,
            "ERROR - executing XtoolChecker",
            self.count["xtool"]["largediff"],
        )

        if file_format == "json":
            assert content_contains(
                content, '"generate_file": "/', self.generated_file_count
            )
            assert content_contains(
                content, '"generate_file": "N/A"', self.generated_file_na_count
            )
            assert content_contains(content, '"result_file"', self.detailed_result_count)
        else:
            assert content_contains(
                content, "Generated Reference File: /", self.generated_file_count
            )
            assert content_contains(
                content,
                "Generated Reference File: N/A",
                self.generated_file_na_count,
            )
            assert content_contains(
                content, "Detailed Result:", self.detailed_result_count
            )

    @staticmethod
    def _method_list():
        """Provide the list of methods that are tested."""
        return ["xtool", "normschecker"]

    @staticmethod
    def _task_list():
        """Provide the list of tasks simulated in the unit test."""
        return [
            "Prep",
            "Forecast",
            "Interpol",
            "TaskToBeSkipped",
            "Marsprep",
            "Creategrib",
            "PostMortem",
        ]

    def _validate_txt_analysis_content(self, summary_txt_path):
        """Validation of the analysis part of the summary in txt format.

        Args:
           summary_txt_path: the file path to the txt summary
        """
        total_count = (
            self.total_success_count + self.missing_file_count + self.total_failure_count
        )
        expected_list = {}
        expected_list["# Generated files:"] = str(self.generated_file_count)
        expected_list["# Successful tests:"] = str(self.total_success_count)
        expected_list["# Failure tests:"] = str(self.total_failure_count)
        expected_list["# Missing files:"] = str(self.missing_file_count)
        expected_list["# Total files:"] = str(total_count)
        expected_list["# Success:"] = str(self.expected_success)
        expected_list["# Result:"] = str(self.analysis_result)

        with open(summary_txt_path, "r") as file:
            lines = file.readlines()

        # the number of lines written in the last part of the summary
        lines_in_analysis = 8
        assert len(lines) > lines_in_analysis

        for key, expected in expected_list.items():
            found = False
            for line in lines[-lines_in_analysis:]:
                if line.startswith(key):
                    clean_line = line.replace("\n", "")
                    value = clean_line.split(":")[1].strip()
                    assert value == expected, (
                        f'{clean_line}\n. Expected "{key} {expected}""'
                    )
                    found = True
                    break
            assert found

    def _validate_json_analysis_content(self, summary_json_path):
        """Validation of the analysis part of the summary in json format.

        Args:
           summary_json_path: the file path to the json summary
        """
        with open(summary_json_path, "r") as file:
            total_count = (
                self.total_success_count
                + self.missing_file_count
                + self.total_failure_count
            )
            data = json.load(file)
            assert len(data["tasks"]) == 5

            assert data["analysis"]["success"] == self.expected_success
            assert data["analysis"]["error_count"] == self.total_failure_count
            assert data["analysis"]["missing_count"] == self.missing_file_count
            assert data["analysis"]["success_count"] == self.total_success_count
            assert data["analysis"]["generated_count"] == self.generated_file_count
            assert data["analysis"]["total_count"] == total_count
            assert data["analysis"]["result"] == self.analysis_result

    def _validate_colored_result_message(self, summary_json_path, case_name):
        """Validation of the colored results message in the summary.

        Args:
            summary_json_path: the file path to the json summary
            case_name: the name of the test case
        """
        with open(summary_json_path, "r") as file:
            summary = json.load(file)

        time = 130  # seconds
        now = datetime.datetime.now()
        creation_date = datetime.datetime.now() - datetime.timedelta(seconds=time)
        assert summary
        colored_message = CheckSummaryAnalysis.colored_result_message(
            summary,
            False,
            case_name,
            summary_json_path,
            len(case_name),
            creation_date,
            now,
        )
        expected = {}
        for test in [
            "check_identical",
            "check_smalldiff",
            "check_diff",
            "check_diff_suppress_exception",
            "check_identical_generate",
            "check_generate_nofile",
        ]:
            expected[test] = f"{test} | "
            if self.expected_success:
                expected[test] += "<green>SUCCESS </green>"
            else:
                expected[test] += "<red>FAILURE </red>"
            expected[test] += "(2 min ago) "
            expected[test] += (
                f"<white>[{self.total_failure_count} error(s), {self.total_success_count} success(es), {self.missing_file_count} missing(s)]</white>"
            )

        expected["generate"] = (
            "generate | <green>MISSING </green>(2 min ago) <white>[check is disabled]</white>"
        )

        assert colored_message == expected[case_name]

    def _simulate_suite_execution(self, basic_config, test_combination):
        """Simulate the execution of a suite.

        You can use the script tests/unit/data/reference_checker/reference/create_ref_links.sh to
        create the reference files needed for the test in the right location.

        Args:
            basic_config: config dictionary
            test_combination: a string defining the combination to test
        Returns:
            True if TestReferenceCheckManager has been tested
        """
        self._prepare_expected_values(test_combination)

        config = configure_for_check_and_generate(
            basic_config,
            test_combination,
            self.check,
            self.generate,
            self.tolerance_xtool,
            self.tolerance_normschecker,
            self.suppress_exception,
        )

        if not self.nofile:
            TestReferenceCheckManager._simulate_forecast_execution(config, self.identical)
        fmanager = FileManager(config)

        for task in TestReferenceCheckManager._task_list():
            manager = ReferenceCheckManager.create_reference_check_manager(config, task)

            if manager:
                # Validate generic settings
                assert manager.taskname == task
                assert manager.check == self.check
                assert manager.generate == self.generate

                # Validate summary settings
                assert manager.create_summary == (task == "Prep")
                assert manager.analyze_summary == (task == "PostMortem")
                assert len(manager.summary_list) == 2

                # Validate rules settings
                assert len(manager.check_definitions) == self.rule_count[task]

                if self.check:
                    assert len(manager.reference_checkers) == self.rule_count[task]

                # Execute the tests
                if (
                    manager.analyze_summary
                    and not self.expected_success
                    and not self.suppress_exception
                ):
                    # test will fail only in the analyse summary step
                    with pytest.raises(RuntimeError):
                        manager.execute(fmanager)
                else:
                    manager.execute(fmanager)
            else:
                assert task == "TaskToBeSkipped" or (
                    (not self.check) and (not self.generate)
                )

            # Validate the summary at the end
            summary_txt_path = os.path.join(
                fmanager.platform.get_system_value("wrk"), "checks/summary.txt"
            )
            summary_json_path = os.path.join(
                fmanager.platform.get_system_value("wrk"), "checks/summary.json"
            )

        if self.check or self.generate:
            self._validate_summary_messages(summary_txt_path, file_format="txt")
            self._validate_summary_messages(summary_json_path, file_format="json")

            self._validate_txt_analysis_content(summary_txt_path)
            self._validate_json_analysis_content(summary_json_path)

            self._validate_colored_result_message(summary_json_path, test_combination)

            return True

        assert not os.path.exists(summary_txt_path)
        assert not os.path.exists(summary_json_path)
        return False

    def test_reference_check_manager_exclude_rules(self, basic_config):
        update = {
            "reference_checker": {
                "rules_active": ["Foo.foo", "Foo.bar"],
                "rules_excluded": ["Foo.bar"],
                "check": True,
                "generate": False,
            }
        }
        config = basic_config.copy(update=update)
        manager = ReferenceCheckManager.create_reference_check_manager(config, "Foo")
        assert manager.rules_active == ["foo"]

    def test_reference_check_manager_execute_nocheck_nogenerate(self, basic_config):
        """Run suite without check and without generate. Nothing should happen."""
        assert not self._simulate_suite_execution(basic_config, "")

    def test_reference_check_manager_execute_check_identical(self, basic_config):
        """Run suite with check (no generate), results must be bit-identical."""
        assert self._simulate_suite_execution(basic_config, "check_identical")

    def test_reference_check_manager_execute_check_smalldiff(self, basic_config):
        """Run suite with check (no generate), results must be below threshold."""
        assert self._simulate_suite_execution(basic_config, "check_smalldiff")

    def test_reference_check_manager_execute_check_diff(self, basic_config):
        """Run suite with check (no generate), results must be above threshold."""
        assert self._simulate_suite_execution(basic_config, "check_diff")

    def test_reference_check_manager_execute_check_diff_suppress_exception(
        self, basic_config
    ):
        """Run suite with with results above threshold but exception suppressed."""
        assert self._simulate_suite_execution(
            basic_config, "check_diff_suppress_exception"
        )

    def test_reference_check_manager_execute_check_generate_nofile(self, basic_config):
        """Run suite with check and generate, an error have to be triggered."""
        assert self._simulate_suite_execution(basic_config, "check_generate_nofile")

    def test_reference_check_manager_execute_generate(self, basic_config):
        """Run suite with generate (no check), no result are available in the end."""
        assert self._simulate_suite_execution(basic_config, "generate")

    def test_reference_check_manager_execute_check_identical_generate(self, basic_config):
        """Run suite with check and generate, results must be bit-identical."""
        assert self._simulate_suite_execution(basic_config, "check_identical_generate")
