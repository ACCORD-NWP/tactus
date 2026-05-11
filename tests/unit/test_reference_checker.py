#!/usr/bin/env python3
"""Unit tests for reference_checker.py."""

import json
import os
import shutil
from pathlib import Path

import pytest
import tomlkit

from tactus.derived_variables import set_times
from tactus.reference_checker import (
    CheckSummary,
    CheckSummaryJson,
    CheckSummaryTxt,
    NormsChecker,
    ReferenceChecker,
    ReferenceCheckManager,
    XToolChecker,
)
from tactus.toolbox import FileManager


@pytest.fixture(scope="module")
def basic_config(default_config):
    config = default_config
    config = config.copy(update=set_times(config))
    return config.copy(update={"general": {"cycle": "CY49t2"}})


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
            result_message = "N/A - check is disabled"
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
        expecteds = {}
        expecteds["# Generated files:"] = str(self.generated_file_count)
        expecteds["# Successful tests:"] = str(self.total_success_count)
        expecteds["# Failure tests:"] = str(self.total_failure_count)
        expecteds["# Missing files:"] = str(self.missing_file_count)
        expecteds["# Total files:"] = str(total_count)
        expecteds["# Success:"] = str(self.expected_success)
        expecteds["# Result:"] = str(self.analysis_result)

        with open(summary_txt_path, "r") as file:
            lines = file.readlines()

        # the number of lines written in the last part of the summary
        lines_in_analysis = 7
        assert len(lines) > lines_in_analysis

        for key, expected in expecteds.items():
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
            return True

        assert not os.path.exists(summary_txt_path)
        assert not os.path.exists(summary_json_path)
        return False

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
