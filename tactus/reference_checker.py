"""Compare results agains references."""

from __future__ import annotations

import filecmp
import json
import os
import shutil
import socket
import subprocess
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime

from arpifs_listings import norms

from tactus.experiment import get_git_info
from tactus.logs import logger
from tactus.os_utils import FileLock, Search, tactusmakedirs
from tactus.toolbox import FileManager, Platform

from .datetime_utils import since_str


class ReferenceChecker:
    """Base class for comparison against a reference."""

    def __init__(self, tool):
        """Construct ReferenceChecker object."""
        self.tool = tool

    def compare(self, test_file, reference_file) -> str:  # noqa ARG001
        """Compare a file to a reference file.

        Args:
            test_file: name of the file to compare
            reference_file: name of the reference file
        Returns:
              str giving the result of the comparison
        """
        return "ERROR - Compare not implemented"

    def prepare(self, platform: Platform):
        """Prepare the binary path by substituting the pattern.

        Args:
            platform: platform class providing the substitution capabilities.
        """

    @staticmethod
    def create_reference_checker(method, config) -> ReferenceChecker:
        """Instanciate a ReferenceChecker following the method and the configuration.

        Args:
           method: str defining the method
           config (ParsedConfig): Configuration
        Returns:
            A ReferenceChecker
        """
        tool = config["methods"][method]["tool"]
        if tool == "namelist_checker":
            ignore_case = config["methods"][method].get("ignore_case", True)
            ignore_blank_lines = config["methods"][method].get("ignore_blank_lines", True)
            ignore_whitespace = config["methods"][method].get("ignore_whitespace", True)
            return NamelistChecker(
                ignore_case=ignore_case,
                ignore_blank_lines=ignore_blank_lines,
                ignore_whitespace=ignore_whitespace,
            )
        if tool == "norms_checker":
            which = config["methods"][method]["which"]
            mode = config["methods"][method]["mode"]
            tolerance = int(config["methods"][method]["tolerance"])
            return NormsChecker(which, tolerance, mode)
        if tool == "xtool":
            binary_pattern = config["methods"][method]["binary"]
            file_format = config["methods"][method]["file_format"]
            tolerance = int(config["methods"][method]["tolerance"])
            args_template = config["methods"][method]["args_template"]
            return XToolChecker(
                binary_pattern,
                file_format=file_format,
                tolerance=tolerance,
                args_template=args_template,
            )

        logger.warning(f"Reference Checker: Comparison {method} not found")
        return None


class NamelistChecker(ReferenceChecker):
    """Compare Fortran NAMELIST files against a reference using diff."""

    def __init__(self, ignore_case=True, ignore_blank_lines=True, ignore_whitespace=True):
        """Construct NamelistChecker object.

        Args:
            ignore_case: if True, ignore case differences (diff -i)
            ignore_blank_lines: if True, ignore blank lines (diff -B)
            ignore_whitespace: if True, ignore whitespace differences (diff -w)
        """
        ReferenceChecker.__init__(self, tool="namelist_checker")
        self.ignore_case = ignore_case
        self.ignore_blank_lines = ignore_blank_lines
        self.ignore_whitespace = ignore_whitespace

    def _build_diff_args(self) -> list[str]:
        """Build the list of options to pass to diff."""
        args = []
        if self.ignore_case:
            args.append("-i")
        if self.ignore_blank_lines:
            args.append("-B")
        if self.ignore_whitespace:
            args.append("-w")
        return args

    def compare(self, test_file, reference_file, out_file) -> str:
        """Compare a NAMELIST file against a reference using diff.

        Args:
            test_file: name of the namelist file to compare
            reference_file: name of the reference namelist file
            out_file: name of the file produced by the comparison

        Returns:
            str giving the result of the comparison

        Raises:
            Exception: Any exception occurring during diff that is not a
                       CalledProcessError with returncode in (0, 1)
        """
        results = []
        unhandled_exception = None

        if not os.path.exists(test_file):
            results.append(f"ERROR - Test file {test_file} not found")
        if not os.path.exists(reference_file):
            results.append(f"ERROR - Reference file {reference_file} not found")

        if os.path.exists(out_file):
            os.remove(out_file)

        if len(results) == 0:
            bit_identical = filecmp.cmp(test_file, reference_file, shallow=False)
            if bit_identical:
                results.append("SUCCESS - Files are bit identical")

        if len(results) == 0:
            cmd = ["diff", *self._build_diff_args(), test_file, reference_file]
            try:
                with open(out_file, "w") as out:
                    completed = subprocess.run(cmd, check=False, stdout=out, stderr=out)
                # diff exit code: 0 = identical, 1 = differences, >1 = error
                if completed.returncode == 0:
                    results.append(
                        "SUCCESS - Namelists are identical (modulo ignored options)"
                    )
                elif completed.returncode == 1:
                    results.append(
                        "FAILURE - Differences found between namelist and reference"
                    )
                else:
                    results.append(
                        "ERROR - executing NamelistChecker\n"
                        + f"Command '{cmd}' failed with exit code: "
                        + f"{completed.returncode}\n"
                    )
            # catching blind exception to make sure we don't miss any error.
            # The exception is stored in unhandled_exception and raised at the end
            except Exception as e:  # noqa: BLE001
                results.append(
                    "ERROR - executing NamelistChecker\n"
                    + "Command 'diff' failed\n"
                    + str(e)
                )
                unhandled_exception = e

        result = "\n".join(results)
        logger.info(f"NamelistChecker result: {result}")

        with open(out_file, "a") as out:
            out.write("\n")
            out.write(result)

        if unhandled_exception:
            raise unhandled_exception

        return result


class NormsChecker(ReferenceChecker):
    """Compare the norms in Node files against a reference."""

    def __init__(self, which, tolerance, mode):
        """Construct NormsChecker object.

        Args:
           which: first_and_last_spectral or all (from arpifs_listing)
           tolerance: integer giving the worstdigit
           mode: get_worst or get_worst_by_step (from arpifs_listings)
        """
        ReferenceChecker.__init__(self, tool="norms_checker")
        self.which = which
        self.tolerance = tolerance
        self.mode = mode

    def compare(self, test_log, reference_log, out_file) -> str:
        """Compare the norms against a reference.

        Args:
              test_log: name of the lof to compare
              reference_log: name of the reference log file
              out_file: name of the file produced by the comparion
        Returns:
              str giving the result of the comparison
        """
        results = []

        if not os.path.exists(test_log):
            results.append(f"ERROR - Test log {test_log} not found")
        if not os.path.exists(reference_log):
            results.append(f"ERROR - Reference log {reference_log} not found")

        if len(results) == 0:
            try:
                l1_n = norms.NormsSet(test_log)
                l2_n = norms.NormsSet(reference_log)
            except UnicodeDecodeError:
                with (
                    open(test_log, "r", errors="replace") as test,
                    open(reference_log, "r", errors="replace") as reference,
                ):
                    test_log_content = test.readlines()
                    reference_log_content = reference.readlines()
                    l1_n = norms.NormsSet(test_log_content)
                    l2_n = norms.NormsSet(reference_log_content)

            if len(l1_n.norms_at_each_step) == 0:
                results.append(f"ERROR - No norms found in {test_log}")
            if len(l2_n.norms_at_each_step) == 0:
                results.append(f"ERROR - No norms found in {reference_log}")

        with open(out_file, "w") as out:
            if len(results) == 0:
                onlymaxdiff = self.mode == "get_worst"
                worstdigit = norms.compare_normsets(
                    l1_n,
                    l2_n,
                    mode=self.mode,
                    which=self.which,
                    onlymaxdiff=onlymaxdiff,
                    out=out,
                )
                if worstdigit <= self.tolerance:
                    results.append(
                        f"SUCCESS - Worst digit is {worstdigit} <= tol = "
                        + f"{self.tolerance} (mode={self.mode}, which={self.which})"
                    )
                else:
                    results.append(
                        f"FAILURE - Worst digit is {worstdigit} > tol = {self.tolerance}"
                    )

            result = "\n".join(results)
            out.write(result)

        logger.info(f"NormsChecker result: {result}")
        return result


class XToolChecker(ReferenceChecker):
    """Compare fields against reference using xtool."""

    def __init__(self, binary_pattern, file_format, tolerance, args_template=None):
        """Construct XToolChecker object.

        Args:
             binary_pattern: the filepath to xtool executable
             file_format: the file format to be passed to xtool
             tolerance: the tolerance value to be passed to xtool
             args_template: arguments template to be passed to xtool
        """
        ReferenceChecker.__init__(self, tool="xtool")
        self.binary_pattern = binary_pattern
        self.file_format = file_format
        self.tolerance = tolerance
        self.args_template = args_template
        self.binary = binary_pattern

    def prepare(self, platform: Platform):
        """Prepare the binary path by substituting the pattern.

        Args:
            platform: platform class providing the substitution capabilities.
        """
        self.binary = platform.substitute(self.binary_pattern)

    def compare(self, test_file, reference_file, out_file) -> str:
        """Compare the fields against a reference using xtool.

        Args:
              test_file: name of the lof to compare
              reference_file: name of the reference log file
              out_file: name of the file produced by the comparison

        Returns:
              str giving the result of the comparison

        Raises:
            Exception: Any exception occuring during xtool that is no CalledProcessError
        """
        results = []
        unhandled_exception = None

        if not os.path.exists(test_file):
            results.append(f"ERROR - Test file {test_file} not found")
        if not os.path.exists(reference_file):
            results.append(f"ERROR - Reference file {reference_file} not found")
        if not os.path.exists(self.binary):
            results.append(f"ERROR - {self.binary} not found")
        if self.file_format not in ["GRIB", "FA"]:
            results.append(f"ERROR - Unsupported file format {self.file_format}")

        if len(results) == 0:
            bit_identical = filecmp.cmp(test_file, reference_file, shallow=False)
            if bit_identical:
                results.append("SUCCESS - Files are bit identical")

        if os.path.exists(out_file):
            os.remove(out_file)

        if len(results) == 0:
            file_format = "-f" if self.file_format == "FA" else ""
            if self.args_template:
                args = self.args_template
            else:
                args = (
                    f"-f1 {test_file} -f2 {reference_file} {file_format}"
                    + f"-s -of SCREEN -de -to {self.tolerance}"
                )

            args = args.format(
                test_file=test_file,
                reference_file=reference_file,
                file_format=file_format,
                tolerance=self.tolerance,
            )
            try:
                cmd = [self.binary, *args.split()]
                with open(out_file, "w") as out:
                    subprocess.run(cmd, check=True, stdout=out, stderr=out)

            except subprocess.CalledProcessError as e:
                results.append(
                    "ERROR - executing XtoolChecker\n"
                    + f"Command '{e.cmd}' failed with exit code: {e.returncode}\n"
                )
            # catching blind exception to make sure that we don't miss any error.
            # The exception is stored unhandled_exception and raised again at the end
            except Exception as e:  # noqa: BLE001
                results.append(
                    "ERROR - executing XtoolChecker\n"
                    + f"Command '{self.binary}' failed\n"
                    + str(e)
                )
                unhandled_exception = e
        if len(results) == 0:
            results.append(
                f"SUCCESS - All results within Tolerance Level = {self.tolerance}\n"
            )

        result = "\n".join(results)
        logger.info(f"XToolChecker result: {result}")

        with open(out_file, "a") as out:
            out.write(result)

        if unhandled_exception:
            raise unhandled_exception

        return result


class CheckItem:
    """An item to be checked, describing the test performed and the result."""

    def __init__(
        self, test_file, reference_file, generate_file, result_file, result="N/A"
    ):
        """Creation of a CheckItem.

        Args:
           test_file: the input file that has been tested
           reference_file: the file which was used as reference
           generate_file: the generated reference file
           result_file: the comparison file produced by the method
           result: the result of the comparison (Default: "N/A")
        """
        self.test_file = test_file
        self.reference_file = reference_file
        self.generate_file = generate_file
        self.result_file = result_file
        self.result = result


class CheckDefinition:
    """A definition containing the required information to perform a check."""

    def __init__(
        self,
        taskname,
        rulename,
        label_suffix,
        method,
        inpath_pattern,
        files_pattern,
        references_pattern,
        results_dir_pattern,
        generate_dir_pattern,
        tool="",
    ):
        """Creation of a CheckDefinition.

        Args:
                taskname: the name of the task
                rulename: the name of the rule
                label_suffix: the suffix for the label
                method: the method to perform the comparison
                inpath_pattern:  path to the files to be tested
                files_pattern: pattern defining the files to be tested
                references_pattern: pattern defining the reference files
                results_dir_pattern: pattern defining the directory where to store results
                generate_dir_pattern: pattern defining the directory where to store \
                                    generated references
                tool: the tool used for comparison (Default: "")
        """
        self.taskname = taskname
        self.rulename = rulename
        self.label_suffix = label_suffix
        self.method = method
        self.inpath_pattern = inpath_pattern
        self.files_pattern = files_pattern
        self.references_pattern = references_pattern
        self.results_dir_pattern = results_dir_pattern
        self.generate_dir_pattern = generate_dir_pattern
        self.tool = tool

    @staticmethod
    def create_list_of_check_definitions(
        config, taskname, label_suffix, rules_active, check, generate
    ) -> list[CheckDefinition]:
        """Create the list of items to be checked.

        Args:
            config (ParsedConfig): Configuration
            taskname: the name of the task
            label_suffix: the suffix for the label
            rules_active: list of rules that are active
            check: boolean indicating if the check should be performed
            generate: boolean indicating if the reference generation should be performed
        Returns:
            list of CheckDefinition
        """
        check_definitions = []

        # iterates over tasks and rules from the config file to create check definitions
        if taskname in config["task"]:
            for rulename in rules_active:
                if rulename in config["task"][taskname]:
                    parameters = ["method", "inpath", "pattern", "result_folder"]
                    if generate:
                        parameters.append("generate_folder")
                    if check:
                        parameters.append("reference_folder")

                    have_all_parameters = True
                    for parameter in parameters:
                        if parameter not in config["task"][taskname][rulename]:
                            logger.warning(
                                f"Reference Checker - {parameter} not defined for"
                                + f" task {taskname} and rule {rulename}."
                            )
                            have_all_parameters = False
                    if not have_all_parameters:
                        logger.warning(
                            f"Skipping reference check definition for {taskname}"
                            + f" and rule {rulename}"
                        )
                        continue

                    method = config["task"][taskname][rulename]["method"]
                    inpath = config["task"][taskname][rulename]["inpath"]
                    pattern = config["task"][taskname][rulename]["pattern"]
                    results_dir_pattern = config["task"][taskname][rulename][
                        "result_folder"
                    ]
                    if generate:
                        generate_dir_pattern = config["task"][taskname][rulename][
                            "generate_folder"
                        ]
                    if check:
                        references_pattern = config["task"][taskname][rulename][
                            "reference_folder"
                        ]
                    check_definition = CheckDefinition(
                        taskname,
                        rulename,
                        label_suffix,
                        method,
                        inpath,
                        pattern,
                        references_pattern if check else None,
                        results_dir_pattern,
                        generate_dir_pattern if generate else None,
                    )
                    check_definitions.append(check_definition)
        return check_definitions

    def create_items(self, platform: Platform):
        """Transform the patterns in the definition into a list of items with filepath.

        Args:
            platform: platform class providing the substitution capabilities.
        """
        self.items = []
        self.files = platform.substitute(self.files_pattern)
        self.inpath = platform.substitute(self.inpath_pattern)
        result_dir = platform.substitute(self.results_dir_pattern)

        suffix = platform.substitute(self.label_suffix)
        self.uniquename = f"{self.taskname}.{suffix}"

        reference_dir = (
            platform.substitute(self.references_pattern)
            if self.references_pattern
            else None
        )
        generate_dir = (
            platform.substitute(self.generate_dir_pattern)
            if self.generate_dir_pattern
            else None
        )

        try:
            test_filepaths = Search.find_files(
                directory=self.inpath,
                pattern=self.files,
                prefix="",
                postfix="",
                recursive=False,
                onlyfiles=True,
                fullpath=True,
            )
        except FileNotFoundError:
            logger.warning(
                "Reference Checker - No file found using"
                + f"{self.files_pattern} in {self.inpath}"
            )
            test_filepaths = []

        for test_file in test_filepaths:
            filename = os.path.basename(test_file)
            item = CheckItem(
                test_file,
                os.path.join(reference_dir, filename) if reference_dir else "N/A",
                os.path.join(generate_dir, filename) if generate_dir else "N/A",
                os.path.join(result_dir, f"{filename}.diff"),
            )

            self.items.append(item)


class CheckSummary:
    """Generic class for summary files generation containing comparisons results."""

    def __init__(self, fileformat, filename):
        """Creation of a CheckSummary.

        Args:
           fileformat: the format of the summary file (e.g. "txt", "json", "ciboulai")
           filename: the pattern defining the name of the summary file
        """
        self.fileformat = fileformat
        self.filename = filename
        self.fullpath = None

    @staticmethod
    def create_summary_list(config) -> list[CheckSummary]:
        """Create the list of summary_list from the configuration.

        Args:
           config (ParsedConfig): Configuration
        Returns:
           list of CheckSummary

        """
        summary_list = []
        summaries = config.get("summary", {})

        for summary in config["summary_active"]:
            if summary not in summaries:
                logger.warning(
                    "Reference Checker Summary - no definition found for "
                    f"{summary!r}: add a "
                    f"[reference_checker.summary.{summary}] section or remove it "
                    "from summary_active"
                )
                continue
            file_format = summaries[summary]["format"]
            file_name = summaries[summary]["file"]
            if file_format == "json":
                summary_list.append(CheckSummaryJson(file_format, file_name))
            elif file_format == "txt":
                summary_list.append(CheckSummaryTxt(file_format, file_name))
            elif file_format == "ciboulai":
                summary_list.append(
                    CheckSummaryCiboulai(file_format, file_name, config.get("ciboulai"))
                )
            else:
                logger.warning(
                    f"Reference Checker Summary - format unknown: {file_format}"
                )

        return summary_list

    def init_full_path(self, platform):
        """Init self.full path using platform substitute.

        Args:
           platform: Platform
        """
        if not self.fullpath:
            self.fullpath = platform.substitute(self.filename)

    def delete(self):
        """Delete file if exists with a logger information message."""
        if os.path.exists(self.fullpath):
            logger.info("Delete reference checker summary:", self.fullpath)
            os.remove(self.fullpath)


class CheckSummaryAnalysis:
    """Manage the final section of a summary, containing global tests results."""

    def __init__(self, check):
        """Create a CheckSummaryAnalysis object."""
        self.error_count = 0
        self.success_count = 0
        self.missing_count = 0
        self.generated_count = 0
        self.check = check
        self.error_message = ""

    def increment_error_count(self):
        """Increment error_count."""
        self.error_count = self.error_count + 1

    def increment_success_count(self):
        """Increment success_count."""
        self.success_count = self.success_count + 1

    def increment_missing_count(self):
        """Increment missing_count."""
        self.missing_count = self.missing_count + 1

    def increment_generated_count(self):
        """Increment generated_count."""
        self.generated_count = self.generated_count + 1

    def append_error_message(self, error_message):
        """Append an error message."""
        if len(self.error_message) == 0:
            self.error_message = error_message
        else:
            self.error_message = f"{self.error_message}\n{error_message}"

    def success(self):
        """Return true iff all the tests are successful."""
        if len(self.error_message) > 0:
            return False

        if not self.check:
            return True

        return self.error_count == 0 and self.missing_count == 0

    def total_count(self):
        """Retern total number of files tested and missing."""
        return self.error_count + self.success_count + self.missing_count

    def message(self):
        """Return a summary message."""
        if len(self.error_message) > 0:
            return f"ERROR : {self.error_message}"

        if not self.check:
            result_message = "MISSING - check is disabled"
            if self.missing_count > 0:
                result_message = f"{result_message}. {self.missing_count} missing(s)"
            return result_message

        result_message = "SUCCESS" if self.success() else "FAILURE"
        return (
            f"{result_message} - {self.error_count} error(s),"
            + f" {self.success_count} success(es), {self.missing_count} missing(s)"
        )

    @staticmethod
    def colored_result_message(
        summary, verbose, case_name, filename, width, datetime, now
    ):
        """Return a colored message summarizing the result of the analysis.

        Args:
            summary: the summary analysis containing the result of the analysis
            verbose: boolean indicating if the message should contain details
            case_name: the name of the case being analyzed
            filename: the name of the summary file being analyzed
            width: the width to be used for the case name in the message
            datetime: the datetime of the summary file being analyzed
            now: the current datetime

        Returns:
            A colored message summarizing the result of the analysis
        """
        message = ""
        color = "cyan"

        if isinstance(summary, str):
            return f"{case_name:<{width}} |<{color}> {summary}</{color}>"

        since = since_str(datetime, now)
        if "analysis" not in summary:
            color = "yellow"
            message = f"{case_name:<{width}} |<{color}> RUNNING</{color}> ({since})"

        else:
            color = "green"
            if summary["analysis"]["missing_count"] > 0:
                color = "red"
            if summary["analysis"]["error_count"] > 0:
                color = "red"
            result = summary["analysis"]["result"].split("-")
            result[1] = result[1].strip()
            message = (
                f"{case_name:<{width}} | <{color}>{result[0]}</{color}>({since})"
                + f" <white>[{result[1]}]</white>"
            )

        if verbose:
            message += "\n"
            message += f"{'from':>10} | {filename}\n"
            if "analysis" not in summary:
                message += (
                    f"{'Unknown':>10} | <{color}>Test is still running"
                    + f" or has failed. </{color}> \n"
                )
            else:
                for task_name, results in summary["tasks"].items():
                    for test_type, result in results.items():
                        if test_type == "Create":
                            continue
                        try:
                            label = f"{task_name}.{test_type}"
                            result_message = (
                                f"{label:>30} | {result['items'][0]['result']}"
                            )
                            result_message = result_message.replace("\n", "")
                            result_message = result_message.replace(
                                "SUCCESS", "<green>SUCCESS</green>"
                            )
                            result_message = result_message.replace(
                                "FAILURE", "<red>FAILURE</red>"
                            )

                            message = f"{message}{result_message}\n"
                        except KeyError:
                            message += f"{test_type:>10} |\
                                {result['items'][0]['warning']}"

        return message


class CheckSummaryTxt(CheckSummary):
    """Class to generate summary File in txt format."""

    def __init__(self, fileformat, filename):
        """Creation of a CheckSummaryTxt object.

        Args:
           fileformat: the format of the summary file (e.g. "txt", "json", "ciboulai")
           filename: the pattern defining the name of the summary file
        """
        CheckSummary.__init__(self, fileformat, filename)
        self.version = "1.0.0"

    def create(self):
        """Create a summary file with header."""
        with (
            FileLock(self.fullpath, delete_existing=True),
            open(self.fullpath, "w") as summary_file,
        ):
            summary_file.write(f"# ReferenceChecker Summary File {self.version}\n")
            git_info = get_git_info()
            summary_file.write("# Git:\n")
            for label in git_info:
                summary_file.write(f"#   {label}:{git_info[label]}\n")
            summary_file.write("\n")
            summary_file.write("-\n")
            summary_file.write("Task: Preparation\n")
            summary_file.write("Rule: Create\n")
            summary_file.write(f"Description: Creation of {self.fullpath}\n")
            summary_file.write("\n")

    def append(self, check_definitions: list[CheckDefinition]):
        """Write check_definitions to summary file to disk in the txt format.

        Args:
            check_definitions: the input list of check definitions

        Raises:
            FileNotFoundError: First call CheckSummaryTxt.create to generate
        """
        if not os.path.exists(self.fullpath):
            raise FileNotFoundError(
                f"First call CheckSummaryTxt.create to generate {self.fullpath}"
            )

        with FileLock(self.fullpath), open(self.fullpath, "a") as summary_file:
            for check_definition in check_definitions:
                for item in check_definition.items:
                    CheckSummaryTxt._to_txt(summary_file, check_definition, item)
                if len(check_definition.items) == 0:
                    CheckSummaryTxt._to_txt(summary_file, check_definition)

        logger.info(f"Appended results to reference checking summary: {self.fullpath}")

    @staticmethod
    def _to_txt(summary_file, check_definition: CheckDefinition, item: CheckItem = None):
        """Append the content of the SummaryItem to a file in txt format.

        Args:
            summary_file: the txt file (needs to be already opened)
            check_definition: the check definition being processed
            item: the check item being processed
        """
        summary_file.write("-\n")
        summary_file.write(f"Name: {check_definition.uniquename}\n")
        summary_file.write(f"Task: {check_definition.taskname}\n")
        summary_file.write(f"Rule: {check_definition.rulename}\n")
        summary_file.write(f"Method: {check_definition.method}\n")
        summary_file.write(f"Tool: {check_definition.tool}\n")
        if item:
            summary_file.write(f"Test File: {item.test_file}\n")
            summary_file.write(f"Reference File: {item.reference_file}\n")
            summary_file.write(f"Generated Reference File: {item.generate_file}\n")
            summary_file.write(f"Result: {item.result}\n")
            summary_file.write(f"Detailed Result: {item.result_file}\n")
        else:
            summary_file.write(
                "Warning: No file found using "
                + f"{check_definition.files_pattern} at {check_definition.inpath}\n"
            )
        summary_file.write("\n")

    def compute_and_append_analysis(self, check):
        """Perform an analysis of the txt summary and append it at the end."""
        analysis = CheckSummaryAnalysis(check)

        with FileLock(self.fullpath):
            with open(self.fullpath, "r") as file:
                for line in file.readlines():
                    clean_line = line.replace("\n", "")
                    if clean_line.startswith("Result:"):
                        result = clean_line.split(":")[1].strip()
                        if check:
                            if "SUCCESS" not in result:
                                analysis.increment_error_count()
                            else:
                                analysis.increment_success_count()
                    if clean_line.startswith("Warning: No file found using"):
                        analysis.increment_missing_count()
                    if clean_line.startswith("Generated Reference File:"):
                        generated = line.split(":")[1].strip()
                        if generated != "N/A":
                            analysis.increment_generated_count()
                    if clean_line.startswith("Task: ReferenceChecker"):
                        analysis = CheckSummaryAnalysis(check)
                        analysis.append_error_message(
                            f"Summary analysis already present in {self.fullpath}"
                        )
                        return analysis

            with open(self.fullpath, mode="a", encoding="utf8") as outfile:
                outfile.write("\n")
                outfile.write("-\n")
                outfile.write("Task: ReferenceChecker\n")
                outfile.write("Rule: Create Summary\n")
                outfile.write(f"# Generated files: {analysis.generated_count}\n")
                outfile.write(f"# Successful tests: {analysis.success_count}\n")
                outfile.write(f"# Failure tests: {analysis.error_count}\n")
                outfile.write(f"# Missing files: {analysis.missing_count}\n")
                outfile.write(f"# Total files: {analysis.total_count()}\n")
                outfile.write(f"# Success: {analysis.success()}\n")
                outfile.write(f"# Result: {analysis.message()}\n")
                outfile.write("\n")

        return analysis

    def contains_summary_analysis(self):
        """Return True if the summary file contains an analysis."""
        with FileLock(self.fullpath), open(self.fullpath, "r") as file:
            for line in file.readlines():
                clean_line = line.replace("\n", "")
                if clean_line.startswith("Task: ReferenceChecker"):
                    return True
        return False


class CheckSummaryJson(CheckSummary):
    """Class to generate summary File in txt format."""

    def __init__(self, fileformat, filename):
        """Creation of a CheckSummaryJson.

        Args:
           fileformat: the format of the summary file (e.g. "txt", "json", "ciboulai")
           filename: the pattern defining the name of the summary file
        """
        CheckSummary.__init__(self, fileformat, filename)
        self.version = "1.0.0"

    def create(self):
        """Create a summary file with header."""
        complete_dict = {}
        complete_dict["header"] = self._header_to_dict()
        complete_dict["tasks"] = {}
        complete_dict["tasks"]["Prep"] = {}
        complete_dict["tasks"]["Prep"]["Create"] = {}
        complete_dict["tasks"]["Prep"]["Create"]["description"] = (
            f"Creation of {self.fullpath}"
        )
        with (
            FileLock(self.fullpath, delete_existing=True),
            open(self.fullpath, mode="w", encoding="utf8") as outfile,
        ):
            json.dump(complete_dict, outfile, indent=True)
            outfile.write("\n")

    def append(self, check_definitions: list[CheckDefinition]):
        """Write the summary file to disk in the json format.

        Args:
            check_definitions: the input list of check definitions

        Raises:
            FileNotFoundError: First call CheckSummaryJson.create to generate
        """
        if not os.path.exists(self.fullpath):
            raise FileNotFoundError(
                f"First call CheckSummaryJson.create to generate {self.fullpath}"
            )

        complete_dict = {}

        if check_definitions:
            results_dict = {}
            for check_definition in check_definitions:
                for item in check_definition.items:
                    CheckSummaryJson._to_dict(results_dict, check_definition, item)
                if len(check_definition.items) == 0:
                    CheckSummaryJson._to_dict(results_dict, check_definition)
            complete_dict["tasks"] = results_dict

        lines = json.dumps(complete_dict, indent=True)
        with FileLock(self.fullpath):
            if os.path.exists(self.fullpath):
                # Avoid to re-read the full summary.
                # We make the assumption that the json file ends with a list of tasks
                # and remove the closing braces to append new tasks
                with open(self.fullpath, "rb+") as f:
                    f.seek(-6, os.SEEK_END)
                    f.truncate()
                # To merge correctly into existing "tasks" section, remove
                # the first line and add a comma
                header_length = len("""{\n "tasks": { """)
                lines = f",\n{lines[header_length:]}"
            with open(self.fullpath, "a") as outfile:
                outfile.write(lines)
                outfile.write("\n")
        logger.info(f"Appended results to reference checking summary: {self.fullpath}")

    def _header_to_dict(self):
        header_dict = {}
        header_dict["header"] = {}
        header_dict["header"]["version"] = self.version
        header_dict["header"]["git_info"] = get_git_info()

        return header_dict

    def compute_and_append_analysis(self, check):
        """Perform an analysis of the json summary and append it at the end."""
        analysis = CheckSummaryAnalysis(check)
        data = {}
        with FileLock(self.fullpath):
            with open(self.fullpath, "r") as file:
                data = json.load(file)
            if "analysis" in data:
                analysis.append_error_message(
                    f"Summary analysis already present in {self.fullpath}."
                )
                return analysis
            for task in data["tasks"]:
                for rule in data["tasks"][task]:
                    if "items" in data["tasks"][task][rule]:
                        for item in data["tasks"][task][rule]["items"]:
                            if "result" in item:
                                result = item["result"]
                                if check:
                                    if "SUCCESS" not in result:
                                        analysis.increment_error_count()
                                    else:
                                        analysis.increment_success_count()
                            elif "warning" in item:
                                result = item["warning"]
                                if "No file found" in result:
                                    analysis.increment_missing_count()
                            if "generate_file" in item:
                                generated = item["generate_file"]
                                if generated != "N/A":
                                    analysis.increment_generated_count()

            analysis_dict = {}
            analysis_dict["generated_count"] = analysis.generated_count
            analysis_dict["success_count"] = analysis.success_count
            analysis_dict["error_count"] = analysis.error_count
            analysis_dict["missing_count"] = analysis.missing_count
            analysis_dict["total_count"] = analysis.total_count()
            analysis_dict["success"] = analysis.success()
            analysis_dict["result"] = analysis.message()

            # Since we had to parse the json to perform the analysis,
            # We just append the analysis to data, and rewrite the complete summary

            data["analysis"] = analysis_dict
            with open(self.fullpath, mode="w", encoding="utf8") as outfile:
                json.dump(data, outfile, indent=True)
                outfile.write("\n")
        return analysis

    def contains_summary_analysis(self):
        """Return True if the summary file contains an analysis."""
        with FileLock(self.fullpath), open(self.fullpath, "r") as file:
            data = json.load(file)
            if "analysis" in data:
                return True
        return False

    @staticmethod
    def _to_dict(summary_dict, check_definition: CheckDefinition, item: CheckItem = None):
        """Append the SummaryItem to an existing dictionary.

        Args:
            summary_dict: the JSON file (needs to be already opened)
            check_definition: the check definition being processed
            item: the check item being processed
        """
        if check_definition.uniquename not in summary_dict:
            summary_dict[check_definition.uniquename] = {}

        if check_definition.rulename not in summary_dict[check_definition.uniquename]:
            summary_dict[check_definition.uniquename][check_definition.rulename] = {}

            summary_dict[check_definition.uniquename][check_definition.rulename][
                "rule"
            ] = check_definition.rulename
            summary_dict[check_definition.uniquename][check_definition.rulename][
                "method"
            ] = check_definition.method

            summary_dict[check_definition.uniquename][check_definition.rulename][
                "task"
            ] = check_definition.taskname

            summary_dict[check_definition.uniquename][check_definition.rulename][
                "tool"
            ] = check_definition.tool
            summary_dict[check_definition.uniquename][check_definition.rulename][
                "items"
            ] = []

        if item:
            content = {
                "test_file": item.test_file,
                "reference_file": item.reference_file,
                "generate_file": item.generate_file,
                "result": item.result,
                "result_file": item.result_file,
            }
        else:
            content = {
                "warning": f"No file found using {check_definition.files_pattern}"
                + f" at {check_definition.inpath}\n"
            }

        summary_dict[check_definition.uniquename][check_definition.rulename][
            "items"
        ].append(content)


class PublishToCiboulai:
    """Class to publish summary stack files to a Ciboulai server, like Davai."""

    def __init__(self, ciboulai_config, platform, stack_dir):
        """Creation of a PublishToCiboulai.

        Args:
           ciboulai_config: the [reference_checker.ciboulai] configuration
           platform: Platform, used for path substitution and machine name
           stack_dir: the summary stack directory to publish
        """
        ciboulai_default_server = "https://www.umr-cnrm.fr/davai/"
        self.enabled = bool(ciboulai_config.get("enabled", True))
        server = str(
            self._expand(platform, ciboulai_config.get("server"), ciboulai_default_server)
        )
        if not server.endswith("/api/"):
            server = "/".join([server.rstrip("/"), "api", ""])
        self.api_url = server
        self.token_file = self._expand(platform, ciboulai_config.get("token_file"))
        self.name_file = self._expand(
            platform,
            ciboulai_config.get("name_file"),
            os.path.join(os.path.dirname(stack_dir), "ciboulai.cfg"),
        )
        self.counter_file = self._expand(
            platform,
            ciboulai_config.get("counter_file"),
            os.path.join(os.path.dirname(self.name_file), "ciboulai_counter"),
        )

        default_xp_name = "ttr-{counter}-{machine}@{user}"
        self.name_pattern = str(ciboulai_config.get("experiment_name", default_xp_name))
        self.tries = int(ciboulai_config.get("tries", 3))
        self.sleep = int(ciboulai_config.get("sleep", 3))
        self.timeout = int(ciboulai_config.get("timeout", 60))
        self.stack_dir = stack_dir
        self.machine = platform.get_platform_value("host") or socket.gethostname()
        self.user = os.environ.get("USER", "unknown")
        self.unix_group = platform.get_platform_value("unix_group")
        self.xpid = None
        self._token = None
        self._skipped = False

    @staticmethod
    def _expand(platform, value, default=None):
        """Substitute the macros of a configured path, or return the default.

        Args:
            platform: Platform, providing the substitution service
            value: the configured path pattern, possibly None or empty
            default: fallback path when no value is configured

        Returns:
            the expanded absolute path, or default
        """
        if not value:
            return default
        return os.path.expanduser(platform.substitute(value))

    def _makedirs(self, directory):
        """Create a directory of the publication environment if missing."""
        if directory and not os.path.exists(directory):
            tactusmakedirs(directory, unixgroup=self.unix_group)

    def _get_token(self):
        """Read the Ciboulai token from the token file.

        Returns:
            the token string, or None when it cannot be found
        """
        if self._token is None:
            token = None
            if self.token_file and os.path.exists(self.token_file):
                with open(self.token_file, "r", encoding="utf8") as file:
                    token = file.readline().strip()
            self._token = token or None
        return self._token

    def _read_name_file(self):
        """Return the experiment name stored in the name file, or None."""
        if os.path.exists(self.name_file):
            with open(self.name_file, "r", encoding="utf8") as file:
                name = file.read().strip()
            if name:
                return name
        return None

    def _write_name_file(self):
        """Store the resolved experiment name in the name file."""
        self._makedirs(os.path.dirname(self.name_file))
        with (
            FileLock(self.name_file, delete_existing=True),
            open(self.name_file, "w", encoding="utf8") as file,
        ):
            file.write(f"{self.xpid}\n")

    def _next_counter(self):
        """Return the next experiment counter.

        Returns:
            the incremented counter value
        """
        self._makedirs(os.path.dirname(self.counter_file))
        counter = 0
        with FileLock(self.counter_file, delete_existing=True):
            if os.path.exists(self.counter_file):
                with open(self.counter_file, "r", encoding="utf8") as file:
                    value = file.read().strip()
                counter = int(value) if value else 0
            counter += 1
            with open(self.counter_file, "w", encoding="utf8") as file:
                file.write(f"{counter}\n")
        return counter

    def _build_name(self, counter):
        """Build the experiment name from the configured pattern.

        Args:
            counter: the experiment counter, padded to 4 digits

        Returns:
            the experiment name, e.g. ttr-0112-atos_bologna@cvap
        """
        return self.name_pattern.format(
            counter=f"{counter:04d}", machine=self.machine, user=self.user
        )

    def _xpinfo_payload(self):
        """Build the xpinfo payload of the experiment (re)initialisation.

        Returns:
            the JSON string to post with type 'xpinfo'
        """
        path = os.path.join(self.stack_dir, "xpinfo.json")
        if os.path.exists(path):
            with open(path, "r", encoding="utf8") as file:
                xpinfo = json.load(file)
        else:
            xpinfo = {}
        xpinfo["xpid"] = self.xpid
        xpinfo["user"] = self.user
        return json.dumps({"xpinfo": xpinfo}, indent=4, sort_keys=True)

    def _http_post_form(self, url, form):
        """POST a form-encoded dictionary, return (http_status, body_text).

        Args:
            url: the destination url
            form: dictionary of form fields

        Returns:
            the HTTP status code and the body text
        """
        data = urllib.parse.urlencode(form).encode("utf-8")
        request = urllib.request.Request(  # noqa: S310
            url=url,
            data=data,
            headers={"Content-Type": "application/x-www-form-urlencoded"},
        )
        try:
            # S310: the url is built from the configured ciboulai server
            with urllib.request.urlopen(  # noqa: S310
                request, timeout=self.timeout
            ) as response:
                return response.getcode(), response.read().decode(
                    "utf-8", errors="replace"
                )
        except urllib.error.HTTPError as error:
            return error.code, error.read().decode("utf-8", errors="replace")

    def _post_api(self, jsondata, kind, token):
        """Send data to the Ciboulai api, with retries on network failures.

        Args:
            jsondata: the payload, formatted as output from json.dumps
            kind: the type of data, 'xpinfo' or 'taskinfo'
            token: the Ciboulai token

        Returns:
            (ok, info): ok is True when the server accepted the payload
            (returnCode == 0), info the parsed server answer or the error
        """
        form = {
            "jsonData": jsondata,
            "xpid": self.xpid,
            "type": kind,
            "token": token,
        }
        info = None
        for attempt in range(1, self.tries + 1):
            try:
                status, body = self._http_post_form(self.api_url, form)
            except OSError as error:
                info = {"error": f"connection to {self.api_url} failed: {error}"}
                if attempt < self.tries:
                    logger.warning(f"Ciboulai publication: {info['error']}, retrying...")
                    time.sleep(self.sleep)
                    continue
                return False, info
            try:
                answer = json.loads(body)
            except ValueError:
                return False, {"http_status": status, "body": body[:500]}
            if "returnCode" not in answer:
                return False, {"http_status": status, "body": body[:500]}
            ok = answer["returnCode"] == 0
            info = {
                "http_status": status,
                "returnCode": answer["returnCode"],
                "message": " ".join(str(answer.get("message", "")).split()),
            }
            if not ok and "Token is not present or wrong" in info["message"]:
                # No point in hammering the server: the token is invalid.
                self._skipped = True
                return ok, info
            if ok or attempt == self.tries:
                return ok, info
            time.sleep(self.sleep)
        return False, info

    def ensure_experiment(self, reset):
        """Make sure the experiment exists on the server for publishing.

        The experiment name is resolved from the name file, or built from a
        new counter value and stored. The experiment is created or cleared
        by posting its xpinfo, always on first resolution and, when reset
        is requested, at every stack (re)creation.

        Args:
            reset: also post the xpinfo when reusing an existing name

        Returns:
            True when the experiment name is available for publishing
        """
        if not self.enabled or self._skipped:
            return False
        token = self._get_token()
        if token is None:
            logger.warning("No ciboulai token found, skipping publication to the server")
            self._skipped = True
            return False
        is_new = self.xpid is None
        if is_new:
            self.xpid = self._read_name_file()
            is_new = self.xpid is None
        if is_new:
            self.xpid = self._build_name(self._next_counter())
            self._write_name_file()
            logger.info(f"Created the Ciboulai experiment: {self.xpid}")
        if reset or is_new:
            ok, info = self._post_api(self._xpinfo_payload(), "xpinfo", token)
            if ok:
                logger.info(f"Initialised the Ciboulai experiment: {info['message']}")
            else:
                logger.warning(
                    f"Could not initialise the Ciboulai experiment {self.xpid}: {info}"
                )
        return True

    def publish(self, task, scope, summary):
        """Send one summary dictionary to the server.

        Args:
            task: the task name, first level key of the payload
            scope: the scope of the summary, 'itself' or 'consistency'
            summary: the dictionary written to <task>.<scope>.json

        Returns:
            True when the server accepted the payload
        """
        if not self.enabled or self._skipped:
            return False
        if self.xpid is None and not self.ensure_experiment(reset=False):
            return False
        jsondata = json.dumps({task: {scope: summary}})
        ok, info = self._post_api(jsondata, "taskinfo", self._token)
        if ok:
            logger.info(f"Published to Ciboulai: {self.xpid} {task}.{scope}")
        else:
            logger.warning(f"Could not publish {task}.{scope} to Ciboulai: {info}")
        return ok


class CheckSummaryCiboulai(CheckSummary):
    """Class to generate summary files in the Ciboulai summary stack format.

    Instead of one aggregate file, this writer produces one pair of files per
    checked task. When a [reference_checker.ciboulai] section is configured and enabled,
    the files of the stack are also published to the Ciboulai server, see
    PublishToCiboulai.
    """

    STATUS_ENDED = {
        "symbol": "E",
        "short": "Ended",
        "text": "Ended: Task ended without crash.",
    }
    STATUS_NO_REF = {
        "symbol": "0",
        "short": "- No ref -",
        "text": "No reference to be compared to",
    }
    STATUS_OK = {
        "symbol": "OK",
        "short": "OK",
        "text": 'Success: "all comparisons against the reference were successful"',
    }
    STATUS_INCONSISTENT = {
        "symbol": "IC",
        "short": "Inconsistent",
        "text": "Inconsistency between this task and its reference"
        " in the same experiment",
    }

    def __init__(self, fileformat, filename, ciboulai_config=None):
        """Creation of a CheckSummaryCiboulai.

        Args:
           fileformat: the format of the summary file (e.g. "txt", "json", "ciboulai")
           filename: the pattern defining the summary stack directory
           ciboulai_config: the [reference_checker.ciboulai] configuration,
               when present and enabled the files are also published to the
               Ciboulai server
        """
        CheckSummary.__init__(self, fileformat, filename)
        self.rundir = None
        self.unix_group = None
        self.ciboulai_config = ciboulai_config
        self.publisher = None

    def init_full_path(self, platform):
        """Init self.fullpath and the run context using platform substitute.

        Args:
           platform: Platform
        """
        if not self.fullpath:
            self.fullpath = platform.substitute(self.filename)
            self.rundir = platform.get_system_value("wrk") or os.getcwd()
            self.unix_group = platform.get_platform_value("unix_group")
            if self.ciboulai_config is not None and bool(
                self.ciboulai_config.get("enabled", True)
            ):
                self.publisher = PublishToCiboulai(
                    self.ciboulai_config, platform, self.fullpath
                )

    def create(self):
        """Create the summary stack directory, and initialise the experiment."""
        self._ensure_dir()
        if self.publisher is not None:
            self.publisher.ensure_experiment(reset=True)

    def delete(self):
        """Delete the summary stack directory and all the files it contains."""
        if os.path.isdir(self.fullpath):
            logger.info("Delete reference checker summary:", self.fullpath)
            shutil.rmtree(self.fullpath)

    def append(self, check_definitions: list[CheckDefinition]):
        """Write one pair of summary files per task contained in the definitions.

        Args:
            check_definitions: the input list of check definitions
        """
        self._ensure_dir()

        tasks: dict[str, list[CheckDefinition]] = {}
        for check_definition in check_definitions:
            stem = self._stem(check_definition)
            tasks.setdefault(stem, []).append(check_definition)

        for stem, definitions in tasks.items():
            itself = self._itself_to_dict(definitions)
            consistency = self._consistency_to_dict(definitions)
            self._write(f"{stem}.itself.json", itself)
            self._write(f"{stem}.consistency.json", consistency)
            if self.publisher is not None:
                # the server merges scopes unless 'itself' replaces the task,
                # so itself must be published before consistency
                self.publisher.ensure_experiment(reset=False)
                self.publisher.publish(stem, "itself", itself)
                self.publisher.publish(stem, "consistency", consistency)

        logger.info(
            f"Appended Ciboulai results to reference checking summary: {self.fullpath}"
        )

    def compute_and_append_analysis(self, check):
        """Analyze the consistency files of the stack directory.

        Args:
            check: boolean indicating if the check was performed

        Returns:
            A CheckSummaryAnalysis built from the consistency files
        """
        analysis = CheckSummaryAnalysis(check)
        if not os.path.isdir(self.fullpath):
            analysis.append_error_message(f"No Ciboulai summary found in {self.fullpath}")
            return analysis

        for filename in sorted(os.listdir(self.fullpath)):
            if not filename.endswith(".consistency.json"):
                continue
            with open(
                os.path.join(self.fullpath, filename), "r", encoding="utf8"
            ) as file:
                status = json.load(file).get("comparisonStatus", {})
            symbol = status.get("symbol")
            text = status.get("text", "")
            if check and symbol == "OK":
                analysis.increment_success_count()
            elif check and symbol != "0":
                analysis.increment_error_count()
            if "No file found" in text:
                analysis.increment_missing_count()

        return analysis

    def contains_summary_analysis(self):
        """Return False: Ciboulai files never contain an aggregate analysis."""
        return False

    def _ensure_dir(self):
        """Create the summary stack directory if it does not exist yet."""
        if not os.path.exists(self.fullpath):
            tactusmakedirs(self.fullpath, unixgroup=self.unix_group)

    def _write(self, filename, content):
        """Write a JSON file of the stack directory atomically.

        Args:
            filename: name of the file to write, relative to the stack directory
            content: dictionary to serialize
        """
        filepath = os.path.join(self.fullpath, filename)
        with (
            FileLock(filepath, delete_existing=True),
            open(filepath, mode="w", encoding="utf8") as outfile,
        ):
            json.dump(content, outfile, indent=2, sort_keys=True)
            outfile.write("\n")

    @staticmethod
    def _stem(check_definition: CheckDefinition) -> str:
        """Return the file stem of the task owning the check definition.

        Args:
            check_definition: the check definition being processed

        Returns:
            the task stem, suitable for a file name
        """
        return check_definition.uniquename.replace("/", ".").rstrip(".")

    def _itself_to_dict(self, definitions: list[CheckDefinition]) -> dict:
        """Build the content of the <stem>.itself.json file.

        Args:
            definitions: the check definitions of the task

        Returns:
            the dictionary describing the run status of the task
        """
        content = {
            "Context": {
                "jobname": definitions[0].taskname,
                "rundir": self.rundir,
            },
            "Status": dict(self.STATUS_ENDED),
            "Updated": datetime.now().isoformat(timespec="seconds"),
        }
        listings = sorted({
            item.result_file
            for definition in definitions
            for item in definition.items
            if os.path.exists(item.result_file)
        })
        if listings:
            content["Listing"] = {"Task listing uri(s)": listings}
        return content

    @staticmethod
    def _consistency_to_dict(definitions: list[CheckDefinition]) -> dict:
        """Build the content of the <stem>.consistency.json file.

        Args:
            definitions: the check definitions of the task

        Returns:
            the dictionary containing the comparison verdict
        """
        results = [item.result for definition in definitions for item in definition.items]
        warnings = [
            f"No file found using {definition.files_pattern} at {definition.inpath}"
            for definition in definitions
            if len(definition.items) == 0
        ]

        if len(results) == 0:
            status = dict(CheckSummaryCiboulai.STATUS_NO_REF)
            if warnings:
                status["text"] = "; ".join(warnings)
        elif all(result == "N/A" for result in results):
            status = dict(CheckSummaryCiboulai.STATUS_NO_REF)
        elif all("SUCCESS" in result for result in results):
            status = dict(CheckSummaryCiboulai.STATUS_OK)
        else:
            status = dict(CheckSummaryCiboulai.STATUS_INCONSISTENT)

        if warnings and status["symbol"] != "0":
            status["text"] = f"{status['text']} ({'; '.join(warnings)})"

        return {"comparisonStatus": status}


class ReferenceCheckManager:
    """A class managing the checks against reference."""

    def __init__(
        self,
        config,
        taskname,
        label_suffix,
        rules_active,
        check,
        generate,
        create_summary,
        analyze_summary,
        suppress_exception,
    ):
        """Creation of the ReferenceCheckManager.

        Args:
            config: configuration dictionary
            taskname: the name of the task
            label_suffix: the suffix for the label
            rules_active: list of rules that are active
            check: boolean indicating if the check should be performed
            generate: boolean indicating if the reference generation should be performed
            create_summary: boolean indicating if the summary file should be created
            analyze_summary: boolean indicating if the summary file should be analyzed
            suppress_exception: boolean indicating to suppress exception when
                                checking references or analyzing summaries
        """
        self.taskname = taskname
        self.label_suffix = label_suffix
        self.check = check
        self.generate = generate
        self.rules_active = rules_active
        self.create_summary = create_summary
        self.analyze_summary = analyze_summary
        self.suppress_exception = suppress_exception

        self.check_definitions = CheckDefinition.create_list_of_check_definitions(
            config,
            self.taskname,
            self.label_suffix,
            self.rules_active,
            self.check,
            self.generate,
        )
        self.summary_list = CheckSummary.create_summary_list(config)
        self.reference_checkers = {}
        if check:
            for check_definition in self.check_definitions:
                if check_definition.method not in self.reference_checkers:
                    reference_checker = ReferenceChecker.create_reference_checker(
                        check_definition.method, config
                    )
                    if reference_checker:
                        check_definition.tool = reference_checker.tool
                    else:
                        check_definition.tool = "N/A"
                    self.reference_checkers[check_definition.method] = reference_checker

    @staticmethod
    def create_reference_check_manager(config, taskname) -> "ReferenceCheckManager":
        """Static method to create a ReferenceCheckManager.

        Args:
            config: configuration dictionary
            taskname: the name of the task
        Returns:
            A ReferenceCheckManager object or None
        """
        config_rc = config["reference_checker"]
        check = config_rc["check"]
        generate = config_rc["generate"]
        rules_excluded = config_rc.get("rules_excluded", [])
        rules_active = list(set(config_rc["rules_active"]) - set(rules_excluded))
        task_rules_active = []
        for rules in rules_active:
            rule_array = rules.split(".")
            active_task_name = rule_array[0]
            rule_name = rule_array[1]
            if taskname == active_task_name:
                task_rules_active.append(rule_name)
        summary_creation_tasks = config_rc["create_summary"]
        summary_analysis_tasks = config_rc["analyze_summary"]

        create_summary = taskname in summary_creation_tasks
        analyze_summary = taskname in summary_analysis_tasks
        task_is_active = len(task_rules_active) > 0
        suppress_exception = config_rc["suppress_exception"]
        label_suffix = config_rc["label_suffix"]

        logger.debug(
            f"ReferenceChecker configuration for task {taskname}:\n\
                       label_suffix={label_suffix}\n\
                       task_is_active={task_is_active}\
                       task_rules_active={task_rules_active}\n\
                       check={check}\n\
                       generate={generate}\n\
                       create_summary={create_summary}\n\
                       analyze_summary={analyze_summary}\n\
                       suppress_exception={suppress_exception}"
        )

        if (check or generate) and (task_is_active or create_summary or analyze_summary):
            return ReferenceCheckManager(
                config_rc,
                taskname,
                label_suffix,
                task_rules_active,
                check,
                generate,
                create_summary,
                analyze_summary,
                suppress_exception,
            )

        return None

    def prepare(self, platform: Platform):
        """Prepare for comparing against references.

        Args:
            platform: platform class providing the substitution capabilities.
        """
        # Retrieve the folders from the configuration
        self.references_folder = platform.get_platform_value("references_folder")

        self.references_generation_folder = platform.get_platform_value(
            "references_generation_folder"
        )

        # For each check definition, prepare the file_paths
        for check_definition in self.check_definitions:
            check_definition.create_items(platform)

        # For each summary, transform the pattern defininig the summary into a filepath
        for summary in self.summary_list:
            summary.init_full_path(platform)

        unix_group = platform.get_platform_value("unix_group")

        # create the missing output directories
        workdirs = [os.path.dirname(summary.fullpath) for summary in self.summary_list]

        for check_definition in self.check_definitions:
            workdirs.extend([
                os.path.dirname(item.result_file) for item in check_definition.items
            ])

        for workdir in workdirs:
            if not os.path.exists(workdir):
                tactusmakedirs(workdir, unixgroup=unix_group)
        for reference_checker in self.reference_checkers.values():
            reference_checker.prepare(platform)

    def create_summaries_with_header_if_empty(self):
        """Create summary_list file on disk with the correct output format."""
        for summary in self.summary_list:
            if not os.path.exists(summary.fullpath):
                summary.create()

    def analyze_summaries(self):
        """Analyze the summaries."""
        failed_messages = ""
        for summary in self.summary_list:
            logger.info(f"ReferenceChecker summary: {summary.fullpath}")
            analysis = summary.compute_and_append_analysis(self.check)
            message = analysis.message()
            if not analysis.success():
                failed_messages = f"{failed_messages}{summary.fullpath}\n{message}\n"
                logger.error(f"{message}")
            else:
                logger.info(f"{message}")

        if len(failed_messages) > 0:
            message = "Reference check failed for some summaries:\n" + failed_messages

            if self.suppress_exception:
                logger.warning(message)
                logger.warning("suppress_exception = true, execution continue")
            else:
                logger.error(message)
                raise RuntimeError(failed_messages)

    def append_results_to_summaries(self):
        """Write the summary_list to disk in the correct output format."""
        if len(self.check_definitions) > 0:
            for summary in self.summary_list:
                summary.append(self.check_definitions)

    def check_references(self):
        """Check the result files against the reference files."""
        for check_definition in self.check_definitions:
            reference_checker = self.reference_checkers[check_definition.method]
            # Loop over all the tests, compare files and store the results
            for item in check_definition.items:
                item.result = ""
                if not reference_checker:
                    item.result = "ERROR: comparison {method} is not available\n"
                else:
                    item.result = reference_checker.compare(
                        item.test_file, item.reference_file, item.result_file
                    )

    def generate_references(self, fmanager: FileManager):
        """Generate the reference files into the reference folder.

        Already existing files will be overwitten
        Args:
           fmanager: a file manager
        """
        for check_definition in self.check_definitions:
            for item in check_definition.items:
                logger.info("Generate reference file:", item.generate_file)
                if os.path.exists(item.generate_file):
                    logger.warning(f"{item.generate_file} already exists, overwriting")
                    os.remove(item.generate_file)

                fmanager.output(item.test_file, item.generate_file, provider_id="copy")

    def execute(self, fmanager: FileManager):
        """Do reference against reference checking.

        Args:
           fmanager: a file manager
        """
        self.prepare(fmanager.platform)
        force_deletion = self.create_summary

        for summary in self.summary_list:
            if os.path.exists(summary.fullpath):
                delete = force_deletion
                if not delete:
                    has_summary = summary.contains_summary_analysis()
                    if has_summary:
                        delete = not self.analyze_summary
                if delete:
                    summary.delete()

        self.create_summaries_with_header_if_empty()

        if self.generate:
            self.generate_references(fmanager)

        if self.check:
            self.check_references()

        self.append_results_to_summaries()

        if self.analyze_summary:
            self.analyze_summaries()
