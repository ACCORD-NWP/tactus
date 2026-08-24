#!/usr/bin/env python3
"""Test runner functionality for running integration test cases."""

import concurrent.futures
import contextlib
import copy
import glob
import json
import os
import shutil
import tempfile
from datetime import datetime
from pathlib import Path

import tomli

from . import GeneralConstants
from .config_parser import BasicConfig, ConfigPaths, ParsedConfig
from .datetime_utils import evaluate_date
from .experiment import get_git_info
from .fullpos import flatten_list
from .general_utils import merge_dicts
from .host_actions import TactusHost
from .logs import logger
from .reference_checker import CheckSummaryAnalysis
from .toolbox import Platform


class TestCases:
    """Class to orchestrate the tests."""

    def __init__(self, args):
        """Construct the object.

        Args:
            args: Command line arguments

        """
        ConfigPaths.CONFIG_DATA_SEARCHPATHS.insert(
            0, os.path.join(os.getcwd(), "config_files")
        )

        self.tactus_host = TactusHost().detect_tactus_host()

        definitions = {"general": {}, "modifs": {}}
        if args.config_file is not None:
            logger.info("Using config file: {}", args.config_file)
            self.config = ParsedConfig.from_file(args.config_file, json_schema={})
            self.config_name = Path(args.config_file).resolve().stem
            try:
                definitions = self.config.expand_macros().dict()
            except KeyError:
                definitions = self.config.dict()

        self.verbose = args.verbose
        self.cases = definitions.get("cases", {})
        self.reference_date = evaluate_date(
            f"{definitions['general'].get('reference_date', '-P1D')}"
        )
        self.max_workers = definitions["general"].get("max_workers", None)
        self.cmds = {}
        self.mode = definitions["general"].get("mode", "suite")
        self.extra = definitions["general"].get("extra", [])
        self.get_tag(definitions)
        self.dry = args.dry if args.dry else definitions["general"].get("dry", False)
        self.modifs = definitions["modifs"]
        self.refchecks = definitions.get("refchecks", {})
        self.genchecks = definitions.get("genchecks", {})
        self.test_dir = definitions.get("test_dir", f"{self.tag}_configs")
        self.ial = definitions.get("ial", {})
        self.gl = definitions.get("gl", {})
        self.selection = self.resolve_selection(definitions)
        self.assigned = {}
        self.generate_refs = args.generate_refs if args.generate_refs else False
        if self.generate_refs:
            logger.warning("**************************************************")
            logger.warning("*   Reference checker: generate reference mode   *")
            logger.warning("**************************************************")
        if args.config_file is not None:
            with contextlib.suppress(KeyError):
                if definitions["ial"].get("active", False):
                    self.update_binary_paths()
        logger.info(" tag: {}", self.tag)
        logger.info(" test_dir: {}", self.test_dir)

    def get_tag(self, definitions):
        """Get and validate tag.

        Arguments:
            definitions (dict): Configuration

        Raises:
            ValueError: If tag has leading digits

        """
        if "tag" not in definitions["general"]:
            definitions["general"]["tag"] = self.get_tactus_version()
            logger.info("tag not given but derived from git information")
        self.tag = definitions["general"].get("tag")

        if self.tag[0].isdigit():
            raise ValueError(f"The tag cannot start with an integer. tag={self.tag}")

    def resolve_selection(self, definitions):
        """Resolve the selections.

        Arguments:
            definitions (dict): Configuration

        Returns:
            selection (list): List of selected configurations

        """
        selection = definitions["general"].get("selection", [])
        if len(selection) == 0:
            logger.info("Selection is empty, include all cases")
            selection = list(self.cases)

        # Handle subtags and update selection accordingly
        with contextlib.suppress(KeyError):
            subtags = definitions["general"]["compiler"]
            subtag_selection = []
            for tag, value in subtags.items():
                if not value.get("active", False):
                    continue
                for sel in selection:
                    if any(x in sel for x in value.get("exclude", "")):
                        continue
                    subtag = f"{tag}_{sel}"
                    x = copy.deepcopy(self.cases[sel])
                    if "base" not in x:
                        x["base"] = sel
                    if "host" in x:
                        x["host"] = f"{tag}_{x['host']}"
                    x["subtag"] = tag
                    x["extra"] = [] if "extra" not in x else list(x["extra"])
                    for k in value.get("extra", []):
                        x["extra"].append(k)
                    subtag_selection.append(subtag)
                    self.cases[subtag] = x
            if len(subtag_selection) > 0:
                selection = subtag_selection

        return selection

    def list(self):
        """List configurations."""
        logger.info("Available cases:")
        for x in self.cases:
            logger.info("    {}", x)
        logger.info("Selected cases:")
        for x in self.selection:
            logger.info("    {}", x)
            if self.verbose:
                logger.info("      {}", self.cases[x])

    def get_tactus_version(self):
        """Get tactus version info."""
        tactus_git = get_git_info()
        tag = tactus_git["branch"]
        for character in ["/", ".", "-"]:
            tag = tag.replace(character, "_")
        return tag

    def create(self, cases=None):
        """Create the modif files and populate self.cmds for the given cases.

        Arguments:
            cases (list, optional): Cases to process; defaults to self.selection

        """
        os.makedirs(self.test_dir, exist_ok=True)
        if cases is None:
            cases = self.selection

        logger.info("Create config files in {}", self.test_dir)

        for case, item in self.cases.items():
            if case not in self.assigned:
                self.assigned[case] = self.reference_date

            if case not in cases:  # or "config_name" in self.cases[case]:
                continue

            if "host" in item:
                self.assigned[case] = self.assigned[item["host"]]

            subtag = item.get("subtag", "")
            extra = list(self.extra) + list(item.get("extra", []))

            # Merge and replace macros
            modifs = merge_dicts(self.modifs, self.refchecks, True)
            if self.generate_refs:
                modifs = merge_dicts(modifs, self.genchecks, True)
            modifs = merge_dicts(modifs, self.cases[case].get("modifs", {}), True)
            config = self.config.copy(
                update={
                    "modifs": modifs,
                    "modif_macros": {
                        "reference_date": self.assigned[case],
                        "host_case": item.get("hostname", ""),
                        "host_domain": item.get("hostdomain", ""),
                        "tag": self.tag,
                        "subtag": subtag,
                    },
                }
            )
            with contextlib.suppress(KeyError):
                config = config.expand_macros(True)

            # Save the modifications
            outfile = f"{self.test_dir}/modifs_{case}.toml"
            logger.info(" create: {}", outfile)
            BasicConfig(config["modifs"]).save_as(outfile)

            base_file = (
                str(GeneralConstants.PACKAGE_DIRECTORY)
                + "/data/config_files/configurations/"
                + item.get("base", case)
            )
            base_file = f"?{base_file}" if os.path.exists(base_file) else ""

            # Build the command to execute
            cmd = [
                "case",
                base_file,
                extra,
                outfile,
                "-o",
                self.test_dir,
            ]
            self.cmds[case] = flatten_list(cmd)

    def _build_levels(self):
        """Return cases grouped into dependency levels for topological processing.

        Returns:
            levels (list[list]): Cases at each level, roots first.

        Raises:
            ValueError: If a circular host dependency is detected.
        """
        levels = []
        remaining = list(self.selection)
        resolved = set()
        while remaining:
            level = [
                case
                for case in remaining
                if "host" not in self.cases[case] or self.cases[case]["host"] in resolved
            ]
            if not level:
                message = f"Circular dependency detected in host cases: {remaining}"
                raise ValueError(message)
            levels.append(level)
            resolved.update(level)
            remaining = [case for case in remaining if case not in resolved]
        return levels

    def _run_case(self, case):
        """Run the tactus case command for one case and return its output metadata.

        Uses a per-case temporary directory so that concurrent calls never
        race on the output file.

        Arguments:
            case (str): Case name (must already have an entry in self.cmds)

        Returns:
            (config_name, domain_name): stem of the generated config file and
                                        the domain name read from it.
        """
        from .__main__ import main as tactus_main

        cmd = list(self.cmds[case])
        with tempfile.TemporaryDirectory() as tmpdir:
            cmd[-1] = tmpdir
            logger.info("Configure case {} with\n", case)
            logger.info("Use cmd:\n\n{}\n\n", " ".join(cmd))
            tactus_main(cmd)
            (config_file,) = Path(tmpdir).glob("*.toml")
            with open(config_file, "rb") as f:
                defs = tomli.load(f)
            dest = Path(self.test_dir) / config_file.name
            shutil.move(str(config_file), str(dest))
        return dest.stem, defs["domain"]["name"]

    def start(self):
        """Start the run."""
        # Local import to avoid circular dependency (__main__ -> argparse_wrapper -> here)
        from .__main__ import main as tactus_main

        try:
            with open(f"{self.test_dir}/{self.config_name}_config_names.toml", "rb") as f:
                config_names = tomli.load(f)
        except FileNotFoundError as err:
            msg = "No case mapping available. Run again with '-m'"
            logger.error(msg)
            raise FileNotFoundError(msg) from err

        cases = flatten_list(self._build_levels())
        for case in cases:
            config_name = config_names["config_names"][case]
            if self.mode == "task":
                cmds = [
                    [
                        "run",
                        "--config-file",
                        f"{self.test_dir}/{config_name}.toml",
                        "--task",
                        task,
                        "--job",
                        f"{self.test_dir}/{task}.{config_name}.job",
                        "--output",
                        f"{self.test_dir}/{task}.{config_name}.log",
                    ]
                    for task in self.cases[case]["tasks"]
                ]
            else:
                suitefile = f"{self.test_dir}/{config_name}.def"
                cmds = [
                    [
                        "start",
                        "suite",
                        "--config-file",
                        f"{self.test_dir}/{config_name}.toml",
                        "-f",
                        suitefile,
                        "-k",
                    ],
                ]

                # Make sure we remove the case before launching, if requested
                if self.cases[case].get("clean", True):
                    cmds.insert(
                        0,
                        [
                            "remove",
                            "-f",
                            "--execute-removal",
                            f"{self.test_dir}/{config_name}.toml",
                        ],
                    )

            for cmd in cmds:
                cmd_txt = " ".join(cmd)
                logger.info("Use cmd:\n\n{}\n\n", cmd_txt)

                if not self.dry:
                    if self.mode != "task" and os.path.exists(suitefile):
                        os.remove(suitefile)
                    tactus_main(cmd)

    def get_binaries(self):
        """Get the correct binaries."""
        host_settings = {
            "lumi": {"compiler": "gnu", "precision": "R64"},
            "atos_bologna": {"compiler": "intel", "precision": "R64"},
        }

        basedir = os.getcwd()
        ial_hash = self.ial["ial_hash"]
        build_tar_path = self.ial["build_tar_path"]
        try:
            _bindir = self.modifs["submission"]["task_exceptions"]["Forecast"]["bindir"]
        except KeyError:
            _bindir = (
                f"{self.ial['user_binary_path']}/{ial_hash}/@COMPILER@/@PRECISION@/bin"
            )

        files = glob.glob(f"{build_tar_path}/*{ial_hash}*.tar")
        for f in files:
            ff = os.path.basename(f).replace(".tar", "")
            compiler = host_settings[self.tactus_host]["compiler"]
            precision = host_settings[self.tactus_host]["precision"]
            if "-sp-" in ff:
                precision = "R32"
            if "-gnu-" in ff:
                compiler = "gnu"
            cptag = ff.replace(ial_hash, "").replace("ial", "")
            bindir = (
                _bindir
                .replace("@CPTAG@", cptag)
                .replace("@IAL_HASH@", ial_hash)
                .replace("@COMPILER@", compiler)
                .replace("@PRECISION@", precision)
                .replace("/bin", "")
            )
            os.makedirs(bindir, exist_ok=True)
            os.chdir(bindir)
            logger.info("Untar {} into {}", f, bindir)
            if not self.dry:
                os.system(f"tar xf {f}")

        os.chdir(basedir)

        if self.gl:
            gl_hash = self.gl["gl_hash"]
            build_tar_path = self.gl["build_tar_path"]

            try:
                _bindir = self.modifs["submission"]["bindir_gl"]
            except KeyError:
                _bindir = f"{self.gl['user_binary_path']}/{gl_hash}/@COMPILER@/bin"

            files = glob.glob(f"{build_tar_path}/*{gl_hash}*.tar")
            for f in files:
                ff = os.path.basename(f).replace(".tar", "")
                compiler = host_settings[self.tactus_host]["compiler"]
                if "-gnu-" in ff:
                    compiler = "gnu"
                cptag = ff.replace(gl_hash, "").replace("gl", "")
                bindir = (
                    _bindir
                    .replace("@CPTAG@", cptag)
                    .replace("@IAL_HASH@", gl_hash)
                    .replace("@COMPILER@", compiler)
                    .replace("/bin", "")
                )
                os.makedirs(bindir, exist_ok=True)
                os.chdir(bindir)
                logger.info("Untar {} into {}", f, bindir)
                if not self.dry:
                    os.system(f"tar xf {f}")

        logger.info("All binaries copied. Rerun without '-p' to launch tests")

    def update_binary_paths(self):
        """Update the correct binaries in the internal config object."""
        ial_hash = self.ial.get("ial_hash", "latest")
        prefix = f"hash_{ial_hash[0:7]}_"
        self.tag = prefix

        gl_hash = self.gl.get("gl_hash", "latest")
        bin_modifs = {
            "submission": {
                "bindir": (
                    f"{self.ial['user_binary_path']}/{ial_hash}/@COMPILER@/R64/bin"
                ),
                "task_exceptions": {
                    "Forecast": {
                        "bindir": (
                            f"{self.ial['user_binary_path']}/{ial_hash}/"
                            "@COMPILER@/@PRECISION@/bin"
                        )
                    }
                },
            }
        }
        if self.gl.get("active", False):
            bin_modifs["submission"]["bindir_gl"] = (
                f"{self.gl['user_binary_path']}/{gl_hash}/@COMPILER@/bin"
            )
        self.modifs = merge_dicts(bin_modifs, self.modifs, True)

    def update_hostnames(self, hostnames):
        """Update host and domain name.

        Arguments:
            hostnames (dict): Dict of host cases with properties

        """
        for case, item in self.cases.items():
            if "host" in item and item["host"] in hostnames:
                logger.info("Check host for {}:{}", case, item["host"])
                logger.info(
                    "Add {} and {} to {}",
                    hostnames[item["host"]]["config_name"],
                    hostnames[item["host"]]["domain_name"],
                    case,
                )
                self.cases[case]["hostname"] = hostnames[item["host"]]["config_name"]
                self.cases[case]["hostdomain"] = hostnames[item["host"]]["domain_name"]

    @staticmethod
    def get_case_information(config_file):
        """Get case name, json file and reference folder from the config file.

        Arguments:
            config_file (str): Path to the config file

        Returns:
            case_name name: the name of the case.
            json_file: the json file path
            references_folder: the references folder path.
        """
        case_config = ParsedConfig.from_file(config_file, json_schema={})
        platform = Platform(case_config)

        case_name = platform.substitute(
            case_config.get("general.case"),
            basetime=case_config["general.times.start"],
            validtime=case_config["general.times.start"],
        )
        json_file = platform.substitute(
            case_config.get("reference_checker.summary.json.file"),
            basetime=case_config["general.times.start"],
            validtime=case_config["general.times.start"],
        )
        references_folder = platform.get_platform_value("references_folder")
        check = case_config["reference_checker"]["check"]

        return check, case_name, json_file, references_folder

    def collect_summaries(self):
        """Collect summaries from the runs."""
        try:
            with open(f"{self.test_dir}/{self.config_name}_config_names.toml", "rb") as f:
                config_names = tomli.load(f)
        except FileNotFoundError as err:
            msg = "No case mapping available. Run again with '-m'"
            logger.error(msg)
            raise FileNotFoundError(msg) from err

        config_files = [
            f"{self.test_dir}/{config_name}.toml"
            for config_name in config_names["config_names"].values()
        ]

        summaries = {}
        case_files = {}
        width = 0
        now = datetime.now()
        skipped = set()
        for config_file in config_files:
            check, case_name, json_file, references_folder = (
                TestCases.get_case_information(config_file)
            )
            if check:
                case_files[case_name] = json_file
                try:
                    with open(json_file, "r", encoding="utf-8") as f:
                        summary = json.load(f)
                        summary["creation_date"] = datetime.fromtimestamp(
                            os.path.getmtime(json_file)
                        )
                except FileNotFoundError:
                    summary = "MISSING"

                summaries[case_name] = summary
            else:
                summaries[case_name] = "SKIPPED"
                skipped.add(case_name)
            width = max(width, len(case_name))

        if len(summaries) > 0:
            with contextlib.suppress(KeyError):
                logger.info("Our version {}", self.ial["ial_hash"])
            with contextlib.suppress(KeyError):
                logger.info(" from {}", self.ial["pr"])
            logger.info("Comparison against {}", references_folder)

            case_names = sorted([x for x in summaries if x in skipped])
            case_names.extend(sorted([x for x in summaries if x not in skipped]))
            for case_name in case_names:
                summary = summaries[case_name]
                creation_date = (
                    summary["creation_date"] if not isinstance(summary, str) else None
                )
                colored_message = CheckSummaryAnalysis.colored_result_message(
                    summary,
                    self.verbose,
                    case_name,
                    case_files.get(case_name) if case_name in case_files else None,
                    width,
                    creation_date,
                    now,
                )
                logger.opt(colors=True).info(colored_message)

            if not self.verbose:
                logger.opt(colors=True).info("<blue>Add '-v' for more info</blue>")

    def execute(self, args):
        """Execute test cases.

        Arguments:
            args: Command line arguments

        """
        if args.configure:
            directory = Path(self.test_dir)
            for level in self._build_levels():
                self.create(level)
                with concurrent.futures.ThreadPoolExecutor(
                    max_workers=self.max_workers
                ) as executor:
                    futures = {
                        executor.submit(self._run_case, case): case for case in level
                    }
                    for future in concurrent.futures.as_completed(futures):
                        case = futures[future]
                        config_name, domain_name = future.result()
                        self.cases[case]["config_name"] = config_name
                        self.cases[case]["domain_name"] = domain_name
                self.update_hostnames({case: self.cases[case] for case in level})
                BasicConfig({
                    "config_names": {
                        c: item["config_name"]
                        for c, item in self.cases.items()
                        if "config_name" in item
                    }
                }).save_as(f"{directory}/{self.config_name}_config_names.toml")

            if not args.run:
                logger.info("\n\nRerun with '-r' to start the suites\n\n")

        if args.run:
            self.start()


def run_test(args, config=None):
    """Entry point for the ``tactus test`` command.

    Arguments:
        args: Parsed command line arguments
        config: Unused; the test command manages its own config loading

    """
    t = TestCases(args=args)

    if not args.config_file and not args.prepare_binaries and not args.list:
        logger.warning("Nothing to do. Use `tactus test -h` for help.")
        return False
    if args.prepare_binaries:
        t.get_binaries()

    elif args.list:
        t.list()

    elif args.config_file is not None:
        if args.configure or args.run:
            t.execute(args)
        else:
            t.collect_summaries()

    return True
