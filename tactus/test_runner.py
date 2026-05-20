#!/usr/bin/env python3
"""Test runner functionality for running integration test cases."""

import contextlib
import copy
import glob
import os
from datetime import date
from pathlib import Path

import tomli

from . import GeneralConstants
from .config_parser import BasicConfig, ConfigPaths, ParsedConfig
from .datetime_utils import as_datetime
from .fullpos import flatten_list
from .general_utils import merge_dicts
from .host_actions import TactusHost
from .logs import logger


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
            try:
                definitions = self.config.expand_macros().dict()
            except KeyError:
                definitions = self.config.dict()

        self.verbose = args.verbose
        self.cases = definitions.get("cases", {})
        try:
            self.reference_date = as_datetime(
                f"{definitions['general']['reference_date']}T00:00:00Z"
            ).date()
        except KeyError:
            self.reference_date = date.today()
        self.cmds = {}
        self.mode = definitions["general"].get("mode", "suite")
        self.extra = definitions["general"].get("extra", [])
        self.get_tag(definitions)
        self.dry = args.dry if args.dry else definitions["general"].get("dry", False)
        self.modifs = definitions["modifs"]
        self.test_dir = definitions.get("test_dir", f"{self.tag}configs")
        self.ial = definitions.get("ial", {})
        self.gl = definitions.get("gl", {})
        self.selection = self.resolve_selection(definitions)

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
            tag = GeneralConstants.VERSION.replace(".", "_").replace("-", "_") + "_"
            definitions["general"]["tag"] = tag
            logger.info("tag not given, derived from tactus version")
        self.tag = definitions["general"].get("tag")

        if self.tag[0].isdigit():
            self.tag = "v" + self.tag
            #raise ValueError(f"The tag cannot start with an integer. tag={self.tag}")

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

        with contextlib.suppress(KeyError):
            subtags = definitions["general"]["compiler"]
            subtag_selection = []
            for tag, value in subtags.items():
                if not value.get("active", False):
                    continue
                for sel in selection:
                    if any(x in sel for x in value.get("exclude", "")):
                        continue
                    subtag = f"{tag}{sel}"
                    x = copy.deepcopy(self.cases[sel])
                    if "base" not in x:
                        x["base"] = sel
                    if "host" in x:
                        x["host"] = f"{tag}{x['host']}"
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

    def prepare(self):
        """Prepare the host cases.

        Raises:
            KeyError: If case is not found

        Returns:
            host_cases: List of host cases

        """
        try:
            host_cases = [
                self.cases[case]["host"]
                for case in self.selection
                if "host" in self.cases[case]
            ]
        except KeyError as err:
            raise KeyError(
                f"The case is not available\n Available cases are {list(self.cases)}"
            ) from err

        return host_cases

    def create(self, host_cases=None):
        """Create the tests.

        Arguments:
            host_cases (list, optional): List of host cases

        """
        os.makedirs(self.test_dir, exist_ok=True)
        if host_cases is None:
            cases = self.selection
            label = ""
        else:
            label = "host "
            cases = host_cases

        logger.info("Create {}config files in {}", label, self.test_dir)

        assigned = {}
        days_difference = (date.today() - self.reference_date).days
        for i, (case, item) in enumerate(self.cases.items()):
            assigned[case] = i + 1 + days_difference

            if case not in cases or "config_name" in self.cases[case]:
                continue

            counter = assigned[item["host"]] if "host" in item else assigned[case]
            base = item["base"] if "base" in item else case
            subtag = item["subtag"] if "subtag" in item else ""
            host_case = item["hostname"] if "hostname" in item else ""
            host_domain = item["hostdomain"] if "hostdomain" in item else ""
            extra = list(self.extra) + (list(item["extra"]) if "extra" in item else [])

            modifs = merge_dicts(self.modifs, self.cases[case].get("modifs", {}), True)
            config = self.config.copy(
                update={
                    "modifs": modifs,
                    "modif_macros": {
                        "counter": counter,
                        "host_case": host_case,
                        "host_domain": host_domain,
                        "tag": self.tag,
                        "subtag": subtag,
                    },
                }
            )
            with contextlib.suppress(KeyError):
                config = config.expand_macros(True)

            outfile = f"{self.test_dir}/modifs_{case}.toml"
            logger.info(" create: {}", outfile)
            BasicConfig(config["modifs"]).save_as(outfile)

            cmd = [
                "case",
                f"?{GeneralConstants.PACKAGE_DIRECTORY}/data/config_files/configurations/{base}",
                extra,
                outfile,
                "-o",
                self.test_dir,
            ]
            self.cmds[case] = flatten_list(cmd)

    def configure(self, config_hosts=False, cmds=None):
        """Configure tests.

        Arguments:
            config_hosts (bool, optional): Flag for updating the case settings
                                           with host information
            cmds (list, optional): List of commands (str)

        Returns:
            cases (dict): Dict of cases to run

        """
        # Local import to avoid circular dependency (__main__ -> argparse_wrapper -> here)
        from .__main__ import main as tactus_main

        if cmds is None:
            cmds = []
        cases = {}
        for case, cmd in self.cmds.items():
            if "config_name" in self.cases[case]:
                continue

            logger.info("Configure case {} with\n", case)
            for c in cmds:
                cmd.append(c)
            cmd_txt = " ".join(cmd)
            logger.info("Use cmd:\n\n{}\n\n", cmd_txt)

            tactus_main(cmd)

            directory = Path(self.test_dir)
            config_file = max(directory.glob("*.toml"), key=lambda f: f.stat().st_mtime)
            with open(config_file, "rb") as f:
                definitions = tomli.load(f)

            self.cases[case]["config_name"] = os.path.basename(config_file.stem)
            self.cases[case]["domain_name"] = definitions["domain"]["name"]

            if config_hosts:
                cases[case] = {
                    "config_name": os.path.basename(config_file.stem),
                    "domain_name": definitions["domain"]["name"],
                }

        return cases

    def start(self):
        """Start the run."""
        # Local import to avoid circular dependency (__main__ -> argparse_wrapper -> here)
        from .__main__ import main as tactus_main

        for case in self.cmds:
            config_name = self.cases[case]["config_name"]
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
                cmds = [
                    [
                        "start",
                        "suite",
                        "--config-file",
                        f"{self.test_dir}/{config_name}.toml",
                        "-f",
                        f"{self.test_dir}/{config_name}.def",
                        "-k",
                    ]
                ]

            for cmd in cmds:
                cmd_txt = " ".join(cmd)
                logger.info("Use cmd:\n\n{}\n\n", cmd_txt)

                if not self.dry:
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
                logger.info(
                    "Add {} and {} to {}",
                    hostnames[item["host"]]["config_name"],
                    hostnames[item["host"]]["domain_name"],
                    case,
                )
                self.cases[case]["hostname"] = hostnames[item["host"]]["config_name"]
                self.cases[case]["hostdomain"] = hostnames[item["host"]]["domain_name"]


def execute(t, args):
    """Execute test cases.

    Arguments:
        t (TestCases): Object with test cases to execute
        args: Command line arguments

    """
    host_cases = t.prepare()
    t.create(host_cases)
    hostnames = t.configure(config_hosts=True)
    t.update_hostnames(hostnames)
    t.create()

    if args.run:
        t.configure()
        t.start()


def run_test(args, config=None):
    """Entry point for the ``tactus test`` command.

    Arguments:
        args: Parsed command line arguments
        config: Unused; the test command manages its own config loading

    """
    t = TestCases(args=args)

    if args.prepare_binaries:
        t.get_binaries()

    elif args.remove:
        if args.remove_search_path is not None:
            files = args.remove_search_path
        elif t.test_dir is not None:
            files = [
                p
                for p in Path(".").glob(f"{t.test_dir}/*.toml")
                if "modifs_" not in p.name
            ]
        else:
            files = []
        args.config_files = files
        args.dry_run = args.dry
        remove_config_file = "config_files/remove.toml"
        with open(remove_config_file, "rb") as f:
            remove_config = tomli.load(f)
        logger.info("Read cleaning rules from {}", remove_config_file)
        args.force_remove = remove_config["remove"].pop("force_remove", False)

    elif args.list:
        t.list()

    elif args.config_file is not None:
        execute(t, args)
