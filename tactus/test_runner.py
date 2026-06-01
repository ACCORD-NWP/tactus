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
from .experiment import get_git_info
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
        self.assigned = {}

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

    def get_tactus_version(self):
        """Get tactus version info."""
        tactus_git = get_git_info()
        tag = tactus_git["branch"]
        for character in ["/", ".", "-"]:
            tag = tag.replace(character, "_")
        tag += "_"
        return tag

    def prepare(self):
        """Prepare the host cases.

        Returns:
            host_cases: List of host cases

        Raises:
            KeyError: If case is not found
        """
        try:
            host_cases = [
                self.cases[case]["host"]
                for case in self.selection
                if "host" in self.cases[case]
            ]
        except KeyError as err:
            logger.error(f"The case is not available\n Available cases are {list(self.cases)}")
            raise KeyError() from err

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

        days_difference = (date.today() - self.reference_date).days
        for i, (case, item) in enumerate(self.cases.items()):
            if case not in self.assigned:
                self.assigned[case] = i + 1 + days_difference

            if case not in cases:  # or "config_name" in self.cases[case]:
                continue

            if "host" in item:
                self.assigned[case] = self.assigned[item["host"]]

            subtag = item.get("subtag", "")
            extra = list(self.extra) + list(item.get("extra", []))

            # Merge and replace macros
            modifs = merge_dicts(self.modifs, self.cases[case].get("modifs", {}), True)
            config = self.config.copy(
                update={
                    "modifs": modifs,
                    "modif_macros": {
                        "counter": self.assigned[case],
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

    def populate_cmds(self):
        """Create the tests."""
        days_difference = (date.today() - self.reference_date).days
        for i, (case, item) in enumerate(self.cases.items()):
            if case not in self.assigned:
                self.assigned[case] = i + 1 + days_difference

            if case not in self.selection:
                continue

            if "host" in item:
                self.assigned[case] = self.assigned[item["host"]]

            base = item.get("base", case)
            extra = list(self.extra) + list(item.get("extra", []))

            outfile = f"{self.test_dir}/modifs_{case}.toml"

            base_file = (
                str(GeneralConstants.PACKAGE_DIRECTORY)
                + "/data/config_files/configurations/"
                + base
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

            # Call tactus main to create new config, and possibly start suite
            tactus_main(cmd)

            # Update the case settings
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
            else:
                config_names = {
                    case: item["config_name"]
                    for case, item in self.cases.items()
                    if "config_name" in item
                }
                BasicConfig({"config_names": config_names}).save_as(
                    f"{directory}/config_names.toml"
                )

        return cases

    def start(self):
        """Start the run."""
        # Local import to avoid circular dependency (__main__ -> argparse_wrapper -> here)
        from .__main__ import main as tactus_main

        with open(f"{self.test_dir}/config_names.toml", "rb") as f:
            config_names = tomli.load(f)

        for case in self.cmds:
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
                logger.info("Check host for {}:{}", case, item["host"])
                logger.info(
                    "Add {} and {} to {}",
                    hostnames[item["host"]]["config_name"],
                    hostnames[item["host"]]["domain_name"],
                    case,
                )
                self.cases[case]["hostname"] = hostnames[item["host"]]["config_name"]
                self.cases[case]["hostdomain"] = hostnames[item["host"]]["domain_name"]

    def execute(self, args):
        """Execute test cases.

        Arguments:
            args: Command line arguments

        """
        if args.prep:
            host_cases = self.prepare()
            self.create(host_cases)
            hostnames = self.configure(config_hosts=True)
            self.update_hostnames(hostnames)
            self.create()
            # Cases with hostname were configured before mirror info was available;
            # clear config_name so they are reconfigured with the updated modifs.
            for case in self.cases.values():
                if "hostname" in case:
                    case.pop("config_name", None)

        if args.configure:
            self.populate_cmds()
            self.configure()

        if args.run:
            self.populate_cmds()
            self.start()


def run_test(args, config=None):
    """Entry point for the ``tactus test`` command.

    Arguments:
        args: Parsed command line arguments
        config: Unused; the test command manages its own config loading

    """
    t = TestCases(args=args)

    if args.prepare_binaries:
        t.get_binaries()

    elif args.list:
        t.list()

    elif args.config_file is not None:
        t.execute(args)
