"""Boundary interpolation."""

import json
import os
import re
import subprocess
from typing import Dict, Set

from tactus.boundary_utils import Boundary
from tactus.config_parser import ConfigPaths
from tactus.datetime_utils import as_datetime
from tactus.logs import logger
from tactus.namelist import NamelistGenerator
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob


class InterpolateBoundaries(Task):
    """Boundary interpolation task."""

    def __init__(self, config, cams: bool = False):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
            cams (bool): Interpolate the CAMS aerosols instead of the full
                host model state. Defaults to False.
        """
        self.boundary = Boundary(config, cams=cams)
        label = "c903_cams" if cams else self.boundary.method
        name = f"{label}_{self.boundary.min_index}-{self.boundary.max_index}".upper()
        Task.__init__(self, config, name)

        if self.boundary.method == "e927":
            self.namelists = {self.boundary.method: "fort.4"}
            self.logs = ["fort.4", "NODE.001_01"]
            self.outfile = "PF@CNMEXP@000+0000"

        elif self.boundary.method == "c903":
            suffix = "_cams" if cams else ""
            self.namelists = {
                f"c903_main{suffix}": "fort.4",
                f"c903_domain{suffix}": "namelist_c903_domain",
            }
            self.logs = ["fort.4", "namelist_c903_domain", "NODE.001_01"]
            self.outfile = "ELSCFMARS@FPDOMAIN@+@NNN@"

        # Host model input directory. None means the path in the input definition.
        self.host_model_dir = "@BDDIR_CAMS@" if cams else None
        # Output directory of the interpolated boundaries
        self.output_dir = (
            self.platform.get_value("nrt_aerosols.intp_bddir")
            if cams
            else self.platform.get_system_value("intp_bddir")
        )

        self.input_definition = f"{self.boundary.method}_input_definition"
        self.nlgen = NamelistGenerator(self.config, "master")
        self.master = self.get_binary("MASTERODB", task_name=self.boundary.method.upper())

    def host_model_file(self, src: str) -> str:
        """Return the host model file name with the boundary member filled in.

        Default to 0 for bdmember if no bdmember specified. This is to be able to
        reference files created by marsprep, which contains bdmember = 0 even for
        "deterministic" runs. CAMS data are always deterministic.

        Args:
            src (str): File name template from the input definition

        Returns:
            str: File name template
        """
        if self.boundary.cams or not self.config.get(
            f"boundaries.{self.boundary.bdmodel}.bdmember"
        ):
            return src.replace("@BDMEMBER@", "0")
        return src

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Fetch input data
        input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value(self.input_definition)
        )

        logger.info("Read input data spec from: {}", input_definition)
        with open(input_definition, "r", encoding="utf-8") as f:
            input_data = json.load(f)

        host_model_files: Dict[str, str | Dict[str, str]] = input_data.pop(
            "host_model_files"
        )

        if self.boundary.method == "c903":
            host_model_static_files: Dict[str, str | Dict[str, str]] = input_data.pop(
                "host_model_static_files"
            )
            path_static = self.host_model_dir or host_model_static_files["path"]
            logger.info("*** host_model_static_files: {}", host_model_static_files)

            for dst, src in host_model_static_files["files"].items():
                src_local = self.host_model_file(src)
                self.fmanager.input(
                    f"{path_static}/{src_local}",
                    dst,
                    basetime=self.boundary.bd_basetime,
                    validtime=as_datetime(
                        self.boundary.bd_index_time_dict[self.boundary.min_index]
                    ),
                )
        # Link the static data
        self.fmanager.input_data_iterator(input_data, basetime=self.boundary.bd_basetime)

        # Host model input files
        path = self.host_model_dir or host_model_files["path"]
        logger.info("*** host_model_files: {}", host_model_files)
        for dst, src in host_model_files["files"].items():
            src_local = self.host_model_file(src)
            for bd_index, bd_time in self.boundary.bd_index_time_dict.items():
                self.fmanager.input(
                    f"{path}/{src_local}",
                    dst.replace("@NNN@", f"{bd_index - self.boundary.min_index}"),
                    basetime=self.boundary.bd_basetime,
                    validtime=as_datetime(bd_time),
                )

        # Generate namelists
        for key, value in self.namelists.items():
            if key.startswith("c903_main"):
                self.nlgen.load(key)
                self.nlgen.update(
                    {"NAMCT0": {"NFRPOS": len(self.boundary.bd_index_time_dict)}},
                    "bd_dynamic_update",
                )
                nml = self.nlgen.assemble_namelist(key)
                self.nlgen.write_namelist(nml, value)
            else:
                self.nlgen.generate_namelist(key, value)

        # Run masterodb
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(self.master)

        # Store result
        for bd_index in self.boundary.bd_index_time_dict:
            target = (
                f"{self.output_dir}/"
                + f"{self.config['file_templates.interpolated_boundaries.archive']}"
            ).replace("@NNN@", f"{bd_index:03}")
            if "target_suffix" in self.config["task.args"]:
                target += self.config["task.args.target_suffix"]

            self.fmanager.output(
                self.outfile.replace("@NNN@", f"{bd_index - self.boundary.min_index:04}"),
                target,
            )

        self.archive_logs(self.logs)


class E927(InterpolateBoundaries):
    """E927."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config)


class C903(InterpolateBoundaries):
    """C903."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config)


class C903Cams(InterpolateBoundaries):
    """Interpolate the CAMS aerosols to the model domain.

    Runs c903 on the CAMS data retrieved by MarsprepCams and writes aerosol-only
    boundary files to nrt_aerosols.intp_bddir. MergeCamsLbc adds them to the
    boundary files from C903.
    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        InterpolateBoundaries.__init__(self, config, cams=True)


class MergeCamsLbc(Task):
    """Add the CAMS aerosols from C903Cams to the boundary files from C903.

    The fields are merged with lfitools. lfi_alt_merge keeps the first copy of a
    field found in both files, so the C903 file is given first: its header, surface
    pressure, orography etc. are kept and only the aerosol fields are added.
    """

    # Field that tells whether a boundary file already holds the CAMS aerosols
    AEROSOL_FIELD = "SEASALT1"

    def __init__(self, config):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
        """
        self.boundary = Boundary(config)
        name = f"{__class__.__name__}_{self.boundary.min_index}-{self.boundary.max_index}"
        Task.__init__(self, config, name)

        self.lbc_dir = self.platform.get_system_value("intp_bddir")
        self.aerosol_dir = self.platform.get_value("nrt_aerosols.intp_bddir")
        self.template = self.config["file_templates.interpolated_boundaries.archive"]
        self.lfitools = self.get_binary("lfitools", task_name=__class__.__name__)

    def lfi_fields(self, filename: str) -> Set[str]:
        """List the fields in an FA/LFI file.

        Args:
            filename (str): File to list

        Returns:
            Set[str]: Field names

        Raises:
            RuntimeError: If no fields are found
        """
        result = subprocess.run(
            [self.lfitools, "lfilist", filename],
            capture_output=True,
            text=True,
            check=True,
        )
        fields = set(re.findall(r"^\[\s*'([^']+)'", result.stdout, flags=re.MULTILINE))
        if not fields:
            raise RuntimeError(f"No fields found in {filename}")
        return fields

    def merge(self, lbc_file: str, aerosol_file: str, merged_file: str):
        """Merge the aerosol fields into a boundary file.

        Args:
            lbc_file (str): Boundary file from C903, replaced by the merged file
            aerosol_file (str): Aerosol-only boundary file from C903Cams
            merged_file (str): Name of the merged file in the work directory

        Raises:
            FileNotFoundError: If an input file is missing
            RuntimeError: If the boundary file already holds aerosols, or if the
                merged file does not hold the expected fields
        """
        for filename in (lbc_file, aerosol_file):
            if not os.path.isfile(filename):
                raise FileNotFoundError(f"Missing boundary file {filename}")

        lbc_fields = self.lfi_fields(lbc_file)
        if any(field.endswith(self.AEROSOL_FIELD) for field in lbc_fields):
            raise RuntimeError(
                f"{lbc_file} already holds aerosol fields, "
                "rerun C903 for these boundaries before merging again"
            )
        aerosol_fields = self.lfi_fields(aerosol_file)

        batch = BatchJob(os.environ, wrapper="")
        batch.run(
            f"{self.lfitools} lfi_alt_merge --lfi-file-in {lbc_file} {aerosol_file} "
            f"--lfi-file-out {merged_file}.idx"
        )
        batch.run(
            f"{self.lfitools} lfi_alt_pack --lfi-file-in {merged_file}.idx "
            f"--lfi-file-out {merged_file}"
        )

        expected_fields = lbc_fields | aerosol_fields
        merged_fields = self.lfi_fields(merged_file)
        if merged_fields != expected_fields:
            raise RuntimeError(
                f"{merged_file} holds {len(merged_fields)} fields, expected "
                f"{len(expected_fields)}. Missing: "
                f"{sorted(expected_fields - merged_fields)}"
            )
        logger.info(
            "{}: {} fields + {} aerosol fields",
            merged_file,
            len(lbc_fields),
            len(merged_fields) - len(lbc_fields),
        )
        self.fmanager.output(merged_file, lbc_file)

    def execute(self):
        """Run task."""
        for bd_index in sorted(self.boundary.bd_index_time_dict):
            filename = self.platform.substitute(
                self.template.replace("@NNN@", f"{bd_index:03}")
            )
            self.merge(
                f"{self.lbc_dir}/{filename}",
                f"{self.aerosol_dir}/{filename}",
                filename,
            )
