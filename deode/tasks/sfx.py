"""Surfex tasks."""
import json
import os

from pysurfex.cli import pgd, prep

from deode.boundary_utils import Boundary
from deode.config_parser import ConfigPaths
from deode.datetime_utils import as_datetime
from deode.logs import logger
from deode.namelist import NamelistGenerator
from deode.os_utils import deodemakedirs
from deode.tasks.base import Task
from deode.tasks.batch import BatchJob


class PySurfexBaseTask(Task):
    """Base task class for pysurfex tasks."""

    def __init__(self, config, name):
        """Construct pysurfex-experiment base class.

        Args:
        -------------------------------------------
            config (ParsedConfig): Configuration.
            name (str): Task name.

        """
        Task.__init__(self, config, name)

        # Domain/geo
        conf_proj_dict = {
            "nam_pgd_grid": {"cgrid": "CONF PROJ"},
            "nam_conf_proj_grid": {
                "nimax": self.config["domain.nimax"],
                "njmax": self.config["domain.njmax"],
                "xloncen": self.config["domain.xloncen"],
                "xlatcen": self.config["domain.xlatcen"],
                "xdx": self.config["domain.xdx"],
                "xdy": self.config["domain.xdy"],
                "ilone": self.config["domain.ilone"],
                "ilate": self.config["domain.ilate"],
            },
            "nam_conf_proj": {
                "xlon0": self.config["domain.xlon0"],
                "xlat0": self.config["domain.xlat0"],
            },
        }

        # Binary input data
        self.pysurfex_input_definition = ConfigPaths.path_from_subpath(
            self.platform.get_system_value("sfx_input_definition")
        )
        self.pysurfex_input_definition = self.pysurfex_input_definition.as_posix()
        # Create PySurfex system paths
        system_paths = self.config["system"].dict()
        platform_paths = self.config["platform"].dict()
        exp_file_paths = {}
        for key, val in system_paths.items():
            lkey = self.platform.substitute(key)
            lval = self.platform.substitute(val)
            exp_file_paths.update({lkey: lval})
        for key, val in platform_paths.items():
            lkey = self.platform.substitute(key)
            lval = self.platform.substitute(val)
            exp_file_paths.update({lkey: lval})

        # Create a file in the working directory
        self.create_wdir()
        self.pysurfex_domain_file = f"{self.wdir}/domain.json"
        self.pysurfex_system_file = f"{self.wdir}/system.json"
        self._dump_config(self.pysurfex_system_file, exp_file_paths, force=True)
        self._dump_config(self.pysurfex_domain_file, conf_proj_dict, force=True)

        # Store in climdir for reference
        climdir = self.platform.get_system_value("climdir")
        deodemakedirs(climdir)
        self._dump_config(f"{climdir}/system.json", exp_file_paths)
        self._dump_config(f"{climdir}/domain.json", conf_proj_dict)

    @staticmethod
    def _dump_config(jsonfile, config_settings, force=False):
        """Dump the config file if it does not exist.

        Args:
            jsonfile (string): Full path to json file to write
            config_settings (dict): Dict to dump
            force (boolean): Force output
        """
        if not os.path.exists(jsonfile) or force:
            with open(jsonfile, mode="w", encoding="utf8") as fhandler:
                json.dump(config_settings, fhandler)

    def execute(self):
        """Execute."""
        raise NotImplementedError("Base class is not supposed to be executed")


class Pgd(PySurfexBaseTask):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        PySurfexBaseTask.__init__(self, config, __class__.__name__)
        self.program = "pgd"
        self.nlgen = NamelistGenerator(self.config, "surfex")
        self.climdir = self.platform.get_system_value("climdir")
        self.one_decade = self.config["pgd.one_decade"]
        self.basetime = config["task.args.basetime"]
        self.pgd_prel = self.platform.substitute(
            self.config["file_templates.pgd_prel.archive"], basetime=self.basetime
        )
        self.force = True

    def execute(self):
        """Execute."""
        output = f"{self.climdir}/{self.pgd_prel}"

        if not os.path.exists(output) or self.force:
            binary = self.get_binary("PGD")

            self.nlgen.load(self.program)
            settings = self.nlgen.assemble_namelist(self.program)
            self.nlgen.write_namelist(settings, "OPTIONS.nam_input")

            argv = [
                "--domain",
                self.pysurfex_domain_file,
                "--system-file-paths",
                self.pysurfex_system_file,
                "--input-binary-data",
                self.pysurfex_input_definition,
                "--namelist-path",
                "OPTIONS.nam_input",
                "--print-namelist",
                "--binary",
                binary,
                "--output",
                output,
                "--wrapper",
                self.wrapper,
            ]

            if self.force:
                argv.append("--force")

            if self.one_decade:
                basetime = as_datetime(self.basetime).strftime("%Y%m%d%H")
                argv += ["--basetime", basetime, "--one-decade"]

            pgd(argv=argv)
            self.archive_logs(["OPTIONS.nam", "LISTING_PGD.txt"], target=self.climdir)
        else:
            logger.warning("Output already exists: ", output)


class Prep(PySurfexBaseTask):
    """Prep."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        PySurfexBaseTask.__init__(self, config, __class__.__name__)
        self.nlgen = NamelistGenerator(self.config, "surfex")
        self.archive = self.platform.get_system_value("archive")
        self.intp_bddir_sfx = self.platform.get_system_value("intp_bddir_sfx")
        self.force = True

    def execute(self):
        """Execute."""
        cnmexp = self.config["general.cnmexp"]
        output = f"{self.intp_bddir_sfx}/ICMSH{cnmexp}INIT.sfx"

        if not os.path.exists(output) or self.force:
            binary = self.get_binary("PREP")
            deodemakedirs(self.archive)

            bd_has_surfex = self.config["boundaries.bd_has_surfex"]
            namelist_task = "prep"
            self.nlgen.load(namelist_task)
            settings = self.nlgen.assemble_namelist(namelist_task)
            self.nlgen.write_namelist(settings, "OPTIONS.nam_input")

            # TODO this should be externalized
            # Select the correct input file
            basetime = as_datetime(self.config["general.times.basetime"])
            self.boundary = Boundary(self.config)
            bddir_sfx = self.config["system.bddir_sfx"]
            bdfile_sfx_template: str = self.config["file_templates.bdfile_sfx.archive"]
            if not self.config.get(f"boundaries.{self.boundary.bdmodel}.bdmember"):
                bdfile_sfx_template = bdfile_sfx_template.replace("@BDMEMBER@", "0")
            prep_input_file = self.platform.substitute(
                f"{bddir_sfx}/{bdfile_sfx_template}",
                basetime=self.boundary.bd_basetime_sfx,
                validtime=basetime,
            )

            # PGD file input update
            const_clim = self.config["file_templates.pgd.archive"]
            pgdfile_source = self.platform.substitute(f"@CLIMDIR@/{const_clim}")

            argv = [
                "--basetime",
                basetime.strftime("%Y%m%d%H"),
                "--pgd",
                pgdfile_source,
                "--prep-file",
                prep_input_file,
                "--prep-filetype",
                "grib",
                "--system-file-paths",
                self.pysurfex_system_file,
                "--input-binary-data",
                self.pysurfex_input_definition,
                "--namelist-path",
                "OPTIONS.nam_input",
                "--print-namelist",
                "--binary",
                binary,
                "--output",
                output,
                "--wrapper",
                self.wrapper,
            ]

            if self.force:
                argv.append("--force")

            if bd_has_surfex:
                const_clim_host = self.config["file_templates.pgd_host.archive"]
                pgd_host_source = self.platform.substitute(
                    f"@BDCLIMDIR@/{const_clim_host}"
                )
                argv += ["--prep-pgdfile", pgd_host_source, "--prep-pgdfiletype", "FA"]

            prep(argv=argv)
            self.archive_logs(["OPTIONS.nam", "LISTING_PREP0.txt"])

        else:
            logger.info("Output already exists: {}", output)


class PgdFilterTownFrac(Task):
    """PgdFilterTownFrac.

    Reduce Town frac in Pgd file made with eccoclimapII.
    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __class__.__name__)

        self.climdir = self.platform.get_system_value("climdir")

        self.pgd_frac = self.get_binary("pgd_frac")
        self.basetime = config["task.args.basetime"]
        self.inoutfile = self.platform.substitute(
            self.config["file_templates.pgd.archive"], basetime=self.basetime
        )

    def execute(self):
        """Run task."""
        self.fmanager.input(
            f"{self.climdir}/{self.inoutfile}", self.inoutfile, provider_id="copy"
        )

        # Run pgd_frac
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(f"{self.pgd_frac} {self.inoutfile}")

        self.fmanager.output(self.inoutfile, f"{self.climdir}/{self.inoutfile}")
