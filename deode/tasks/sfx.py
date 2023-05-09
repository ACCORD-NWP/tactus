"""Forecast."""

import logging
import os

import f90nml
from pysurfex.binary_input import (
    OfflineInputData,
    PgdInputData,
    PrepInputData,
    SodaInputData,
)
from pysurfex.configuration import Configuration
from pysurfex.file import PGDFile, PREPFile, SURFFile
from pysurfex.platform import SystemFilePaths
from pysurfex.run import PerturbedOffline, SURFEXBinary

from ..datetime_utils import as_datetime
from ..namelist import NamelistGenerator
from .base import Task
from .batch import BatchJob


class SurfexBinaryTask(Task):
    """Task."""

    def __init__(self, config, name=None, mode=None):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration
            name (str): Name og task
            mode (str): Type of surfex binary

        """
        if name is None:
            name = self.__class__.__name__

        Task.__init__(self, config, name)

        self.mode = mode
        self.need_pgd = True
        self.need_prep = True
        self.pgd = False
        self.do_prep = False
        self.perturbed = False
        self.soda = False
        self.force = True

        self.nlgen = NamelistGenerator(self.config, "surfex")

        # SURFEX config added to general config
        cfg = self.config.get_value("SURFEX").dict()
        sfx_config = {"SURFEX": cfg}
        self.sfx_config = Configuration(sfx_config)

        # Get paths from file manager (example)
        ecosg_dir = self.fmanager.platform.get_platform_value("ECOSG_DATA_PATH")
        pgd_dir = self.fmanager.platform.get_platform_value("PGD_DATA_PATH")
        climdir = self.fmanager.platform.get_system_value("climdir")

        # TODO get all needed paths
        exp_file_paths = {
            "tree_height_dir": f"{ecosg_dir}/HT/",
            "flake_dir": f"{pgd_dir}",
            "sand_dir": f"{climdir}",
            "clay_dir": f"{climdir}",
            "soc_top_dir": f"{climdir}",
            "soc_sub_dir": f"{climdir}",
            "ecoclimap_sg_cover_dir": f"{ecosg_dir}/COVER/",
            "albnir_soil_dir": f"{ecosg_dir}/ALB_SAT/",
            "albvis_soil_dir": f"{ecosg_dir}/ALB_SAT/",
            "albnir_veg_dir": f"{ecosg_dir}/ALB_SAT/",
            "albvis_veg_dir": f"{ecosg_dir}/ALB_SAT/",
            "lai_dir": f"{ecosg_dir}/LAI_SAT/",
            "oro_dir": f"{climdir}",
        }
        self.exp_file_paths = SystemFilePaths(exp_file_paths)

        self.dtg = as_datetime(self.config.get_value("general.times.validtime"))
        self.wrapper = ""
        self.check_existence = False
        self.print_namelist = True

    def execute_binary(
        self,
        binary,
        output,
        pgd_file_path=None,
        prep_file_path=None,
        archive_data=None,
        forc_zs=False,
        masterodb=True,
        perturbed_file_pattern=None,
        fcint=3,
        prep_file=None,
        prep_filetype=None,
        prep_pgdfile=None,
        prep_pgdfiletype=None,
    ):
        """Execute the surfex binary.

        Args:
            binary (str): Full path to binary
            output (str): Full path to output file
            pgd_file_path (str, optional): _description_. Defaults to None.
            prep_file_path (str, optional): _description_. Defaults to None.
            archive_data (surfex.OutputDataFromSurfexBinaries, optional): A mapping of produced files and where to archive them. Defaults to None.
            forc_zs (bool, optional): _description_. Defaults to False.
            masterodb (bool, optional): _description_. Defaults to True.
            perturbed_file_pattern (str, optional):
            File pattern for perturbations. Defaults to None.
            fcint (int, optional): Interval between analyses. Defaults to 3.
            prep_file (str, optional):
            Path to input prepfile. Defaults to None.
            prep_filetype (str, optional):
            Filetype of prep input file. Defaults to None.
            prep_pgdfile (str, optional):
            PGD file path for prep input file. Defaults to None.
            prep_pgdfiletype (str, optional):
            PGD filetype for prep input file. Defaults to None.

        Raises:
            NotImplementedError: Unknown implementation.
        """
        rte = os.environ
        if self.mode == "pgd":
            self.pgd = True
            self.need_pgd = False
            self.need_prep = False
            input_data = PgdInputData(
                self.sfx_config, self.exp_file_paths, check_existence=self.check_existence
            )
        elif self.mode == "prep":
            self.do_prep = True
            self.need_prep = False
            input_data = PrepInputData(
                self.sfx_config,
                self.exp_file_paths,
                check_existence=self.check_existence,
                prep_file=prep_file,
                prep_pgdfile=prep_pgdfile,
            )
        elif self.mode == "offline":
            input_data = OfflineInputData(
                self.sfx_config, self.exp_file_paths, check_existence=self.check_existence
            )
        elif self.mode == "soda":
            self.soda = True
            input_data = SodaInputData(
                self.sfx_config,
                self.exp_file_paths,
                check_existence=self.check_existence,
                dtg=self.dtg,
                masterodb=masterodb,
                perturbed_file_pattern=perturbed_file_pattern,
            )
        elif self.mode == "perturbed":
            self.perturbed = True
            input_data = OfflineInputData(
                self.sfx_config, self.exp_file_paths, check_existence=self.check_existence
            )
        else:
            raise NotImplementedError(self.mode + " is not implemented!")

        rte = os.environ
        batch = BatchJob(rte, wrapper=self.wrapper)

        # Overide settings with static namelists
        self.nlgen.generate_namelist(self.mode, "OPTIONS.nam")
        settings = f90nml.read("OPTIONS.nam")

        filetype = settings["nam_io_offline"]["csurf_filetype"]
        pgdfile = settings["nam_io_offline"]["cpgdfile"]
        prepfile = settings["nam_io_offline"]["cprepfile"]
        surffile = settings["nam_io_offline"]["csurffile"]

        if prep_file is not None:
            settings["nam_prep_surf_atm"]["cfile"] = os.path.basename(prep_file)

        lfagmap = False
        if "LFAGMAP" in settings["NAM_IO_OFFLINE"]:
            lfagmap = settings["NAM_IO_OFFLINE"]["LFAGMAP"]

        if self.need_pgd:
            pgdfile = PGDFile(
                filetype, pgdfile, input_file=pgd_file_path, lfagmap=lfagmap
            )

        if self.need_prep:
            prepfile = PREPFile(
                filetype, prepfile, input_file=prep_file_path, lfagmap=lfagmap
            )

        if self.need_prep and self.need_pgd:
            surffile = SURFFile(filetype, surffile, archive_file=output, lfagmap=lfagmap)
        else:
            surffile = None

        if self.perturbed:
            if self.pert > 0:
                PerturbedOffline(
                    binary,
                    batch,
                    prepfile,
                    self.ivar,
                    settings,
                    input_data,
                    pgdfile=pgdfile,
                    surfout=surffile,
                    archive_data=archive_data,
                    print_namelist=self.print_namelist,
                )
            else:
                SURFEXBinary(
                    binary,
                    batch,
                    prepfile,
                    settings,
                    input_data,
                    pgdfile=pgdfile,
                    surfout=surffile,
                    archive_data=archive_data,
                    print_namelist=self.print_namelist,
                )
        elif self.pgd:
            pgdfile = PGDFile(
                filetype,
                pgdfile,
                input_file=pgd_file_path,
                archive_file=output,
                lfagmap=lfagmap,
            )
            SURFEXBinary(
                binary,
                batch,
                pgdfile,
                settings,
                input_data,
                archive_data=archive_data,
                print_namelist=self.print_namelist,
            )
        elif self.do_prep:
            prepfile = PREPFile(filetype, prepfile, archive_file=output, lfagmap=lfagmap)
            SURFEXBinary(
                binary,
                batch,
                prepfile,
                settings,
                input_data,
                pgdfile=pgdfile,
                archive_data=archive_data,
                print_namelist=self.print_namelist,
            )
        else:
            SURFEXBinary(
                binary,
                batch,
                prepfile,
                settings,
                input_data,
                pgdfile=pgdfile,
                surfout=surffile,
                archive_data=archive_data,
                print_namelist=self.print_namelist,
            )


class Pgd(SurfexBinaryTask):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        SurfexBinaryTask.__init__(self, config, "Pgd", "pgd")

    def execute(self):
        """Execute."""
        output = self.platform.get_system_value("climdir") + "/PGD_prel.fa"
        binary = self.platform.get_system_value("bindir") + "/PGD"

        if not os.path.exists(output) or self.force:
            SurfexBinaryTask.execute_binary(self, binary=binary, output=output)
        else:
            print("Output already exists: ", output)


class Prep(SurfexBinaryTask):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        SurfexBinaryTask.__init__(self, config, "Prep", "prep")

    def execute(self):
        """Execute."""
        pgd_file_path = self.platform.get_system_value("climdir") + "/Const.Clim.sfx"
        # TODO get from config?
        prep_file = (
            self.platform.get_system_value("bddir_sfx")
            + "/"
            + self.platform.get_system_value("bdfile_sfx_template")
        )
        prep_filetype = "GRIB2"
        cnmexp = self.config.get_value("general.cnmexp")
        archive = self.platform.get_system_value("archive")

        output = f"{archive}/ICMSH{cnmexp}INIT.sfx"
        binary = self.platform.get_system_value("bindir") + "/PREP"

        if not os.path.exists(output) or self.force:

            # TODO temporary  # noqa
            os.system(f"touch {pgd_file_path}")  # noqa
            SurfexBinaryTask.execute_binary(
                self,
                binary,
                output,
                pgd_file_path=pgd_file_path,
                prep_file=prep_file,
                prep_filetype=prep_filetype,
            )
        else:
            logging.info("Output already exists: %s", output)
