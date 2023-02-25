"""E923."""

import os
import shutil
import f90nml
from deode.tasks.batch import BatchJob
from .base import Task


class E923(Task):
    """E93 task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        # Temporary namelists
        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")

        self.namelist_path = self.platform.get_platform_value('namelists')
        self.climdir = self.platform.get_system_value('climdir')
        self.months = [f'{mm:02d}'for mm in range(1, 13)]

        self.namelists = ["nam923_1_smoothing",
                          "nam923_1", "nam923_2", "nam923_3",
                          "nam923_4", "nam923_5", "nam923_6",
                          "nam923_7", "nam923_8", "nam923_9"]

        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

    def load_namelist(self, i):
        """Read and adjust namelist.

        Args:
            i (int) : sequence number

        Returns :
            nam (f90nml object): loaded namelist
        """
        namelist = f"{self.namelist_path}/{self.namelists[i]}"
        self.logger.info("Read namelist: %s", namelist)
        nam = f90nml.read(namelist)
        nam.uppercase = True
        nam.end_comma = True
        return nam

    def myexec(self, cmd, i):
        """Execute binary task."""
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(cmd)

        log = "NODE.001_01"
        try:
            shutil.copy(log, f"{log}_part_{i}")
        except FileNotFoundError:
            self.logger.info("No logfile %s produced", log)

    def write_namelist(self, nam):
        """Write namelist with uppercase and commas.

        Args:
            nam (f90nml object) : namelist object to write
        """
        nam.uppercase = True
        nam.end_comma = True
        print(nam)
        with open('fort.4', 'w', encoding="utf-8") as nml_file:
            f90nml.write(nam, nml_file)

    def remove_links(self, link):
        """Remove link.

        Args:
            link (list) : List of links to remove
        """
        self.logger.info("clean %s", link)
        for x in link:
            os.unlink(x)

    def print_part(self, part, month=None):
        """Run the constant part of e923.

        Args:
            part : which step
            month : month
        """
        if month is None:
            self.logger.info('Executing PART %s', part)
        else:
            self.logger.info('Executing PART %s, month %s', part, month)

    def constant_part(self, constant_file):
        """Run the constant part of e923.

        Args:
            constant_file : filename of the resulting file
        """
        self.logger.info("Create: %s", constant_file)

        # RTTM input
        for ifile in ["MCICA", "RADSRTM"]:
            self.fmanager.input(f"@RRTM_DIR@/{ifile}", ifile)

        # PGD input

        self.fmanager.input(f"{self.climdir}/PGD_prel.fa", "Neworog")

        # Part 0
        i = 0
        topo_files = ["Water_Percentage", "Oro_Mean", "Sigma", "Nb_Peaks",
                      "Urbanisation", "Dh_over_Dx_Dh_over_Dy",
                      "Dh_over_Dx_square", "Dh_over_Dy_square",
                      "Hmax-HxH-Hmin_ov4"]
        for fname in topo_files:
            self.fmanager.input(f"@E923_DATA@/GTOPT030/{fname}", fname)

        nam = self.load_namelist(i)
        self.write_namelist(nam)
        self.print_part(0)
        self.myexec(self.master, 0)

        # Cleanup
        self.remove_links(['Neworog'])
        os.rename("Const.Clim", "Neworog")

        # Part 1
        i = 1
        nam = self.load_namelist(i)
        self.write_namelist(nam)
        self.print_part(1)
        self.myexec(self.master, 1)
        self.remove_links(topo_files)

        # Part 2
        i = 2
        nam = self.load_namelist(i)
        self.write_namelist(nam)
        ifiles = ["itp_GL", "alb_GL", "emi_GL", "dps_GL",
                  "arg_GL", "sab_GL", "vgx_GL", "dpr_GL"]
        for fname in ifiles:
            self.fmanager.input(f"@E923_DATA@/SURFACE_G/{fname}", fname)

        self.print_part(2)
        self.myexec(self.master, 2)
        self.remove_links(ifiles)

        self.fmanager.output("Const.Clim", constant_file, provider_id="copy")

    def monthly_part(self, constant_file):
        """Run the monthly part of e923.

        Args:
            constant_file : filename of the input constant file
        """
        # Make sure constant file is in wdir
        if not os.path.exists("Const.Clim"):
            self.fmanager.input(constant_file, "Const.Clim", provider_id="copy")

        for ifile in ["MCICA", "RADSRTM"]:
            self.fmanager.input(f"@RRTM_DIR@/{ifile}", ifile)

        # Part 3 expects 12 input files, silly...
        i = 3
        for mm in self.months:
            shutil.copy(constant_file, f"Const.Clim.{mm}")

        for ifile in ["N108_GL"]:
            self.fmanager.input(f"@E923_DATA@/N108/{ifile}", ifile)

        nam = self.load_namelist(i)
        self.write_namelist(nam)
        self.print_part(i)
        self.myexec(self.master, i)

        for ifile in ["z0v_GL", "alv_GL", "rsm_GL"]:
            self.fmanager.input(f"@E923_DATA@/SURFACE_G/{ifile}", ifile)

        for ifile in ["msk_HR", "itp_HR", "dpr_HR", "rsm_HR", "vgx_HR", "alv_HR",
                      "z0v_HR"]:
            self.fmanager.input(f"@E923_DATA@/SURFACE_L/{ifile}", ifile)

        for ifile in ["rel_GL.Z"]:
            self.fmanager.input(f"@E923_DATA@/CLIM_G/v2/{ifile}", ifile,
                                provider_id="copy")
        os.system("gunzip rel_GL.Z")  # noqa

        for mm in self.months:

            os.rename(f"Const.Clim.{mm}", "Const.Clim")

            # PART 4
            files = []
            for vegtype in ["veg", "lai"]:
                source = f"@E923_DATA@/SURFACE_G/{vegtype}_{mm}_GL"
                target = f"{vegtype}_GL"
                files.append(target)
                self.fmanager.input(source, target)

            nam = self.load_namelist(4)
            self.write_namelist(nam)
            self.print_part(4, mm)
            self.myexec(self.master, 4)
            self.remove_links(files)

            # PART 5
            files = []
            for vegtype in ["veg", "lai"]:
                source = f"@E923_DATA@/SURFACE_L/{vegtype}_{mm}_HR"
                target = f"{vegtype}_HR"
                files.append(target)
                self.fmanager.input(source, target)

            nam = self.load_namelist(5)
            self.write_namelist(nam)
            self.print_part(5, mm)
            self.myexec(self.master, 5)
            self.remove_links(files)

            # PART 6
            indata = ["tpl", "wpl", "snl"]
            for x in indata:
                target = f"{x}_GL.Z"
                source = f"@E923_DATA@/CLIM_G/v2/{x}_{mm}_GL.Z"
                self.fmanager.input(source, target, provider_id="copy")

            for x in indata:
                os.system(f"gunzip -f {x}_GL.Z")  # noqa

            self.fmanager.input('tpl_GL', 'tsl_GL', provider_id="copy")
            self.fmanager.input('wpl_GL', 'wsl_GL', provider_id="copy")

            nam = self.load_namelist(6)
            self.write_namelist(nam)
            self.print_part(6, mm)
            self.myexec(self.master, 6)

            # PART 8
            self.fmanager.input(f"@E923_DATA@/abc_O3/abc_quadra_{mm}", 'abc_coef')
            nam = self.load_namelist(8)
            self.write_namelist(nam)
            self.print_part(8, mm)
            self.myexec(self.master, 8)

            # PART 9
            self.fmanager.input(f"@E923_DATA@/aero_tegen/aero.tegen.m{mm}_GL",
                                'aero_GL')

            nam = self.load_namelist(9)
            self.write_namelist(nam)
            self.print_part(9, mm)
            self.myexec(self.master, 9)

            # Finished. Archive output
            self.fmanager.output("Const.Clim", f"Const.Clim.{mm}")

    def execute(self):
        """Run task.

        Define run sequence.

        """
        os.makedirs(self.climdir, exist_ok=True)

        constant_file = f"{self.climdir}/Const.Clim.const"

        self.logger.debug('Constant file:%s', constant_file)

        if not os.path.isfile(constant_file):
            # Run the constant part
            self.constant_part(constant_file)

        # Run the monthly part
        self.monthly_part(constant_file)

        # Store the data
        for mm in self.months:
            source = f'Const.Clim.{mm}'
            target = f'{self.climdir}/Const.Clim.{mm}'
            self.fmanager.output(source, target)


class PgdUpdate(Task):
    """PgdUpdate.

    Ensure consistency in orography between the atmosphere and surfex
    Read gridpoint orography from the atmospheric file
    and insert it in the PGD file Const.Clim.sfx.
    """

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, 'PgdUpdate')

        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")
        self.climdir = self.platform.get_system_value('climdir')
        self.namelists = self.platform.get_platform_value("NAMELISTS")
        self.gl = f"{self.platform.get_system_value('bindir')}/gl"  # noqa

    def execute(self):
        """Run task."""
        outfile = 'Const.Clim.sfx'
        for ifile in ['Const.Clim.const', 'PGD_prel.fa']:
            self.fmanager.input(f"{self.climdir}/{ifile}", ifile, provider_id='copy')
            self.fmanager.input(f"{self.namelists}/namgl_pgd_update", 'namgl')

        self.fmanager.input(f"{self.climdir}/{ifile}", outfile, provider_id='copy')
        # Run MASTERODB
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(f"{self.gl} -n namgl")

        self.fmanager.output(outfile, f"{self.climdir}/{outfile}")
