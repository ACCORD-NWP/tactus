"""E923."""

from .base import Task
from deode.tasks.batch import BatchJob
import os
import shutil
import f90nml


class E923(Task):
    """Forecast task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.pgd_path = self.fmanager.platform.substitute(config.get_value('platform.PGD_PATH'))
        self.e923_path = self.fmanager.platform.substitute(config.get_value('platform.E923_PATH'))
        self.outdir = config.get_value('system.archive')
        self.caero = "tegen"
        self.months = ['{:02d}'.format(mm) for mm in range(1, 13)]

        self.e923_data = '{}/E923_DATA'.\
                         format(config.get_value('platform.CLIMATE_INPUT'))

        self.rrtm_files = {"path": config.get_value('platform.RRTM_DIR'),
                           "files": ["MCICA", "RADSRTM"]}

        self.cdb = {"path": self.e923_data,
                    "dirs": ["GTOPT030", "GTOPT030", "SURFACE_G", "N108",
                             None, "SURFACE_L", "CLIM_G/v2",
                             None, "abc_O3", "aero_tegen"]}

        part_0_and_1 = ["Water_Percentage", "Oro_Mean", "Sigma", "Nb_Peaks",
                        "Urbanisation", "Dh_over_Dx_Dh_over_Dy",
                        "Dh_over_Dx_square", "Dh_over_Dy_square",
                        "Hmax-HxH-Hmin_ov4"]

        self.cdf = [part_0_and_1,
                    part_0_and_1, ["itp_GL", "alb_GL", "emi_GL", "dps_GL",
                                   "arg_GL", "sab_GL", "vgx_GL", "dpr_GL"]]

        self.namelists = {"path": self.pgd_path,
                          "files": ["nam923_1_smoothing",
                                    "nam923_1", "nam923_2", "nam923_3",
                                    "nam923_4", "nam923_5", "nam923_6",
                                    "nam923_7", "nam923_8", "nam923_9"]}

        self.master = os.path.join(self.platform.substitute(config.get_value('platform.BINDIR')), 'MASTERODB')

    def load_namelist(self, i):
        """Read and adjust namelist.

        Args:
            i (int) : sequence number

        Returns :
            nam (f90nml object): loaded namelist
        """
        namelist = '{}/{}'.format(self.namelists["path"],
                                  self.namelists["files"][i])
        self.logger.info("Read namelist: %s", namelist)
        nam = f90nml.read(namelist)
        nam.uppercase = True
        nam.end_comma = True
        return nam

    def myexec(self, cmd):
        """Execute binary task."""
        batch = BatchJob(os.environ, "")
        batch.run(cmd)

        if os.path.exists("NODE.001_01"):
            os.system("ls -lrt ; cat NODE.001_01")  # noqa

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

    def wrap_input(self, struct):
        """Run the constant part of e923.

        Args:
            struct : file/path dict
        """
        for f in struct['files']:
            ff = os.path.join(struct['path'], f)
            self.fmanager.input(ff, f)

    def constant_part(self, constant_file):
        """Run the constant part of e923.

        Args:
            constant_file : filename of the resulting file
        """
        self.logger.info("Create:%s", constant_file)
        self.wrap_input(self.rrtm_files)

        # PGD input
        self.fmanager.input('{}/PGD_prel.fa'.format(self.pgd_path), "Neworog")

        # Part 0
        i = 0
        d = {"path": '{}/{}'.format(self.cdb["path"],
                                    self.cdb["dirs"][i]), "files": self.cdf[i]}
        self.wrap_input(d)
        nam = self.load_namelist(i)
        self.write_namelist(nam)
        self.print_part(0)
        self.myexec(self.master)

        # Cleanup
        self.remove_links(['Neworog'])
        os.rename("Const.Clim", "Neworog")

        # Part 1
        i = 1
        nam = self.load_namelist(i)
        self.write_namelist(nam)
        self.print_part(1)
        self.myexec(self.master)
        self.remove_links(self.cdf[i])

        # Part 2
        i = 2
        nam = self.load_namelist(i)
        self.write_namelist(nam)
        d = {"path": '{}/{}'.format(self.cdb["path"],
                                    self.cdb["dirs"][i]), "files": self.cdf[i]}
        self.wrap_input(d)
        self.print_part(2)
        self.myexec(self.master)
        self.remove_links(self.cdf[i])

        self.fmanager.output("Const.Clim", constant_file, provider_id="copy")

    def monthly_part(self, constant_file, months):
        """Run the monthly part of e923.

        Args:
            constant_file : filename of the input constant file
            months : list of months to process
        """
        self.wrap_input(self.rrtm_files)

        # Part 3 expects 12 input files, silly...
        i = 3
        for mm in range(1, 13):
            shutil.copy(constant_file, 'Const.Clim.{:02d}'.format(mm))

        d = {"path": '{}/{}'.format(self.cdb["path"], self.cdb["dirs"][i]),
             "files": ["N108_GL"]}
        self.wrap_input(d)

        nam = self.load_namelist(i)
        self.write_namelist(nam)
        self.print_part(i)
        self.myexec(self.master)

        d = {"path": '{}/{}'.format(self.cdb["path"], self.cdb["dirs"][2]),
             "files": ["z0v_GL", "alv_GL", "rsm_GL"]}
        self.wrap_input(d)

        d = {"path": '{}/{}'.format(self.cdb["path"], self.cdb["dirs"][5]),
             "files": ["msk_HR", "itp_HR", "dpr_HR",
                       "rsm_HR", "vgx_HR", "alv_HR", "z0v_HR"]}

        self.wrap_input(d)

        self.fmanager.input(os.path.join(self.cdb["path"], self.cdb["dirs"][6], "rel_GL.Z"),
                            "rel_GL.Z", provider_id="copy")
        os.system("gunzip rel_GL.Z")  # noqa

        for i in months:

            mm = '{:02d}'.format(int(i))

            os.rename('Const.Clim.{}'.format(mm), "Const.Clim")

            for k, v in {4: [2, 'GL'], 5: [5, 'HR']}.items():
                data = {'{}_{}'.format(x, v[1]) : '{}/{}/{}_{}_{}'.format(self.e923_data, self.cdb["dirs"][v[0]], x, mm, v[1]) for x in ["veg", "lai"]}
                for target, source in data.items():
                    self.fmanager.input(source, target)

                nam = self.load_namelist(k)
                self.write_namelist(nam)
                self.print_part(k, mm)
                self.myexec(self.master)
                self.remove_links(data.keys())

            indata = ["tpl", "wpl", "snl"]
            clean_list = []
            for x in indata:
                target = '{}_GL.Z'.format(x)
                source = '{}/{}/{}_{}_GL.Z'.format(self.e923_data, self.cdb["dirs"][6], x, mm)
                self.fmanager.input(source, target, provider_id="copy")
                clean_list.append(target.replace(".Z", ""))

            for x in indata:
                os.system('gunzip -f {}_GL.Z'.format(x))  # noqa
            self.fmanager.input('tpl_GL', 'tsl_GL', provider_id="copy")
            self.fmanager.input('wpl_GL', 'wsl_GL', provider_id="copy")
            clean_list.append('tsl_GL')
            clean_list.append('wsl_GL')

            nam = self.load_namelist(6)
            self.write_namelist(nam)
            self.print_part(6, mm)
            self.myexec(self.master)

            self.fmanager.input('{}/{}/abc_quadra_{}'.format(self.e923_data, self.cdb["dirs"][8], mm), 'abc_coef')
            clean_list.append('abc_coef')
            nam = self.load_namelist(8)
            self.write_namelist(nam)
            self.print_part(8, mm)
            self.myexec(self.master)

            self.fmanager.input('{}/{}/aero.{}.m{}_GL'.
                                format(self.e923_data, self.cdb["dirs"][9], self.caero, mm), 'aero_GL')
            clean_list.append('aero_GL')

            nam = self.load_namelist(9)
            self.write_namelist(nam)
            self.print_part(9, mm)
            self.myexec(self.master)
            self.remove_links(clean_list)

            self.fmanager.output("Const.Clim", 'Const.Clim.{}'.format(mm))

    def execute(self):
        """Run task.

        Define run sequence.

        """
        os.makedirs(self.e923_path, exist_ok=True)

        constant_file = os.path.join(self.e923_path, "Const.Clim.const")

        self.logger.debug('Constant file:%s', constant_file)

        if not os.path.isfile(constant_file):
            # Run the constant part
            self.constant_part(constant_file)

        # Run the monthly part
        self.monthly_part(constant_file, self.months)

        # Store the data
        for mm in self.months:
            source = 'Const.Clim.{:02d}'.format(int(mm))
            target = '{}/Const.Clim.{:02d}'.format(self.e923_path, int(mm))
            self.fmanager.output(source, target)
