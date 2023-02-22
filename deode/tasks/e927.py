"""E927."""

from .base import Task
from deode.tasks.batch import BatchJob
from deode.datetime_utils import as_datetime
import f90nml
from isoduration import parse_duration
import os


class E927(Task):
    """E927 task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __name__)

        self.wrapper = self.config.get_value(f"task.{self.name}.wrapper")
        self.climdir = self.platform.get_system_value('climdir')
        self.basetime = as_datetime(self.config.get_value("general.times.basetime"))
        self.bdint = self.config.get_value("general.bdint")
        self.cnmexp = self.config.get_value("general.cnmexp")
        self.forecast_range = self.config.get_value("general.forecast_range")
        self.bdclimdir = self.platform.get_system_value('bdclimdir')
        self.bdfile_template = self.platform.get_system_value('bdfile_template')
        self.bddir = self.platform.get_system_value('bddir')

        self.namelist_path = self.platform.get_path('NAMELISTS')
        self.master = f"{self.platform.get_system_value('bindir')}/MASTERODB"  # noqa

    def load_namelist(self, namelist):
        """Read and adjust namelist.

        Args:
            namelist (str) : namelist file

        Returns :
            nam (f90nml object): loaded namelist
        """
        self.logger.info("Read namelist: %s", namelist)
        nam = f90nml.read(namelist)
        nam.uppercase = True
        nam.end_comma = True
        return nam

    def myexec(self, cmd):
        """Execute binary task."""
        batch = BatchJob(os.environ, wrapper=self.wrapper)
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

    def expand(self, struct):
        """Expand path, file structure.

        Args:
            struct (dict) : containing path and files

        Returns:
            res (dict) : expanded paths
        """
        p = struct["path"]
        res = {"link": ['{}/{}'.format(p, v) for v in struct["files"]]}
        return res

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # RTTM input
        for ifile in ["MCICA", "RADSRTM"]:
            self.fmanager.input(f"@RRTM_DIR@/{ifile}", ifile)

        # Climate files
        mm = self.basetime.strftime("%m")
        self.fmanager.input("{}/Const.Clim.{}".format(self.climdir, mm), 'const.clim.000')
        self.fmanager.input("{}/Const.Clim.{}".format(self.bdclimdir, mm), 'Const.Clim')

        # Namelist
        namelist = f"{self.namelist_path}/fort.4_e927"
        nam = self.load_namelist(namelist)
        self.write_namelist(nam)

        # Forecast range
        cdtg = self.basetime
        dtgend = self.basetime + parse_duration(self.forecast_range)
        i = 0
        while cdtg <= dtgend:

            # Input file
            initfile = f'ICMSH{self.cnmexp}INIT'
            self.fmanager.input("{}/{}".format(self.bddir, 'PFIFSDEOL+@LLLL@'), initfile, basetime=self.basetime, validtime=cdtg)
            self.myexec(self.master)
            target = '{}/ELSCF{}ALBC{:03d}'.format(self.wrk, self.cnmexp, i)
            self.fmanager.output(f'PF{self.cnmexp}000+0000', target)
            self.remove_links([initfile, 'ncf927'])
            cdtg += parse_duration(self.bdint)
            i += 1
