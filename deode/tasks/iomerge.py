"""IO merge task."""
import datetime
import glob
import os
from pathlib import Path
from time import sleep, time

from ..datetime_utils import as_datetime, as_timedelta, oi2dt_list
from ..logs import logger
from .base import Task
from .batch import BatchJob


class IOmerge(Task):
    """IO merge task."""

    def __init__(self, config):
        """Construct IOmerge object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.cycle = self.config["general.cycle"]
        self.cnmexp = self.config["general.cnmexp"]
        self.domain = self.config["domain.name"]

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        self.forecast_range = self.config["general.times.forecast_range"]

        self.deode_home = self.config["platform.deode_home"]
        self.output_settings = self.config["general.output_settings"]
        self.surfex = self.config["general.surfex"]

        self.n_io_merge = self.config["suite_control.n_io_merge"]
        self.ionr = int(config.get("task.args.tasknr", "0"))
        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config["platform.deode_home"]
        self.file_templates = self.config["file_templates"]
        self.nproc_io = int(self.config.get("task.args.nproc_io", "0"))
        self.iomerge = self.config["submission.iomerge"]

        self.fc_path = self.config["system.forecast_dir_link"]
        # NOTE: this is a link to the current Forecast directory.
        #       you could manipulate via os.path.basename(os.path.realpath(fc_path))
        #       e.g. to get the real path name or even add "Finished_task_Forecast_".
        self.age_limit_forecast_directory = self.iomerge["age_limit"][
            "forecast_directory"
        ]

        self.name = f"{self.name}_{self.ionr:02}"

    def check_fc_path(self):
        """Check if fc_path exists.

        Raises:
            FileNotFoundError: If directory is not present

        """
        fc_path_resolved = Path(self.fc_path).resolve()
        if not os.path.exists(fc_path_resolved):
            raise FileNotFoundError(
                f"Forecast directory does not exist: {fc_path_resolved}"
            )

    @staticmethod
    def wait_for_file(filename, age_limit=15):
        """Wait until a file is at least N seconds old.

        Args:
            filename (str): full path to file
            age_limit (int): minimum age of last modification
        """
        now = time()
        st = os.stat(filename).st_mtime
        while now - st < age_limit:
            sleep(age_limit + now - st + 1)
            now = time()
            st = os.stat(filename).st_mtime

    def wait_for_io(self, filetype, lt, maxtries=2):
        """Wait for all io_server output to be stable.

        Args:
            filetype (str): kind of output file
            lt (timeDelta): lead time
            maxtries (int): maximum number of file search trials

        Returns:
            file_list (list): List of files expected

        Raises:
            RuntimeError: In case of erroneous number of files

        """
        ftemplate = self.file_templates[filetype]["model"]
        age_limit = self.iomerge["age_limit"][filetype]
        files_expected = self.iomerge["files_expected"][filetype]

        validtime = self.basetime + lt
        filename = self.platform.substitute(ftemplate, validtime=validtime)

        ntries = 0
        while True:
            ntries += 1
            file_list = []
            if filetype == "history":
                files_expected = files_expected if files_expected != 0 else -1
                for io in range(self.nproc_io):
                    iopath = f"io_serv.{io+1:06}.d"
                    file_list += glob.glob(f"{self.fc_path}/{iopath}/{filename}.speca.*")
                    file_list += glob.glob(f"{self.fc_path}/{iopath}/{filename}.gridall")
            elif filetype == "surfex":
                files_expected = (
                    files_expected if files_expected != 0 else (1 + self.nproc_io)
                )
                file_list += [f"{self.fc_path}/{filename}"]
                for io in range(self.nproc_io):
                    iopath = f"io_serv.{io+1:06}.d"
                    file_list += glob.glob(f"{self.fc_path}/{iopath}/{filename}")
            elif filetype == "fullpos":
                files_expected = files_expected if files_expected != 0 else self.nproc_io
                # FIXME: what if we have fullpos output in FA format?
                for io in range(self.nproc_io):
                    iopath = f"io_serv.{io+1:06}.d"
                    file_list += glob.glob(f"{self.fc_path}/{iopath}/{filename}.hfp")
            else:
                files_expected = 0
                logger.error("Bad file_type {}", filetype)

            for ff in file_list:
                self.wait_for_file(ff, age_limit)

            files_found = len(file_list)
            if files_found == files_expected or files_expected < 0:
                break

            if ntries == maxtries:
                logger.error(
                    "Expected {} files found {} after {} scans",
                    files_expected,
                    files_found,
                    ntries,
                )
                raise RuntimeError(f"Expected {files_expected} files found {files_found}")

        return file_list

    def merge_output(self, lt, filetype):
        """Merge distributed forecast model output.

        Args:
            lt (timeDelta): lead time
            filetype (str): Type of file

        """
        logger.info("merge_output called at lt = {}, filetype = {}", lt, filetype)

        ftemplate = self.file_templates[filetype]["model"]
        ftemplate_out = self.file_templates[filetype]["archive"]
        validtime = self.basetime + lt
        filename = self.platform.substitute(ftemplate, validtime=validtime)
        filename_out = self.platform.substitute(ftemplate_out, validtime=validtime)
        logger.info("Merging file {}", filename)
        self.check_fc_path()

        file_list = self.wait_for_io(filetype, lt)
        files = " ".join(file_list)
        logger.info("{} input files:", len(file_list))
        for x in file_list:
            logger.info("  {}", x)

        if filetype in ("history", "surfex"):
            lfitools = self.get_binary("lfitools")

            # NOTE: .sfx also has a part in the working directory,
            #        so you *must* change the name
            cmd = f"{lfitools} facat all {files} {filename}"

        elif filetype == "fullpos":
            # FIXME: fullpos output /can/ be FA or GRIB2
            # Fullpos (grib2) output has .hfp as extra file extension
            cmd = f"cat {files} > {filename}"
        else:
            logger.error("Unsupported filetype {}", filetype)
            cmd = None

        if cmd is not None:
            BatchJob(os.environ, wrapper="").run(cmd)

            # Write output to archive (or to the Forecast directory...)
            self.fmanager.output(filename, f"{self.archive}/{filename_out}")

    def execute(self):
        """Execute IO merge."""
        if self.nproc_io == 0:
            # Normally, this shouldn't happen, but maybe outside of ecFlow.
            logger.info("IOmerge called with NPROC_IO == 0.")
            logger.info("Nothing left to do.")
            return
        if self.n_io_merge == 0:
            # Normally, this shouldn't happen, but maybe outside of ecFlow.
            logger.info("IOmerge called with n_io_merge == 0")
            logger.info("Nothing left to do.")
            return
        if self.ionr < 0:
            # Normally, this shouldn't happen, but maybe outside of ecFlow.
            logger.info("IOmerge called with ionr < 0")
            logger.info("Nothing left to do.")
            return

        io_path = f"{self.fc_path}/io_serv.000001.d"
        echis = f"{io_path}/ECHIS"

        # build the list of lead times (all output types combined):
        # NOTE: like in the Forecast task, we assume that a filetype is
        #       in output_settings AND in file_templates.
        #       Alternatively, ["history", "fullpos", "surfex"] could be
        #       hard coded.
        dt_list = {}
        for filetype, oi in self.output_settings.items():
            if filetype in list(self.file_templates.keys()):
                logger.debug("filetype: {}, oi: {}", filetype, oi)
                for dt in oi2dt_list(oi, self.forecast_range):
                    if dt not in dt_list:
                        dt_list[dt] = []
                    dt_list[dt].append({"dt": dt, "filetype": filetype})

        full_merge_list = [x for dt in sorted(dt_list) for x in dt_list[dt]]

        # Now subset for this worker:
        merge_list = full_merge_list[self.ionr :: self.n_io_merge]
        logger.debug("IONR = {}, N_IO_MERGE = {}", self.ionr, self.n_io_merge)
        logger.debug("merge_list {}", merge_list)

        # We must wait for the io_server output to appear
        # BUT: how long should we wait before aborting?
        logger.info("Waiting for forecast output.")
        start = time()
        waiting_time = 0
        while not os.path.exists(echis):
            sleep(5)
            now = time()
            waiting_time = now - start
            if waiting_time > self.age_limit_forecast_directory:
                raise FileNotFoundError(
                    "Forecast output has not appeared after "
                    f"{self.age_limit_forecast_directory}s"
                )

        logger.info("IO_SERVER {} detected!", echis)

        # Now we wait for the output times allotted to this IO worker.
        for items in merge_list:
            dt, filetype = items["dt"], items["filetype"]
            logger.info("Waiting for {} {}", dt, filetype)
            while True:
                self.check_fc_path()
                with open(f"{io_path}/ECHIS", "r") as ll:
                    lt = ll.read()
                    hh, mm, ss = [int(x) for x in lt.split(":")]
                cdt = datetime.timedelta(hours=hh, minutes=mm, seconds=ss)
                if dt > cdt:
                    sleep(5)
                else:
                    break

            # we have reached the next 'merge time'
            # NOTE: merge_output will first wait for files to be "complete"
            start = time()
            self.merge_output(dt, filetype)
            end = time()
            logger.info(
                "Merge of {} for {} took {} seconds", dt, filetype, int(end - start)
            )

        # signal completion of all merge tasks to the forecast task
        BatchJob(os.environ, wrapper="").run(
            f"touch {self.fc_path}/io_merge_{self.ionr:02}"
        )

        if full_merge_list[-1] == items:
            logger.info("Remove Forecast directory since all files have been processed")
            # Wait for all io_merge tasks to finish.
            # This is signaled by creating (empty) files.
            for ionr in range(self.n_io_merge):
                io_name = f"{self.fc_path}/io_merge_{ionr:02}"
                while not os.path.exists(io_name):
                    logger.info("Waiting for {}", io_name)
                    sleep(5)

            # Handle the Forecast task working directory
            fc_path_resolved = Path(self.fc_path).resolve()
            logger.debug("fc_path_resolved:{}", fc_path_resolved)

            if os.path.islink(self.fc_path):
                os.unlink(self.fc_path)
            self.post(source=fc_path_resolved, target="Forecast")
