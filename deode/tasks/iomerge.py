"""IO merge task."""
import datetime
import glob
import os
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
        Task.__init__(self, config, "IOmerge")

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
        self.ionr = int(config["task.args.ionr"])
        self.archive = self.platform.get_system_value("archive")
        self.deode_home = self.config["platform.deode_home"]
        self.file_templates = self.config["file_templates"]
        self.nproc_io = config.get("submission.task_exceptions.Forecast.NPROC_IO", 0)

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

    def wait_for_io(self, filetype, lt, fc_path="../Forecast"):
        """Wait for all io_server output to be stable.

        Args:
            filetype (str): kind of output file
            lt (timeDelta): lead time
            fc_path (str): path to forecast directory
        """
        ftemplate = self.file_templates[filetype]["model"]
        validtime = self.basetime + lt
        filename = self.platform.substitute(ftemplate, validtime=validtime)

        file_list = []
        if filetype == "history":
            age_limit = 20
            for io in range(self.nproc_io):
                iopath = f"io_serv.{io+1:06}.d"
                file_list += glob.glob(f"{fc_path}/{iopath}/{filename}.speca.*")
                file_list += glob.glob(f"{fc_path}/{iopath}/{filename}.gridall")
        elif filetype == "surfex":
            age_limit = 15
            file_list += [f"{fc_path}/{filename}"]
            for io in range(self.nproc_io):
                iopath = f"io_serv.{io+1:06}.d"
                file_list += glob.glob(f"{fc_path}/{iopath}/{filename}")
        elif filetype == "fullpos":
            age_limit = 15
            # FIXME: what if we have fullpos output in FA format?
            for io in range(self.nproc_io):
                iopath = f"io_serv.{io+1:06}.d"
                file_list += glob.glob(f"{fc_path}/{iopath}/{filename}.hfp")
        else:
            logger.error("Bad file_type {}", filetype)

        for ff in file_list:
            self.wait_for_file(ff, age_limit)

    def merge_output(self, lt, fc_path="../Forecast"):
        """Merge distributed forecast model output.

        Args:
            lt (timeDelta): lead time
            fc_path (str): path to forecast directory

        """
        logger.info("Merge_output called at lt = {}", lt)
        io_path = f"{fc_path}/io_serv*.d"

        for filetype, oi in self.output_settings.items():
            if filetype not in list(self.file_templates.keys()):
                continue

            #   NOTE: various output types (history, surfex, fullpos)
            #   may be created at other time intervals
            #   so we have to check validtime for every filetype
            dt_list = oi2dt_list(oi, self.forecast_range)
            if lt not in dt_list:
                logger.info("{} not required at time {}", filetype, lt)
                continue

            ftemplate = self.file_templates[filetype]["model"]
            ftemplate_out = self.file_templates[filetype]["archive"]
            validtime = self.basetime + lt
            filename = self.platform.substitute(ftemplate, validtime=validtime)
            filename_out = self.platform.substitute(ftemplate_out, validtime=validtime)
            logger.info("Merging file {}", filename)

            self.wait_for_io(filetype, lt, fc_path=fc_path)

            if filetype == "history":
                lfitools = self.get_binary("lfitools")
                cmd = f"{lfitools} facat all {io_path}/{filename}.gridall "
                cmd += f"{io_path}/{filename}.speca* {filename}"
                logger.debug(cmd)
                BatchJob(os.environ, wrapper="").run(cmd)

            elif filetype == "surfex":
                # NOTE: .sfx also has a part in the working directory,
                #        so you *must* change the name
                lfitools = self.get_binary("lfitools")
                cmd = f"{lfitools} facat all {fc_path}/{filename} "
                cmd += f"{io_path}/{filename} {filename}"
                logger.debug(cmd)
                BatchJob(os.environ, wrapper="").run(cmd)

            elif filetype == "fullpos":
                # FIXME: fullpos output /can/ be FA or GRIB2
                # Fullpos (grib2) output has .hfp as extra file extension
                cmd = f"cat {io_path}/{filename}*.hfp > {filename}"
                logger.debug(cmd)
                BatchJob(os.environ, wrapper="").run(cmd)
            else:
                logger.error("Unsupported filetype {}", filetype)
                continue

            # write output to archive (or to the Forecast directory...)
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

        fc_path = "../Forecast"
        # NOTE: this is a link to the current Forecast directory.
        #       you could manipulate via os.path.basename(os.path.realpath(fc_path))
        #       e.g. to get the real path name or even add "Finished_task_Forecast_".
        io_path = f"{fc_path}/io_serv.000001.d"
        echis = f"{io_path}/ECHIS"

        full_dt_list = []
        oi_list = []

        # build the list of lead times (all output types combined):
        # NOTE: like in the Forecast task, we assume that a filetype is
        #       in output_settings AND in file_templates.
        #       Alternatively, ["history", "fullpos", "surfex"] could be
        #       hard coded.
        for filetype, oi in self.output_settings.items():
            if filetype in list(self.file_templates.keys()):
                logger.debug("filetype: {}, oi: {}", filetype, oi)
                if oi not in oi_list:
                    oi_list += oi
                    full_dt_list += oi2dt_list(oi, self.forecast_range)
        # remove duplicates and sort
        full_dt_list = list(set(full_dt_list))
        full_dt_list.sort()
        # now subset for this worker:
        dt_list = full_dt_list[self.ionr :: self.n_io_merge]
        logger.info("IONR = {}, N_IO_MERGE = {}", self.ionr, self.n_io_merge)
        logger.info("DT list {}", dt_list)

        # We must wait for the io_server output to appear
        # BUT: how long should we wait before aborting?
        logger.info("Waiting for forecast output.")
        while not os.path.exists(echis):
            sleep(5)

        logger.info("IO_SERVER {} detected!", echis)

        # Now we wait for the output times allotted to this IO worker.
        for dt in dt_list:
            logger.info("Waiting for {}", dt)
            while True:
                with open(f"{io_path}/ECHIS", "r") as ll:
                    lt = ll.read()
                    hh, mm, ss = [int(x) for x in lt.split(":")]
                cdt = datetime.timedelta(hours=hh, minutes=mm, seconds=ss)
                if dt > cdt:
                    sleep(5)
                else:
                    break

            # we have reached the next 'merge time'
            logger.info("lead time {} is available", dt)
            # NOTE: merge_output will first wait for files to be "complete"
            self.merge_output(dt, fc_path=fc_path)

        # signal completion of all merge tasks to the forecast task
        BatchJob(os.environ, wrapper="").run(f"touch {fc_path}/io_merge_{self.ionr:02}")
