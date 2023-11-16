"""Marsprep."""
import ast
import contextlib
import glob
import os
import shutil
from datetime import datetime

import pandas as pd

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..geo_utils import Projection, Projstring
from ..logs import logger
from ..os_utils import deodemakedirs
from ..tasks.batch import BatchJob
from .base import Task


class Marsprep(Task):
    """Marsprep task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration
        Raises:
            ValueError: No data for this date.
        """
        Task.__init__(self, config, __name__)

        # Get paths
        self.selection = self.config["boundaries.ifs.selection"]
        try:
            self.mars = self.config[f"mars.{self.selection}"]
        except KeyError:
            # This experiment is note defined fallback to RD_DEFAULT
            self.mars = self.config[f"mars.RD_DEFAULT"]
            self.mars["expver"] = self.selection
            logger.warning("SELECTION={} not defined, using RD_DEFAULT", self.selection)

        self.sfcdir = self.config["system.global_sfcdir"]
        # Get boundary strategy
        self.strategy = self.config["boundaries.bdstrategy"]
        # Get output file template
        self.template = "mars_prefetch_[levtype]_[date]_[time]+[step]"

        # Get the times from config.toml
        self.basetime = as_datetime(self.config["general.times.basetime"])
        start_date = as_datetime(self.mars["start_date"])
        if self.basetime < start_date:
            raise ValueError(f"No data for {self.basetime}!")
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        # Get forecast range
        self.forecast_range = as_timedelta(self.config["general.times.forecast_range"])
        # Get boundary shift
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        self.bdshift = as_timedelta(self.config["boundaries.bdshift"])
        self.bdcycle = as_timedelta(self.config["boundaries.bdcycle"])
        self.int_bdcycle = int(self.bdcycle.total_seconds()) // 3600
        self.cy_offset = cycle_offset(self.basetime, self.bdcycle)

        self.unix_group = self.platform.get_platform_value("unix_group")

        bd_basetime = self.basetime - self.cy_offset

        self.prepdir = self.platform.substitute(
            self.config["system.marsdir"],
            basetime=bd_basetime,
            validtime=self.basetime,
        )
        self.prep_filename = self.platform.substitute(
            self.config["system.bdfile_sfx_template"],
            basetime=bd_basetime,
            validtime=self.basetime,
        )

        # Make linting happy
        self.data = b""
        self.executable = None

        # Make MARS requests follow the template as requested
        os.environ["MARS_MULTITARGET_STRICT_FORMAT"] = "1"

        # Debug start
        logger.info("Construct PREP/Mars task")

    @staticmethod
    def get_domain_data(config):
        """Read and return domain data.

        Args:
             config: config method

        Returns:
             String containing the domain info for MARS
        """
        # Get domain specs
        domain_spec = {
            "nlon": config["domain.nimax"],
            "nlat": config["domain.njmax"],
            "latc": config["domain.xlatcen"],
            "lonc": config["domain.xloncen"],
            "lat0": config["domain.xlat0"],
            "lon0": config["domain.xlon0"],
            "gsize": config["domain.xdx"],
        }

        # Get domain properties
        projstring = Projstring()
        projection = Projection(
            projstring.get_projstring(lon0=domain_spec["lon0"], lat0=domain_spec["lat0"])
        )
        domain_properties = projection.get_domain_properties(domain_spec)
        fdomainstr = "/".join(
            [
                str(domain_properties["maxlat"]),
                str(domain_properties["minlon"]),
                str(domain_properties["minlat"]),
                str(domain_properties["maxlon"]),
            ]
        )

        return fdomainstr

    def list_files_join(self, marsfolder, f_pattern):
        """Read and return file names based on given pattern.

        Args:
             marsfolder: path with MARS file location
             f_pattern: glob pattern

        Returns:
             list of files that should be joined
        """
        pattern_list = os.path.join(marsfolder, f_pattern)
        self.filenames = glob.glob(pattern_list)

        return self.filenames

    def check_value(self, value, key):
        """Check value according to key.
        - If a string returnts the value itself
        - If key is a date search for the most suitable match in value
        - Else return the value matching the key

        Args:
            value (str, BaseConfig object): Values to select
            key (str): key for value checking

        Returns:
            value (str): Found value

        Raises:
            ValueError: Exception
        """
        if isinstance(value, str):
            return value
        else:
            try:
                ref_date = as_datetime(f"{key}T00:00:00Z")
                for k, v in sorted(value.items(), reverse=True):
                    if ref_date >= as_datetime(k):
                        return v
            except ValueError:
                k = str(key)
                if k in value:
                    return value[k]

        raise ValueError(f"Value not found for {key} within {value}")

    def split_date(self, date, strategy, length, interval, bdshift):
        """Manipulate the dates for Mars request.

        Args:
            date:       full date provided for spliting into parts
            strategy:   valid boundary strategy
            length:     forecast cycle length in hours
            interval:   boundary interval in hours "1H","3H","6H"
            bdshift:    boundary shift in hours

        Returns:
            Pandas dataframe object

        Raises:
            ValueError: Boundary strategy is not implemented
        """
        # Check if the selected boundary strategy is valid
        if strategy in [
            "same_forecast",
        ]:
            # Check options for manipulating dates for MARS extraction
            # Still needs some more relations with the fclen here
            strategy_options = {strategy: {}}

            for i in range(self.int_bdcycle):
                strategy_options[strategy].update(
                    {i: {"shifthours": i + bdshift, "shiftrange": i, "pickrange": 0}}
                )

            # Check if we're dealing with 00 or 03 ... etc.
            init_hour = int(date.strftime("%H")) % self.int_bdcycle
            boundary_options = strategy_options[strategy][init_hour]

        else:
            raise ValueError("Invalid boundary strategy {}".format(strategy))
        # Create Pandas date series for MARS extraction
        interval_int = int(interval.total_seconds()) // 3600
        request_date_frame = pd.Series(
            pd.date_range(
                date - pd.Timedelta(hours=boundary_options["shifthours"]),
                periods=length // interval_int + 1,
                freq=interval,
            )
        )
        # Strip the series object according to the selected boundary strategy
        request_date_frame = pd.concat(
            [
                request_date_frame.iloc[boundary_options["pickrange"] :],
            ]
        )

        return request_date_frame

    # Create MARS request based on pre-defined parameters
    def update_data_request(
        self,
        param,
        data_type,
        date,
        time,
        steps,
        prefetch,
        levtype,
        specify_domain,
        target,
        grid=None,
    ):
        """Create ECMWF MARS system request.

        Args:
            param:              String of parameters to extract sep=/
            data_type:          Type of data to extract, 'analysis' or 'forecast'
            date:               Date to extract
            time:               Time to extract
            steps:              Forecast steps to be extracted
            prefetch:           Retrieve or stage
            levtype:            SFC or ML
            specify_domain:     Use lat/lon and rotation or use global (default)
            target:             Filename to write
            grid:               Specific grid for some request. Default None.

        Returns:
            Pandas dataframe object
        """
        # General request parameters
        d = {
            "CLASS": [self.mars["class"]],
            "EXPVER": [self.mars["expver"]],
            "LEVTYPE": [levtype],
            "DATE": [date],
            "TIME": [time],
            "STEP": [steps],
            "PARAM": [param],
            "TARGET": [target],
        }
        if grid is not None:
            d.update(
                {
                    "GRID": [grid],
                }
            )
        # Additional request parameters
        if data_type == "forecast":
            d.update(
                {
                    "TYPE": [self.mars["type_FC"]],
                }
            )
        elif data_type == "analysis":
            d.update(
                {
                    "TYPE": [self.mars["type_AN"]],
                }
            )
        try:
            bdmember = int(self.config["boundaries.ifs.bdmember"])
            d.update(
                {
                    "NUMBER": [self.config["boundaries.ifs.bdmember"]],
                }
            )
        except ValueError:
            pass

        if prefetch:
            d.update(
                {
                    "PROCESS": ["LOCAL"],
                }
            )

        # Try multilevel
        if levtype == "ML" and steps == "00" and prefetch:
            d.update(
                {
                    "LEVELIST": [1],
                }
            )

        elif levtype == "ML":
            levelist = self.check_value(self.mars["levelist"], date)
            logger.info("levelist:{}", levelist)
            d.update(
                {
                    "LEVELIST": [levelist],
                }
            )

        stream = self.check_value(self.mars["stream"], time)
        d["STREAM"] = [stream]

        if specify_domain:
            d.update({"GRID": [self.check_value(self.mars["grid"], date)]})
            d["AREA"] = [self.get_domain_data(self.config)]

        self.datarequest = pd.DataFrame(data=d)

        return self.datarequest

    # Write a MARS request into a file
    @staticmethod
    def write_mars_req(nam, name, method):
        """Write a request for MARS.

        Args:
            nam:    namelist object to write
            name:   request file name
            method: selected method, retrieve or stage
        """
        sep0 = "{:<2}".format("")
        sep1 = " = "
        sep2 = ","

        with open(name, "w") as f:
            f.write(str(method.upper()) + ",\n")

            for col in nam.columns:
                for _index, row in nam.iterrows():
                    # do not use the comma in the last line
                    if col == nam.columns[-1]:
                        row_str = sep0 + str(col) + sep1 + str(row[col])

                    else:
                        row_str = sep0 + str(col) + sep1 + str(row[col]) + sep2

                f.write(row_str + "\n")

            f.close()

    def create_executable(self, marsfile):
        """Create task for binary.

        Args:
            marsfile: intermediate request file

        Returns:
            None
        """
        self.executable = "{} {}".format("mars", marsfile)
        return self.executable

    def check_file_exists(self, steps, file_name):
        """Check if which mars file already exist."""
        base_list = []
        for step in steps:
            step1 = int(step)
            mars_file_check = os.path.join(self.prepdir, f"{file_name}+{step1:02d}")
            if not os.path.exists(mars_file_check):
                base_list.append(step)

        base = "/".join(base_list)
        return base

    def execute(self):
        """Run task.

        Define run sequence.

        Raises:
            RuntimeError: If there is an issue with the work folder.
        """
        try:
            # Part1
            if not os.path.exists(self.prepdir):
                deodemakedirs(self.prepdir, unixgroup=self.unix_group)
        except OSError as e:
            raise RuntimeError(f"Error while preparing the mars folder: {e}") from e

        # Get the time information based on boundary strategy and forecast length
        # Need to check the forecast range
        dateframe = self.split_date(
            self.basetime,
            self.strategy,
            int(self.forecast_range.days * 24 + self.forecast_range.seconds / 3600),
            self.bdint,
            int(self.bdshift.seconds / 3600),
        )
        # Get initial date information in detail
        date_str = dateframe.iloc[0].strftime("%Y%m%d")
        hour_str = dateframe.iloc[0].strftime("%H")

        # Format MARS steps
        step = int(self.bdint.total_seconds() / 3600)
        str_steps = [
            "{0:02d}".format(
                dateframe.index.tolist()[0]
                + (i * step)
                + self.basetime.hour % self.int_bdcycle
            )
            for i in range(len(dateframe.index.tolist()))
        ]

        #
        # Do full extraction for global (c903 approach).
        # Decided to split for easyer maintanance, still to be discussed
        #

        # Prefetch GG
        base = self.check_file_exists(str_steps, "ICMGG")
        if base != "":
            param = self.check_value(self.mars["GG"], date_str)
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=base,
                prefetch=True,
                levtype="SFC",
                param=param,
                specify_domain=False,
                target='"ICMGG+[STEP]"',
            )
            self.write_mars_req(self.datarequest, "ICMGG.req", "retrieve")
            self.create_executable("ICMGG.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            param = self.check_value(self.mars["GG_sea"], date_str)
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="SFC",
                param=param,
                specify_domain=False,
                target="ICMGG.sea",
            )
            self.write_mars_req(self.datarequest, "ICMGG.sea.req", "retrieve")
            self.create_executable("ICMGG.sea.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            with open("ICMGG.sea", "rb") as fp:
                datagg_sea = fp.read()
            fp.close()

            exist_soil = False
            with contextlib.suppress(KeyError):
                param1 = self.check_value(self.mars["GG_soil"], date_str)
                param2 = self.check_value(self.mars["GG1"], date_str)
                exist_soil = True

            if exist_soil:
                self.update_data_request(
                    data_type="analysis",
                    date=date_str,
                    time=hour_str,
                    steps="00",
                    prefetch=True,
                    levtype="SFC",
                    param=param1,
                    specify_domain=False,
                    target="ICMGG.soil",
                )

                self.write_mars_req(self.datarequest, "ICMGG.soil.req", "retrieve")
                self.create_executable("ICMGG.soil.req")
                batch = BatchJob(os.environ, wrapper=self.wrapper)
                batch.run(self.executable)

                self.update_data_request(
                    data_type="analysis",
                    date=date_str,
                    time=hour_str,
                    steps="00",
                    prefetch=True,
                    levtype="SFC",
                    param=param2,
                    grid=self.check_value(self.mars["grid_GG1"], date_str),
                    specify_domain=False,
                    target="ICMGG",
                )
                self.write_mars_req(self.datarequest, "ICMGG1.req", "retrieve")
                self.create_executable("ICMGG1.req")
                batch = BatchJob(os.environ, wrapper=self.wrapper)
                batch.run(self.executable)

                # Read the single-step MARS files first
                with open("ICMGG", "rb") as fp:
                    datagg = fp.read()
                with open("ICMGG.soil", "rb") as fp:
                    datagg_soil = fp.read()
                fp.close()

                os.remove("ICMGG")
                os.remove("ICMGG.soil")
                self.data += datagg + datagg_sea + datagg_soil

            else:
                tco = self.check_value(self.mars["tco"], date_str)
                self.fmanager.input(f"{self.sfcdir}{tco}9_4/sdfor", "sdfor")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_aluvp", "aluvp")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_aluvd", "aluvd")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_alnip", "alnip")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_alnid", "alnid")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_lail", "lail")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_laih", "laih")

                self.fmanager.input(f"{self.sfcdir}{tco}l_2/sfc", "sfc")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/ISSOIL", "issoil")
                self.fmanager.input(f"{self.sfcdir}{tco}9_4/cvh", "cvh")
                self.fmanager.input(f"{self.sfcdir}{tco}l_2/month_alb", "alb")

                with open("sdfor", "rb") as fp:
                    sdfor = fp.read()
                with open("aluvp", "rb") as fp:
                    aluvp = fp.read()
                with open("aluvd", "rb") as fp:
                    aluvd = fp.read()
                with open("alnip", "rb") as fp:
                    alnip = fp.read()
                with open("alnid", "rb") as fp:
                    alnid = fp.read()
                with open("lail", "rb") as fp:
                    lail = fp.read()
                with open("laih", "rb") as fp:
                    laih = fp.read()
                with open("ICMGG.sea", "rb") as fp:
                    datagg_sea = fp.read()
                with open("sfc", "rb") as fp:
                    sfc = fp.read()
                with open("issoil", "rb") as fp:
                    issoil = fp.read()
                with open("cvh", "rb") as fp:
                    cvh = fp.read()
                with open("alb", "rb") as fp:
                    alb = fp.read()
                fp.close()
                # Join the data from the single-step MARS files
                self.data += (
                    sdfor
                    + aluvp
                    + aluvd
                    + alnip
                    + alnid
                    + lail
                    + laih
                    + datagg_sea
                    + sfc
                    + issoil
                    + cvh
                    + alb
                )
            os.remove("ICMGG.sea")
            for i in base.split("/"):
                i = int(i)
                i_fstring = f"{i:02d}"
                if os.path.exists(f"ICMGG+{i}"):
                    with open(f"ICMGG+{i}", "ab") as fp:
                        fp.write(self.data)
                    fp.close()
                    shutil.move(
                        f"ICMGG+{i}",
                        os.path.join(self.prepdir, f"ICMGG+{i_fstring}"),
                    )

        # Prefetch SH
        base = self.check_file_exists(str_steps, "ICMSH")
        if base != "":
            param = self.check_value(self.mars["SH"], date_str)
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=base,
                prefetch=True,
                levtype="ML",
                param=param,
                grid=self.mars["grid_ML"],
                specify_domain=False,
                target='"ICMSH+[STEP]"',
            )
            self.write_mars_req(self.datarequest, "ICMSH.req", "retrieve")
            self.create_executable("ICMSH.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            param = self.check_value(self.mars["SHZ"], date_str)
            d_type = self.check_value(self.mars["SHZ_type"], date_str)
            self.update_data_request(
                data_type=d_type,
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="ML",
                param=param,
                grid=self.mars["grid_ML"],
                specify_domain=False,
                target="ICMSH.Z",
            )
            self.write_mars_req(self.datarequest, "ICMSHZ.req", "retrieve")
            self.create_executable("ICMSHZ.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            with open("ICMSH.Z", "rb") as fp:
                data_z = fp.read()
            fp.close()
            os.remove("ICMSH.Z")

            for i in base.split("/"):
                i = int(i)
                i_fstring = f"{i:02d}"
                if os.path.exists(f"ICMSH+{i}"):
                    with open(f"ICMSH+{i}", "ab") as fp:
                        fp.write(data_z)
                    fp.close()
                    shutil.move(
                        f"ICMSH+{i}",
                        os.path.join(self.prepdir, f"ICMSH+{i_fstring}"),
                    )

        # Prefetch UA
        base = self.check_file_exists(str_steps, "ICMUA")
        if base != "":
            param = self.check_value(self.mars["UA"], date_str)
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=base,
                prefetch=True,
                levtype="ML",
                param=param,
                grid=self.mars["grid_ML"],
                specify_domain=False,
                target='"ICMUA+[step]"',
            )
            self.write_mars_req(self.datarequest, "ICMUA.req", "retrieve")
            self.create_executable("ICMUA.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            # Concat files
            for i in base.split("/"):
                i = int(i)
                i_fstring = f"{i:02d}"
                if os.path.exists(f"ICMUA+{i}"):
                    shutil.move(
                        f"ICMUA+{i}",
                        os.path.join(self.prepdir, f"ICMUA+{i_fstring}"),
                    )

        #
        # Split the lat/lon part and perform it here
        #
        mars_file_check = os.path.join(self.prepdir, self.prep_filename)
        if os.path.exists(mars_file_check):
            logger.debug("Warning: Prep file allready exists")
        else:
            str_step = "{}".format(str_steps[0])
            param = self.check_value(self.mars["GG"], date_str)
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=str_step,
                prefetch=True,
                levtype="SFC",
                param=param,
                specify_domain=True,
                target='"{}"'.format(self.template),
            )

            self.write_mars_req(self.datarequest, "latlonGG.req", "retrieve")
            self.create_executable("latlonGG.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            # Retrieve surface geopotential for lat lon
            param = self.check_value(self.mars["SHZ"], date_str)
            d_type = self.check_value(self.mars["SHZ_type"], date_str)
            self.update_data_request(
                data_type=d_type,
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="ML",
                param=param,
                specify_domain=True,
                target='"{}"'.format(self.template + ".Z"),
            )

            self.write_mars_req(self.datarequest, "latlonz.req", "retrieve")
            self.create_executable("latlonz.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            # Retrieve for lat/lon sea data
            param = self.check_value(self.mars["GG_sea"], date_str)
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="SFC",
                param=param,
                specify_domain=True,
                target='"{}"'.format(self.template + ".sea"),
            )
            self.write_mars_req(self.datarequest, "latlonsea.req", "retrieve")
            self.create_executable("latlonsea.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            # Retrieve for lat/lon soil data
            exist_soil = False
            with contextlib.suppress(KeyError):
                param1 = self.check_value(self.mars["GG_soil"], date_str)
                exist_soil = True

            if exist_soil:
                self.update_data_request(
                    data_type="analysis",
                    date=date_str,
                    time=hour_str,
                    steps="00",
                    prefetch=True,
                    levtype="SFC",
                    param=param1,
                    specify_domain=True,
                    target='"{}"'.format(self.template + ".soil"),
                )

                self.write_mars_req(self.datarequest, "latlonsoil.req", "retrieve")
                self.create_executable("latlonsoil.req")
                batch = BatchJob(os.environ, wrapper=self.wrapper)
                batch.run(self.executable)

            # Get the file list to join
            prep_pattern = "mars_prefetch_*_{}_{}*".format(date_str, hour_str)
            self.list_files_join(self.wdir, prep_pattern)
            logger.info(self.filenames)
            with open(self.prep_filename, "ab") as output_file:
                for filename in self.filenames:
                    with open(filename, "rb") as input_file:
                        output_file.write(input_file.read())
                    os.remove(filename)
            shutil.move(
                self.prep_filename,
                os.path.join(self.prepdir, self.prep_filename),
            )
