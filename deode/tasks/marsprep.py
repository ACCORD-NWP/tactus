"""Marsprep."""
import ast
import contextlib
import glob
import os
import shutil

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
        Task.__init__(self, config, __class__.__name__)

        # Get MARS selection
        self.mars = self.mars_selection()

        self.sfcdir = self.platform.get_platform_value("global_sfcdir")
        logger.info(f"sfc dir: {self.sfcdir}")
        # Get output file template
        self.template = "mars_prefetch_[levtype]_[date]_[time]+[step]"

        # Get the times from config.toml
        self.basetime = as_datetime(self.config["general.times.basetime"])
        start_date = as_datetime(self.mars["start_date"])
        self.split_mars = self.config["suite_control.split_mars"]

        if self.split_mars:
            self.bdnr = int(self.config["task.args.bd_nr"])
            self.prep_step = ast.literal_eval(self.config["task.args.prep_step"])

        if self.basetime < start_date:
            raise ValueError(
                f"No data for {self.basetime}! Data available after {start_date}"
            )
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        # Get forecast range
        self.forecast_range = as_timedelta(self.config["general.times.forecast_range"])
        # Get boundary time information
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        bdcycle = as_timedelta(self.mars["ifs_cycle_length"])
        bdcycle_start = as_timedelta(self.mars["ifs_cycle_start"])
        self.bdshift = as_timedelta(self.config["boundaries.bdshift"])

        if self.bdshift.total_seconds() % bdcycle.total_seconds() != 0:
            raise ValueError("bdshift needs to be a multiple of bdcycle!")

        self.int_bdcycle = int(bdcycle.total_seconds()) // 3600
        self.bd_basetime = self.basetime - cycle_offset(
            self.basetime, bdcycle, bdcycle_start=bdcycle_start, bdshift=-self.bdshift
        )
        logger.info("bd_basetime: {}", self.bd_basetime)
        tco = self.mars["tco"]
        self.tco_high = tco + "9_4"
        self.tco_low = self.tco_high if self.mars["class"] == "D1" else tco + "l_2"

        self.unix_group = self.platform.get_platform_value("unix_group")

        self.prepdir = self.platform.substitute(
            self.config["system.marsdir"],
            basetime=self.bd_basetime,
            validtime=self.basetime,
        )
        self.prep_filename = self.platform.substitute(
            self.config["system.bdfile_sfx_template"],
            basetime=self.bd_basetime,
            validtime=self.basetime,
        )
        logger.info("MARS data expected in:{}", self.prepdir)

        self.mars_bin = self.get_binary("mars")

        logger.info("bin: {}", self.mars_bin)

        # Make linting happy
        self.data = b""
        self.executable = None

        # Make MARS requests follow the template as requested
        os.environ["MARS_MULTITARGET_STRICT_FORMAT"] = "1"

    def mars_selection(self, selection=None):
        """Copy default settings if requested.

        Args:
             selection: default self.config["boundaries.ifs.selection"]

        Returns:
             mars (dict): updated mars config section

        """
        if selection is None:
            selection = self.platform.substitute(self.config["boundaries.ifs.selection"])
        mars = self.config[f"mars.{selection}"].dict()
        if "expver" not in mars:
            mars["expver"] = selection

        # Copy default settings if requested
        if "default" in mars:
            default = self.config[f"mars.{mars['default']}"]
            for k in default:
                if k not in mars:
                    mars[k] = default[k]

        logger.debug("MARS selection config:{}", mars)

        return mars

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
        - Else return the value matching the key.

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

    def split_date(self, date, length, interval):
        """Manipulate the dates for Mars request.

        Args:
            date:       full date provided for spliting into parts
            length:     forecast cycle length in hours
            interval:   boundary interval in hours "1H","3H","6H"

        Returns:
            Pandas dataframe object

        """
        # Check options for manipulating dates for MARS extraction
        # Still needs some more relations with the fclen here

        # Check if we're dealing with 00 or 03 ... etc.
        init_hour = int(date.strftime("%H")) % self.int_bdcycle

        # Create Pandas date series for MARS extraction
        interval_int = int(interval.total_seconds()) // 3600

        request_date_frame = pd.Series(
            pd.date_range(
                date - pd.Timedelta(hours=init_hour),
                periods=(length + init_hour) // interval_int + 1,
                freq=interval,
            )
        )
        # Strip the series object according to the selected bdshift
        request_date_frame = pd.concat(
            [
                request_date_frame.iloc[init_hour:],
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

        Raises:
            ValueError: Wrong call of read.

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

        if "GRID" in d and self.mars["class"] == "D1":
            del d["GRID"]
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
        with contextlib.suppress(ValueError):
            _bdmember = int(self.config["boundaries.ifs.bdmember"])
            d.update(
                {
                    "NUMBER": [self.config["boundaries.ifs.bdmember"]],
                }
            )

        if self.mars["class"] != "D1":
            d.update(
                {
                    "PROCESS": ["LOCAL"],
                }
            )

        # Try multilevel
        if levtype == "ML" and param == "129":
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

        if not prefetch:
            if target == "mars_latlonZ":
                d.update(
                    {
                        "SOURCE": [f'"{self.prepdir}/ICMSH+{steps}"'],
                        "REPRESS": ["GG"],
                        "STEP": ["00"],
                    }
                )
                if self.mars["class"] == "D1":
                    d.update(
                        {
                            "DATE": ["20201019"],
                            "TIME": ["1200"],
                            "CLASS": ["OD"],
                        }
                    )

            elif target == "mars_latlonGG":
                d.update(
                    {
                        "SOURCE": [f'"{self.prepdir}/ICMGG+{steps}"'],
                    }
                )
            else:
                raise ValueError("Wrong call of read")

        if self.mars["class"] == "D1":
            d["DATABASE"] = "fdb"
            d["DATASET"] = "extremes-dt"

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
            method: selected method, retrieve or read
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
        self.executable = f"{self.mars_bin} {marsfile}"
        return self.executable

    def check_file_exists(self, steps, path, file_name):
        """Check if which mars file already exist."""
        base_list = []
        for step in steps:
            step1 = int(step)
            step_str = str(step1) if path == "" else f"{step1:02d}"
            mars_file_check = os.path.join(path, f"{file_name}+{step_str}")
            if not os.path.exists(mars_file_check):
                base_list.append(step)
                logger.info("Missing file:{}", mars_file_check)

        base = "/".join(base_list)
        return base

    def fetch_info(self, steps, tag):
        """Print what we are actually fetching.

        Args:
            steps (str): list of steps to retrieve
            tag (str): file type identifier

        """
        logger.info("Actual {} steps to fetch:{}", tag, steps)

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

        # Get the time information based on bdshift and forecast length
        # Need to check the forecast range
        dateframe = self.split_date(
            self.bd_basetime,
            int(self.forecast_range.total_seconds() // 3600),
            self.bdint,
        )

        # Get initial date information in detail
        date_str = dateframe.iloc[0].strftime("%Y%m%d")
        hour_str = dateframe.iloc[0].strftime("%H")

        # Format MARS steps
        step = int(self.bdint.total_seconds() // 3600)
        str_steps = [
            "{0:02d}".format(
                int(self.bdshift.total_seconds() // 3600)
                + (i * step)
                + self.basetime.hour % self.int_bdcycle
            )
            for i in (dateframe.index.tolist())
        ]

        if self.split_mars and self.prep_step:
            logger.debug("*** Need only latlon data")
        else:
            if self.split_mars:
                str_steps = [str_steps[self.bdnr]]

            logger.info("Need data for:{}", dateframe)
            logger.info("Need steps:{}", str_steps)

            #
            # Do full extraction for global (c903 approach).
            # Decided to split for easyer maintanance, still to be discussed
            #

            # Prefetch GG
            tag = "ICMGG"
            base = self.check_file_exists(str_steps, self.prepdir, tag)
            if base != "":
                self.fetch_info(base, tag)
                logger.info("marsGG: {}, {}", self.mars["GG"], date_str)
                param = (
                    self.check_value(self.mars["GG"], date_str)
                    + "/"
                    + self.check_value(self.mars["GG_sea"], date_str)
                )
                self.update_data_request(
                    data_type="forecast",
                    date=date_str,
                    time=hour_str,
                    steps=base,
                    prefetch=True,
                    levtype="SFC",
                    param=param,
                    specify_domain=False,
                    target=f'"{tag}+[STEP]"',
                )
                self.write_mars_req(self.datarequest, f"{tag}.req", "retrieve")
                self.create_executable(f"{tag}.req")
                batch = BatchJob(os.environ, wrapper=self.wrapper)
                batch.run(self.executable)

                file_check = self.check_file_exists(base.split("/"), "", tag)
                if file_check != "":
                    raise FileNotFoundError(f"There is no data in fdb for {tag}")

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
                        target=f"{tag}.soil",
                    )

                    self.write_mars_req(self.datarequest, f"{tag}.soil.req", "retrieve")
                    self.create_executable(f"{tag}.soil.req")
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
                        target=tag,
                    )
                    self.write_mars_req(self.datarequest, f"{tag}1.req", "retrieve")
                    self.create_executable(f"{tag}1.req")
                    batch = BatchJob(os.environ, wrapper=self.wrapper)
                    batch.run(self.executable)

                    # Read the single-step MARS files first
                    with open(tag, "rb") as fp:
                        datagg = fp.read()
                    with open(f"{tag}.soil", "rb") as fp:
                        datagg_soil = fp.read()
                    fp.close()

                    os.remove(tag)
                    os.remove(f"{tag}.soil")
                    self.data += datagg + datagg_soil

                else:
                    tco = self.mars["tco"]
                    self.tco_high = tco + "9_4"
                    self.tco_low = (
                        self.tco_high if self.mars["class"] == "D1" else tco + "l_2"
                    )

                    self.fmanager.input(f"{self.sfcdir}/{self.tco_high}/sdfor", "sdfor")
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_low}/month_aluvp", "aluvp"
                    )
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_low}/month_aluvd", "aluvd"
                    )
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_low}/month_alnip", "alnip"
                    )
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_low}/month_alnid", "alnid"
                    )
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_low}/month_lail", "lail"
                    )
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_low}/month_laih", "laih"
                    )

                    self.fmanager.input(f"{self.sfcdir}/{self.tco_high}/sfc", "sfc")
                    self.fmanager.input(f"{self.sfcdir}/{self.tco_low}/ISSOIL", "issoil")
                    self.fmanager.input(f"{self.sfcdir}/{self.tco_high}/cvh", "cvh")
                    self.fmanager.input(f"{self.sfcdir}/{self.tco_low}/month_alb", "alb")

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
                        + sfc
                        + issoil
                        + cvh
                        + alb
                    )
                for j in base.split("/"):
                    i = int(j)
                    i_fstring = f"{i:02d}"
                    if os.path.exists(f"{tag}+{i}"):
                        with open(f"{tag}+{i}", "ab") as fp:
                            fp.write(self.data)
                        fp.close()
                        shutil.move(
                            f"{tag}+{i}",
                            os.path.join(self.prepdir, f"{tag}+{i_fstring}"),
                        )

            # Prefetch SH
            tag = "ICMSH"
            base = self.check_file_exists(str_steps, self.prepdir, tag)
            if base != "":
                self.fetch_info(base, tag)
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
                    target=f'"{tag}+[STEP]"',
                )
                self.write_mars_req(self.datarequest, f"{tag}.req", "retrieve")
                self.create_executable(f"{tag}.req")
                batch = BatchJob(os.environ, wrapper=self.wrapper)
                batch.run(self.executable)

                file_check = self.check_file_exists(base.split("/"), "", tag)

                if file_check != "":
                    raise ValueError(f"There is no data in fdb for {tag}")

                if self.mars["class"] == "D1":
                    tco = self.mars["tco"]
                    self.tco_low = (
                        self.tco_high if self.mars["class"] == "D1" else tco + "l_2"
                    )
                    self.fmanager.input(
                        f"{self.sfcdir}/{self.tco_high}/sporog", f"{tag}.Z"
                    )

                else:
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
                        target=f'"{tag}.Z"',
                    )
                    self.write_mars_req(self.datarequest, f"{tag}Z.req", "retrieve")
                    self.create_executable(f"{tag}Z.req")
                    batch = BatchJob(os.environ, wrapper=self.wrapper)
                    batch.run(self.executable)

                with open(f"{tag}.Z", "rb") as fp:
                    data_z = fp.read()
                fp.close()
                os.remove(f"{tag}.Z")

                for j in base.split("/"):
                    i = int(j)
                    i_fstring = f"{i:02d}"
                    if os.path.exists(f"{tag}+{i}"):
                        with open(f"{tag}+{i}", "ab") as fp:
                            fp.write(data_z)
                        fp.close()
                        shutil.move(
                            f"{tag}+{i}",
                            os.path.join(self.prepdir, f"{tag}+{i_fstring}"),
                        )

            # Prefetch UA
            tag = "ICMUA"
            base = self.check_file_exists(str_steps, self.prepdir, tag)
            if base != "":
                self.fetch_info(base, tag)
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
                    target=f'"{tag}+[STEP]"',
                )
                self.write_mars_req(self.datarequest, f"{tag}.req", "retrieve")
                self.create_executable(f"{tag}.req")
                batch = BatchJob(os.environ, wrapper=self.wrapper)
                batch.run(self.executable)

                file_check = self.check_file_exists(base.split("/"), "", tag)

                if file_check != "":
                    raise ValueError(f"There is no data in fdb for {tag}")

                # Concat files
                for j in base.split("/"):
                    i = int(j)
                    i_fstring = f"{i:02d}"
                    if os.path.exists(f"{tag}+{i}"):
                        shutil.move(
                            f"{tag}+{i}",
                            os.path.join(self.prepdir, f"{tag}+{i_fstring}"),
                        )

        #
        # Split the lat/lon part and perform it here
        #

        mars_file_check = os.path.join(self.prepdir, self.prep_filename)
        if os.path.exists(mars_file_check):
            logger.debug("Warning: Prep file allready exists")
        elif self.split_mars and not self.prep_step:
            logger.debug("No need Prep file")
        else:
            prefetch = False

            method = self.mars["latlon_method"]
            str_step = "{}".format(str_steps[0])
            param = (
                self.check_value(self.mars["GG"], date_str)
                + "/"
                + self.check_value(self.mars["GG_sea"], date_str)
            )

            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=str_step,
                prefetch=prefetch,
                levtype="SFC",
                param=param,
                specify_domain=True,
                target="mars_latlonGG",
            )

            self.write_mars_req(self.datarequest, "latlonGG.req", method)
            self.create_executable("latlonGG.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            # Retrieve for Surface Geopotential in lat/lon
            param = self.check_value(self.mars["SHZ"], date_str)
            d_type = self.check_value(self.mars["GGZ_type"], date_str)
            lev_type = self.check_value(self.mars["Zlev_type"], date_str)
            self.update_data_request(
                data_type=d_type,
                date=date_str,
                time=hour_str,
                steps=str_step,
                prefetch=prefetch,
                levtype=lev_type,
                param=param,
                specify_domain=True,
                target="mars_latlonZ",
            )
            self.write_mars_req(self.datarequest, "latlonz.req", method)
            self.create_executable("latlonz.req")
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.executable)

            # Get the file list to join
            prep_pattern = "mars_latlon*"
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
