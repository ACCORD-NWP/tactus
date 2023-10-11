"""MarsprepGlobalDT."""

import glob
import os
import shutil
from datetime import datetime

import pandas as pd

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset
from ..geo_utils import Projection, Projstring
from ..logs import logger
from ..tasks.batch import BatchJob
from .base import Task


class MarsprepGlobalDT(Task):
    """MarsprepGlobalDT task."""

    def __init__(self, config):
        """Construct forecast object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)

        # Get paths
        self.expver = self.config["general.mars_expver"]
        self.sfcdir = self.config["system.global_sfcdir"]

        # Get boundary strategy
        self.strategy = self.config["boundaries.bdstrategy"]
        # Get output file template
        self.template = self.config["system.marsfile_template"]

        # Get the times from config.toml
        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.cycle_length = as_timedelta(self.config["general.times.cycle_length"])
        # Get forecast range
        self.forecast_range = as_timedelta(self.config["general.times.forecast_range"])
        # Get boundary shift
        self.bdint = as_timedelta(self.config["boundaries.bdint"])
        self.bdshift = as_timedelta(self.config["boundaries.bdshift"])
        self.bdcycle = as_timedelta(self.config["boundaries.bdcycle"])
        self.int_bdcycle = int(self.bdcycle.total_seconds()) // 3600
        self.cy_offset = cycle_offset(self.basetime, self.bdcycle)

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
        self.grid = None
        self.data = b""
        self.executable = None

        # Make MARS requests follow the template as requested
        os.environ["MARS_MULTITARGET_STRICT_FORMAT"] = "1"

        # Debug start
        logger.info("Construct PREP/MarsGlobalDT task")

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

    def check_resolution(self, date):
        """Check grid resolution according to the date.

        Args:
            date:      date for resolution checking

        Returns:
            String containing the grid/resolution value
        """
        date = datetime.strptime(date, "%Y%m%d%H")

        if date >= datetime(2016, 3, 8):
            self.grid = "0.09/0.09"
        elif date >= datetime(2010, 1, 26, 6):
            self.grid = "0.15/0.15"
        elif date >= datetime(2006, 2, 1, 6):
            self.grid = "0.25/0.25"
        else:
            self.grid = "0.50/0.50"

        return self.grid

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
        expver,
        data_type="analysis",
        date=None,
        time=None,
        steps=None,
        prefetch=False,
        levtype=None,
        specify_domain=False,
        domain_dimensions=None,
        grid_dimensions=None,
        target=None,
        levlist=None,
    ):
        """Create ECMWF MARS system request.

        Args:
            param:              List of parameters to extract
            expver:             Name of experiment
            data_type:          Type of data to extract, 'analysis' or 'forecast'
            date:               Date to extract
            time:               Time to extract
            steps:              Forecast steps to be extracted
            prefetch:           Default is stage
            levtype:            SFC or ML (to be updated)
            specify_domain:     Use lat/lon and rotation or use global (default)
            domain_dimensions:  Domain dimensions S/W/N/E and rotation (if applicable)
            grid_dimensions:    Grid resolution (if applicable)
            target:             Filename to write
            levlist:            List of levels

        Returns:
            Pandas dataframe object
        """
        # General request parameters
        d = {
            "CLASS": ["RD"],
            "EXPVER": [self.expver],
            "LEVTYPE": [levtype],
            "STREAM": ["OPER"],
            "DATE": [date],
            "TIME": [time],
            "STEP": [steps],
            "PARAM": [param],
            "TYPE": ["AN"],
            "TARGET": [target],
        }
        # Additional request parameters
        if data_type == "forecast":
            if len(steps.split("/")) > 1:
                d.update(
                    {
                        "STEP": [steps],
                        "TYPE": ["FC"],
                    }
                )
            else:
                d["TYPE"] = ["FC"]
        if prefetch:
            d.update(
                {
                    "PROCESS": ["LOCAL"],
                }
            )

        # Soil level
        if levtype == "SOL":
            d.update({"LEVELIST": ["1/2/3/4/5"]})
        # Try multilevel
        if levtype == "ML" and steps == "00" and prefetch:
            if data_type == "analysis":
                d.update(
                    {
                        "LEVELIST": ["1"],
                        "GRID": ["AV"],
                    }
                )
            elif data_type == "forecast":
                d.update(
                    {
                        "TYPE": ["FC"],
                        "GRID": ["AV"],
                        "LEVELIST": ["1"],
                    }
                )
            d["LEVTYPE"] = [levtype]
            d["STEP"] = [steps]

        elif levtype == "ML":
            d.update(
                {
                    "LEVELIST": ["1/to/137"],
                    "GRID": ["AV"],
                }
            )
            d["LEVTYPE"] = [levtype]
        if param == "74/163/160/161/162/15/16/17/18/66/67":
            d.update({"GRID": "O640"})
        # Taken from functions.ksh, not yet clear. Override stream to OPER if hour are 06 or 18.
        if time == "06" or time == "18":
            d["STREAM"] = ["SCDA"]
        if specify_domain:
            d.update({"GRID": [grid_dimensions]})
            d["AREA"] = [domain_dimensions]

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

    def execute(self, cmd):
        """Execute binary task."""
        batch = BatchJob(os.environ, wrapper=self.wrapper)
        batch.run(cmd)

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

    def run(self):
        """Run task.

        Define run sequence.

        Raises:
            ValueError: If there is an issue with the work folder.
        """
        try:
            # Part1
            if not os.path.exists(self.prepdir):
                os.makedirs(self.prepdir, exist_ok=True)
        except Exception as e:
            raise ValueError("Error while preparing the mars folder: {}".format(e))

        os.makedirs(self.wdir, exist_ok=True)
        os.chdir(self.wdir)
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
        date_time_str = dateframe.iloc[0].strftime("%Y%m%d%H")
        date_str = dateframe.iloc[0].strftime("%Y%m%d")
        hour_str = dateframe.iloc[0].strftime("%H")

        # Get domain resolution based on the date
        self.check_resolution(date_time_str)

        # Get domain specs
        fdomainstr = self.get_domain_data(self.config)

        # link files from sfcdir
        expn = ""
        if self.expver == "i1vt":
            expn = "255"
        elif self.expver == "ht3e":
            expn = "399"
        elif self.expver == "i4ql":
            expn = "255"

        self.fmanager.input(f"{self.sfcdir}{expn}9_4/sdfor", "sdfor")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_aluvp", "aluvp")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_aluvd", "aluvd")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_alnip", "alnip")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_alnid", "alnid")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_lail", "lail")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_laih", "laih")

        self.fmanager.input(f"{self.sfcdir}{expn}l_2/sfc", "sfc")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/ISSOIL", "issoil")
        self.fmanager.input(f"{self.sfcdir}{expn}9_4/cvh", "cvh")
        self.fmanager.input(f"{self.sfcdir}{expn}l_2/month_alb", "alb")

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
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=base,
                prefetch=True,
                levtype="SFC",
                param="32.128/33.128/39.128/40.128/41.128/42.128/139.128/141.128/170.128/172.128/183.128/198.128/235.128/236.128/35.128/36.128/37.128/38.128/238.128/243.128/245.128",
                expver=self.expver,
                target='"ICMGG+[STEP]"',
            )
            self.write_mars_req(self.datarequest, "ICMGG.req", "retrieve")
            self.create_executable("ICMGG.req")
            self.execute(self.executable)

            # Prefetch.Sea

            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="SFC",
                param="31/34",
                expver=self.expver,
                target="ICMGG.sea",
            )
            self.write_mars_req(self.datarequest, "ICMGGsea.req", "retrieve")
            self.create_executable("ICMGGsea.req")
            self.execute(self.executable)

            # Read the single-step MARS files first
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

            for i in base.split("/"):
                i = int(i)
                i_fstring = f"{i:02d}"
                if os.path.exists(f"ICMGG+{i}"):
                    with open(f"ICMGG+{i}", "ab") as fp:
                        fp.write(self.data)
                    fp.close()
                    shutil.move(
                        f"ICMGG+{i}", os.path.join(self.prepdir, f"ICMGG+{i_fstring}")
                    )

        # Prefetch SH
        base = self.check_file_exists(str_steps, "ICMSH")
        if base != "":
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=base,
                prefetch=True,
                levtype="ML",
                param="152/138/155/130",
                expver=self.expver,
                target='"ICMSH+[STEP]"',
            )
            self.write_mars_req(self.datarequest, "ICMSH.req", "retrieve")
            self.create_executable("ICMSH.req")
            self.execute(self.executable)

            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="ML",
                param="129.128",
                expver=self.expver,
                target="ICMSH.Z",
            )
            self.write_mars_req(self.datarequest, "ICMSHZ.req", "retrieve")
            self.create_executable("ICMSHZ.req")
            self.execute(self.executable)

            with open("ICMSH.Z", "rb") as fp:
                data_z = fp.read()
            fp.close()

            for i in base.split("/"):
                i = int(i)
                i_fstring = f"{i:02d}"
                if os.path.exists(os.path.join(self.wdir, f"ICMSH+{i}")):
                    with open(os.path.join(self.wdir, f"ICMSH+{i}"), "ab") as fp:
                        fp.write(data_z)
                    fp.close()
                    shutil.move(
                        f"ICMSH+{i}", os.path.join(self.prepdir, f"ICMSH+{i_fstring}")
                    )

        # Prefetch.UA
        base = self.check_file_exists(str_steps, "ICMUA")
        if base != "":
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=base,
                prefetch=True,
                levtype="ML",
                param="133/75/76/246/247/248",
                expver=self.expver,
                target='"ICMUA+[step]"',
            )
            self.write_mars_req(self.datarequest, "ICMUA.req", "retrieve")
            self.create_executable("ICMUA.req")
            self.execute(self.executable)

            for i in base.split("/"):
                i = int(i)
                i_fstring = f"{i:02d}"
                if os.path.exists(os.path.join(self.wdir, f"ICMUA+{i}")):
                    shutil.move(
                        f"ICMUA+{i}",
                        os.path.join(self.prepdir, f"ICMUA+{i_fstring}"),
                    )

        #
        # Split the lat/lon part and perform it here
        #
        mars_file_check = os.path.join(self.prepdir, self.prep_filename)
        if os.path.exists(mars_file_check):
            logger.debug("PREP file already exists!")
        else:

            str_step = "{}".format(str_steps[0])

            # Stage for lat/lon
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=str_step,
                prefetch=False,
                levtype="SFC",
                param="32.128/33.128/39.128/40.128/41.128/42.128/139.128/141.128/170.128/172.128/183.128/198.128/235.128/236.128/35.128/36.128/37.128/38.128/238.128/243.128/245.128",
                specify_domain=True,
                domain_dimensions=fdomainstr,
                grid_dimensions=self.grid,
                expver=self.expver,
                target='"{}"'.format(self.template),
            )

            self.write_mars_req(self.datarequest, "mars_script.req", "stage")
            self.create_executable("mars_script.req")
            self.execute(self.executable)

            # Retrieve for lat/lon sea data
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="SFC",
                param="31/34",
                specify_domain=True,
                domain_dimensions=fdomainstr,
                grid_dimensions=self.grid,
                expver=self.expver,
                target='"{}"'.format(self.template + ".sea"),
            )
            self.write_mars_req(self.datarequest, "lonlatICMGG.sea.req", "retrieve")
            self.create_executable("lonlatICMGG.sea.req")
            self.execute(self.executable)

            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps="00",
                prefetch=True,
                levtype="ML",
                param="129.128",
                specify_domain=True,
                domain_dimensions=fdomainstr,
                grid_dimensions=self.grid,
                expver=self.expver,
                target='"{}"'.format(self.template + ".Z"),
            )

            self.write_mars_req(self.datarequest, "latlonZ.req", "retrieve")
            self.create_executable("latlonZ.req")
            self.execute(self.executable)

            # Retrieve for lat/lon surface data
            self.update_data_request(
                data_type="forecast",
                date=date_str,
                time=hour_str,
                steps=str_step,
                prefetch=True,
                levtype="SFC",
                param="32.128/33.128/39.128/40.128/41.128/42.128/141.128/139.128/170.128/172.128/183.128/198.128/235.128/236.128/35.128/36.128/37.128/38.128/238.128/243.128/245.128",
                specify_domain=True,
                domain_dimensions=fdomainstr,
                grid_dimensions=self.grid,
                expver=self.expver,
                target='"{}"'.format(self.template),
            )

            self.write_mars_req(self.datarequest, "latlonGG.req", "retrieve")
            self.create_executable("latlonGG.req")
            self.execute(self.executable)

            # Get the file list to join
            prep_pattern = "mars_prefetch_*_{}_{}*".format(date_str, hour_str)
            self.list_files_join(self.wdir, prep_pattern)

            with open(os.path.join(self.wdir, self.prep_filename), "ab") as output_file:
                for filename in self.filenames:
                    with open(filename, "rb") as input_file:
                        output_file.write(input_file.read())

            shutil.move(
                os.path.join(self.wdir, self.prep_filename),
                os.path.join(self.prepdir, self.prep_filename),
            )
