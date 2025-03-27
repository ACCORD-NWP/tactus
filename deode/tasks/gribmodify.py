"""AddCalculatedFields."""

import math

from ..datetime_utils import as_datetime, as_timedelta
from ..logs import logger
from ..toolbox import FileManager
from .base import Task

# For now (on ATOS), only tasks with prgenv/gnu can import eccodes in python
try:
    import eccodes
except (ImportError, OSError, RuntimeError):
    logger.warning("eccodes python API could not be imported. Usually OK.")


class AddCalculatedFields(Task):
    """Create grib files."""

    def __init__(self, config):
        """Construct create grib object.

        Args:
            config (deode.ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.archive = self.platform.get_system_value("archive")

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]

        self.output_settings = self.config["general.output_settings"]
        self.file_templates = self.config["file_templates"]

    def add_field_to_grib(self, fname, params, operation, short_name):
        """Add sum of two fields to the same grib.

        Args:
            fname (str): gribe file
            params (list): list of paramaterNumbers of input parameters
            operation: operarion to compute new field
            short_name (str) : short_name of added field

        Raises:
            NotImplementedError: Operation not implemented yet"
        """
        gids = [None for i in range(len(params))]
        if any(params) is None:
            raise ValueError("Parameters are not define")

        with open(fname, "rb") as f_in, open(fname, "ab") as f_out:
            while None in gids:
                gid = eccodes.codes_grib_new_from_file(f_in)
                par_nam = eccodes.codes_get(gid, "shortName")
                if par_nam in params:
                    gids[params.index(par_nam)] = gid
                else:
                    eccodes.codes_release(gid)

            values_list = [eccodes.codes_get_values(gid) for gid in gids]

            if operation == "add":
                result_values = [sum(values) for values in zip(*values_list)]
            elif operation == "vectorLength":
                if len(params) != 2:
                    raise ValueError("Vector must has 2 components!")
                result_values = [
                    math.sqrt(values[0] * values[0] + values[1] * values[1])
                    for values in zip(*values_list)
                ]
            elif operation == "vectorDirection":
                if len(params) != 2:
                    raise ValueError("Vector must has 2 components!")
                result_values = [
                    (math.atan2(values[0], values[1]) * 180 / math.pi) % 360
                    for values in zip(*values_list)
                ]
            else:
                raise NotImplementedError("Operation {} not implemented yet.", operation)

            gid_res = eccodes.codes_clone(gids[0])

            eccodes.codes_set_values(gid_res, result_values)
            eccodes.codes_set(gid_res, "shortNameECMF", short_name)
            eccodes.codes_set(gid_res, "shortName", short_name)

            eccodes.codes_write(gid_res, f_out)
            eccodes.codes_release(gid_res)
            for gid in gids:
                eccodes.codes_release(gid)
            f_in.close()
            f_out.close()

    def find_par(self, short_name, fname):
        """Check if field with shortName already exists in grib.

        Args:
            short_name: shortName,
            fname: grib file
        Returns:
            bool: True if field exists
        """
        found = False
        with open(fname, "rb") as f_in:
            while not found:
                gid = eccodes.codes_grib_new_from_file(f_in)
                if gid is None:
                    break
                name = eccodes.codes_get(gid, "shortName")
                eccodes.codes_release(gid)
                found = name == short_name
            f_in.close()

        return found

    def execute(self):
        """Execute add_total_precip."""
        file_handle = FileManager.create_list(
            self,
            self.basetime,
            self.forecast_range,
            self.file_templates["fullpos"]["archive"],
            self.output_settings["fullpos"],
        )
        del file_handle[self.basetime]
        modify_rules = self.config["gribmodify"]
        for validtime, fname in file_handle.items():
            dt = validtime - self.basetime
            logger.info("Process {}", fname)
            for name, rules in modify_rules.items():
                logger.debug("Check rule {}:{}", name, rules)
                if not self.find_par(rules["output"], fname):
                    min_freq = (
                        as_timedelta(rules["minimum_frequency"])
                        if "minimum_frequency" in rules
                        else as_timedelta(dt)
                    )
                    if as_timedelta(dt) % min_freq != as_timedelta("PT0H"):
                        logger.info("Skip field with shortName: {}", rules["output"])
                        continue

                    logger.info("Adding field with shortName: {}", rules["output"])
                    if self.find_par(rules["input"][0], fname) and self.find_par(
                        rules["input"][1], fname
                    ):
                        self.add_field_to_grib(
                            fname,
                            rules["input"],
                            rules["operator"],
                            rules["output"],
                        )
                    else:
                        raise ValueError(
                            "There are no input parameters {} for output parameter {}",
                            rules["input"],
                            rules["output"],
                        )
