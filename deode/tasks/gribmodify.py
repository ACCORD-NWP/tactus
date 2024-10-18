"""AddTotalPrec."""

from ..datetime_utils import as_datetime
from ..logs import logger
from ..toolbox import FileManager
from .base import Task

# For now (on ATOS), only tasks with prgenv/gnu can import eccodes in python
try:
    import eccodes
except (ImportError, OSError, RuntimeError):
    logger.warning("eccodes python API could not be imported. Usually OK.")


class AddTotalPrec(Task):
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
        gid1 = None
        gid2 = None
        if any(params) is None:
            raise ValueError("Parameters are not define")

        with open(fname, "rb") as f_in, open(fname, "ab") as f_out:
            while gid1 is None or gid2 is None:
                gid = eccodes.codes_grib_new_from_file(f_in)
                par_nam = eccodes.codes_get(gid, "shortName")
                if par_nam == params[0]:
                    gid1 = gid
                elif par_nam == params[1]:
                    gid2 = gid
                else:
                    eccodes.codes_release(gid)

            values1 = eccodes.codes_get_values(gid1)
            values2 = eccodes.codes_get_values(gid2)
            if operation == "add":
                values_sum = [v1 + v2 for v1, v2 in zip(values1, values2)]
            else:
                raise NotImplementedError("Operation {} not implemented yet.", operation)

            gid_sum = eccodes.codes_clone(gid1)

            eccodes.codes_set_values(gid_sum, values_sum)
            eccodes.codes_set(gid_sum, "shortNameECMF", short_name)
            eccodes.codes_set(gid_sum, "shortName", short_name)

            eccodes.codes_write(gid_sum, f_out)
            eccodes.codes_release(gid1)
            eccodes.codes_release(gid2)
            eccodes.codes_release(gid_sum)
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
        for fname in file_handle.values():
            for name in modify_rules:
                logger.info(fname)
                if not self.find_par(modify_rules[name]["output"], fname):
                    logger.info(
                        "Adding field with shortName: {}", modify_rules[name]["output"]
                    )
                    if self.find_par(
                        modify_rules[name]["input"][0], fname
                    ) and self.find_par(modify_rules[name]["input"][1], fname):
                        self.add_field_to_grib(
                            fname,
                            modify_rules[name]["input"],
                            modify_rules[name]["operator"],
                            modify_rules[name]["output"],
                        )
                    else:
                        raise ValueError(
                            "There are no input parameters {} for output parameter {}",
                            modify_rules[name]["input"],
                            modify_rules[name]["output"],
                        )
