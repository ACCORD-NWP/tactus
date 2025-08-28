"""AddCalculatedFields."""

import copy
import json
import math
import os
from contextlib import ExitStack
from itertools import product

import numpy as np

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

        self.tasknr = int(config.get("task.args.tasknr", "0"))
        self.ntasks = int(config.get("task.args.ntasks", "1"))

        self.archive = self.platform.get_system_value("archive")

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.forecast_range = self.config["general.times.forecast_range"]

        self.output_settings = self.config["general.output_settings"]
        self.file_templates = self.config["file_templates"]
        self.conversions = self.config["gribmodify"]["conversions"]

        self.csc = self.config["general.csc"]

        self.name = f"{self.name}_{self.tasknr:02}"
        self.toc = {}

    def expand_input_grib_id(self, rules):
        """Expand input_grib_id entries into separate instances."""
        expanded_rules = []
        for rule in rules:
            base_rule = copy.deepcopy(rule)
            input_grib_ids = base_rule.pop("input_grib_id")

            expanded_input_grib_id = []

            for entry in input_grib_ids:
                keys_with_lists = {
                    key: value for key, value in entry.items() if isinstance(value, list)
                }
                if keys_with_lists:
                    keys, lists = zip(*keys_with_lists.items())
                    for values in product(*lists):
                        new_entry = copy.deepcopy(entry)
                        for key, value in zip(keys, values):
                            new_entry[key] = value
                        expanded_input_grib_id.append(new_entry)
                else:
                    expanded_input_grib_id.append(entry)

            new_rule = copy.deepcopy(base_rule)
            new_rule["input_grib_id"] = expanded_input_grib_id
            expanded_rules.append(new_rule)

        return expanded_rules

    def find_additional_files(self, additional_files, validtime):
        """Find multiple additional files."""
        additional_file_paths = []
        for additional_file in additional_files:
            additional_file_base = additional_file["base"]
            additional_file_entry = additional_file["entry"]
            additional_file_static = additional_file.get("static", "False")

            additional_file_value = self.config[additional_file_base][
                additional_file_entry
            ]

            if isinstance(additional_file_value, tuple):
                if len(additional_file_value) > 1:
                    raise ValueError(
                        "Only one file pattern is currently supported per entry."
                    )
                additional_file_value = additional_file_value[0]

            # The additional file may be a static file or a file with a validtime
            if additional_file_static == "False":
                additional_file_path = self.platform.substitute(
                    additional_file_value, validtime=validtime
                )
                additional_file_path = f"{self.archive}/{additional_file_path}"
            else:
                additional_file_path = self.platform.substitute(additional_file_value)

            if not os.path.exists(additional_file_path):
                logger.warning("Additional files requested:{}", additional_files)
                raise ValueError(f"Additional file {additional_file_path} does not exist")
            additional_file_paths.append(additional_file_path)

        return additional_file_paths

    def find_par(self, param, fname):
        """Check if field with specified parameters already exists in grib.

        Args:
            param: parameter dictionary,
            fname: grib file
        Returns:
            bool: True if field exists
        """
        found = False
        param_sorted = dict(sorted(param.items()))
        keys_hash = hash(str(param_sorted.keys()))
        vals_hash = hash(str(param_sorted.values()))

        if fname not in self.toc:
            self.toc[fname] = {}
        if keys_hash not in self.toc[fname]:
            self.toc[fname][keys_hash] = {}

        # Search through the dict
        if vals_hash in self.toc[fname][keys_hash]:
            return True

        with open(fname, "rb") as f_in:
            while not found:
                gid = eccodes.codes_grib_new_from_file(f_in)
                if gid is None:
                    break
                keys = {
                    key: self.safe_codes_get(gid, key) for key, value in param.items()
                }
                keys_sorted = dict(sorted(keys.items()))
                grib_vals_hash = hash(str(keys_sorted.values()))
                if grib_vals_hash not in self.toc[fname][keys_hash]:
                    self.toc[fname][keys_hash][grib_vals_hash] = keys
                found = grib_vals_hash == vals_hash
                eccodes.codes_release(gid)
            f_in.close()

        return found

    def safe_codes_get(self, gid, key):
        """Safely get the value for a key from a GRIB message.

        Returning None if the key is not found.
        """
        try:
            return eccodes.codes_get(gid, key)
        except eccodes.KeyValueNotFoundError:
            logger.debug("Key not found {}", key)
            return None

    def find_in_files(self, param, fname, additional_files):
        """Check if field with specified parameters exists.

        Checks the main file and all additional files.

        Args:
            param: parameter dictionary,
            fname: main grib file
            additional_files: list of additional grib files
        Returns:
            bool: True if field exists in any file
        """
        # Check the main file
        if self.find_par(param, fname):
            return True

        # Check additional files
        for additional_file in additional_files:
            if self.find_par(param, additional_file):
                return True

        return False

    def add_field_to_grib(
        self, fnames, params, operation, output_params, layer_weights, physical_range
    ):
        """Add fields to the same grib following specified modification.

        Args:
            fnames (list): list of grib files
            params (list): list of parameter dictionaries of input parameters
            operation (str): operation to compute new field
            output_params (dict) : list of parameter dictionaries of output parameters
            layer_weights (dict): dict of level weights with level numbers as keys
            physical_range (list): list of physical range for each parameter

        Raises:
            NotImplementedError: Operation not implemented yet"
        """
        gids = [None for _ in range(len(params))]
        gid_to_file_map = {}

        if any(params) is None:
            raise ValueError("Parameters are not defined")

        with ExitStack() as stack:
            files = [stack.enter_context(open(fname, "rb")) for fname in fnames]
            f_out = stack.enter_context(open(fnames[0], "ab"))

            while None in gids:
                for f_in, fname in zip(files, fnames):
                    gid = eccodes.codes_grib_new_from_file(f_in)
                    if gid is None:
                        continue
                    for i, param in enumerate(params):
                        match = all(
                            self.safe_codes_get(gid, key) == value
                            for key, value in param.items()
                        )
                        if match:
                            gids[i] = gid
                            gid_to_file_map[gid] = fname
                            break
                    else:
                        eccodes.codes_release(gid)

            values_list = [eccodes.codes_get_values(gid) for gid in gids]
            bitmap_list = list(len(values_list) * [None])
            for i, gid in enumerate(gids):
                if eccodes.codes_get(gid, "bitmapPresent"):
                    bitmap_list[i] = eccodes.codes_get_array(gid, "bitmap", int)

            if operation == "add":
                result_values = [sum(values) for values in zip(*values_list)]
            elif operation == "vectorLength":
                if len(params) != 2:
                    raise ValueError("Vector must have 2 components!")
                result_values = [
                    math.sqrt(values[0] * values[0] + values[1] * values[1])
                    for values in zip(*values_list)
                ]
            elif operation == "vectorDirection":
                if len(params) != 2:
                    raise ValueError("Vector must have 2 components!")
                result_values = [
                    (math.atan2(values[0], values[1]) * 180 / math.pi) % 360
                    for values in zip(*values_list)
                ]
            elif operation == "multiply":
                result_values = [math.prod(values) for values in zip(*values_list)]
            elif operation == "albedo_calc":
                radiation_threshold = 1  # J/m^2. Below this value, albedo is set to 0.1
                default_albedo = 0.1
                ssr = self.get_value_for_params(
                    params,
                    {"shortName": "ssr"},
                    values_list,
                    bitmap_list,
                    apply_bitmap=True,
                )
                ssrd = self.get_value_for_params(
                    params,
                    {"shortName": "ssrd"},
                    values_list,
                    bitmap_list,
                    apply_bitmap=True,
                )
                with np.errstate(divide="ignore", invalid="ignore"):
                    result_values = 1 - np.divide(
                        ssr, ssrd, out=np.zeros_like(ssr), where=ssrd != 0
                    )  # Albedo is set to 0 if ssrd is 0
                    # Set albedo to 0.1 if ssrd is below threshold
                    result_values = np.where(
                        ssrd < radiation_threshold, default_albedo, result_values
                    )
                    result_values = (
                        100 * result_values
                    )  # Converted to percent to comply with WMO
            elif operation == "patch_averaging_moisture":
                result_values = self.calc_patch_averaging(
                    params,
                    layer_weights,
                    values_list,
                    bitmap_list,
                    physical_range,
                    nature_weighting=True,
                )
            elif operation == "patch_averaging_temperature":
                result_values = self.calc_patch_averaging(
                    params,
                    layer_weights,
                    values_list,
                    bitmap_list,
                    physical_range,
                    nature_weighting=False,
                )
                # Setting missing values to average of non-zero values
                avg_values = np.mean(result_values[np.nonzero(result_values)])
                result_values = np.where(result_values == 0, avg_values, result_values)
            elif operation == "patch_averaging_surface":
                result_values = self.calc_patch_averaging(
                    params,
                    layer_weights,
                    values_list,
                    bitmap_list,
                    physical_range,
                    nature_weighting=False,
                )
            else:
                raise NotImplementedError(
                    "Operation {} not implemented yet.".format(operation)
                )

            # Prioritize cloning GRIB message from original file
            # followed by order in "additional files"
            gid_res = None
            found = False
            for file in fnames:
                for gid in gids:
                    if gid is not None and gid_to_file_map[gid] == file:
                        gid_res = eccodes.codes_clone(gid)
                        found = True
                        break
                if found:
                    break

            eccodes.codes_set_values(gid_res, result_values)
            for output_parameter in output_params:
                if output_parameter == "productDefinitionTemplateNumber":
                    # Special case for productDefinitionTemplateNumber as
                    # setting this changes the time unit
                    self.set_while_retaining_time_unit(
                        gid_res,
                        output_parameter,
                        output_params[output_parameter],
                    )

                else:
                    eccodes.codes_set(
                        gid_res, output_parameter, output_params[output_parameter]
                    )

            eccodes.codes_write(gid_res, f_out)
            eccodes.codes_release(gid_res)
            for gid in gids:
                eccodes.codes_release(gid)
            for parameter in params:
                parameter.pop("gid", None)

    def get_value_for_params(
        self,
        params,
        match_dict,
        values_list,
        bitmap_list,
        apply_bitmap=False,
        return_bitmap=False,
    ):
        """Get the index of the dict in the list that matches all key-value pairs."""
        for index, param in enumerate(params):
            if all(param.get(key) == value for key, value in match_dict.items()):
                value = values_list[index]
                bitmap = bitmap_list[index]
                if bitmap is not None and apply_bitmap:
                    value = value * bitmap
                if return_bitmap:
                    return value, bitmap
                return value

        # Return appropriate default when no match is found
        if return_bitmap:
            return None, None
        return None

    def calc_patch_averaging(
        self,
        params,
        layer_weights,
        values_list,
        bitmap_list,
        physical_range,
        nature_weighting,
    ):
        """Calculate patch averaging.

        Bitmaps are used to mask out invalid values.
        This is necessary, as the values for a parameter may be missing in some tiles
        despite the fact that the tile fraction is not 0.
        This is solved by updating the layer weights to always sum to 1 while excluding
        missing data.
        """
        result_values = None
        tile_fraction_name = "tifr"
        nature_tile = "GNATU"
        tile_fraction_range = [0, 1]
        tiles = self.get_unique_values(params, "tile", exclude=nature_tile)

        # Verify that the layer_weights is a dict with layer numbers as keys
        if not isinstance(layer_weights, dict):
            raise ValueError("Layer weights must be a dict with layer numbers as keys.")
        if not all(isinstance(key, str) and key.isdigit() for key in layer_weights):
            raise ValueError("Layer weights must be a dict with layer numbers as keys.")

        # Verify that the layer weights sum to 1 within numerical uncertainty
        weight_sum = sum(layer_weights.values())
        if (abs(weight_sum) - 1.0) > 1e-6:
            raise ValueError(f"Layer weights must sum to 1. Current sum: {weight_sum}")

        physical_parameter_name = self.get_unique_values(
            params, "shortName", exclude=tile_fraction_name
        )
        if len(physical_parameter_name) != 1:
            raise ValueError("Only one physical parameter is allowed.")
        physical_parameter_name = physical_parameter_name[0]

        # Get the keys to iterate over for levels
        weight_keys = list(layer_weights.keys())
        weight_keys.sort(key=int)  # Sort numerically: ["1", "2", "3", ...]

        # Initialize arrays
        num_tiles = len(tiles)
        num_layers = len(layer_weights)
        layer_weight_array = np.zeros((num_tiles, num_layers, len(values_list[0])))
        tile_fraction_values = np.zeros((num_tiles, len(values_list[0])))
        tile_fraction_bitmaps = np.zeros((num_tiles, len(values_list[0])), dtype=bool)
        physical_parameter_values = np.zeros((num_tiles, num_layers, len(values_list[0])))
        physical_parameter_bitmaps = np.zeros(
            (num_tiles, num_layers, len(values_list[0]))
        )

        # Populate arrays with values and bitmaps
        for tile_index, tile in enumerate(tiles):
            tile_fraction_value, tile_fraction_bitmap = self.get_value_for_params(
                params,
                {"shortName": tile_fraction_name, "tile": tile},
                values_list,
                bitmap_list,
                apply_bitmap=True,
                return_bitmap=True,
            )
            # Set tile fraction to 0 if outside the range
            tile_fraction_value = np.where(
                np.logical_and(
                    tile_fraction_value >= np.min(tile_fraction_range),
                    tile_fraction_value <= np.max(tile_fraction_range),
                ),
                tile_fraction_value,
                0,
            )
            tile_fraction_values[tile_index] = tile_fraction_value
            tile_fraction_bitmaps[tile_index] = tile_fraction_bitmap

            for layer_index, weight_key in enumerate(weight_keys):
                level = int(weight_key)  # Convert string to int
                layer_weight = layer_weights[
                    weight_key
                ]  # Get the layer weight for this level
                (
                    physical_parameter_value,
                    physical_parameter_bitmap,
                ) = self.get_value_for_params(
                    params,
                    {
                        "shortName": physical_parameter_name,
                        "tile": tile,
                        "level": level,
                    },
                    values_list,
                    bitmap_list,
                    return_bitmap=True,
                )

                if physical_range is not None:
                    physical_parameter_value = np.where(
                        np.logical_and(
                            physical_parameter_value >= np.min(physical_range),
                            physical_parameter_value <= np.max(physical_range),
                        ),
                        physical_parameter_value,
                        0,
                    )
                physical_parameter_values[
                    tile_index, layer_index
                ] = physical_parameter_value
                physical_parameter_bitmaps[
                    tile_index, layer_index
                ] = physical_parameter_bitmap
                layer_weight_array[tile_index, layer_index] = np.where(
                    tile_fraction_bitmap != physical_parameter_bitmap, 0, layer_weight
                )  # Sets the layer weight to 0 if the tile fraction
                # and physical parameter bitmaps are not equal

        # Do the layer weighting
        for layer_index in range(num_layers):
            for tile_index, _ in enumerate(tiles):
                tile_fraction_value = tile_fraction_values[tile_index]

                # The tile fraction value is normalized by the sum of the fraction values
                tile_fraction_sum = np.sum(tile_fraction_values, axis=0)
                tile_fraction_value = tile_fraction_value / tile_fraction_sum
                physical_parameter_value = physical_parameter_values[
                    tile_index, layer_index
                ]

                # The layer weight is normalized by the sum of the layer weights
                layer_weight = layer_weight_array[tile_index, layer_index] / (
                    np.sum(layer_weight_array[tile_index], axis=0)
                )

                value = physical_parameter_value * layer_weight * tile_fraction_value
                if nature_weighting:
                    nature_fraction = self.get_value_for_params(
                        params,
                        {"shortName": tile_fraction_name, "tile": "GNATU"},
                        values_list,
                        bitmap_list,
                        apply_bitmap=True,
                    )

                    nature_fraction = np.where(
                        np.logical_and(
                            nature_fraction >= np.min(tile_fraction_range),
                            nature_fraction <= np.max(tile_fraction_range),
                        ),
                        nature_fraction,
                        0,
                    )  # Sets the nature fraction to 0 if outside the range
                    value = value * nature_fraction

                if result_values is None:
                    result_values = value
                else:
                    result_values += value

        return result_values

    def get_unique_values(self, dicts, key, exclude=None):
        """Get unique values of a key in a list of dictionaries."""
        unique_values = set()
        for d in dicts:
            if key in d and (exclude is None or d[key] != exclude):
                unique_values.add(d[key])

        return list(unique_values)

    def set_while_retaining_time_unit(self, gid_res, key, value):
        """Set a key in a GRIB message while retaining the time unit."""
        # Get the current time unit and forecast time from the original GRIB message
        time_unit_indicator = eccodes.codes_get(gid_res, "indicatorOfUnitForForecastTime")
        forecast_time = eccodes.codes_get(gid_res, "forecastTime")

        # Set the new value
        eccodes.codes_set(gid_res, key, value)

        # Restore the time unit and forecast time
        eccodes.codes_set(gid_res, "indicatorOfUnitForForecastTime", time_unit_indicator)
        eccodes.codes_set(gid_res, "forecastTime", forecast_time)

    def execute(self):
        """Execute gribmodify."""
        compute_list = []
        for filetype in self.conversions:
            if self.config["general"].get(filetype, None) is False:
                logger.info(
                    "Skipping as conversion of {} is not set for CSC {}",
                    filetype,
                    self.csc,
                )
                continue
            file_handle = FileManager.create_list(
                self,
                self.basetime,
                self.forecast_range,
                self.file_templates[filetype]["grib"],
                self.output_settings[filetype],
            )

            # Load gribmodify rules from JSON file
            grib_modify_rules_file = self.platform.substitute(
                self.config["gribmodify"]["gribmodify_rules_file"]
            )
            with open(grib_modify_rules_file, "r") as f:
                modify_rules = json.load(f)

            # Expand input_grib_id entries
            expanded_modify_rules = self.expand_input_grib_id(modify_rules)
            modify_rules = expanded_modify_rules
            config_modify_rules = self.config["gribmodify"][filetype]

            for validtime, fname in file_handle.items():
                compute_list.append(
                    {
                        "validtime": validtime,
                        "fname": fname,
                        "modify_rules": modify_rules,
                        "config_modify_rules": config_modify_rules,
                    }
                )

        # Loop over computations to be executed for this task
        for items in compute_list[self.tasknr :: self.ntasks]:
            validtime, fname, modify_rules, config_modify_rules = (
                items["validtime"],
                items["fname"],
                items["modify_rules"],
                items["config_modify_rules"],
            )

            dt = validtime - self.basetime
            logger.info("Process {} {}", fname, validtime)
            for name in config_modify_rules:
                rule = next(
                    (
                        rule
                        for rule in modify_rules
                        if rule["output_name"] == config_modify_rules[name]["output_name"]
                    ),
                    None,
                )

                if rule is None:
                    raise ValueError(
                        f"No modify rule found for output: "
                        f"{config_modify_rules[name]['output_name']}"
                    )

                # Check if the rule is valid at basetime
                if validtime == self.basetime:
                    valid_at_basetime = rule.get("valid_at_basetime", False)
                    static_field = rule.get("static_field", False)
                    if not (valid_at_basetime or static_field):
                        logger.info(
                            "Skipping field with name: {} as it is not valid at basetime",
                            rule["output_name"],
                        )
                        continue
                elif rule.get("static_field", False):
                    # Do not process static fields at times different from basetime
                    logger.debug(
                        "Skipping field with name: {} as it is not basetime",
                        rule["output_name"],
                    )
                    continue

                additional_files = rule.get("additional_files", [])
                additional_file_paths = (
                    self.find_additional_files(additional_files, validtime)
                    if additional_files
                    else []
                )

                layer_weights = rule.get("layer_weights")
                csc_specific = config_modify_rules[name].get("csc_specific", False)
                physical_range = rule.get("physical_range")

                if csc_specific:
                    if isinstance(csc_specific, str):
                        csc_specific = csc_specific.split()

                    csc_specific = [
                        csc_specific.casefold() for csc_specific in csc_specific
                    ]

                    if self.csc.casefold() not in csc_specific:
                        logger.info(
                            "Skipping field with name: {} for CSC {}",
                            rule["output_name"],
                            self.csc,
                        )
                        continue

                if not self.find_par(rule["output_grib_id"], fname):
                    min_freq = (
                        as_timedelta(config_modify_rules[name]["minimum_frequency"])
                        if "minimum_frequency" in config_modify_rules[name]
                        else as_timedelta(dt)
                    )
                    if min_freq != as_timedelta("PT0H") and as_timedelta(
                        dt
                    ) % min_freq != as_timedelta("PT0H"):
                        logger.info(
                            "Skip field with label: {}",
                            config_modify_rules[name]["output_name"],
                        )
                        continue

                    logger.info("Adding field with name: {}", rule["output_name"])

                    if all(
                        self.find_in_files(input_param, fname, additional_file_paths)
                        for input_param in rule["input_grib_id"]
                    ):
                        self.add_field_to_grib(
                            [fname, *additional_file_paths],
                            rule["input_grib_id"],
                            rule["operator"],
                            rule["output_grib_id"],
                            layer_weights,
                            physical_range,
                        )
                    else:
                        raise ValueError(
                            "Missing input parameters {} for output {}".format(
                                rule["input_grib_id"],
                                rule["output_grib_id"]["shortName"],
                            )
                        )
                else:
                    logger.info("Field with name: {} already exists", rule["output_name"])
