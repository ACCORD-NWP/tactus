"""Surfex tasks."""

import json
import os

from ..datetime_utils import as_datetime, as_timedelta, cycle_offset, get_decade
from ..logs import logger
from ..namelist import NamelistGenerator
from ..os_utils import deodemakedirs
from .base import Task
from .batch import BatchJob


class InputDataFromNamelist:
    """Binary input data for offline executables."""

    def __init__(
        self,
        nml,
        input_data,
        program,
        platform,
        basetime=None,
        validtime=None,
        one_decade=False,
    ):
        """Construct the binary input set up from namelist.

        Args:
            nml (f90nml): Namelist
            input_data (dict): Input data definitions
            program (str): Program mode to set up for (pgd/prep etc)
            platform (Platform): Platform dependent settings
            basetime (as_datetime, optional): basetime. Defaults to None.
            validtime (as:datetime, optional): Validtime. Defaults to None.
            one_decade (bool, optional): one decade or not. Defaults to False.

        Raises:
            RuntimeError: Could not find program

        """
        self.nml = nml
        self.platform = platform
        self.basetime = basetime
        self.validtime = validtime
        self.one_decade = one_decade

        try:
            self.data = input_data[program]
        except KeyError:
            raise RuntimeError(f"Could not find program {program}") from KeyError
        self.data = self.process_data()

    def get(self):
        """Return data."""
        return self.data

    @staticmethod
    def get_nml_value(nml, block, key, indices=None):
        """Get namelist value.

        Args:
            nml (nmlf90.Namelist): Namelist
            block (str): Namelist block
            key (str): Namelist key
            indices (list, optional): Indices to read. Defaults to None.

        Returns:
            setting (any): Namelist setting

        """
        logger.debug("Checking block={} key={}", block, key)
        if block not in nml:
            return None

        if key in nml[block]:
            vals = []
            val_dict = {}
            val = nml[block][key]
            logger.debug("namelist type={}", type(val))
            if indices is not None:
                logger.debug("indices={}", indices)
                if len(indices) == 2:
                    val = nml[block][key][indices[1]][indices[0]]
                else:
                    val = nml[block][key][indices[0]]
                    logger.debug("Found 1D value {}", val)
                    if isinstance(val, list):
                        return None
                val_dict.update({"value": val, "indices": None})
                vals.append(val_dict)

            elif isinstance(val, list):
                dim_size = len(val)
                logger.debug("dim_size={}", dim_size)
                dims = []
                tval = val
                more_dimensions = True
                while more_dimensions:
                    logger.debug("tval={} type(tval)={}", tval, type(tval))
                    if isinstance(tval, int):
                        more_dimensions = False
                    else:
                        logger.debug("type(tval)={}", type(tval))
                        if not isinstance(tval, list) or isinstance(tval[0], int):
                            more_dimensions = False
                        else:
                            logger.debug("len(tval)={} type(tval)={}", len(tval), type)
                            dim_size = len(tval)
                            dims.append(dim_size)

                            tval = tval[0]
                            logger.debug("New tval={} dim_size={}", tval, dim_size)

                logger.debug("dims={}", dims)
                logger.debug("type(val)={}", type(val))
                if len(dims) == 2:
                    for i in range(dims[0]):
                        for j in range(dims[1]):
                            val_dict = {}
                            indices = [j, i]
                            lval = val[i][j]
                            val_dict.update({"value": lval, "indices": indices})
                            logger.debug("value={} indices={}", lval, indices)
                            vals.append(val_dict)
                elif len(dims) == 1:
                    for i in range(dims[0]):
                        val_dict = {}
                        indices = [i]
                        logger.debug("i={}, val[i]={}", i, val[i])
                        lval = val[i]
                        val_dict.update({"value": lval, "indices": indices})
                        logger.debug("value={} indices={}", lval, indices)
                        vals.append(val_dict)
                elif len(dims) == 0:
                    val_dict = {}
                    logger.debug("val={}", val)
                    val_dict.update({"value": val, "indices": None})
                    vals.append(val_dict)
            else:
                val_dict = {}
                if isinstance(val, bool):
                    val = str(val)
                val_dict.update({"value": val, "indices": None})
                vals.append(val_dict)

            logger.debug("Found: value={}", val_dict["value"])
            return vals
        return None

    @staticmethod
    def get_nml_value_from_string(nml, string, sep="#", indices=None):
        """Get namelist value from a string.

        Args:
            nml (nmlf90.Namelist): Namelist
            string (str): Namelist identifier
            sep (str, optional): _description_. Defaults to "#".
            indices (list, optional): Indices to read. Defaults to None.

        Returns:
            setting (any): Namelist setting

        """
        nam_section = string.split(sep)[0]
        nam_key = string.split(sep)[1]
        return InputDataFromNamelist.get_nml_value(
            nml, nam_section, nam_key, indices=indices
        )

    def substitute(self, key, val, macros=None, micro="@"):
        """Substitute patterns.

        Args:
            key (str): _description_
            val (str): _description_
            macros (dict, optional): Macros. Defaults to None.
            micro (str, optional): Micro character. Defaults to "@".

        Returns:
            dict: Substituted key=value

        """
        logger.debug(
            "Substitute key={} and val={} {} {}", key, val, self.basetime, self.validtime
        )
        pkey = key
        pval = val
        if macros is not None:
            for macro_key, macro_val in macros.items():
                logger.debug("macro_key={} macro_val={}", macro_key, macro_val)
                pkey = pkey.replace(f"{micro}{macro_key}{micro}", macro_val)
                pval = pval.replace(f"{micro}{macro_key}{micro}", macro_val)

        pkey = self.platform.substitute(
            pkey, basetime=self.basetime, validtime=self.validtime
        )
        pval = self.platform.substitute(
            pval, basetime=self.basetime, validtime=self.validtime
        )
        logger.debug("SUBSTITUTED: pkey={} pval={}", pkey, pval)
        return pkey, pval

    def read_macro_setting(self, macro_defs, key, default=None, sep="#"):
        """Read a macro setting.

        Args:
            macro_defs (dict): Macro definition
            key (str): Macro setting to get.
            default (str, optional): Default value. Defaults to None.
            sep (str, optional): Namelist key separator. Defaults to "#".

        Returns:
            setting (any)
        """
        try:
            setting = macro_defs[key]
            if isinstance(setting, str) and setting.find(sep) > 0:
                logger.debug("Read macro setting from namelist {}", setting)
                setting = self.get_nml_value_from_string(self.nml, setting)
                if isinstance(setting, list):
                    setting = setting[0]["value"]
            return setting
        except KeyError:
            return default

    def extend_macro(self, key, val, macros, sep="#"):
        """Extend entries from macro.

        Args:
            key (_type_): _description_
            val (_type_): _description_
            macros (dict): Macros
            sep (str, optional): Namelist key separator. Defaults to "#".

        Raises:
            NotImplementedError: _description_
            NotImplementedError: _description_

        Returns:
            dict: Key, value dictionary
        """
        logger.debug("extenders={}", macros)
        if macros is None:
            return {key: val}

        processed_data = {}
        for macro, macro_types in macros.items():
            loop = {}
            for macro_type, macro_defs in macro_types.items():
                logger.debug("macro_defs={}", macro_defs)

                if macro_type == "ekfpert":
                    nncvs = self.read_macro_setting(macro_defs, "list", sep=sep)
                    logger.debug("nncvs={}", nncvs)
                    nncvs = nncvs.copy()
                    duplicate = self.read_macro_setting(macro_defs, "duplicate", sep=sep)
                    if duplicate:
                        nncvs += nncvs
                    loop.update({"0": "0"})
                    icounter1 = 1
                    icounter2 = 1
                    for nncv in nncvs:
                        if nncv == 1:
                            loop.update({str(icounter1): str(icounter2)})
                            icounter1 += 1
                        icounter2 += 1
                elif macro_type == "dict":
                    values = self.get_nml_value_from_string(self.nml, macro_defs)
                    counter = 0
                    for key, val in values[0].items():
                        loop.update({str(key): str(val)})
                        counter += 1

                elif macro_type == "iterator":
                    start = self.read_macro_setting(macro_defs, "start", sep=sep)
                    end = self.read_macro_setting(macro_defs, "end", sep=sep)
                    fmt = self.read_macro_setting(
                        macro_defs, "fmt", sep=sep, default=None
                    )
                    if fmt is None:
                        fmt = "{:d}"
                    for lval_ in range(start, end):
                        lval = fmt.format(lval_)
                        loop.update({str(lval): str(lval)})
                else:
                    raise NotImplementedError

            # Loop normal macros not being nml arrays
            unprocessed_data = processed_data.copy()
            unprocessed_data = processed_data if processed_data else {key: val}

            for key_, val in unprocessed_data.items():
                key = key_
                for vmacro1, vmacro2 in loop.items():
                    logger.debug(
                        "key={} val={} macro={} vmacro1={} vmacro2={}",
                        key,
                        val,
                        macro,
                        vmacro1,
                        vmacro2,
                    )
                    if key.find("#") > 0:
                        key = self.get_nml_value_from_string(self.nml, key, sep=sep)
                    pkey = key.replace(f"@{macro}@", vmacro1)
                    pval = val.replace(f"@{macro}@", vmacro2)
                    processed_data.update({pkey: pval})

        logger.debug("Processed data={}", processed_data)
        return processed_data

    def process_macro(self, key, val, macros, sep="#", indices=None):
        """Process macro.

        Args:
            key (str): Key
            val (str): Value
            macros (dict): Macros
            sep (str, optional): Namelist key separator. Defaults to "#".
            indices (list, optional): Process macro from namelist indices.

        Raises:
            NotImplementedError: Only 2 dimensions are implemented

        Returns:
            dict: Key, value dictionary
        """
        logger.debug("macros={}", macros)
        if macros is None:
            return key, val

        logger.debug("indices={}", indices)
        if indices is None:
            return key, val

        pkey = key
        pval = val
        for macro in macros:
            lindex = None
            if len(indices) == 2:
                lindex = indices[1] if macro == "DECADE" else indices[0]
            elif len(indices) == 1:
                lindex = indices[0]
            elif len(indices) > 2:
                raise NotImplementedError("Only 2 dimensions are implemented")

            vmacro = None
            if lindex is not None:
                try:
                    macro_defs = macros[macro]
                    logger.debug("macro_defs={}", macro_defs)
                except KeyError:
                    logger.warning(
                        "Macro {} not defined. Use index value {}", macro, lindex
                    )
                    vmacro = str(lindex + 1)

                if macro == "VTYPE":
                    vmacro = str(lindex + 1)
                elif "DECADE" in macros:
                    ntime = self.read_macro_setting(macro_defs, "ntime", sep=sep)
                    dec_days = int(360 / float(ntime))
                    dec_start = int(dec_days / 2)
                    dec_end = 360 + dec_start
                    dec = 0
                    for day in range(dec_start, dec_end, dec_days):
                        logger.debug("day={}, dec={} lindex={}", day, dec, lindex)
                        month = int(day / 30) + 1
                        mday = int(day % 30)
                        if dec == lindex:
                            vmacro = f"{month:02d}{mday:02d}"
                        dec += 1
                    if self.one_decade:
                        basetime = as_datetime(self.basetime)
                        vmacro = get_decade(basetime)

                logger.debug(
                    "Substitute @{}@ with {} pkey={} pval={}", macro, vmacro, pkey, pval
                )
                if isinstance(pkey, str):
                    pkey = pkey.replace(f"@{macro}@", vmacro)
                if isinstance(pval, str):
                    pval = pval.replace(f"@{macro}@", vmacro)
                logger.debug(
                    "Substitute @{}@ with {} pkey={} pval={}", macro, vmacro, pkey, pval
                )
        return pkey, pval

    def matching_value(self, data, val, sep="#", indices=None):
        """Match the value. Possibly also read namelist value.

        Args:
            data (dict): Data to check keys for
            val (str): Key to find
            sep (str, optional): Namelist separator. Defaults to "#".
            indices(list, optional): Indices in namelist

        Raises:
            RuntimeError: "Malformed input data"

        Returns:
            dict: Matching entry in data.

        """
        if val in ["macro", "extenders"]:
            return None
        logger.debug("type(data)={}", type(data))
        logger.debug("type(val)={}", type(val))
        logger.debug("indices={}", indices)
        mdata = data.keys() if isinstance(data, dict) else [data]
        val = str(val)
        logger.debug("Check if val={} matches mdata={}", val, mdata)
        sval = None
        for mval in mdata:
            if val.find(sep) > 0:
                logger.debug("val={} is a namelist variable", val)
                sval = self.get_nml_value_from_string(self.nml, val, indices=indices)
                logger.debug("Got sval={}", sval)
                if sval is None:
                    return None
                indices = sval[0]["indices"]
                sval = sval[0]["value"]
            if mval == val:
                logger.debug("Found matching data. val={} data={}", val, data)
                try:
                    rval = data[val]
                except TypeError:
                    raise RuntimeError("Malformed input data") from TypeError
                if sval is not None:
                    rval = {sval: rval}
                logger.debug("Return data rval={}", rval)
                return rval
        logger.warning("Value={} not found in data", val)
        return None

    def process_data(self, sep="#"):
        """Process input definitions on files to map.

        Args:
            sep (str, optional): Namelist separator. Defaults to "#".

        Returns:
            mapped_data (dict): A dict with mapped local names and target files.

        """
        logger.debug("Process data: {}", self.data)

        def _process_data(mapped_data, data, indices=None, macros=None, extenders=None):
            for key, value in data.items():
                logger.debug(".................. key={}", key)
                # Required namelist variable
                if key.find(sep) > 0:
                    vals = self.get_nml_value_from_string(self.nml, key, indices=indices)
                else:
                    vals = [{"value": value, "indices": None}]

                if isinstance(vals, list):
                    for val_dict in vals:
                        logger.debug("=========== val_dict={}", val_dict)
                        val = val_dict["value"]
                        indices = val_dict["indices"]

                        setting = self.matching_value(
                            value, val, sep=sep, indices=indices
                        )
                        logger.debug("Setting={}", setting)
                        if setting is not None:
                            if "macros" in setting:
                                macros = setting.copy()
                                macros = macros["macros"]
                            if "extenders" in setting:
                                extenders = setting.copy()
                                extenders = extenders["extenders"]

                            last_dict = True
                            if isinstance(setting, dict):
                                for tval in setting.values():
                                    if isinstance(tval, dict):
                                        last_dict = False

                            if not last_dict:
                                logger.debug("------ Call next loop. setting={}", setting)
                                _process_data(
                                    mapped_data,
                                    setting,
                                    indices=indices,
                                    macros=macros,
                                    extenders=extenders,
                                )
                            else:
                                for key2_, value2 in setting.items():
                                    key2 = key2_
                                    logger.debug(
                                        "Setting1 key={} value={} indices={}",
                                        key2,
                                        value2,
                                        indices,
                                    )
                                    if key2.find(sep) > 0:
                                        keys = self.get_nml_value_from_string(
                                            self.nml, key2, indices=indices
                                        )
                                        key2 = keys[0]["value"]

                                    processed = False
                                    logger.debug(
                                        "Setting2 key={} value={} indices={}",
                                        key2,
                                        value2,
                                        indices,
                                    )
                                    if macros is not None:
                                        processed = True
                                        key3, value3 = self.process_macro(
                                            key2, value2, macros, indices=indices
                                        )
                                        if value3.endswith(".dir"):
                                            dir_key = key3 + ".dir"
                                            dir_val = value3
                                            hdr_key = key3 + ".hdr"
                                            hdr_val = value3.replace(".dir", ".hdr")
                                            hdr_key, hdr_val = self.substitute(
                                                hdr_key, hdr_val
                                            )
                                            dir_key, dir_val = self.substitute(
                                                dir_key, dir_val
                                            )
                                            mapped_data.update({hdr_key: hdr_val})
                                            mapped_data.update({dir_key: dir_val})
                                        elif value3.endswith(".nc"):
                                            my_key, my_val = self.substitute(key3, value3)
                                            logger.debug(
                                                "my_key={}, my_val={}", my_key, my_val
                                            )
                                            if not my_key.endswith(".nc"):
                                                my_key = my_key + ".nc"
                                            mapped_data.update({my_key: my_val})
                                        elif value3.endswith(
                                            ".txt"
                                        ):  # need to remove the ".txt"
                                            my_key, my_val = self.substitute(key3, value3)
                                            logger.debug(
                                                "my_key={}, my_val={}", my_key, my_val
                                            )
                                            if not my_key.endswith(".txt"):
                                                my_key = my_key + ""
                                            mapped_data.update({my_key: my_val})
                                        else:
                                            my_key, my_val = self.substitute(key3, value3)
                                            mapped_data.update({my_key: my_val})

                                    if extenders is not None:
                                        processed = True
                                        processed_values = self.extend_macro(
                                            key2, value2, extenders
                                        )
                                        for pkey3, pval3 in processed_values.items():
                                            logger.debug(
                                                "pkey3={} pval3={}", pkey3, pval3
                                            )
                                            new_pkey3, new_pval3 = self.substitute(
                                                pkey3, pval3
                                            )
                                            logger.debug(
                                                "pkey3={} pval3={}", pkey3, pval3
                                            )
                                            mapped_data.update({new_pkey3: new_pval3})

                                    if not processed:
                                        pkey3 = key2
                                        pval3 = value2
                                        logger.debug("pkey3={} pval3={}", pkey3, pval3)
                                        if pval3.endswith(".nc") and not pkey3.endswith(
                                            ".nc"
                                        ):
                                            pkey3 = pkey3 + ".nc"
                                        pkey3, pval3 = self.substitute(pkey3, pval3)
                                        if pval3.endswith(".dir"):
                                            dir_key = pkey3 + ".dir"
                                            dir_val = pval3
                                            hdr_key = pkey3 + ".hdr"
                                            hdr_val = pval3.replace(".dir", ".hdr")
                                            hdr_key, hdr_val = self.substitute(
                                                hdr_key, hdr_val
                                            )
                                            dir_key, dir_val = self.substitute(
                                                dir_key, dir_val
                                            )
                                            mapped_data.update({hdr_key: hdr_val})
                                            mapped_data.update({dir_key: dir_val})
                                        else:
                                            mapped_data.update({pkey3: pval3})

                                indices = None
                        elif key not in ["macros", "extenders"]:
                            logger.warning("Could not match key={} value={}", key, val)
                else:
                    logger.warning("Could not find namelist key={}", key)
                    indices = None

        mapped_data = {}
        _process_data(mapped_data, self.data)
        logger.debug("Mapped data={}", mapped_data)
        return mapped_data


class Pgd(Task):
    """Task."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, "Pgd")
        self.program = "pgd"
        self.nlgen = NamelistGenerator(self.config, "surfex")
        self.climdir = self.platform.get_system_value("climdir")
        self.one_decade = self.config["pgd.one_decade"]
        self.pgd_prel = self.platform.substitute(
            self.config["file_templates.pgd_prel.archive"]
        )
        # TODO get from args
        self.force = True

    def execute(self):
        """Execute."""
        basetime = as_datetime(self.config["general.times.basetime"])
        output = f"{self.climdir}/{self.pgd_prel}"

        if not os.path.exists(output) or self.force:
            binary = self.get_binary("PGD")
            batch = BatchJob(os.environ, wrapper=self.wrapper)

            self.nlgen.load(self.program)
            settings = self.nlgen.assemble_namelist(self.program)
            self.nlgen.write_namelist(settings, "OPTIONS.nam")

            filetype = settings["nam_io_offline"]["csurf_filetype"].lower()
            pgdfile = settings["nam_io_offline"]["cpgdfile"]
            pgdfile = f"{pgdfile}.{filetype}"

            # Input data
            sfx_input_defs = self.platform.get_system_value("sfx_input_defs")
            with open(sfx_input_defs, "r", encoding="utf-8") as f:
                input_data = json.load(f)

            if self.one_decade:

                def replace(data, match, repl):
                    if isinstance(data, dict):
                        for k, v in data.items():
                            if isinstance(data[k], str):
                                data[k] = data[k].replace(match, repl)
                            replace(v, match, repl)
                    return data

                input_data = replace(input_data, "@DECADE@", get_decade(basetime))

            # Could potentially manipulate input_data depending on settings
            # or send input_data as input from an external file
            binput_data = InputDataFromNamelist(
                settings,
                input_data,
                self.program,
                self.platform,
                basetime=basetime,
                one_decade=self.one_decade,
            ).get()
            for dest, target in binput_data.items():
                logger.debug("target={}, dest={}", target, dest)
                self.fmanager.input(target, dest)

            # Run PGD
            batch.run(binary)
            self.fmanager.output(pgdfile, output)
            self.archive_logs(["OPTIONS.nam", "LISTING_PGD.txt"], target=self.climdir)
        else:
            logger.warning("Output already exists: ", output)


class Prep(Task):
    """Prep."""

    def __init__(self, config):
        """Construct object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, "Prep")
        self.nlgen = NamelistGenerator(self.config, "surfex")
        self.archive = self.platform.get_system_value("archive")
        # TODO get from args
        self.force = True

    def execute(self):
        """Execute."""
        cnmexp = self.config["general.cnmexp"]
        output = f"{self.archive}/ICMSH{cnmexp}INIT.sfx"

        if not os.path.exists(output) or self.force:
            binary = self.get_binary("PREP")
            deodemakedirs(self.archive)
            batch = BatchJob(os.environ, wrapper=self.wrapper)

            bd_has_surfex = self.config["boundaries.bd_has_surfex"]
            basetime = as_datetime(self.config["general.times.basetime"])

            namelist_task = "prep"
            self.nlgen.load(namelist_task)
            settings = self.nlgen.assemble_namelist(namelist_task)
            settings["nam_prep_surf_atm"]["nyear"] = int(basetime.strftime("%Y"))
            settings["nam_prep_surf_atm"]["nmonth"] = int(basetime.strftime("%m"))
            settings["nam_prep_surf_atm"]["nday"] = int(basetime.strftime("%d"))
            settings["nam_prep_surf_atm"]["xtime"] = (
                basetime.hour * 3600 + basetime.minute * 60 + basetime.second
            )

            self.nlgen.write_namelist(settings, "OPTIONS.nam")

            # Input data
            sfx_input_defs = self.platform.get_system_value("sfx_input_defs")
            with open(sfx_input_defs, "r", encoding="utf-8") as f:
                input_data = json.load(f)

            # Determine PGD type and name
            filetype = settings["nam_io_offline"]["csurf_filetype"].lower()
            pgd = settings["nam_io_offline"]["cpgdfile"]
            pgdfile = f"{pgd}.{filetype}"

            # PGD file input update
            const_clim = self.config["file_templates.pgd.archive"]
            pgdfile_source = self.platform.substitute(f"@CLIMDIR@/{const_clim}")
            input_data["prep"]["NAM_IO_OFFLINE#CPGDFILE"] = {
                pgd: {pgdfile: pgdfile_source}
            }

            if bd_has_surfex:
                # Host model PGD type and name
                filetype = settings["nam_prep_surf_atm"]["cfilepgdtype"].lower()
                pgd_host = settings["nam_prep_surf_atm"]["cfilepgd"]
                const_clim_host = self.config["file_templates.pgd_host.archive"]
                pgd_host_source = self.platform.substitute(
                    f"@BDCLIMDIR@/{const_clim_host}"
                )
                input_data["prep"]["NAM_PREP_SURF_ATM#CFILEPGD"] = {
                    pgd_host: {f"{pgd_host}.{filetype}": pgd_host_source}
                }

            # Determine prep output name
            prep_output_file = settings["nam_io_offline"]["cprepfile"]
            prep_output_file = f"{prep_output_file}.{filetype}"

            # Select the correct input file
            basetime = as_datetime(self.config["general.times.basetime"])
            bddir_sfx = self.config["system.bddir_sfx"]
            bdfile_sfx_template = self.config["system.bdfile_sfx_template"]
            bdcycle = as_timedelta(self.config["boundaries.bdcycle"])
            bdshift = as_timedelta(self.config["boundaries.bdshift"])

            bd_basetime = basetime - cycle_offset(basetime, bdcycle, shift=-bdshift)
            prep_input_file = self.platform.substitute(
                f"{bddir_sfx}/{bdfile_sfx_template}",
                basetime=bd_basetime,
                validtime=basetime,
            )

            # Update input file linking
            filetype_ext = {"FA": ".fa", "GRIB": ""}
            cfiletype = settings["nam_prep_surf_atm"]["cfiletype"].upper()
            cfile = settings["nam_prep_surf_atm"]["cfile"]
            cfileext = filetype_ext[cfiletype]
            input_data["prep"]["NAM_PREP_SURF_ATM#CFILETYPE"] = {
                cfiletype: {f"{cfile}{cfileext}": prep_input_file}
            }

            # Fetch the input
            binput_data = InputDataFromNamelist(
                settings, input_data, "prep", self.platform
            ).get()
            for dest, target in binput_data.items():
                logger.debug("target={}, dest={}", target, dest)
                self.fmanager.input(target, dest)

            # Run PREP and archive output
            batch.run(binary)
            self.fmanager.output(prep_output_file, output)
            self.archive_logs(["OPTIONS.nam", "LISTING_PREP0.txt"])

        else:
            logger.info("Output already exists: {}", output)
