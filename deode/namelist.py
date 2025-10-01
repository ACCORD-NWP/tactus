#!/usr/bin/env python3
"""Namelist handling for MASTERODB w/SURFEX."""
import ast
import copy
import os
import re
import subprocess
from collections import OrderedDict
from pathlib import Path

import f90nml
import yaml
from omegaconf import OmegaConf
from omegaconf.listconfig import ListConfig

from .config_parser import ConfigPaths
from .csc_actions import SelectTstep
from .datetime_utils import as_timedelta, oi2dt_list
from .logs import logger
from .toolbox import Platform


def flatten_cn(li):
    """Recursively flatten a list of lists (of lists)."""
    for x in li:
        if isinstance(x, (list, ListConfig)):
            yield from flatten_cn(x)
        else:
            yield x


def to_dict(x):
    """Recursively modify OrderedDict etc. to normal dict (required for OmegaConf)."""
    # Namelist and OrderedDict both inherit from dict
    if isinstance(x, dict):
        return {k: to_dict(v) for k, v in x.items()}
    if isinstance(x, list):
        return [to_dict(k) for k in x]
    return x


def find_value(s):
    """Purpose: un-quote (list of) numbers and booleans."""
    if isinstance(s, list):
        return [find_value(x) for x in s]

    if isinstance(s, dict):
        # This happens if the namelist is read with f90nml
        # any name with "%" is parsed as a sub-namelist
        # so an extra level in the structure
        logger.debug("EVALUATE: SUB-DICT")
        return {k: find_value(v) for k, v in s.items()}

    if not isinstance(s, str):
        return s
    if s.lower() == "true":
        logger.debug("EVALUATE BOOLEAN: {} -> {}", s, True)
        result = True
    elif s.lower() == "false":
        logger.debug("EVALUATE BOOLEAN: {} -> {}", s, False)
        result = False
    else:
        try:
            result = ast.literal_eval(s)
            logger.debug("EVALUATE: {} -> {}", s, result)
        except (ValueError, SyntaxError):
            logger.debug("UN-EVALUATED: {}", s)
            result = s
    if isinstance(result, tuple):
        result = list(result)

    if isinstance(result, list):
        logger.debug("EVALUATE NESTED: {} -> {}")
        result = [find_value(x) for x in result]

    return result


def list_set_at_index(li, ix, val):
    """Handle 'sparse' lists (that may contain null values)."""
    try:
        li[ix] = val
    except IndexError:
        for _ in range(ix - len(li) + 1):
            li.append(None)
        li[ix] = val


def represent_ordereddict(dumper, data):
    """YAML representer, simplifies working with namelists read by f90nml."""
    my_od = []
    for k, v in data.items():
        # Convert plain keys to upper case, but leave the internal ones in lower
        ku = k if k.startswith("_") else k.upper()
        km = dumper.represent_data(ku)
        vm = dumper.represent_data(v)
        my_od.append((km, vm))
    return yaml.nodes.MappingNode("tag:yaml.org,2002:map", my_od)


def write_namelist(nml, output_file):
    """Write namelist using f90nml.

    Args:
        nml (f90nml.Namelist): namelist to write
        output_file (str) : namelist file name

    """
    if isinstance(nml, dict):
        nml = f90nml.Namelist(nml)
    # Write result.
    nml.uppercase = True
    nml.true_repr = ".TRUE."
    nml.false_repr = ".FALSE."
    nml.end_comma = True  # AD: temp fix for IO_SERVER bug
    nml.write(output_file, force=True)

    logger.debug("Wrote: {}", output_file)


class InvalidNamelistKindError(ValueError):
    """Custom exception."""


class InvalidNamelistTargetError(ValueError):
    """Custom exception."""


class NamelistComparator:
    """Helper class for namelist generation and integration."""

    def __init__(self, config):
        """Construct the comparator.

        Args:
            config (deode.ParsedConfig): Configuration

        Raises:
            SystemExit

        """
        self.config = config
        self.platform = Platform(config)

    def compare_dicts(self, dbase, dcomp, action):
        """Compare two dictionaries, recursively if needed.

        The dict must only consist of dict, list, str, float, int, bool

        Args:
            dbase: base dict
            dcomp: comparison dict
            action(str): one of 'intersection', 'diff' or 'union'

        Returns:
            dout: result dict from the specified comparison action

        Raises:
            SystemExit   # noqa: DAR401

        """
        if action not in ("intersection", "diff", "union"):
            raise SystemExit(f"Unknown action {action}")
        dout = {}
        dout["_start_index"] = {}
        for key in dbase:
            valb = dbase[key]
            if key in dcomp:
                valc = dcomp[key]
                if isinstance(valb, dict) and isinstance(valc, dict):
                    if not key.startswith("_"):
                        # Invoke ourselves recursively
                        msg = f"recursive dict comp for {key}"
                        logger.debug(msg)
                        dout[key] = self.compare_dicts(dbase[key], dcomp[key], action)
                elif isinstance(valb, list) and isinstance(valc, list):
                    kl = key.lower()
                    try:
                        sib = dbase["_start_index"][kl]
                    except KeyError:
                        sib = [1]
                    try:
                        sic = dcomp["_start_index"][kl]
                    except KeyError:
                        sic = [1]
                    # Start index in output depends on the action, thus:
                    sio = [-999 for _ in range(len(sib))]
                    msg = f"Compare lists {key}, start indices {sib}, {sic}"
                    logger.debug(msg)
                    dout[key] = self.compare_lists(
                        dbase[key], dcomp[key], sib, sic, sio, action
                    )
                    dout["_start_index"][kl] = sio
                elif valc == valb:
                    # This should be a scalar, with same value
                    msg = f"{key} : {valb} == {valc}"
                    logger.debug(msg)
                    if action != "diff":
                        dout[key] = valc
                else:
                    # also scalar, but values differ
                    msg = f"{key} : {valb} /= {valc}"
                    logger.debug(msg)
                    if action != "intersection" and valc is not None:
                        dout[key] = valc
            elif action == "union":
                # Key only found in base
                dout[key] = valb
            elif action == "diff":
                # Mark for deletion in case of 'union'
                dout[key] = None
        if action != "intersection":
            # Add keys not in base
            for key in dcomp:
                if key not in dbase:
                    dout[key] = dcomp[key]
            # Remove empty namelists in diffs
            if action == "diff":
                todel = []
                for key in dout:
                    if isinstance(dout[key], dict) and len(dout[key]) == 0:
                        todel.append(key)  # noqa: PERF401
                # Delayed deletion to avoid "dictionary changed size during iteration"
                for key in todel:
                    del dout[key]
        # Avoid empty _start_index
        if "_start_index" in dout and not dout["_start_index"]:
            del dout["_start_index"]
        return dout

    def compare_lists(self, libase, licomp, sib, sic, sio, action):
        """Compare two lists, recursively if needed.

        The list must only consist of dict, list, str, float, int, bool

        Args:
            libase: base list
            licomp: comparison list
            sib: list of start indices for libase
            sic: list of start indices for licomp
            sio: list of start indices for liout (modified)
            action(str): one of 'intersection', 'diff' or 'union'

        Returns:
            liout: result list from the specified comparison action

        Raises:
            SystemExit   # noqa: DAR401

        """
        if action not in ("intersection", "diff", "union"):
            raise SystemExit(f"Unknown action {action}")
        liout = []
        if action == "intersection":
            sio[0] = max(sib[0], sic[0])
        else:
            sio[0] = min(sib[0], sic[0])
        for ib in range(len(libase)):
            ic = ib + sib[0] - sic[0]
            io = ib + sib[0] - sio[0]
            valb = libase[ib]
            if ic in range(len(licomp)):
                valc = licomp[ic]
                if isinstance(valb, dict) and isinstance(valc, dict):
                    list_set_at_index(
                        liout, io, self.compare_dicts(libase[ib], licomp[ic], action)
                    )
                elif isinstance(valb, list) and isinstance(valc, list):
                    # Invoke ourselves recursively
                    list_set_at_index(
                        liout,
                        io,
                        self.compare_lists(
                            libase[ib], licomp[ic], sib[1:], sic[1:], sio[1:], action
                        ),
                    )
                elif valc == valb:
                    # Scalar, same value
                    if action != "diff":
                        list_set_at_index(liout, io, valc)
                elif action != "intersection":
                    # Scalar, different value
                    list_set_at_index(liout, io, valc)
            elif action == "union":
                list_set_at_index(liout, io, valb)
            elif action == "diff":
                # Mark for deletion in case of 'union'
                list_set_at_index(liout, io, None)
        if action != "intersection":
            # Add elements not in base
            for ic in range(len(licomp)):
                ib = ic + sic[0] - sib[0]
                io = ic + sic[0] - sio[0]
                if ib not in range(len(libase)):
                    list_set_at_index(liout, io, licomp[ic])
        return liout


class NamelistGenerator:
    """Fortran namelist generator based on hierarchical merging of (yaml) dicts."""

    def __init__(self, config, kind, substitute=True):
        """Construct the generator.

        Args:
            config (deode.ParsedConfig): Configuration
            kind (str): one of 'master' or 'surfex'
            substitute (boolean): flag for substitution

        Raises:
            InvalidNamelistKindError   # noqa: DAR401

        """
        if kind not in ("master", "surfex"):
            raise InvalidNamelistKindError(kind)

        self.config = config
        self.platform = Platform(config)
        self.kind = kind
        self.substitute = substitute
        self.nlcomp = NamelistComparator(config)
        self.cycle = self.config["general.cycle"]
        self.cnfile = ConfigPaths.path_from_subpath(f"{self.cycle}/assemble_{kind}.yml")
        self.nlfile = ConfigPaths.path_from_subpath(f"{self.cycle}/{kind}_namelists.yml")
        self.domain_name = self.config["domain.name"]
        self.accept_static_namelist = self.config["general.accept_static_namelists"]

    def load_user_namelist(self):
        """Read user provided namelist.

        Returns:
            found (boolean) : Logics if namelist is found or not
            nldict : Namelist as dictionary
            cndict : Rules for dictionary

        """
        namelists = self.platform.get_system_value("namelists")
        ref_namelist = f"{namelists}/namelist_{self.kind}_{self.target}"

        logger.debug("Check if reference namelist {} exists", ref_namelist)
        if os.path.isfile(ref_namelist):
            logger.info("Use reference namelist {}", ref_namelist)
            nl = f90nml.read(ref_namelist)
            target = "user_namelist"
            # NOTE: f90nml.todict() returns OrderedDict
            #       which makes OmegaConf fail.
            #       but maybe we should consider to_dict(nl.todict()) ???
            nldict = {target: to_dict(nl.todict())}
            cndict = {self.target: [target]}
            found = False
        else:
            logger.warning("No reference namelist {} exists.", ref_namelist)
            logger.warning("Fallback to yaml files")

            found = True
            nldict = {}
            cndict = {}

        return found, nldict, cndict

    def fn_stepfreq(self, arg):
        """Resolve namelist function stepfreq."""
        tstep = self.platform.substitute(self.config["domain.tstep"])
        try:
            tstep = int(tstep)
        except ValueError:
            tstep = self.platform.evaluate(tstep, SelectTstep)

        freq = as_timedelta(arg)
        result = int(freq.seconds / tstep)
        return f"{result}"

    def fn_tstep(self, arg):
        """Resolve namelist function tstep."""
        try:
            result = int(arg)
        except ValueError:
            result = self.platform.evaluate(arg, SelectTstep)

        return f"{result}"

    def fn_steplist(self, time_intervals):
        """Resolve namelist function steplist."""
        forecast_range = self.config["general.times.forecast_range"]
        tstep = self.platform.substitute(self.config["domain.tstep"])
        try:
            tstep = int(tstep)
        except ValueError:
            tstep = self.platform.evaluate(tstep, SelectTstep)
        # default value:
        output_timesteps = [1, -1]
        # decode string into list
        time_intervals = find_value(time_intervals)

        dtlist = oi2dt_list(time_intervals, forecast_range)
        logger.debug("steplist: {} // {}", time_intervals, forecast_range)
        logger.debug("dtlist: {}", dtlist)
        output_timesteps = [
            int((dt.days * 24 * 3600 + dt.seconds) / tstep) for dt in dtlist
        ]
        output_timesteps.insert(0, len(output_timesteps))
        logger.debug("result: {}", output_timesteps)
        # NOTE: a resolver can not return a list
        # so turn into a string
        return f"{output_timesteps}"

    def fn_config(self, arg, default=None):
        """Resolve namelist function cfg."""
        try:
            _result = self.platform.config[arg]
            result = self.platform.substitute(_result)
            logger.debug("CFG INSERT: {} -> {}", arg, result)
        except KeyError:
            result = default if default is not None else arg
            logger.debug("CFG UNKNOWN: {} default {}", arg, default)
        # NOTE: all values are returned as STRINGS
        #       which means you must re-interpret with find_val()
        #       this is only really necessary for e.g. lists
        return f"{result}"

    def resolve_macros(self, cn):
        """Resolve all macros in a nested dict (both keys and values!)."""
        if isinstance(cn, list):
            result = [self.platform.substitute(x) for x in cn]
        elif isinstance(cn, dict):
            result = {
                self.platform.substitute(x): self.resolve_macros(y) for x, y in cn.items()
            }
        else:
            result = self.platform.substitute(cn)
        return result

    def load(self, target):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for

        Raises:
            InvalidNamelistTargetError   # noqa: DAR401

        Returns:
            nlres (dict): Assembled namelist

        """
        self.target = target
        # define OmegaConf resolvers
        OmegaConf.clear_resolvers()
        OmegaConf.register_new_resolver("stepfreq", lambda arg: self.fn_stepfreq(arg))
        OmegaConf.register_new_resolver("steplist", lambda arg: self.fn_steplist(arg))
        OmegaConf.register_new_resolver(
            "cfg", lambda arg, default=None: self.fn_config(arg, default)
        )
        OmegaConf.register_new_resolver("timestep", lambda arg: self.fn_tstep(arg))

        # Use static namelist if given
        use_yaml = True
        if self.accept_static_namelist:
            use_yaml, nldict0, cndict0 = self.load_user_namelist()
            nldict = OmegaConf.create(nldict0)
            cndict = OmegaConf.create(cndict0)

        if use_yaml:
            logger.info("Namelist generation input:")
            logger.info(" namelists: {}", self.nlfile)
            logger.info(" rules: {}", self.cnfile)
            # Read namelist file with all the categories
            with open(self.nlfile, mode="rt", encoding="utf-8") as file:
                nldict = OmegaConf.load(file)

            # Read file that describes assembly category order
            # for the various targets (tasks)
            with open(self.cnfile, mode="rt", encoding="utf-8") as file:
                cndict_m1 = yaml.safe_load(file)
                cndict_m2 = self.resolve_macros(cndict_m1)
                cndict = OmegaConf.create(cndict_m2)
                OmegaConf.resolve(cndict)

        # Check target is valid
        if target not in cndict:
            logger.warning(
                "Could not find target namelist '{}' in {}", target, str(self.cnfile)
            )
            msg = "Available namelist targets:"
            for key in cndict:
                if not re.match(r"_.+", key):
                    msg += " " + key + ","
            logger.warning(msg[:-1])
            raise InvalidNamelistTargetError(target)

        self.nldict = nldict
        self.cndict = cndict

        logger.info("Namelist updating")
        self.update_from_config(target)

        return self.nldict, self.cndict

    def assemble_namelist(self, target):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for

        Returns:
            nlres (f90nml.Namelist): Assembled namelist

        """
        nldict = self.nldict

        # Assemble the target namelists based on the given category order
        # also replace all macro's ("@XXX@")
        cnlist = [self.platform.substitute(x) for x in flatten_cn(self.cndict[target])]

        # Merge all the partial namelists
        logger.info("Namelist assembly sections {}", cnlist)
        nl_merged = OmegaConf.to_container(
            OmegaConf.merge(*[nldict[i] for i in cnlist if i in nldict]),
            resolve=self.substitute,
        )

        # make sure that booleans, integers etc. are not represented as strings!
        if self.substitute:
            nlres = {
                n: {v: find_value(vx) for v, vx in nx.items()}
                for n, nx in nl_merged.items()
            }
        else:
            nlres = nl_merged

        logger.debug("FINAL NAMELIST: {}", nlres)
        return f90nml.Namelist(nlres)

    def write_namelist(self, nml, output_file):
        """Write namelist using f90nml.

        Args:
            nml (f90nml.Namelist): namelist to write
            output_file (str) : namelist file name
        """
        logger.info("Writing main namelist to {}", output_file)
        write_namelist(nml, output_file)

    # NOTE: should also work with OmegaConf objects
    def update_from_config(self, target):
        """Update with additional namelist dict from config.

        Args:
            target (str): task to generate namelists for

        """
        # Try to update potential global settings first
        if target != "all_targets":
            self.update_from_config("all_targets")

        try:
            _update = self.config["namelist_update"][self.kind][target].dict()
            # Make sure everything is in upper case
            update = {}
            for namelist, keyval in _update.items():
                nu = namelist.upper()
                update[nu] = {}
                for key, val in keyval.items():
                    update[nu][key.upper()] = self.platform.substitute(val)

            self.update(update, f"namelist_update_{target}")
            logger.info("Namelist update found for {} {}", self.kind, target)
        except KeyError:
            pass

    # NOTE: should also work with OmegaConf objects
    def update(self, nldict, cndict_tag):
        """Update with additional namelist dict.

        Args:
            nldict (dict): additional namelist dict
            cndict_tag: name to be used for recognition

        """
        self.cndict[self.target].append(cndict_tag)
        self.nldict[cndict_tag] = nldict  # maybe OmegaConf.create(nldict)

    def generate_namelist(self, target, output_file):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for
            output_file: where to write the result (fort.4 or EXSEG1.nam typically)

        """
        logger.info("Generate namelist for: {}", target)
        self.load(target)
        nml = self.assemble_namelist(target)
        self.write_namelist(nml, output_file)


class NamelistIntegrator:
    """Helper class to read fortran namelists and store as yaml, in a more compact form.

    Reduces duplication if several namelists have similar settings.

    """

    def __init__(self, config):
        """Construct the integrator.

        Args:
            config (deode.ParsedConfig): Configuration

        Raises:
            SystemExit   # noqa: DAR401

        """
        self.config = config
        self.platform = Platform(config)
        yaml.add_representer(OrderedDict, represent_ordereddict)

    def ftn2dict(self, ftnfile):
        """Read fortran namelist file with f90nml and return as plain dict."""
        fnml = f90nml.read(ftnfile)
        # The internal representation used in f90nml is not easy to work with, thus
        ynml = yaml.safe_load(yaml.dump(fnml.todict(complex_tuple=True)))
        # Check if there are duplicated namelists
        #   - ignore empty ones if they don't come first
        onml = {}
        dupl = {}
        for key in ynml:
            # Should not need to know the internal coding of f90nml :(
            if key.startswith("_grp_"):
                a = key[5:].rsplit("_", 1)
                namu = a[0].upper()
                nseq = int(a[1])
                if nseq == 0:
                    onml[namu] = ynml[key]
                else:
                    lk = len(ynml[key])
                    if lk == 0:
                        msg = f"Ignoring empty duplicate namelist {namu}"
                        logger.warning(msg)
                    else:
                        dupl[namu] = lk
                        msg = f"Found duplicate namelist {namu} with {lk} extra line(s)!"
                        logger.warning(msg)
            else:
                onml[key] = ynml[key]
        if len(dupl) > 0:
            which = ", ".join(dupl)
            msg = (
                f"The following namelists in {ftnfile} have non-empty duplicates: {which}"
            )
            logger.warning(msg)
        return onml

    @staticmethod
    def yml2dict(ymlfile):
        """Read yaml namelist file and return as dict."""
        with open(ymlfile, mode="rt", encoding="utf-8") as file:
            ynml = yaml.safe_load(file)
        return ynml

    @staticmethod
    def dict2yml(nmldict, ymlfile, ordered_sections=None):
        """Write dict as yaml file."""
        with open(ymlfile, mode="wb") as file:
            if ordered_sections:
                for section in ordered_sections:
                    if section in nmldict:
                        output_dict = {}
                        output_dict[section] = nmldict[section]
                        yaml.dump(
                            output_dict, file, encoding="utf-8", default_flow_style=False
                        )
            else:
                yaml.dump(nmldict, file, encoding="utf-8", default_flow_style=False)


class NamelistConverter:
    """Helper class to convert namelists between cycles, based on thenamelisttool."""

    @staticmethod
    def get_known_cycles():
        """Return the cycles handled by the converter."""
        return ["CY48t2", "CY48t3", "CY49", "CY49t1", "CY49t2", "CY50t1"]

    @staticmethod
    def get_to_next_version_tnt_filenames():
        """Return the tnt file names between get_known_cycles()."""
        return [
            None,  # CY48t2 to CY48t3
            "cy48t2_to_cy49.yaml",  # CY48t3 to CY49
            "cy49_to_cy49t1.yaml",  # CY49   to CY49t1
            None,  # CY49t1 to CY49t2,
            "cy50_to_cy50t1.yaml",  # CY49t2 to CY5051
        ]

    @staticmethod
    def get_tnt_files_list(from_cycle, to_cycle):
        """Return the list of tnt directive files required for the conversion."""
        # definitions of the conversion to apply between cycles
        tnt_directives_folder = ConfigPaths.path_from_subpath("tnt_directives")

        if from_cycle and to_cycle:
            known_cycles = NamelistConverter.get_known_cycles()
            to_next_version_tnt_filenames = (
                NamelistConverter.get_to_next_version_tnt_filenames()
            )

            try:
                start_index = known_cycles.index(from_cycle)
            except ValueError:
                raise SystemExit(
                    f"ERROR: from-cycle {from_cycle} unknown"
                ) from ValueError

            try:
                target_index = known_cycles.index(to_cycle)
            except ValueError:
                raise SystemExit(f"ERROR: to-cycle {to_cycle} unknown") from ValueError

            # Verify that to_cycle is older than from_cycle
            if start_index > target_index:
                raise SystemExit(
                    f"ERROR: No conversion possible between {from_cycle} and {to_cycle}"
                )
        else:
            start_index = 0
            target_index = 0

        if start_index == target_index:
            # Apply empty conversion
            tnt_files = [tnt_directives_folder / "empty.yaml"]
        else:
            # Apply all the intermediate conversions
            tnt_files = [
                tnt_directives_folder / to_next_version_tnt_filenames[index]
                for index in range(start_index, target_index)
                if to_next_version_tnt_filenames[index]
            ]

        return tnt_files

    @staticmethod
    def convert_yml(input_yml, output_yml, from_cycle, to_cycle):
        """Convert a namelist in yml file between two cycles.

        Args:
            input_yml: the input yaml filename
            output_yml: the output yaml filename
            from_cycle: the input cycle
            to_cycle: the target cycle

        Raises:
            SystemExit: when conversion failed
        """
        tnt_files = NamelistConverter.get_tnt_files_list(from_cycle, to_cycle)

        # Read the input namelist file (yaml)
        logger.info(f"Read {input_yml}")
        nmldict = NamelistIntegrator.yml2dict(Path(input_yml))

        with open(Path(input_yml), mode="rt", encoding="utf-8") as file:
            ordered_sections = [
                line.split(":")[0]
                for line in file.readlines()
                if ":" in line and line.split(":")[0] in nmldict
            ]

        for tnt_file in tnt_files:
            nmldict = NamelistConverter.apply_tnt_directives_to_namelist_dict(
                tnt_file, nmldict
            )
            if not nmldict:
                raise SystemExit("Name list conversion failed.  ")

        # Write the output namelist file (yaml)
        logger.info(f"Write {output_yml}")
        if "empty" in nmldict and "empty" not in ordered_sections:
            ordered_sections.append("empty")

        NamelistIntegrator.dict2yml(nmldict, Path(output_yml), ordered_sections)

    @staticmethod
    def convert_ftn(input_ftn, output_ftn, from_cycle, to_cycle):
        """Convert a namelist in fortran file between two cycles.

        Args:
            input_ftn: the input fortran filename
            output_ftn: the output fortran filename
            from_cycle: the input cycle
            to_cycle: the target cycle
        """
        tnt_files = NamelistConverter.get_tnt_files_list(from_cycle, to_cycle)

        logger.info(f"Read {input_ftn}")
        ftn_file = input_ftn
        temporary_files = []
        for tnt_file in tnt_files:
            NamelistConverter.apply_tnt_directives_to_ftn_namelist(tnt_file, ftn_file)
            ftn_file = ftn_file + ".tnt"
            temporary_files.append(ftn_file)

        nl = f90nml.read(ftn_file)
        logger.info(f"Write {output_ftn}")
        write_namelist(nl, output_ftn)

        for file in temporary_files:
            os.remove(file)

    @staticmethod
    def apply_tnt_directives_to_namelist_dict(tnt_directive_filename, namelist_dict):
        """Apply the tnt directives to a namelist as dictionary.

        Args:
            tnt_directive_filename: the tnt directive filename
            namelist_dict: the namelist dictionary

        Returns:
            new_namelist: the converted namelist dictionary

        Raises:
            SystemExit: when conversion failed
        """
        logger.info(f"Apply {tnt_directive_filename}")
        # Open the directive file
        with open(tnt_directive_filename, mode="rt", encoding="utf-8") as file:
            tnt_directives = yaml.safe_load(file)
        file.close()

        # Use a copy to be able to modify dictionaries during iterations
        new_namelist = copy.deepcopy(namelist_dict)

        # Move keys from one section to another
        if "keys_to_move" in tnt_directives:
            for old_block in tnt_directives["keys_to_move"]:
                for old_key in tnt_directives["keys_to_move"][old_block]:
                    for new_block in tnt_directives["keys_to_move"][old_block][old_key]:
                        new_key = tnt_directives["keys_to_move"][old_block][old_key][
                            new_block
                        ]

                        for namelists_section in namelist_dict:
                            for namelist_block in namelist_dict[namelists_section]:
                                if (
                                    old_block in namelist_block
                                    and old_key
                                    in namelist_dict[namelists_section][namelist_block]
                                ):
                                    if new_block not in new_namelist[namelists_section]:
                                        new_namelist[namelists_section][new_block] = {}
                                    new_namelist[namelists_section][new_block][
                                        new_key
                                    ] = namelist_dict[namelists_section][old_block][
                                        old_key
                                    ]
                                    del new_namelist[namelists_section][old_block][
                                        old_key
                                    ]
                                    if (
                                        len(new_namelist[namelists_section][old_block])
                                        == 0
                                    ):
                                        del new_namelist[namelists_section][old_block]

        if "keys_to_set" in tnt_directives:
            for block_to_set in tnt_directives["keys_to_set"]:
                for namelists_section in namelist_dict:
                    if namelists_section != "empty":
                        for keys_to_set in tnt_directives["keys_to_set"][block_to_set]:
                            if block_to_set in namelist_dict[namelists_section]:
                                key_value = tnt_directives["keys_to_set"][block_to_set][
                                    keys_to_set
                                ]
                                if key_value in [".T.", ".TRUE."]:
                                    key_value = True
                                if key_value in [".F.", ".FALSE."]:
                                    key_value = False
                                new_namelist[namelists_section][block_to_set][
                                    keys_to_set
                                ] = key_value
                            else:
                                logger.warning(
                                    f"'No {block_to_set} in {namelists_section}: \
                                    skip insertion of {keys_to_set}'"
                                )

        # Creation of new blocks
        if "new_blocks" in tnt_directives:
            for new_block in tnt_directives["new_blocks"]:
                if "empty" not in new_namelist:
                    new_namelist["empty"] = {}
                new_block_upper = new_block.upper()
                if new_block_upper not in new_namelist["empty"]:
                    new_namelist["empty"][new_block_upper] = {}

        # Move of blocks(Not implemented)
        if "blocks_to_move" in tnt_directives:
            for blocks in tnt_directives["blocks_to_move"]:
                if blocks in namelist_block:
                    raise SystemExit("conversion FAILED: blocks_to_move not implemented")

        # Delete keys
        if "keys_to_remove" in tnt_directives:
            for block_to_remove in tnt_directives["keys_to_remove"]:
                for namelists_section in namelist_dict:
                    for namelist_block in namelist_dict[namelists_section]:
                        if block_to_remove in namelist_block:
                            for key_to_remove in tnt_directives["keys_to_remove"][
                                block_to_remove
                            ]:
                                if (
                                    key_to_remove
                                    in namelist_dict[namelists_section][block_to_remove]
                                ):
                                    del new_namelist[namelists_section][block_to_remove][
                                        key_to_remove
                                    ]
                                    if (
                                        len(
                                            new_namelist[namelists_section][
                                                block_to_remove
                                            ]
                                        )
                                        == 0
                                    ):
                                        del new_namelist[namelists_section][
                                            block_to_remove
                                        ]

        return new_namelist

    @staticmethod
    def apply_tnt_directives_to_ftn_namelist(tnt_directive_filename, input_ftn):
        """Apply the tnt directives to a fotran namelist using tnt.

        Args:
            tnt_directive_filename: the tnt directive filename
            input_ftn: the namelist fortran file

        Raises:
           SystemExit: when conversion failed
        """
        logger.info(f"Apply {tnt_directive_filename}")
        tnt_directives_folder = ConfigPaths.path_from_subpath(
            "tnt_directives",
        )
        command = [
            "tnt.py",
            "-d",
            tnt_directives_folder / tnt_directive_filename,
            input_ftn,
        ]

        try:
            subprocess.check_call(command)  # noqa S603
        except subprocess.CalledProcessError as exception:
            raise SystemExit(f"tnt failed with {exception!r}") from exception
