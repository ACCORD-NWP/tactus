#!/usr/bin/env python3
"""Fullpos namelist generation."""


import yaml

from .config_parser import ConfigPaths
from .general_utils import merge_dicts
from .logs import logger


def flatten_list(li):
    """Recursively flatten a list of lists (of lists)."""
    if li == []:
        return li
    if isinstance(li[0], list):
        return flatten_list(li[0]) + flatten_list(li[1:])
    return li[:1] + flatten_list(li[1:])


class InvalidSelectionCombinationError(ValueError):
    """Custom exception."""


class Fullpos:
    """Fullpos namelist generator based on (yaml) dicts."""

    def __init__(self, domain, fpdir=".", fpfiles=None, fpdict=None, rules=None):
        """Construct the fullpos generator.

        Args:
            domain (str): Domain name
            fpdir (str): Path to fullpos config files
            fpfiles (list): List of fullpos config files to read (without the yml suffix)
            fpdict (dict): A dictionary of fullpos settings
            rules (dict): A dictionary of substitution rules

        """
        self.domain = domain
        self.fpdir = fpdir
        self.nldict = {}
        self.rules = rules if rules is not None else {}
        if fpfiles is not None:
            self.nldict.update(self.load(fpdir, fpfiles))

        if fpdict is not None:
            self.nldict.update(fpdict)

    def expand(self, v, levtype, levels, domain, i):
        """Expand fullpos namelists to levels and domains.

        Args:
            v (str): parameter list
            levtype (str): type of vertical level in fullpos syntax
            levels (list): list of levels
            domain (str): domain name
            i (int): parameter conuter

        Returns:
            d (dict): expaned names
            i (int): parameter counter

        """
        d = {}
        for p in v["CL3DF"]:
            i += 1
            j = 0
            par = f"CL3DF({i})"
            d[par] = p
            for level in v[levtype]:
                j += 1
                lev = f"IL3DF({j},{i})"
                dom = f"CLD3DF({j},{i})"
                d[lev] = levels.index(level) + 1
                d[dom] = domain

        return d, i

    def load(self, fpdir, fpfiles):
        """Load fullpos yaml file.

        Arguments:
            fpdir (str): Path do fullpos settings
            fpfiles (list): List of yml files to read

        Returns:
            nldict (dict): fullpos settings

        """
        s = "selection"
        nldict = {s: {}}
        for fpfile in fpfiles:
            f = ConfigPaths.path_from_subpath(f"{fpdir}/{fpfile}.yml")
            logger.info("Read {}", f)
            with open(f, mode="rt", encoding="utf-8") as file:
                n = yaml.safe_load(file)
                file.close()
                if s in n:
                    nldict[s] = merge_dicts(nldict[s], n[s])
                else:
                    nldict.update(n)

        return nldict

    def update_selection(self, additions_list=None, additions_dict=None):
        """Add choices to the selection section.

        Args:
            additions_list (list): Additional selection to be read from files
            additions_dict (dict): Additional selection as a dictionary

        """
        if additions_list is not None:
            # Read the update
            for addition in additions_list:
                fpfile = ConfigPaths.path_from_subpath(f"{self.fpdir}/{addition}.yml")
                with open(fpfile, mode="rt", encoding="utf-8") as file:
                    nldict = yaml.safe_load(file)
                    file.close()

                self.nldict["selection"] = merge_dicts(self.nldict["selection"], nldict)

        if additions_dict is not None:
            self.nldict["selection"] = merge_dicts(
                self.nldict["selection"], additions_dict
            )

    def replace_rules(self, vin):
        """Replace patterns from the rules dict.

        Args:
            vin (str|list): Input values

        Raises:
            RuntimeError: Invalid type

        Returns:
            v (str|list): possibly replaced strings

        """
        v = vin if isinstance(vin, str) else vin.copy()
        for rp, rv in self.rules.items():
            if isinstance(v, list):
                for y in v:
                    if isinstance(y, str) and y == rp:
                        i = v.index(y)
                        v[i] = rv
            else:
                raise RuntimeError("Invalid type:", type(v), v)
        return v

    def construct(self):
        """Construct the fullpos namelists.

        Returns:
            namfpc_out (dict): namfpc part
            selection (dict): xxtddddhhmm part

        Raises:
            InvalidSelectionCombination: Invalid combination in selection

        """
        namfpc_out = {"NAMFPC": self.nldict["NAMFPC"].copy()}
        selection = self.nldict["selection"].copy()
        level_map = self.nldict["LEVEL_MAP"]
        param_map = self.nldict["PARAM_MAP"]

        namfpc = {v: [] for k, v in level_map.items()}
        for v in param_map.values():
            for vv in v.values():
                namfpc[vv] = []

        # Set empty namelists
        empty_namelists = {k: [] for k in level_map}
        for k in param_map:
            empty_namelists[k] = []

        # Map all fields and levels to the correct
        # entries in NAMFPC
        for namelists in selection.values():
            for namelist, namelist_key in namelists.items():
                if namelist in ["NAMFPPHY", "NAMPPC", "NAMFPDY2"]:
                    for key, fields in namelist_key.items():
                        x = param_map[namelist][key]
                        namfpc[x].append(fields)

                elif "CL3DF" in namelist_key:
                    # Handle selection sections without label
                    if len(namelist_key.keys()) > 2:
                        raise InvalidSelectionCombinationError(namelist_key.keys())
                    lmap = level_map[namelist]
                    if len(self.rules) > 0 and lmap == "NRFP3S":
                        namelist_key[lmap] = self.replace_rules(namelist_key[lmap])
                    namfpc[lmap].append(namelist_key[lmap])
                    pmap = param_map[namelist]["CL3DF"]
                    namfpc[pmap].append(namelist_key["CL3DF"])

                else:
                    # Handle selection sections with label
                    lmap = level_map[namelist]
                    for y in namelist_key.values():
                        if len(self.rules) > 0 and lmap == "NRFP3S":
                            y[lmap] = self.replace_rules(y[lmap])
                        namfpc[lmap].append(y[lmap])
                        pmap = param_map[namelist]["CL3DF"]
                        namfpc[pmap].append(y["CL3DF"])

        namfpc = {k: list(set(flatten_list(v))) for k, v in namfpc.items()}

        for key in namfpc:
            if len(namfpc[key]) > 0:
                namfpc[key].sort()
                namfpc_out["NAMFPC"][key] = namfpc[key]

        # Add domain and level mapping
        for time_section, namelists in selection.items():
            # Initialize with empty namelists
            tmp = {namelist: {} for namelist in empty_namelists}
            for namelist, keys in namelists.items():
                if namelist in ["NAMFPPHY", "NAMPPC", "NAMFPDY2"]:
                    section = {}
                    for key, val in keys.items():
                        param = "".join([key[0:2], "D", key[2:]])
                        section[key] = val
                        section[param] = [self.domain for j in range(len(val))]
                    tmp[namelist] = section
                elif "CL3DF" in keys:
                    # Handle selection sections without label
                    level_key = level_map[namelist]
                    keys[level_key] = list(set(flatten_list(keys[level_key])))
                    tmp[namelist], i = self.expand(
                        keys, level_key, namfpc[level_key], self.domain, 0
                    )
                else:
                    # Handle selection sections with label
                    level_key = level_map[namelist]
                    i = 0
                    params = {}
                    # Set levels per requested field
                    for sub_selection in keys.values():
                        for param in sub_selection["CL3DF"]:
                            if param not in params:
                                params[param] = {
                                    "CL3DF": [param],
                                    "NRFP3S": [sub_selection[level_key]],
                                }
                            params[param]["NRFP3S"].append(sub_selection[level_key])

                    for par in params:
                        params[par]["NRFP3S"] = list(
                            set(flatten_list(params[par]["NRFP3S"]))
                        )

                    for sub_selection in params.values():
                        sub_selection[level_key] = list(
                            set(flatten_list(sub_selection[level_key]))
                        )
                        d, i = self.expand(
                            sub_selection, level_key, namfpc[level_key], self.domain, i
                        )
                        tmp[namelist].update(d)

            # Update the selection
            selection[time_section] = tmp

        if "xxt00000000" in selection:
            self.check_non_instant_fields(selection, "xxt00000000")

        return namfpc_out, selection

    def check_non_instant_fields(self, selection, time_selection):
        """Search for non instant fields.

        Args:
            selection (dict): Dict with fullpos settings to be examined
            time_selection (str): Which selection time rules to check

        Raises:
            RuntimeError: Non instant fields found

        """
        field_list = []
        for parlist in selection[time_selection].values():
            for var in parlist.values():
                if isinstance(var, list):
                    fields = [x for x in var if x in self.nldict["NON_INSTANT_FIELDS"]]
                    field_list.append(fields)

        field_list = flatten_list(field_list)
        if len(field_list) > 0:
            logger.error("Non instant fields found for {}", time_selection)
            logger.error(field_list)
            logger.info(
                "Change selection or empty `NON_INSTANT_FIELDS` in rules.yml to override"
            )
            raise RuntimeError("Non instant fields found")
