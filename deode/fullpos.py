#!/usr/bin/env python3
"""Fullpos namelist generation."""

import os

import yaml

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
            fpfiles (list): List of fullpos config files to read (wihtout the yml suffix)
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
            f = os.path.join(fpdir, f"{fpfile}.yml")
            logger.info("Read {}", f)
            with open(f, mode="rt", encoding="utf-8") as file:
                n = yaml.safe_load(file)
                file.close()
                if s in n:
                    nldict[s] = self.merge_dict(nldict[s], n[s])
                else:
                    nldict.update(n)

        return nldict

    def merge_dict(self, d1, d2):
        """Merge two dictionaries, tailored for the fullpos yml structure.

        Args:
            d1 (dict): Reference dict
            d2 (dict): Update dict

        Returns:
            d (dict): Merged dict

        Raises:
            RuntimeError: Invalid type

        """
        d = d1.copy()
        for k, v in d2.items():
            if k in d:
                if isinstance(v, dict):
                    d[k] = self.merge_dict(d[k], v)
                elif isinstance(v, list):
                    for x in v:
                        if x not in d[k]:
                            d[k].append(x)
                else:
                    raise RuntimeError("Invalid type:", type(v), v)
            else:
                d[k] = v

        return d

    def update_selection(self, additions_list=None, additions_dict=None):
        """Add choices to the selection section.

        Args:
            additions_list (list): Additional selection to be read from files
            additions_dict (dict): Additional selection as a dictionary

        """
        if additions_list is not None:
            # Read the update
            for addition in additions_list:
                fpfile = os.path.join(self.fpdir, f"{addition}.yml")
                with open(fpfile, mode="rt", encoding="utf-8") as file:
                    nldict = yaml.safe_load(file)
                    file.close()

                self.nldict["selection"] = self.merge_dict(
                    self.nldict["selection"], nldict
                )

        if additions_dict is not None:
            self.nldict["selection"] = self.merge_dict(
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

        # Extract empty namelists
        empty_namelists = {k: [] for k in level_map}
        for k in param_map:
            empty_namelists[k] = []

        # Map all fields and levels to the correct
        # entries in NAMFPC
        for vv in selection.values():
            for k, v in vv.items():
                if k in ["NAMFPPHY", "NAMPPC", "NAMFPDY2"]:
                    for s, t in v.items():
                        x = param_map[k][s]
                        namfpc[x].append(t)

                elif "CL3DF" in v:
                    if len(v.keys()) > 2:
                        raise InvalidSelectionCombinationError(v.keys())
                    x = level_map[k]
                    if len(self.rules) > 0 and x == "NRFP3S":
                        v[x] = self.replace_rules(v[x])
                    namfpc[x].append(v[x])
                    x = param_map[k]["CL3DF"]
                    namfpc[x].append(v["CL3DF"])

                else:
                    for y in v.values():
                        x = level_map[k]
                        if len(self.rules) > 0 and x == "NRFP3S":
                            y[x] = self.replace_rules(y[x])
                        namfpc[x].append(y[x])
                        x = param_map[k]["CL3DF"]
                        namfpc[x].append(y["CL3DF"])

        namfpc = {k: list(set(flatten_list(v))) for k, v in namfpc.items()}

        for k in namfpc:
            if len(namfpc[k]) > 0:
                namfpc[k].sort()
                namfpc_out["NAMFPC"][k] = namfpc[k]

        # Add domain and level mapping
        for kk, vv in selection.items():
            tmp = {}
            for k, v in vv.items():
                tmp[k] = {}
                if k in ["NAMFPPHY", "NAMPPC", "NAMFPDY2"]:
                    d = {}
                    for p, q in v.items():
                        x = "".join([p[0:2], "D", p[2:]])
                        d[p] = q
                        d[x] = [self.domain for j in range(len(q))]
                    tmp[k] = d
                elif "CL3DF" in v:
                    x = level_map[k]
                    v[x] = list(set(flatten_list(v[x])))
                    tmp[k], i = self.expand(v, x, namfpc[x], self.domain, 0)
                else:
                    x = level_map[k]
                    i = 0
                    for y in v.values():
                        y[x] = list(set(flatten_list(y[x])))
                        d, i = self.expand(y, x, namfpc[x], self.domain, i)
                        tmp[k].update(d)

            # Update the selection
            for k, v in tmp.items():
                selection[kk][k] = v

            # Make sure all namelists exists
            for k in empty_namelists:
                if k not in selection[kk]:
                    selection[kk][k] = {}

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
