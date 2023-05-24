#!/usr/bin/env python3
"""Fullpos namelist generation."""
import yaml

from .namelist import flatten_list


class Fullpos:
    """Fullpos namelist generator based on (yaml) dicts."""

    def __init__(self, domain, nlfile=None, fullpos_config=None):
        """Construct the fullpos generator.

        Args:
            domain (str): Domain name
            nlfile (str): Fullpos yaml config file
            fullpos_config (dict): Fullpos config as dict

        """
        self.domain = domain
        if nlfile is not None:
            self.nldict = self.load(nlfile)
        elif fullpos_config is not None:
            self.nldict = fullpos_config

    def expand(self, v, levtype, levels, domain):
        """Expand fullpos namelists to levels and domains.

        Args:
            v (str): parameter list
            levtype (str): type of vertical level in fullpos syntax
            levels (list): list of levels
            domain (str): domain name

        Returns:
            d (dict): Expaned names

        """
        i = 0
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

        return d

    def load(self, nlfile):
        """Load fullpos yaml file.

        Arguments:
            nlfile (str): fullpos config _file (yml)

        Returns:
            nldict (dict): fullpos settings

        """
        with open(nlfile, mode="rt", encoding="utf-8") as file:
            nldict = yaml.safe_load(file)

        return nldict

    def construct(self):
        """Construct the fullpos namelists.

        Returns:
            namfpc_out (dict): namfpc part
            selection (dict): xxtddddhhmm part

        """
        namfpc_out = {"NAMFPC": self.nldict["NAMFPC"].copy()}
        selection = self.nldict["selection"].copy()
        level_map = self.nldict["LEVEL_MAP"]
        param_map = self.nldict["PARAM_MAP"]

        namfpc = {v: [] for k, v in level_map.items()}
        for v in param_map.values():
            for vv in v.values():
                namfpc[vv] = []

        # Map all fields and levels to the correct
        # entries in NAMFPC
        for vv in selection.values():
            for k, v in vv.items():
                if k in ["NAMFPPHY", "NAMPPC", "NAMFPDY2"]:
                    for s, t in v.items():
                        x = param_map[k][s]
                        namfpc[x].append(t)

                elif "CL3DF" in v:
                    x = level_map[k]
                    namfpc[x].append(v[x])
                    x = param_map[k]["CL3DF"]
                    namfpc[x].append(v["CL3DF"])

                else:
                    for y in v.values():
                        x = level_map[k]
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
                        d[x] = [self.domain for j in range(0, len(q))]
                    tmp[k] = d
                elif "CL3DF" in v:
                    x = level_map[k]
                    tmp[k] = self.expand(v, x, namfpc[x], self.domain)
                else:
                    x = level_map[k]
                    for y in v.values():
                        tmp[k].update(self.expand(y, x, namfpc[x], self.domain))

            for k, v in tmp.items():
                selection[kk][k] = v

        return namfpc_out, selection
