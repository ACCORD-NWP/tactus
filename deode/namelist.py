#!/usr/bin/env python3
"""Namelist handling for MASTERODB w/SURFEX."""
import re

# TODO: import f90nml
from pathlib import Path

import yaml

from .logs import get_logger_from_config
from .toolbox import Platform


def flatten_list(li):
    """Recursively flatten a list of lists (of lists)."""
    if li == []:
        return li
    if isinstance(li[0], list):
        return flatten_list(li[0]) + flatten_list(li[1:])
    return li[:1] + flatten_list(li[1:])


def find_num(s):
    """Purpose: un-quote numbers."""
    try:
        i = int(s)
        return i
    except ValueError:
        pass
    try:
        f = float(s)
        return f
    except ValueError:
        return s


class InvalidNamelistKindError(ValueError):
    """Custom exception."""

    pass


class InvalidNamelistTargetError(ValueError):
    """Custom exception."""

    pass


class NamelistGenerator:
    """Fortran namelist generator based on hierarchical merging of (yaml) dicts."""

    def __init__(self, config, kind):
        """Construct the generator.

        Args:
            config (deode.ParsedConfig): Configuration
            kind (str): one of 'master' or 'surfex'

        Raises:
            InvalidNamelistKindError   # noqa: DAR401

        """
        if kind not in ("master", "surfex"):
            raise InvalidNamelistKindError(kind)
        self.config = config
        self.kind = kind  # not used elsewhere, though
        self.logger = get_logger_from_config(config)
        self.cnfile = (
            Path(__file__).parent / "namelist_generation_input" / f"assemble_{kind}.yml"
        )
        self.nlfile = (
            Path(__file__).parent / "namelist_generation_input" / f"{kind}_namelists.yml"
        )

    def generate_namelist(self, target, output_file):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for
            output_file : where to write the result (fort.4 or EXSEG1.nam typically)

        Raises:
            InvalidNamelistTargetError   # noqa: DAR401

        """
        # Read namelist file with all the categories
        with open(self.nlfile, mode="rt", encoding="utf-8") as file:
            nldict = yaml.safe_load(file)

        # Read file that describes assembly category order for the various targets (tasks)
        with open(self.cnfile, mode="rt", encoding="utf-8") as file:
            cndict = yaml.safe_load(file)

        # Check target is valid
        if target not in cndict:
            self.logger.debug(
                "Could not find target '%s' in %s", target, str(self.cnfile)
            )
            msg = "Available targets:"
            for key in cndict:
                if not re.match(r"_.+", key):
                    msg += " " + key + ","
            self.logger.debug(msg[:-1])
            raise InvalidNamelistTargetError(target)

        # For access to the config object
        platform = Platform(self.config)

        # Start with empty result dictionary
        nlres = {}

        # Assemble the target namelists based on the given category order
        for item in flatten_list(cndict[target]):
            catg = item
            # variable substitution removed at this level (may be resurrected)
            # assemble namelists for this category
            if catg in nldict:
                for nl in nldict[catg]:
                    if nl not in nlres:
                        # create the result namelist dict
                        nlres[nl] = {}
                    if catg == "rm{" + nl + "}":
                        # clear/remove the given namelist (but not used for now)
                        nlres[nl].clear()
                    else:
                        for key in nldict[catg][nl]:
                            val = nldict[catg][nl][key]
                            finval = val
                            # Replace ${var-def} with value from config, possibly macro-expanded
                            # For now assumes only one subst. per line, could be generalized if needed
                            if str(finval).find("$") >= 0:
                                m = re.search(
                                    r"^([^\$]*)\$\{([\w\.]+)\-?([^}]*)\}(.*)", str(val)
                                )
                                if m:
                                    pre = m.group(1)
                                    nam = m.group(2)
                                    defval = m.group(3)
                                    post = m.group(4)
                                    try:
                                        repval = platform.get_value(nam)
                                    except Exception:
                                        repval = None
                                    if repval is None:
                                        if defval != "":
                                            self.logger.debug(
                                                "Using default value %s for '%s'",
                                                defval,
                                                nam,
                                            )
                                            repval = defval
                                        else:
                                            self.logger.debug(
                                                "No value found for: '%s'", nam
                                            )
                                    else:
                                        self.logger.debug(
                                            "Replaced %s with: %s", nam, str(repval)
                                        )
                                    finval = str(pre) + str(find_num(repval)) + str(post)
                                else:
                                    raise KeyError(val)
                            nlres[nl][key] = finval

        # Write result. TODO: use f90nml(?)
        # Or would it be useful to just return the nlres dict instead(?)
        with open(output_file, mode="w") as file:
            for nl in sorted(nlres.keys()):
                file.write("&" + nl + "\n")
                for key in sorted(nlres[nl].keys()):
                    val = nlres[nl][key]
                    sval = str(val).strip()
                    if sval[-1] != ",":
                        sval += ","
                    file.write("  " + key + " = " + sval + "\n")
                file.write("/\n")
        self.logger.debug("Wrote: %s", output_file)
