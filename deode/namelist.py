#!/usr/bin/env python3
"""Namelist handling for MASTERODB w/SURFEX."""
import os
import re
from pathlib import Path

import f90nml
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
        self.kind = kind  # not used elsewhere, though
        self.substitute = substitute
        self.logger = get_logger_from_config(config)
        self.cycle = self.config["general.cycle"]
        self.cnfile = (
            Path(__file__).parent
            / "namelist_generation_input"
            / f"{self.cycle}"
            / f"assemble_{kind}.yml"
        )
        self.nlfile = (
            Path(__file__).parent
            / "namelist_generation_input"
            / f"{self.cycle}"
            / f"{kind}_namelists.yml"
        )
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

        self.logger.debug("Check if reference namelist %s exists", ref_namelist)
        if os.path.isfile(ref_namelist):
            self.logger.info("Use reference namelist %s", ref_namelist)
            nl = f90nml.read(ref_namelist)
            nldict = {self.target: nl.todict()}
            cndict = {self.target: [self.target]}
            found = False
        else:
            self.logger.warning("No reference namelist exists")
            found = True
            nldict = {}
            cndict = {}

        return found, nldict, cndict

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

        # Use static namelist if given
        use_yaml = True
        if self.accept_static_namelist:
            use_yaml, nldict, cndict = self.load_user_namelist()

        if use_yaml:
            self.logger.debug(
                "Use %s and %s to generate namelist", self.nlfile, self.cnfile
            )
            # Read namelist file with all the categories
            with open(self.nlfile, mode="rt", encoding="utf-8") as file:
                nldict = yaml.safe_load(file)

            # Read file that describes assembly category order for the various targets (tasks)
            with open(self.cnfile, mode="rt", encoding="utf-8") as file:
                cndict = yaml.safe_load(file)

        # Check target is valid
        if target not in cndict:
            self.logger.warning(
                "Could not find target '%s' in %s", target, str(self.cnfile)
            )
            msg = "Available targets:"
            for key in cndict:
                if not re.match(r"_.+", key):
                    msg += " " + key + ","
            self.logger.warning(msg[:-1])
            raise InvalidNamelistTargetError(target)

        self.nldict = nldict
        self.cndict = cndict

        return nldict, cndict

    def assemble_namelist(self, target):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for

        Raises:
            InvalidNamelistTargetError   # noqa: DAR401

        Returns:
            nlres (f90nml.Namelist): Assembled namelist

        """
        # Start with empty result dictionary
        nlres = {}

        nldict = self.nldict
        # Assemble the target namelists based on the given category order
        for item in flatten_list(self.cndict[target]):
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
                            if str(finval).find("$") >= 0 and self.substitute:
                                m = re.search(
                                    r"^([^\$]*)\$\{([\w\.]+)\-?([^}]*)\}(.*)", str(val)
                                )
                                if m:
                                    pre = m.group(1)
                                    nam = m.group(2)
                                    defval = m.group(3)
                                    post = m.group(4)
                                    try:
                                        repval = self.platform.get_value(nam)
                                    except Exception:
                                        repval = None
                                    if repval is None:
                                        if defval != "":
                                            self.logger.debug(
                                                "Using default value %s for '%s'",
                                                defval,
                                                nam,
                                            )
                                            repval = find_num(defval)
                                        else:
                                            self.logger.debug(
                                                "No value found for: '%s'", nam
                                            )
                                            repval = finval
                                    else:
                                        self.logger.debug(
                                            "Replaced %s with: %s", nam, str(repval)
                                        )
                                    if isinstance(repval, str):
                                        finval = str(pre) + str(repval) + str(post)
                                    else:
                                        finval = repval
                                else:
                                    raise KeyError(val)
                            if isinstance(finval, tuple):
                                finval = list(finval)
                            nlres[nl][key] = finval
        return f90nml.Namelist(nlres)

    def update(self, nldict, cndict_tag):
        """Update with additional namelist dict.

        Args:
            nldict (dict): additional namelist dict
            cndict_tag : name to be used for recognition

        """
        self.cndict[self.target].append(cndict_tag)
        self.nldict[cndict_tag] = nldict

    def write_namelist(self, nml, output_file):
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
        nml.write(output_file, force=True)

        self.logger.debug("Wrote: %s", output_file)

    def generate_namelist(self, target, output_file):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for
            output_file : where to write the result (fort.4 or EXSEG1.nam typically)

        """
        self.load(target)
        try:
            update = self.config["namelist_update"]
            if self.kind in update:
                self.update(update, self.kind)
        except KeyError:
            pass

        nml = self.assemble_namelist(target)
        self.write_namelist(nml, output_file)
