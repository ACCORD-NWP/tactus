#!/usr/bin/env python3
"""Namelist handling for MASTERODB w/SURFEX."""
import os
import re
from collections import OrderedDict
from pathlib import Path

import f90nml
import yaml

from .logs import logger
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


class InvalidNamelistKindError(ValueError):
    """Custom exception."""

    pass


class InvalidNamelistTargetError(ValueError):
    """Custom exception."""

    pass


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
                if type(valb) is dict and type(valc) is dict:
                    if not key.startswith("_"):
                        # Invoke ourselves recursively
                        msg = f"recursive dict comp for {key}"
                        logger.debug(msg)
                        dout[key] = self.compare_dicts(dbase[key], dcomp[key], action)
                elif type(valb) is list and type(valc) is list:
                    kl = key.lower()
                    try:
                        sib = dbase["_start_index"][kl]
                    except Exception:
                        sib = [1]
                    try:
                        sic = dcomp["_start_index"][kl]
                    except Exception:
                        sic = [1]
                    # Start index in output depends on the action, thus:
                    sio = []
                    for _ in range(len(sib)):
                        sio.append(-999)
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
            else:
                # Key only found in base
                if action == "union":
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
                    if type(dout[key]) is dict and len(dout[key]) == 0:
                        todel.append(key)
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
                if type(valb) is dict and type(valc) is dict:
                    list_set_at_index(
                        liout, io, self.compare_dicts(libase[ib], licomp[ic], action)
                    )
                elif type(valb) is list and type(valc) is list:
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
                else:
                    # Scalar, different value
                    if action != "intersection":
                        list_set_at_index(liout, io, valc)
            else:
                # Element only found in base
                if action == "union":
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

        logger.debug("Check if reference namelist {} exists", ref_namelist)
        if os.path.isfile(ref_namelist):
            logger.info("Use reference namelist {}", ref_namelist)
            nl = f90nml.read(ref_namelist)
            nldict = {self.target: nl.todict()}
            cndict = {self.target: [self.target]}
            found = False
        else:
            logger.warning(
                "No reference namelist {} exists, fallback to yaml files", ref_namelist
            )
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
            logger.debug("Use {} and {} to generate namelist", self.nlfile, self.cnfile)
            # Read namelist file with all the categories
            with open(self.nlfile, mode="rt", encoding="utf-8") as file:
                nldict = yaml.safe_load(file)

            # Read file that describes assembly category order for the various targets (tasks)
            with open(self.cnfile, mode="rt", encoding="utf-8") as file:
                cndict = yaml.safe_load(file)

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

        return nldict, cndict

    def check_replace_scalar(self, val):
        """Check a scalar for variable substitution from config.

        Args:
            val: value to be checked

        Returns:
            value after possible substitution

        Raises:
            KeyError   # noqa: DAR401

        """
        finval = val
        # Replace ${var-def} with value from config, possibly macro-expanded
        # For now assumes only one subst. per line, could be generalized if needed
        if str(finval).find("$") >= 0 and self.substitute:
            m = re.search(r"^([^\$]*)\$\{([\w\.]+)\-?([^}]*)\}(.*)", str(val))
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
                        logger.debug(
                            "Using default value {} for '{}'",
                            defval,
                            nam,
                        )
                        repval = find_num(defval)
                    else:
                        logger.debug("No value found for: '{}'", nam)
                        repval = finval
                else:
                    logger.debug("Replaced {} with: {}", nam, str(repval))
                if isinstance(repval, str):
                    finval = str(pre) + str(repval) + str(post)
                else:
                    finval = repval
            else:
                raise KeyError(val)
        if isinstance(finval, tuple):
            finval = list(finval)
        return finval

    def traverse(self, node):
        """Traverse a nested structure and do variable substitution where needed.

        Args:
            node: (dict, list, str, float, int, bool)

        Returns:
            node, with values replaced
        """
        if type(node) is dict:
            return {k: self.traverse(v) for k, v in node.items()}
        elif type(node) is list:
            return [self.traverse(v) for v in node]
        else:
            return self.check_replace_scalar(node)

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
        for catg in flatten_list(self.cndict[target]):
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
                        nlres[nl] = self.nlcomp.compare_dicts(
                            nlres[nl], nldict[catg][nl], "union"
                        )
        # Finally perform variable substitution
        nlsubst = self.traverse(nlres)
        return f90nml.Namelist(nlsubst)

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

        logger.debug("Wrote: {}", output_file)

    def generate_namelist(self, target, output_file):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for
            output_file : where to write the result (fort.4 or EXSEG1.nam typically)

        """
        logger.info("Generate namelist for: {}", target)
        self.load(target)
        try:
            update = self.config["namelist_update"]
            if self.kind in update:
                self.update(update, self.kind)
        except KeyError:
            pass

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

    def yml2dict(self, ymlfile):
        """Read yaml namelist file and return as dict."""
        with open(ymlfile, mode="rt", encoding="utf-8") as file:
            ynml = yaml.safe_load(file)
        return ynml

    def dict2yml(self, nmldict, ymlfile):
        """Write dict as yaml file."""
        with open(ymlfile, mode="wb") as file:
            yaml.dump(nmldict, file, encoding="utf-8", default_flow_style=False)
