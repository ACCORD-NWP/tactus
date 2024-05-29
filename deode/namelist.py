#!/usr/bin/env python3
"""Namelist handling for MASTERODB w/SURFEX."""
import copy
import os
import re
import subprocess
import shutil
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
            target = "user_namelist"
            nldict = {target: nl.todict()}
            cndict = {self.target: [target]}
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

            # Read file that describes assembly category order
            # for the various targets (tasks)
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
                except KeyError:
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
        if isinstance(node, dict):
            return {k: self.traverse(v) for k, v in node.items()}

        if isinstance(node, list):
            return [self.traverse(v) for v in node]

        return self.check_replace_scalar(node)

    def expand_cndict(self, target):
        """Recursively generates list of namelist groups to assemble.

        Args:
            target (str): task to generate namelists for

        Raises:
            RuntimeError:

        Returns:
            cnlist (list): list of namelist groups

        """
        cndt = self.cndict_targets
        if target in self.cndict_targets:
            raise RuntimeError(
                f"Target {target} already in cnlist causing endless loop:{cndt}"
            )
        self.cndict_targets.append(target)

        cnlist = [self.platform.substitute(x) for x in flatten_list(self.cndict[target])]
        cnlist_ = cnlist.copy()
        for x in cnlist_:
            if x in self.cndict:
                i = cnlist.index(x)
                cnlist[i] = self.expand_cndict(x)

        cnlist = flatten_list(cnlist)
        return cnlist

    def assemble_namelist(self, target):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for

        Returns:
            nlres (f90nml.Namelist): Assembled namelist

        """
        # Start with empty result dictionary
        nlres = {}
        nldict = self.nldict

        # Assemble the target namelists based on the given category order
        self.cndict_targets = []
        for catg in self.expand_cndict(target):
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
            cndict_tag: name to be used for recognition

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
        nml.end_comma = True  # AD: temp fix for IO_SERVER bug
        nml.write(output_file, force=True)

        logger.debug("Wrote: {}", output_file)

    def generate_namelist(self, target, output_file):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for
            output_file: where to write the result (fort.4 or EXSEG1.nam typically)

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

    def convert_namelist_between_cycles(self, namelist_filename, tnt_directive_yaml):
        tnt_directives_folder = Path(__file__).parent/"namelist_generation_input/tnt_directives/"
        command = ["tnt.py", "-d", tnt_directives_folder / tnt_directive_yaml, namelist_filename]
        subprocess.call(command)

        file_name = os.path.basename(tnt_directive_yaml)
        index = file_name.find("_to_")
        cycle_from = file_name[0:index]
        shutil.move(namelist_filename, namelist_filename+"."+ cycle_from)
        shutil.move(namelist_filename + ".tnt", namelist_filename)

    def upgrade_namelist_to_49t2(self, namelist_filename):
        self.convert_namelist_between_cycles(namelist_filename, "cy48t2_to_cy49.yaml")
        self.convert_namelist_between_cycles(namelist_filename, "cy49_to_cy49t1.yaml")


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
        return ["CY48t2", "CY48t3", "CY49", "CY49t1", "CY49t2"]

    def get_to_next_version_tnt_filenames():
        """Return the tnt file names between get_known_cycles()."""
        return [
            None,  # CY48t2 to CY48t3
            "cy48t2_to_cy49.yaml",  # CY48t3 to CY49
            "cy49_to_cy49t1.yaml",  # CY49   to CY49t1
            None,  # CY49t1 to CY49t2
        ]

    @staticmethod
    def get_tnt_files_list(from_cycle, to_cycle):
        """Return the list of tnt directive files required for the conversion."""
        # definitions of the conversion to apply between cycles
        tnt_directives_folder = (
            Path(__file__).parent / "namelist_generation_input/tnt_directives/"
        )

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

        logger.info(f"Write {output_ftn}")
        shutil.copy(ftn_file, output_ftn)

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
            raise SystemExit("conversion FAILED: keys_to_set not implemented")

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
            for blocks in tnt_directives["keys_to_remove"]:
                for namelists_section in namelist_dict:
                    for namelist_block in namelist_dict[namelists_section]:
                        if blocks in namelist_block:
                            del new_namelist[namelists_section][blocks]

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
        tnt_directives_folder = (
            Path(__file__).parent / "namelist_generation_input/tnt_directives/"
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
