"""Markdown Overview of Master Selection Parameters.

This script generates a Markdown overview of master selection parameters
for different cycles and CSCs by parsing YAML namelist files and GRIB2
definition files. It extracts parameter metadata using eccodes and organizes
them by level type (single, height, pressure, hybrid). The output includes
paramId, shortName, longName, unit, and faField with links to the ECMWF GRIB
parameter database.

Usage:
    python write_output_overview.py <output.md>

Notes:
    - Requires eccodes, numpy, and PyYAML.
    - Assumes specific directory structure for namelist and definition files.
    - Appends results for multiple cycles and CSCs to the output Markdown file.
"""

import os
import re
import sys

import numpy as np
import yaml
from eccodes import (
    codes_get,
    codes_grib_new_from_samples,
    codes_release,
    codes_set,
    codes_set_string,
)


def _read_namelist(namelist_path):
    """Reads a YAML namelist file and returns its contents as a dictionary.

    Args:
        namelist_path: Path to the yaml namelist to be read

    Returns:
        Dictionary containing the contents of the namelist file.
    """
    with open(namelist_path, "r") as f:
        return yaml.safe_load(f)


def _parse_grib2_definitions(fa_name):
    """Parses a .def file and returns the dictionary for the given fa_name.

    Args:
        fa_name: The name of the faField to be parsed.

    Returns:
        Dictionary containing the grib2 key-value pairs from the .def file
        for the given fa_name.
    """
    pwd = os.getcwd()
    def_path = (
        pwd + "/deode/data/eccodes/definitions/grib2/localConcepts/lfpw/faFieldName.def"
    )
    result = {}
    with open(def_path, "r") as f:
        lines = f.readlines()
    inside_block = False
    for line in lines:
        if f'"{fa_name}"' in line and "{" in line:
            inside_block = True
            continue
        if inside_block:
            if "}" in line:
                break
            if "=" in line:
                parts = line.split("=", 1)
                if len(parts) == 2:
                    if any(skip in line for skip in ["LSTCUM", "FMULT", "tablesVersion"]):
                        continue
                    key = parts[0].strip()
                    value = parts[1].strip().rstrip(";").strip('"').replace(" ", "")
                    result[key] = value
    return result


def _get_grib_short_long_names(keys):
    """Extracts grib2 keys.

    Extracts shortName, longName, paramId, and unit from a GRIB2 sample
    using provided keys.
    """
    sample_id = codes_grib_new_from_samples("GRIB2.tmpl")
    # Sort keys so that "productDefinitionTemplateNumber" is first if present
    sorted_keys = sorted(
        keys.keys(), key=lambda k: (k != "productDefinitionTemplateNumber", k)
    )
    keys = {k: keys[k] for k in sorted_keys}
    for key, value in keys.items():
        if "missing()" not in value:
            try:
                v = int(value)
                codes_set(sample_id, key, v)
            except ValueError:
                codes_set(sample_id, key, 255)
        else:
            codes_set_string(sample_id, key, "MISSING")
    sn = codes_get(sample_id, "shortName")
    if sn == "unknown":
        codes_set(sample_id, "typeOfLevel", "hybrid")
        sn = codes_get(sample_id, "shortName")
    ln = codes_get(sample_id, "name")
    parmid = codes_get(sample_id, "paramId")
    unit = codes_get(sample_id, "units")
    codes_release(sample_id)
    return sn, ln, parmid, unit


def main(cycle, csc, output_md):
    """Main function to generate the Markdown overview for a given cycle and CSC.

    Args:
        cycle: Cycle to include in writing the overview, e.g. "CY49t2"
        csc: CSC to include in writing the overview, e.g. "AROME"
        output_md: Path to the output Markdown file
    """
    pwd = os.getcwd()
    namelist_dir = f"{pwd}/deode/data/namelist_generation_input/{cycle}/fullpos"
    namelist_path = f"{namelist_dir}/master_selection_{csc}.yml"
    if not os.path.exists(namelist_path):
        return

    namelist = _read_namelist(namelist_path)

    single_levs = []
    height_levs = []
    pressure_levs = []
    hybrid_levs = []
    selection = namelist.get("selection", {})
    xxtddddhh00 = selection.get("xxtddddhh00", {})

    for outer_key, outer_val in xxtddddhh00.items():
        table = []
        if outer_key.endswith("PDYS"):
            continue
        clcfus = []
        for inner_key in outer_val:
            if not inner_key.startswith("RFP"):
                clcfus = [key for key in outer_val if not key.startswith("RFP")]
        for clcfu in clcfus:
            for entry in outer_val[clcfu]:
                entry_clean = entry.replace(" ", "_")
                if entry_clean.startswith("HUMI_RELATIVE"):
                    entry_clean = entry_clean[:-2]
                keys = _parse_grib2_definitions(entry_clean)
                if entry_clean.startswith("ISOT"):
                    for iso in outer_val.get("RFP3I", []):
                        keys["scaledValueOfFirstFixedSurface"] = str(
                            int(np.round(iso * -100))
                        )
                        sn, ln, parmid, unit = _get_grib_short_long_names(keys)
                        table.append((entry_clean, sn, ln, parmid, unit))
                else:
                    sn, ln, parmid, unit = _get_grib_short_long_names(keys)
                    table.append((entry_clean, sn, ln, parmid, unit))
        if outer_key.endswith("DYH"):
            height_levs.extend(table)
        elif outer_key.endswith("DYP"):
            pressure_levs.extend(table)
        elif outer_key.endswith("3DF"):
            hybrid_levs.extend(table)
        else:
            single_levs.extend(table)
    changelog_path = os.path.join(pwd, "CHANGELOG.md")
    version = "unknown"
    if os.path.exists(changelog_path):
        with open(changelog_path, "r") as changelog_file:
            for line in changelog_file:
                if (
                    "[" in line
                    and "]" in line
                    and "#" in line
                    and "pull" not in line
                    and "2" in line
                ):
                    match = re.search(r"\[(\d+\.\d+\.\d+)\]", line)
                    if match:
                        version = match.group(1)
                        break
                    break
    else:
        version = "unknown"

    with open(output_md, "a") as f:
        if f.tell() == 0:
            f.write(f"# Master Selection Overview as of version {version}\n")
        f.write(f"## Cycle: {cycle}, CSC: {csc}\n")
        f.write("### Single levels\n")
        f.write("| paramId | shortName | longName | unit | faField |\n")
        f.write("|---------|-----------|----------|------|---------|\n")
        for entry, sn, ln, parmid, unit in single_levs:
            f.write(
                f"| [{parmid}](https://codes.ecmwf.int/grib/param-db/{parmid}) | "
                f"{sn} | {ln} | {unit} | {entry} |\n"
            )
        f.write("### Height levels\n")
        f.write("| paramId | shortName | longName | unit | faField |\n")
        f.write("|---------|-----------|----------|------|---------|\n")
        for entry, sn, ln, parmid, unit in height_levs:
            f.write(
                f"| [{parmid}](https://codes.ecmwf.int/grib/param-db/{parmid}) | "
                f"{sn} | {ln} | {unit} | {entry} |\n"
            )
        f.write("### Pressure levels\n")
        f.write("| paramId | shortName | longName | unit | faField |\n")
        f.write("|---------|-----------|----------|------|---------|\n")
        for entry, sn, ln, parmid, unit in pressure_levs:
            f.write(
                f"| [{parmid}](https://codes.ecmwf.int/grib/param-db/{parmid}) | "
                f"{sn} | {ln} | {unit} | {entry} |\n"
            )


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit("Usage: python write_output_overview.py <output.md>")
    cycles = ["CY49t2"]
    output_md = sys.argv[1]
    if os.path.exists(output_md):
        os.remove(output_md)
    for cycle in cycles:
        if "h" in cycle:
            cscs = ["HARMONIE_AROME"]
        else:
            cscs = ["AROME", "ALARO", "HARMONIE_AROME"]
        for csc in cscs:
            main(cycle, csc, output_md)
