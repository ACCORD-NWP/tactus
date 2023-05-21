#!/usr/bin/env python3
"""Config document generation tool using config.toml and main_config_scheme.json."""

import json

from .namelist import flatten_list

SKIP = ("SURFEX", "task", "submission")
CONFIG_MAP = {"general": "GeneralSectionModel", "domain": "Domain", "macros": "Macros"}
TABLE_HEADER = ["", "Key", "Description", "Default", "Options", ""]


class DocConfig:
    """Doc class."""

    def __init__(self, config, schema_file):
        """Construct the generator.

        Args:
            config (dict): Configuration
            schema_file (str): The main schema file

        """
        self.config = config
        self.schema_file = schema_file

    def schema_props(self, header, body):
        """Extract info from a schema json sections.

        Args:
            header (str): Name of the sections
            body (dict): Schema section

        Returns:
            info (list): Extracted info

        """
        info = [""] * 6
        info[1] = header
        for k in ["description", "title"]:
            if k in body:
                info[2] = body[k]
                break
        if "default" in body:
            info[3] = body["default"]
        if "enum" in body:
            info[4] = ", ".join(body["enum"])

        return info

    def expand_schema(self, header, desc, sections, indent):
        """Gather section an subsections from a json schema file.

        Args:
            header (str): Name of the sections
            desc (str): Description
            sections (dict): The dict to digest
            indent (int): Subsection counter

        Returns:
            result (list): List containg all sections and sub sections
        """
        ind = ["#"] * indent
        tag = "".join(ind)
        section = []
        subsection = []
        for k, v in sections.items():
            try:
                isobject = v["type"] == "object"
            except KeyError:
                isobject = False

            if isobject:
                subsection.append(
                    self.expand_schema(k, v["description"], v["properties"], indent + 1)
                )
            else:
                section.append(self.schema_props(k, v))
        result = [{f"{tag} {header}": {"description": desc, "items": section}}]
        if len(subsection) > 0:
            result.append(subsection)

        return result

    def expand_config(self, header, sections, indent):
        """Gather section an subsections from the config.

        Arguments:
            header (str): Name of the sections
            sections (dict): The dict to digest
            indent (int): Subsection counter

        Returns:
            result (list): List containg all sections and sub sections
        """
        ind = ["#"] * indent
        tag = "".join(ind)
        section = []
        subsection = []
        for k, v in sections.items():
            if isinstance(v, dict):
                subsection.append(self.expand_config(k, v, indent + 1))
            else:
                section.append(["", k, "", str(v), "", ""])

        if header in CONFIG_MAP:
            hdr = CONFIG_MAP[header]
        else:
            hdr = header
        result = [{f"{tag} {hdr}": {"description": "", "items": section}}]
        if len(subsection) > 0:
            result.append(subsection)

        return result

    def print_header(self):
        """Print header info in the doc file."""
        print("# The DEODE config file")
        print(
            "This is a file generated from the default config and the main schema file using `dedoe doc config > docs/config.md`, do not edit!"
        )

    def get_schema(self, schema):
        """Organize the schema dict.

        Arguments:
            schema (dict): Schema dict

        Returns:
            sections (dict): Organized dict
        """
        sections = []
        indent = 1
        for k, v in schema["definitions"].items():
            if v["type"] == "object":
                sections.append(
                    self.expand_schema(k, v["description"], v["properties"], indent)
                )
        return flatten_list(sections)

    def get_config(self, config):
        """Organize the config dict.

        Arguments:
            config (dict): Config dict

        Returns:
            sections (dict): Organized dict
        """
        sections = []
        indent = 1
        for k, v in config.items():
            if k not in SKIP:
                sections.append(self.expand_config(k, v, indent))
        return flatten_list(sections)

    def merge(self, sch, cfg):
        """Merge schema and config dicts.

        Arguments:
            sch (dict): Schema dict
            cfg (dict): Config dict

        Returns:
            res (dict): Merged dic
        """
        res = []
        pos = {}
        for i, k in enumerate(cfg):
            res.append(k)
            kk = list(k)[0]
            pos[kk] = i

        for k in sch:
            kk = list(k)[0]
            if kk not in pos:
                res.append(k)
            else:
                j = pos[kk]
                res[j][kk]["description"] = k[kk]["description"]

                for i, x in enumerate(res[j][kk]["items"]):
                    for y in k[kk]["items"]:
                        if x[1] == y[1]:
                            res[j][kk]["items"][i][2] = y[2]
                            res[j][kk]["items"][i][4] = y[4]
        return res

    def print_doc(self):
        """Print a merge of the config and schema dict."""
        with open(self.schema_file, "r") as infile:
            schema = json.load(infile)
            infile.close()

        cfg = self.get_config(self.config)
        sch = self.get_schema(schema)

        summary = self.merge(sch, cfg)

        self.print_header()
        for section in summary:
            hdr = list(section)[0]
            print(f"\n{hdr}")
            print(f"{section[hdr]['description']}")
            txt = "|".join(TABLE_HEADER)
            print(" ", txt)
            txt = "|".join(["", "---", "---", "---", "---", ""])
            print(" ", txt)
            for w in section[hdr]["items"]:
                txt = "|".join(w)
                print(" ", txt)
