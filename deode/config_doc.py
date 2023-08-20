#!/usr/bin/env python3
"""Config document generation tool using config.toml and main_config_scheme.json."""

import datetime
import json

from .namelist import flatten_list

SKIP = ("SURFEX", "task", "submission")
TABLE_HEADER = ["", "Key", "Description", "Default", "Options", "Type", ""]


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
        self.config_map = {}

    def schema_props(self, header, body):
        """Extract info from a schema json sections.

        Args:
            header (str): Name of the sections
            body (dict): Schema section

        Returns:
            info (list): Extracted info

        """
        info = [""] * len(TABLE_HEADER)
        info[1] = header
        header_map = {"description": 2, "default": 3, "type": 5}

        for k, v in header_map.items():
            if k in body:
                info[v] = body[k]
        if "enum" in body:
            info[4] = ", ".join(body["enum"])

        return info

    def expand_schema(self, parent, header, desc, sections, indent):
        """Gather section an subsections from a json schema file.

        Args:
            parent (str): Parent of this section
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
        config_map = []
        config_sub_map = []
        for k, v in sections.items():
            try:
                isobject = v["type"] == "object"
            except KeyError:
                isobject = False

            if isobject:
                hdr = f"{header} > {v['title']}"
                sch, map_item = self.expand_schema(
                    k, hdr, v["description"], v["properties"], indent + 1
                )
                subsection.append(sch)
                config_sub_map.append({f"{parent}.{k}": hdr})
            else:
                section.append(self.schema_props(k, v))
        result = [{f"{tag} {header}": {"description": desc, "items": section}}]
        config_map = [{parent: header}]
        if len(subsection) > 0:
            result.append(subsection)
            config_map.append(config_sub_map)

        return result, config_map

    def expand_config(self, parent, header, sections, indent):
        """Gather section an subsections from the config.

        Arguments:
            parent (str): Parent of the sections
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
                subsection.append(self.expand_config(header, k, v, indent + 1))
            else:
                section.append(["", k, "", str(v), "", "", ""])

        prefix = "" if parent is None else f"{parent}."
        prefix = f"{prefix}{header}"
        if prefix in self.config_map:
            prefix = self.config_map[prefix]
        result = [{f"{tag} {prefix}": {"description": "", "items": section}}]
        if len(subsection) > 0:
            result.append(subsection)

        return result

    def print_header(self):
        """Print header info in the doc file."""
        now = datetime.datetime.now().isoformat(timespec="seconds")
        print("# The DEODE config file")
        print(f"This was automatically generated running `deode doc config` on {now}.")

    def get_schema(self, schema):
        """Organize the schema dict.

        Arguments:
            schema (dict): Schema dict

        Returns:
            sections (dict): Organized dict
        """
        sections = []
        indent = 2
        config_map = []
        for k, v in schema["definitions"].items():
            if v["type"] == "object":
                sch, map_item = self.expand_schema(
                    k, v["title"], v["description"], v["properties"], indent
                )
                sections.append(sch)
                config_map.append(map_item)

        final_map = {}
        for i in flatten_list(config_map):
            for k, v in i.items():
                final_map[k] = v

        self.config_map = final_map
        return flatten_list(sections)

    def get_config(self, config):
        """Organize the config dict.

        Arguments:
            config (dict): Config dict

        Returns:
            sections (dict): Organized dict
        """
        sections = []
        indent = 2
        for k, v in config.items():
            if k not in SKIP:
                sections.append(self.expand_config(None, k, v, indent))
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

                found = []
                for i, x in enumerate(res[j][kk]["items"]):
                    for y in k[kk]["items"]:
                        if x[1] == y[1]:
                            res[j][kk]["items"][i][2] = y[2]
                            res[j][kk]["items"][i][4] = y[4]
                            res[j][kk]["items"][i][5] = y[5]
                            found.append(x[1])

                for y in k[kk]["items"]:
                    if y[1] not in found:
                        res[j][kk]["items"].append(["", y[1], y[2], "", y[4], y[5], ""])

        return res

    def print_doc(self):
        """Print a merge of the config and schema dict."""
        with open(self.schema_file, "r") as infile:
            schema = json.load(infile)
            infile.close()

        sch = self.get_schema(schema)
        cfg = self.get_config(self.config)

        summary = self.merge(sch, cfg)

        self.print_header()
        for section in summary:
            hdr = list(section)[0]
            print(f"\n{hdr}")
            print(f"{section[hdr]['description']}")
            txt = "|".join(TABLE_HEADER)
            print(" ", txt)
            txt = "|".join(["", "---", "---", "---", "---", "---", ""])
            print(" ", txt)
            for w in section[hdr]["items"]:
                txt = "|".join(w)
                print(" ", txt)
