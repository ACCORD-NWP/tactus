"""Configuration file for the Sphinx documentation builder."""

#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

import datetime
import sys
from pathlib import Path

sys.path.insert(0, Path("..").resolve().as_posix())

project = "tactus docs"
project_copyright = "2026, ACCORD"
author = "ACCORD"

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "myst_parser",
    "sphinx.ext.autodoc",
    "sphinx.ext.viewcode",
    "sphinx.ext.napoleon",
]

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]
suppress_warnings = ["myst.xref_missing"]

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = "sphinx_rtd_theme"
html_last_updated_fmt = (
    datetime.datetime
    .now(datetime.timezone.utc)
    .isoformat(timespec="seconds")
    .replace("+00:00", "Z")
)
add_module_names = False
html_show_sourcelink = False

# See <https://sphinx-rtd-theme.readthedocs.io/en/stable/configuring.html>
html_theme_options = {"collapse_navigation": False}

# Further customisation
html_static_path = ["_static"]
html_css_files = ["css/custom.css"]


# -- Custom link transformation for README -----------------------------------
def transform_readme_links(app, doctree, docname):
    """Transform absolute doc URLs to relative links when building documentation.

    This allows the README to use absolute URLs (which work on GitHub) while
    having them automatically converted to relative links in the built docs.
    """
    from docutils import nodes

    doc_base_url = (
        "https://ACCORD-NWP.github.io/tactus/"
    )

    # Iterate through all reference nodes in the document tree
    for node in doctree.traverse(nodes.reference):
        if (
            "refuri" in node
            and isinstance(node["refuri"], str)
            and node["refuri"].startswith(doc_base_url)
        ):
            # Replace absolute URL with relative path
            node["refuri"] = node["refuri"].replace(doc_base_url, "")


def setup(app):
    """Sphinx setup hook to register event handlers."""
    app.connect("doctree-resolved", transform_readme_links)
