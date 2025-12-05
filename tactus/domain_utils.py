"""Utility for domain handling."""

from typing import Any, Dict


def get_domain(config) -> Dict[str, Any]:
    """Read and return domain data.

    Args:
        config (deode.ParsedConfig): Configuration from which we get the domain data
    Returns:
        Dictionary containing the domain
    """
    # Get domain specs
    domain = {
        "nlon": config["domain.nimax"],
        "nlat": config["domain.njmax"],
        "latc": config["domain.xlatcen"],
        "lonc": config["domain.xloncen"],
        "lat0": config["domain.xlat0"],
        "lon0": config["domain.xlon0"],
        "xdx": config["domain.xdx"],
        "xdy": config["domain.xdy"],
        "gsize": config["domain.xdx"],
    }

    return domain
