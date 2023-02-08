"""Canari."""

from .base import Task


class Canari(Task):
    """Canari task."""

    def __init__(self, config):
        """Construct canari object.

        Args:
            config (deode.ParsedConfig): Configuration

        """
        Task.__init__(self, config, __name__)
        self.logger.debug("Constructed canari task")

    def execute(self):
        """Execute task."""
        # Possibly from input dir
        self.fmanager.input("@CLIMDATA@/m@MM@", "m@MM@", check_archive=True)
        self.fmanager.input(
            "@ARCHIVE@/ICMSH@CNMEXP@+0000", "ICMSH@CNMEXP@+0000", provider_id="symlink"
        )

        res_dict = {
            "input": {
                "@CLIMDATA@/const.clim.@DOMAIN@": {"destination": "const.clim.@DOMAIN@"}
            }
        }
        self.fmanager.set_resources_from_dict(res_dict)
