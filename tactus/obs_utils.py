"""Observation handling support classes."""

from tactus.datetime_utils import as_timedelta
from tactus.logs import logger


class Observations:
    """Observation class."""

    def __init__(self, config, da_stream="surface"):
        """Construct object.

        Args:
            config (ParsedConfig): Configuration
            da_stream (str): DA stream type (surface/upper_air)
        """
        self.family = config.get("task.args.da_stream", da_stream)
        logger.info("da_stream:{}", self.family)
        da_config = f"da.{self.family}"

        self.obs_types = config.get(f"{da_config}.obs_types")

        # Providers
        self.obs_provider = config.get("da.obs_provider", "None.")
        all_providers = config.get("da.providers", {})

        if self.obs_provider not in all_providers:
            logger.warning(
                "ObsPrep: obs_provider '{}' not defined in da.providers — "
                "no obs sources will be found; add a [da.providers.{}] block.",
                self.obs_provider,
                self.obs_provider,
            )
            self.provider = {}
        else:
            self.provider = all_providers[self.obs_provider]

        # New schema: obstypes nested under provider. Old schema: direct keys.
        self.obstypes = self.provider.get("obstypes") or self.provider

        # Time control
        self.bator_nbslot = config.get(f"{da_config}.bator_nbslot", 1)
        self.bator_window_len = as_timedelta(
            config.get(f"{da_config}.bator_window_len", "PT3H")
        )
        self.bator_window_shift = as_timedelta(
            config.get(f"{da_config}.bator_window_shift", "-PT90M")
        )
        self.bator_slot_len = as_timedelta(
            config.get(f"{da_config}.bator_slot_len", "PT0H")
        )
        self.bator_center_len = as_timedelta(
            config.get(f"{da_config}.bator_center_len", "PT0H")
        )

        self.nbpool = config.get(f"{da_config}.nbpool")

        self.obs_step = as_timedelta(
            self.provider.get("obs_step", config.get("da.obs_step", "PT0H"))
        )


def odb_io_env(config, nbpool):
    """Build the ODB I/O environment settings from config.

    Config keys (all optional, under ``da``):
        odb_io_method   : ODB I/O method (default 4)
        odb_io_filesize : file size in MB (default 128)
        odb_io_grpsize  : pools per group (default: this stream's pool count)

    Args:
        config (ParsedConfig): Experiment configuration.
        nbpool (int): Pool count for this stream; the default group size.

    Returns:
        dict: Environment variables as strings, ready to merge into ``rte``.

    """
    return {
        "ODB_IO_METHOD": str(config.get("da.odb_io_method", 4)),
        "ODB_IO_FILESIZE": str(config.get("da.odb_io_filesize", 128)),
        "ODB_IO_GRPSIZE": str(config.get("da.odb_io_grpsize", nbpool)),
    }
