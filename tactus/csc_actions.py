#!/usr/bin/env python3
"""Handle tstep selection."""


class SelectTstep:
    """Class for the tstep selection."""

    @staticmethod
    def _select_tstep_based_on_csc_and_resolution(csc: str, resolution: str) -> int:
        """Select tstep based on csc and resolution.

        Args:
            csc (str): The csc to use
            resolution (str): The resolution to use

        Raises:
            ValueError: CSC and/or resolution not supported.

        Returns:
            int: The tstep
        """
        if round(float(resolution)) == 500:
            if csc == "HARMONIE_AROME":
                return 15
            if csc == "ALARO":
                return 20
            if csc == "AROME":
                return 20
            raise ValueError(
                "CSC not supported. Choose between ['HARMONIE_AROME', 'ALARO', 'AROME']."
            )
        if round(float(resolution)) == 750:
            if csc == "HARMONIE_AROME":
                return 20
            if csc == "ALARO":
                return 30
            if csc == "AROME":
                return 30
            raise ValueError(
                "CSC not supported. Choose between ['HARMONIE_AROME', 'ALARO', 'AROME']."
            )

        raise ValueError("Resolution not supported. Choose between ['500', '750'].")
