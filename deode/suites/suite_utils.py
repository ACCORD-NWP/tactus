"""Module for utility functions used in the suite definition scripts."""

from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Dict, Generator, Iterator, List

from ..datetime_utils import as_datetime, as_timedelta
from ..logs import logger


@dataclass(frozen=True, kw_only=True)
class Cycle:
    """Class for representing a cycle."""

    day: str
    time: str
    validtime: str
    basetime: str


@dataclass
class Cycles:
    """Class for generating and iterating over Cycle objects."""

    first_cycle: str  # Format parseable by as_datetime
    last_cycle: str  # Format parseable by as_datetime
    cycle_length: str  # ISO 8601 format
    _current_index: int = field(init=False, default=0)
    _cycles: List[Cycle] = field(init=False, default_factory=list)

    def __post_init__(self):
        # Generate cycles.
        self._generate_cycles()

    def _generate_cycles(self):
        """Generate cycles."""
        # Convert attributes to datetime objects.
        cycle_time = as_datetime(self.first_cycle)
        last_cycle_time = as_datetime(self.last_cycle)
        cycle_length_timedelta = as_timedelta(self.cycle_length)

        # Generate cycles.
        while cycle_time <= last_cycle_time:
            logger.debug("cycle_time {}", cycle_time)
            self._cycles.append(
                Cycle(
                    day=cycle_time.strftime("%Y%m%d"),
                    time=cycle_time.strftime("%H%M"),
                    validtime=cycle_time.strftime("%Y-%m-%dT%H:%M:%SZ"),
                    basetime=cycle_time.strftime("%Y-%m-%dT%H:%M:%SZ"),
                )
            )
            cycle_time += cycle_length_timedelta

    @property
    def current_index(self) -> int:
        """Return the current cycle index."""
        return self._current_index

    @property
    def current_cycle(self) -> Cycle:
        """Return the current Cycle object."""
        return self._cycles[self._current_index]

    @property
    def next_cycle(self) -> Cycle:
        """Return the next Cycle object.

        Raises:
            StopIteration: If there are no more cycles.
        """
        if self._current_index < len(self._cycles) - 1:
            return self._cycles[self._current_index + 1]

        raise StopIteration

    @property
    def end_of_month(self) -> bool:
        """Return True if the next cycle is in a different month.

        Returns:
            bool: True if the next cycle is in a different month
        """
        _end_of_month = False
        try:
            current_cycle = self.current_cycle
            next_cycle = self.next_cycle
            if as_datetime(current_cycle.day).strftime("%m") != as_datetime(
                next_cycle.day
            ).strftime("%m"):
                _end_of_month = True
        except StopIteration:
            logger.debug("It is last cycle")

        return _end_of_month

    def __iter__(self) -> Iterator[Cycle]:
        """Return an iterator over the cycles."""
        self._current_index = 0
        while self._current_index < len(self._cycles):
            yield self._cycles[self._current_index]
            self._current_index += 1


def lbc_times_generator(
    basetime: datetime,
    endtime: datetime,
    step: timedelta,
    mode: str = "start",
    is_first_cycle: bool = True,
    do_interpolsstsic: bool = False,
    lbc_per_task: int = 1,
) -> Generator[Dict[int, str], None, None]:
    """Generate lbc times.

    For each of them there will be LBC[NN] family.

    Args:
        basetime: The base time.
        endtime: The end time.
        step: The step size.
        mode: The mode of the workflow.
        is_first_cycle: Whether this is the first cycle.
        do_interpolsstsic: Whether to do SST/SIC interpolation.
        lbc_per_task: Number of LBC assigned to each task. Default 1.

    Returns:
            datetime: The time period for which the last LBC will be computed.

    Yields:
            datetime: The time period for which the next LBC will be computed.
    """
    index = 0
    if (
        mode == "restart" or (mode == "start" and not is_first_cycle)
    ) and not do_interpolsstsic:
        basetime += step
        index = 1

    while basetime <= endtime:
        batch: Dict[int, datetime] = {}

        for _ in range(lbc_per_task):
            if basetime > endtime:
                break
            batch[index] = basetime.isoformat("T")
            index += 1
            basetime += step

        if batch:
            yield batch
    # Return the last basetime
    return basetime


def slaf_planner(config, lbc_time_generator, me) -> dict:
    """Distribution of work in case several EPS members need the same boundary files.

    This is typically the case with the SLAF method. Note that every member of the EPS
    calls this routine in the present setup (which is strictly not necessary).

    Args:
        config (BaseConfig):             configuration object
        lbc_time_generator (Generator):  as above
        me (int):                        my EPS member number

    Returns:
        doer (dict):                     Holds, for each unique boundary file, information
                                         about which member will do the actual work
                                         and in which SLAF part (0, 1 or 2).

    Raises:
        RuntimeError:                    If the planning logic fails.
    """
    bdint = as_timedelta(config["boundaries.bdint"])
    bdshift = [as_timedelta("PT0H") for i in range(3)]
    # Collect all candidate "doers" for each boundary file
    # - note: we assume that all members have the same forecast range
    cand = {}
    nfuture = 0
    jbatch = 0
    nbatch = {}
    for bd_index_time_dict in lbc_time_generator:
        jbatch += 1
        nbatch[jbatch] = len(bd_index_time_dict)
        for bd_index, lbc_time in bd_index_time_dict.items():
            date_string = lbc_time.replace("+00:00", "Z")
            for member in config["eps.general.members"]:
                # With SLAF, all members need the unperturbed lbc files
                key = f"{date_string};{bd_index}"
                if key in cand:
                    cand[key] += f",{member}:0:{jbatch}"
                else:
                    cand[key] = f"{member}:0:{jbatch}"
                # Now handle the files involved in the perturbations
                slaflag = config.get(f"eps.members.{member}.boundaries.slaflag", "PT0H")
                slafdiff = config.get(f"eps.members.{member}.boundaries.slafdiff", "PT0H")
                if slaflag != "PT0H" and slafdiff != "PT0H":
                    bdshift[1] = as_timedelta(slaflag) - as_timedelta(slafdiff)
                    bdshift[2] = as_timedelta(slaflag)
                    for i in (1, 2):
                        bd_index_shifted = bd_index + bdshift[i] // bdint
                        lbc_time_shifted = as_datetime(lbc_time) - bdshift[i]
                        date_string_shifted = lbc_time_shifted.isoformat(sep="T").replace(
                            "+00:00", "Z"
                        )
                        key = f"{date_string_shifted};{bd_index_shifted}"
                        if key in cand:
                            cand[key] += f",{member}:{i}:{jbatch}"
                        else:
                            cand[key] = f"{member}:{i}:{jbatch}"
                            if bd_index_shifted < 0:
                                nfuture += 1
    # Abort if the setup requires boundary files for "future" cycles
    if nfuture > 0:
        logger.error(
            "SLAF planner: detected need for {} 'future' boundary files, ABORT!", nfuture
        )
        logger.error("Please review your settings of slaflag and slafdiff!")
        raise RuntimeError("Suspect SLAF configuration!")
    # Now distribute the files as evenly as possible, picking one duo for each batch
    n_uniq = len(cand)
    taken = dict.fromkeys(cand, False)
    n_taken = dict.fromkeys(config["eps.general.members"], 0)
    doer = {}
    for key in cand:
        if not taken[key]:
            mbr_min = -1
            taken_min = n_uniq
            part_min = -1
            batch_min = -1
            for trio in cand[key].split(","):
                member, i, jbatch = [int(j) for j in trio.split(":")]
                if n_taken[member] < taken_min:
                    mbr_min = member
                    taken_min = n_taken[member]
                    part_min = i
                    batch_min = jbatch
            # We have a winner, all files in same batch must have same doer
            doer[key] = f"{mbr_min}:{part_min}"
            taken[key] = True
            for k2 in cand:
                if not taken[k2]:
                    for trio in cand[k2].split(","):
                        member, i, jbatch = [int(j) for j in trio.split(":")]
                        if member == mbr_min and i == part_min and jbatch == batch_min:
                            doer[k2] = f"{mbr_min}:{part_min}"
                            taken[k2] = True
                            break
            n_taken[mbr_min] += nbatch[batch_min]
        if not taken[key]:
            # If we reach here, it is a programmer error
            if me == 0:
                logger.error("SLAF planner: key '{}' not taken by any member!", key)
            raise RuntimeError(
                f"The SLAF planner failed to assign worker for key '{key}'"
            )
        if me == 0:
            logger.debug("SLAF planner: key '{}' taken by duo {}", key, doer[key])
    logger.info(
        "SLAF planner: member {} handles {} out of {} files", me, n_taken[me], n_uniq
    )

    return doer
