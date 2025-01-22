"""Module for utility functions used in the suite definition scripts."""

from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Generator, Iterator, List, Tuple, Union

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
    do_prep: bool = True,
) -> Generator[Union[int, datetime], None, None]:
    """Generate lbc times.

    For each of them there will be LBC[NN] family.

    Args:
        basetime: The base time.
        endtime: The end time.
        step: The step size.
        mode: The mode of the workflow.
        do_prep: Whether to do prep.

    Yields:
            datetime: The time period for which the next LBC will be computed.

    Returns:
            datetime: The time period for which the last LBC will be computed.
    """
    if mode == "restart" or (mode == "start" and not do_prep):
        basetime += step

    while basetime <= endtime:
        # Yield the updated basetime
        yield basetime
        basetime += step

    # Return the last basetime
    return basetime


def bd_generator(
    bdint: timedelta,
    mode: str = "start",
    do_prep: bool = True,
) -> Generator[Tuple[int, int, int, int], None, None]:
    """Generate batch numbers.

    Args:
        bdint: The batch interval.
        mode: The mode of the workflow.
        do_prep: Whether to do prep.

    Yields:
        Tuple[int, int, int, int]:
    """
    inthourbdint = int(bdint.total_seconds() // 3600)
    intminbdint = int(bdint.total_seconds() % 3600 // 60)
    intsecbdint = int(bdint.total_seconds() % 60)

    # we don't need LBC000 if this is not first cycle or mode != cold_start
    if mode == "restart" or (mode == "start" and not do_prep):
        bdnr = inthourbdint
        bd_nr = 1
        subbdnr = intminbdint if (intminbdint or intsecbdint) else None
        subminbdnr = intsecbdint if intsecbdint else None
    else:
        bdnr = 0
        bd_nr = 0
        subbdnr = 0 if (intminbdint or intsecbdint) else None
        subminbdnr = 0 if intsecbdint else None

    while True:
        yield (
            bdnr,
            subbdnr,
            subminbdnr,
            bd_nr,
        )

        if subbdnr is not None:
            subbdnr += intminbdint
            if subminbdnr is not None:
                subminbdnr += intsecbdint
                if subminbdnr >= 60:
                    subbdnr += 1
                    subminbdnr -= 60
            if subbdnr >= 60:
                bdnr += 1
                subbdnr -= 60

        bdnr += inthourbdint
        bd_nr += 1
