"""Define custom generators used to generate member specific parameters.

Used in EPS setup. A class of the present module can be referenced from a *.toml
file, which is then used to generate member specific parameters. The class must
inherit from the BaseGenerator class and implement the __iter__ method.

Example from toml file:

[eps.member_settings.dataassimilation]
  ensemble_jk = "tactus.eps.custom_generators.BoolGenerator"

Exports:
    BaseGenerator: Base class that all other generators inherit from.
    TestGenerator: Example generator class to generate random strings.
    BoolGenerator: Example generator class to generate random boolean values.
"""

from abc import abstractmethod
from typing import Generator, Generic, List, TypeVar

from pydantic.dataclasses import dataclass as pydantic_dataclass

GeneratorT = TypeVar("GeneratorT")


@pydantic_dataclass
class BaseGenerator(Generic[GeneratorT]):
    """Base generator class that all other generators inherit from.

    Generic[GeneratorT] is used to define the type of the generated parameters.
    When inheriting from this class, one specifies the type of the generated
    parameters by providing the type of the GeneratorT, e.g. BaseGenerator[str].

    Attributes:
        members (List[int]): The list of member indexes to generate values for

    Examples:
    @pydantic_dataclass
    class RandomStringGenerator(BaseGenerator[str]):
        '''Example generator class to generate random strings.'''

        def __iter__(self):
            for member in self.members:
                yield "".join(random.choices(string.ascii_letters, k=member))

    @pydantic_dataclass
    class RandomBoolGenerator(BaseGenerator[bool]):
        '''Example generator class to generate random boolean values.'''

        def __iter__(self):
            for _ in self.members:
                yield random.choice([True, False])
    """

    members: List[int]

    @abstractmethod
    def __iter__(self) -> Generator[GeneratorT, None, None]:
        """Overwrite this method to define the generator's behaviour."""


@pydantic_dataclass
class FDBTypeGenerator(BaseGenerator[dict]):
    """Generator class to generate type values for FDB."""

    def __iter__(self):
        if len(self.members) == 1:
            yield "fc"
        for member in self.members:
            # For now, type="cf" does not have "number" in mars namespace
            # and thus cannot be archived to FDB enfo stream
            # The line below is the correct one to use when type="cf"
            # is supported in mars namespace
            # yield "cf" if member == 0 else "pf" pylint: disable=ERA001
            yield "pf" if member == 0 else "pf"


class CmodelGenerator(BaseGenerator[dict]):
    """Generator class to generate a full fdb.grib_set dictionary per member."""

    def __iter__(self):
        if len(self.members) == 1:
            yield "destine-@CYCLE@-@CSC@-oper"
        for member in self.members:
            yield f"destine-@CYCLE@-@CSC@-enfo-{member}"
