"""Optional components that are added at the end of suite families."""

import inspect
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from ..logs import logger
from ..submission import TaskSettings
from .base import EcflowNode, EcflowSuiteFamily, EcflowSuiteTask


@dataclass
class ComponentContext:
    """What a component needs to add its nodes to a family."""

    config: Any
    task_settings: TaskSettings
    input_template: str
    ecf_files: str
    trigger: Any = None
    ecf_files_remotely: Optional[str] = None


class SuiteComponent:
    """Base class for optional suite components.

    Subclasses set ``name`` and ``family``, the EcflowSuiteFamily subclass at
    whose end the component is added, and implement ``add_nodes``.
    """

    name: str = ""
    family: Optional[type] = None

    def is_active(self, config) -> bool:  # ruff:ignore[unused-method-argument]
        """Return whether the component should be added for this config.

        Args:
            config: Experiment config.
        """
        return True

    def add_nodes(self, parent: EcflowNode, ctx: ComponentContext):
        """Add the nodes of the component to ``parent``.

        Args:
            parent: Family to add the component nodes to.
            ctx: Context of the family.

        Raises:
            NotImplementedError: Must be implemented by subclasses.
        """
        raise NotImplementedError


class TaskComponent(SuiteComponent):
    """Component adding a single task named ``name``.

    The task is triggered on the trigger of the context.
    """

    def add_nodes(self, parent: EcflowNode, ctx: ComponentContext):
        """Add the task to ``parent``.

        Args:
            parent: Family to add the task to.
            ctx: Context of the family.

        Returns:
            EcflowSuiteTask: The added task.
        """
        return EcflowSuiteTask(
            self.name,
            parent,
            ctx.config,
            ctx.task_settings,
            ctx.ecf_files,
            input_template=ctx.input_template,
            trigger=ctx.trigger,
            ecf_files_remotely=ctx.ecf_files_remotely,
        )


_components: Dict[type, Dict[str, SuiteComponent]] = {}


def register_component(cls):
    """Class decorator registering a suite component.

    Registering a component with the same name and family again replaces the
    earlier one.

    Args:
        cls (type): SuiteComponent subclass to register.

    Returns:
        type: The unchanged class.

    Raises:
        ValueError: If name is not set, or family is not an EcflowSuiteFamily
            subclass.
    """
    component = cls()
    if not component.name:
        raise ValueError(f"{cls.__name__} must set name")
    family = component.family
    if not (
        inspect.isclass(family)
        and issubclass(family, EcflowSuiteFamily)
        and family is not EcflowSuiteFamily
    ):
        raise ValueError(
            f"{cls.__name__} has family {family!r}, "
            "it must be a subclass of EcflowSuiteFamily"
        )
    _components.setdefault(family, {})[component.name] = component
    return cls


def get_components(family: type) -> List[SuiteComponent]:
    """Return the components added at the end of a family class.

    Components registered for a parent class of ``family`` are included.

    Args:
        family: EcflowSuiteFamily subclass.

    Returns:
        List of components, those of ``family`` first, each in registration order.
    """
    return [
        component
        for cls in family.__mro__
        for component in _components.get(cls, {}).values()
    ]


def add_family_components(family: EcflowSuiteFamily, init, args, kwargs) -> list:
    """Add the active components registered for a family to its end.

    The components are triggered on all nodes already in the family, and are
    inside it, so whatever waits for the family also waits for them.

    Args:
        family: The constructed family.
        init: The __init__ that constructed it.
        args: Positional arguments given to ``init``, without self.
        kwargs: Keyword arguments given to ``init``.

    Returns:
        list: The values returned by ``add_nodes`` of the added components.
    """
    components = get_components(type(family))
    if not components:
        return []

    if family.__dict__.get("node_type") != "family":
        logger.warning(
            "{} is not an ecflow family here, not adding components {}",
            type(family).__name__,
            [component.name for component in components],
        )
        return []

    arguments = inspect.signature(init).bind(family, *args, **kwargs)
    arguments.apply_defaults()
    arguments = arguments.arguments
    config = arguments["config"]
    ctx = ComponentContext(
        config,
        arguments["task_settings"],
        arguments["input_template"],
        arguments["ecf_files"],
        trigger=list(family.children) or None,
        ecf_files_remotely=arguments.get("ecf_files_remotely"),
    )

    added = []
    for component in components:
        if component.is_active(config):
            logger.debug("Adding suite component {} to {}", component.name, family.path)
            added.append(component.add_nodes(family, ctx))
    return added
