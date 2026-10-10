# Plug-ins
Tactus has the capability to add plug-ins. Internally tactus itself is also treated as a plug-in, which is always enabled.

## Plugin capablilities
In tactus suites and tasks are handled as plugins. The namespaces suites and tasks are searched for possible suites and tasks.

## Running your own suite
The configuration setting below allows you to use an alternative suite defintion.

```
[general.suite_control]
suite_definition = "MySuite"
```

This should pick up the suite definition MySuite if it's implementing the SuiteDefinition class from tactus.

## Enable your own plug-in
The config file has a plugin_registry key inside the general part. The loaded plug-ins are defined inside the `plugins` key in the registry. The loaded plugins are stored as a dictionary, which uses namespaces as keys and paths as values.

An example could be to add the plugin with namespace "example" in location /tmp would be to add:

```
[general.plugin_registry.plugins]
"example": "/tmp"
```

In this case it is expected that the namespace "example" is located in `/tmp/example`. Suites in `/tmp/example/suites` and tasks in `/tmp/example/`tasks will be picked up.

## Add optional components to suite families
Instead of writing a whole suite, a plug-in can add tasks or families at the end of any family of the tactus suite. A component is a class registered with `register_component` from `tactus.suites.suite_extensions`. Its `family` is the suite family class, a subclass of `EcflowSuiteFamily`, it is added to. It is added each time that family is created, if its `is_active(config)` returns True.

The component is added after everything else in the family and is triggered on all of it. As it is inside the family, everything waiting for the family also waits for the component. Components registered for a family class are also added to its subclasses.

For a single task, subclass `TaskComponent`:

```python
from tactus.suites.suite_extensions import TaskComponent, register_component
from tactus.suites.tactus_suite_components import ForecastFamily


@register_component
class MyTaskComponent(TaskComponent):
    name = "MyTask"  # Name of the task
    family = ForecastFamily  # Added at the end of every forecast family

    def is_active(self, config):
        return config.get("my_section.active", False)
```

For anything else, subclass `SuiteComponent` and implement `add_nodes(parent, ctx)`, where `ctx` holds the config, task settings, templates and trigger, taken from the arguments the family was created with.

Put the component in a module of the plug-in's `suites` package: those modules are imported when tactus discovers suites, which registers the component. Components are not added to plain `EcflowSuiteFamily` families, such as the day, time and member families, nor to family classes that do not create an ecflow family, such as `TimeDependentFamily`, for which a warning is logged.
