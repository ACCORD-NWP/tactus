# Plug-ins
Deode has the capability to add plug-ins. Internally deode it self is also treated as a plug-in, which always is enabled.

## Plugin capablilities
In Deode suites and tasks are handled as plugins. The namespaces suites and tasks are searched for possible suites and tasks.

## Running your own suite
The configuration setting below allows you to use an alternative suite defintion.

'''
[general.suite_control]
suite_definition = "MySuite"
'''

This should pick up the suite definition MySuite if it's implementing the SuiteDefinition class from Deode.

## Enable your own plug-in
The config file has a plugin_registry key inside the general part. The loaded plug-ins are inside the "plugins" key in the registry. The loaded plugins is as a dictionary which has namespaces as keys and paths as values.

An example could be to add the plugin with namespace "example" in location /tmp would be to add:

'''
[general.plugin_registry.plugins]
"example": "/tmp"
'''

In this case it is expected that the namespace "example" is located in /tmp/example. Suites in /tmp/example/suites and tasks in /tmp/example/tasks would be picked up.
