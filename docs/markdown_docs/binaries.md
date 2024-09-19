# Handling of binaries
Describes the handling of binaries in deode.

## Compilation and external sources
Currently the system provides no support for compilation of any of the IAL code required. This is expected to be prepared and compiled separately by e.g. gmkpack, cmake or similar. For the list of IAL cycles tested check the `cycle` key in the config documentation.

## How to specify binaries
There are several ways to specify which binary to be used. The standard way is to set `bindir` in the system config section. This assumes all required binaries are in the given directory and with the names used in the task. If, for whatever reason, a specific task should pick binaries from another directory, or with another name we can define location and/or name on task level by introducing a new section in the config file like:

```
[submission.task_exceptions.Forecast]
 bindir = "SOME_NEW_PATH"
 binary = "SOME_NEW_NAME"
```

If a specific task should pick more then one binary from different directories, we can define it like this:

```
[submission.task_exceptions.TASK_NAME.binaries]
 NAME_OF_BINARY = { "binary" = "somename" , "bindir" = "somewhere" }
 NAME_OF_BINARY1= { "binary" = "somename" , "bindir" = "somewhere" }
```
where NAME\_OF\_BINARY is the argument of the function `get_binary()` used in the code. For e.g. the mars client it's sometimes advisable to run with `-n` ( as in next version ). This then achieved by ( example from lumi )
```
[submission.task_exceptions.Marsprep.binaries]
 mars = { "binary" = "mars -n" , "bindir" = "/appl/local/destine/bin" }
```
