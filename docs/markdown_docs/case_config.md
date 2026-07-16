# Configure cases

## The case setup

Tactus is designed to be highly configurable and driven from a single config file. The `tactus case` functionality offers a way to reduce the number of lengthy config files by building the final config file from smaller sections of configuration settings. A number of pre-defined configurations are available under `tactus/data/config_files/configurations`

Example usage would be:
```
tactus case ?tactus/data/config_files/configurations/cy49t2_alaro -o test.toml
```

where ? is a file includer operator where all the arguments are defined line by line. I.e. `tactus/data/config_files/configurations/cy49t2_alaro` contains a list of arguments to be evaluated. In this case we have

```
--config-file
tactus/data/config_files/config.toml
tactus/data/config_files/modifications/csc/alaro.toml
tactus/data/config_files/include/vertical_levels/MF_87.toml
tactus/data/config_files/modifications/cycle/CY49t2.toml
tactus/data/config_files/modifications/@HOST@.toml
```

When the first config file, `config.toml`, has been read the appropriate files for the current host is included for `scheduler` `platform`and `submission`.

The following configuration files will be read and added in order of appearence. If we check the various files we find that e.g. `alaro.toml` only contains the bare minimum changes on top of the default configuration file required to run the ALARO CSC.

The processed configuration output file, here `test.toml`, is self contained from a config point of view. All configuration settings (also defaults from json schema) are in the generated configuration file.

The produced config file, `test.toml` is now used to start a run the usual way.
```
tactus start suite --config-file test.toml
```

We can also do everything in one by adding the `--start-suite` flag
```
tactus case ?tactus/data/config_files/configurations/cy49t2_alaro -o test.toml --start-suite
```

To see all commands available for the case functionality run `tactus case --help`.

## Time handling

A typical use case is to run the same configuration for a number of dates or a longer period. The example above could easily be modified to run for any arbitrary date by running
```
tactus case ?tactus/data/config_files/configurations/cy49t2_alaro time.toml -o test.toml
```
where `time.toml` contains

```
[general.times]
  end = "YYYY-MM-DD:HH:mm:ssZ"
  start = "YYYY-MM-DD:HH:mm:ssZ"
```
or any additional extra information.


## Compiling IAL via the `compile` command

In addition to `tactus case` and `tactus start suite`, Tactus provides a dedicated `tactus compile` command that builds a configuration tailored for compiling the IAL (IFS/Arpege Library) sources. It is a thin wrapper around `tactus case` that:

* forces inclusion of the host-specific modifications (`@HOST@.toml`)
* forces inclusion of the compilation suite modifications (`compile_suite.toml`)
* injects the requested IAL Git tag/branch into `compile.ial_git_branch`
* uses the `CompilationSuiteDefinition` suite

### Basic usage

```
tactus compile --ial-tag develop
```

produces and launches case named `IAL_develop_compile`.

`compile_suite.toml` defines the case name as:

```toml
[general]
  case = "IAL_@IAL_TAG@_compile"
```

The `@IAL_TAG@` macro is resolved from `compile.ial_git_branch` (see `tactus/data/config_files/include/macros.toml`), which is set from the `--ial-tag` argument.
