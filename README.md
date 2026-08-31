[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/ACCORD-NWP/tactus)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://ACCORD-NWP.github.io/tactus/)


[![Linting](https://github.com/ACCORD-NWP/tactus/actions/workflows/linting.yaml/badge.svg)](https://github.com/ACCORD-NWP/tactus/actions/workflows/linting.yaml)
[![Tests](https://github.com/ACCORD-NWP/tactus/actions/workflows/tests.yaml/badge.svg
)](https://github.com/ACCORD-NWP/tactus/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/ACCORD-NWP/tactus/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/ACCORD-NWP/tactus)

# TACTUS Scripting System

## About

The [tactus scripting system](https://github.com/ACCORD-NWP/tactus/) provides a `tactus` python package.

See the [project's documentation page](https://ACCORD-NWP.github.io/tactus) for more information.


## Installation

First checkout the `tactus` source code from github:
```shell
git clone git@github.com:ACCORD-NWP/tactus.git
cd tactus
```

For development, use forks as specified in the [Development guidelines](https://ACCORD-NWP.github.io/tactus/development_guidelines_link.html).
To clone the forked repository, use the following command, replacing \<username\> with your GitHub username:
```shell
git clone git@github.com:<username>/tactus.git
cd tactus
```

> [!IMPORTANT]
> Tactus should be installed in a folder accessible by ecflow server. On Atos, it should be installed in your $HOME or $PERM directory.

Then install [`Pixi`](https://pixi.sh) by following the installation instructions at https://pixi.sh/latest/#installation, or use your system package manager:

```shell
# On macOS with Homebrew
brew install pixi

# On Linux (after following Pixi docs)
curl -fsSL https://pixi.sh/install.sh | bash
```

After installing Pixi, set up Pixi environment from the root of the cloned repository:
```shell
pixi install
```

If you want to install the environment in another directory than the default (`<project-root>/.pixi/envs/`) add `detached-environments = "/path/to/env-location/"` to a pixi config file. Supported locations of the pixi config is outlined in https://pixi.prefix.dev/latest/reference/pixi_configuration/.

Pixi caches the downloaded packages and shares them between projects to speed up environment installation. To change the cache directory, add the following to the pixi config:
```toml
[cache]
  root = "/path/to/cache"
  netfs-redirect = "never"
```
## Usage

Navigate to the root level of the `tactus` install directory and use Pixi to run:
```shell
pixi run tactus -h
```

Alternatively, to activate the environment and work interactively:
```shell
pixi shell
tactus -h
```

### The Configuration File
Before you can use `tactus` (apart from the `-h` option), you will need a configuration file written in the
[TOML](https://en.wikipedia.org/wiki/TOML) format. Please take a look at
 the default
 [config.toml](https://github.com/ACCORD-NWP/tactus/blob/develop/tactus/data/config_files/config.toml) file, as well as the
 [project's Doc Page](https://ACCORD-NWP.github.io/tactus),
 for more information about this.

 To see all configs currently in place in your `tactus` setup, please run
 ```shell
 tactus show config
 ```

### Command line options

After completing the setup, you should be able to run
```shell
tactus [opts] SUBCOMMAND [subcommand_opts]
```
where `[opts]` and `[subcommand_opts]` denote optional command line arguments
that apply, respectively, to `tactus` in general and to `SUBCOMMAND`
specifically.

**Please run `tactus -h` for information** about the supported subcommands
and general `tactus` options. For info about specific subcommands and the
options that apply to them only, **please run `tactus SUBCOMMAND -h`** (note
that the `-h` goes after the subcommand in this case).

## Examples

These examples assume that you have successfully [installed](#installation) tactus, navigated to the root level of your `tactus` install directory, and entered into a shell by running `pixi shell`. The examples also assume that the binaries and input data for the [ACCORD CSCs](https://www.umr-cnrm.fr/accord/?Canonical-System-Configurations-CSC) is in place. Please contact your local ACCORD members for advice if this is not the case.

### Running ecflow suite on ATOS

The following command will run the full suite using the default experiment:
```shell
tactus case ?tactus/data/config_files/configurations/cy49t2_arome --case-name my_first_test --start-suite
```
This will generate a new config file `my_first_test.toml` that is used to launch the suite. The working directories and final results can be found under `$SCRATCH/tactus/my_first_test'.

### Running a single task from command line
From the example above we can rerun e.g. the `Forecast` task from command line by

```
tactus run --task Forecast -c my_first_test.toml
```
This will create `Forecast.job` in the current directory and submit the job. The log from the job will appear as `Forecast.log` and the result will be found in the same directories as above.


For other platforms a new config file would have to be created first. Please consult the [configure cases](https://ACCORD-NWP.github.io/tactus/misc_section_in_doc_page.html#configure-cases) section in the documentation for more information.
