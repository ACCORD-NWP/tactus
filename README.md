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


## Set up environment

**Make sure you have python>=3.10**

<a name="#put-poetry-in-path"></a> Start by adding the `$HOME/.local/bin`
directory in your `PATH`:
```shell
export PATH="$HOME/.local/bin:$PATH"
```

Then, run:

* On Atos (`hpc-login`)
  ```shell
  module load python3/3.10.10-01
  module load ecflow
  ```

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


Then install/reinstall [`poetry`](https://python-poetry.org) by runnning the following commands in your shell:
  ```shell
  # Clean eventual previous install
  curl -sSL https://install.python-poetry.org | python3 - --uninstall
  rm -rf ${HOME}/.cache/pypoetry/ ${HOME}/.local/bin/poetry ${HOME}/.local/share/pypoetry
  # Download and install poetry
  curl -sSL https://install.python-poetry.org | python3 -
  poetry install
  # Add the poetry shell command as a plugin (for poetry >= v2.0.0)
  poetry self add poetry-plugin-shell
  ```

Finally, install [`pygdal`](https://pypi.org/project/pygdal/), which is required for climate generation. [`pygdal`](https://pypi.org/project/pygdal/) depends on [`gdal`](https://gdal.org/), which is notoriously troublesome as dependency when targeting many systems. The versions of `pygdal` and the system's `gdal`should match.

  To install gdal and pygdal run the follow in commands in your shell:

  * On Atos (`hpc-login`)
    ```shell
    module load gdal/3.6.2
    poetry shell
    pip install pygdal==3.6.2.11
    ```
  If installation is not succesful, please contact the IT support in your organisation or HPC facility.

### Important

Tactus should be installed in a folder accessible by ecflow server.

On Atos, it should be installed in your $HOME or $PERM directory.


## Usage

Initially set up the environment by repeating the steps in [Set up environment](#set-up-environment), navigate to the root level of the `tactus` install directory and activate python virtual environment:
```shell
poetry shell
```
Alternatively, to activate a `tactus` installation located in an arbitrary
directory `MY_TACTUS_SOURCE_DIRECTORY`, please run:
```shell
poetry shell --directory=MY_TACTUS_SOURCE_DIRECTORY
```

Test that `tactus` works by running:
```shell
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

These examples assume that you have successfully [Set up environment](#set-up-environment) [installed](#installation) tactus, navigated to the root level of your `tactus` install directory and loaded the python environment. The examples also assume that the binaries and input data for the [ACCORD CSCs](https://www.umr-cnrm.fr/accord/?Canonical-System-Configurations-CSC) is in place. Please contact your local ACCORD members for advice if this is not the case.

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
