[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Workflow)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-workflow-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Workflow/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Workflow)

# DEODE Scripting System

## About

The [DEODE Scripting System](https://github.com/destination-earth-digital-twins/Deode-Workflow/) provides a `deode` python package that runs the [Destination Earth on Demand Extremes system](https://github.com/destination-earth-digital-twins).

See the [project's documentation page](https://destination-earth-digital-twins.github.io/deode-workflow-docs) for more information.


## Set up environment

**Make sure you have python>=3.10**

<a name="#put-poetry-in-path"></a> Start by adding the `$HOME/.local/bin`
directory in your `PATH`:
```shell
export PATH="$HOME/.local/bin:$PATH"
```

We **highly recommend** you to also put the statement listed above in your shell configuration file, so you don't need to do this the next time you log in. Then, run:

* On Atos (`hpc-login`)
  ```shell
  module load python3/3.10.10-01
  module load ecflow
  ```

* On LUMI
  ```shell
  ml use /scratch/project_465000527/jasinskas/scl/modules/
  ml pyeccodes_23
  ml scl-ecflow_23
  ```

* On Macs (local install only)
  ```shell
  brew install pyenv
  pyenv install 3.10.10
  # (or which ever version you want to universalise)
  pyenv global 3.10.10
   ```
  Add eval "$(pyenv init --path)" to ~/.zprofile (or ~/.bash_profile or ~/.zshrc, whichever you need). Relaunch the shell and check that Python works, or run $ source ~/.zprofile

## Installation

First checkout the `deode` source code from github: 
```shell
git clone git@github.com:destination-earth-digital-twins/Deode-Workflow.git
cd Deode-Workflow
```

For development, use forks as specified in the [Development guidelines](https://github.com/destination-earth-digital-twins/Deode-Workflow/blob/develop/docs/markdown_docs/development_guide.md).
To clone the forked repository, use the following command, replacing \<username\> with your GitHub username:
```shell
git clone git@github.com:<username>/Deode-Workflow.git
cd Deode-Workflow
```

Then install/reinstall [`poetry`](https://python-poetry.org) by runnning the following commands in your shell:
  ```shell
  # Clean eventual previous install
  curl -sSL https://install.python-poetry.org | python3 - --uninstall
  rm -rf ${HOME}/.cache/pypoetry/ ${HOME}/.local/bin/poetry ${HOME}/.local/share/pypoetry
  # Download and install poetry
  curl -sSL https://install.python-poetry.org | python3 -
  poetry install
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



## Usage

Initially set up the environment by repeating the steps in [Set up environment](#set-up-environment), navigate to the root level of the `Deode-Workflow` install directory and activate python virtual environment:
```shell
poetry shell
```
   
Alternatively, to activate a `deode` installation located in an arbitrary
directory `MY_DEODE_SOURCE_DIRECTORY`, please run:
```shell
poetry shell --directory=MY_DEODE_SOURCE_DIRECTORY
```

Test that `deode` works by running:
```shell
deode -h
```
### The Configuration File
Before you can use `deode` (apart from the `-h` option), you will need a configuration file written in the
[TOML](https://en.wikipedia.org/wiki/TOML) format. Please take a look at
 the default
 [config.toml](https://github.com/destination-earth-digital-twins/Deode-Workflow/blob/develop/deode/data/config_files/config.toml) file, as well as the
 [project's Doc Page](https://destination-earth-digital-twins.github.io/deode-workflow-docs),
 for more information about this.

 To see all configs currently in place in your `deode` setup, please run
 ```shell
 deode show config
 ```

### Command line options

After completing the setup, you should be able to run
```shell
deode [opts] SUBCOMMAND [subcommand_opts]
```
where `[opts]` and `[subcommand_opts]` denote optional command line arguments
that apply, respectively, to `deode` in general and to `SUBCOMMAND`
specifically.

**Please run `deode -h` for information** about the supported subcommands
and general `deode` options. For info about specific subcommands and the
options that apply to them only, **please run `deode SUBCOMMAND -h`** (note
that the `-h` goes after the subcommand in this case).

## Examples

These examples assume that you have successfully [Set up environment](#set-up-environment) [installed](#installation) Deode-Workflow, navigated to the root level of your `deode` install directory and loaded the python environment. The examples also assume that the binaries and input data for the [ACCORD CSCs](https://www.umr-cnrm.fr/accord/?Canonical-System-Configurations-CSC) is in place. Please contact your local ACCORD members for advice if this is not the case.

### Running ecflow suite on ATOS or LUMI

The following command will run  the full suite using the default experiment:
```shell
deode case ?deode/data/config_files/configurations/cy48t3_arome -o cy48t3_arome.toml --start-suite
```

### Running the `"Forecast"` task from the `hpc-login`'s command line

The command below runs `deode`'s task `"Forecast"` using the batch system rules defined in your `config.toml`:
```shell
deode run --task Forecast --config-file cy48t3_arome.toml 
```

Note that this requires a previous run of the [ecflow suite](#running-ecflow-suite-on-atos-or-lumi) for the given config file to have finished succesfully.

This way, the stand alone forecast will pick the input data from the existing run and output the result in the same directories (as defined by the config file).

For other platforms a new config file would have to be created first. Please consult the [configure cases](misc_section_in_doc_page.rst#configure-cases) section in the documentation for more information.

