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


## System Requirements

**Make sure you have python>=3.10**

### Prepare your environment on the HPC machines
<a name="#put-poetry-in-path"></a> Start by putting the `$HOME/.local/bin`
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

### Install Dependencies

* [`poetry`](https://python-poetry.org)

  To install/reinstall and configure it, run the following commands in your shell:
  ```shell
  # Clean eventual previous install
  curl -sSL https://install.python-poetry.org | python3 - --uninstall
  rm -rf ${HOME}/.cache/pypoetry/ ${HOME}/.local/bin/poetry ${HOME}/.local/share/pypoetry
  # Download and install poetry
  curl -sSL https://install.python-poetry.org | python3 -
  ```

### Optional System Requirements
* [`pygdal`](https://pypi.org/project/pygdal/)

  The python library [`pygdal`](https://pypi.org/project/pygdal/) is needed to use certain parts of the system, especially for climate generation. This library depends on [`gdal`](https://gdal.org/), which is notoriously troublesome as dependency when targeting many systems. The versions of `pygdal` and the system's `gdal`should match. 
  
  On Atos, if you have issues with gdal, simply run:
  ```shell
  ml gdal
  ```
  If installation is not succesful, please contact the IT support in your organisation or HPC facility.


## Installation

For the time being the recommended installation method is the [developer-mode installation](#developer-mode-installation). Before proceeding, please make sure to have followed the instructions under [system requirements](#system-requirements) and that everything correctly set up.

### Developer Mode Installation

This is for those who need/wish to make changes to `deode`'s
source code, or use code from a different branch than `master` and it's currently the recommended way to use `deode`.
```shell
git clone git@github.com:destination-earth-digital-twins/Deode-Workflow.git
cd Deode-Workflow
poetry install
```

This will will install `deode` and its dependencies in an isolated virtual environment located inside the package's source directory.

Installing in developer mode means that changes made in any of the package's source files become visible as soon as the package is reloaded.


## Usage

### Running `deode` after [Developer-Mode Install](#developer-mode-installation)
  1. Activate the created virtual environment

      After having [prepared your environment](#prepare-your-environment-on-the-hpc-machines) as described in the [system requirements](#system-requirements) section, navigate to the root level of the package's install directory and run:

      ```shell
      poetry shell
      ```

      Alternatively, to activate a `deode` install located in an arbitrary
      directory `MY_DEODE_SOURCE_DIRECTORY`, please run:

      ```shell
      poetry shell --directory=MY_DEODE_SOURCE_DIRECTORY
      ```
  2. Test that `deode` works by running:
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

These examples assume that you have successfully [initialised your environment](#prepare-your-environment-on-the-hpc-machines) and [installed `deode`](#installation). They should be run from the root level of your `deode` install directory. The examples also assume that the necessary
input data is in place.

### Running ecflow suite on ATOS or LUMI

The following command will run  the full suite using the default experiment:
```shell
deode case ?deode/data/config_files/configurations/cy48t3_arome -o cy48t3_arome.toml --start-suite
```


### Running the `"Forecast"` task from the `hpc-login`'s command line

Given that input data is prepared and in the right location any task visible in the ecflow suite can be executed outside of ecflow. The command below runs the task `"Forecast"` using the batch system rules defined in your `cy48t3_config.toml`:
```shell
deode run --task Forecast --config-file cy48t3_config.toml 
```
The generated job and logfiles will appear in the current directory.

For other platforms a new config file would have to be created first. Please consult the [configure cases](misc_section_in_doc_page.rst#configure-cases) section in the documentation for more information.
