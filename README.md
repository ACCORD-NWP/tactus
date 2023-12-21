[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Prototype)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-prototype-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

# DEODE Scripting System

## About

The [DEODE Scripting System](https://github.com/destination-earth-digital-twins/Deode-Prototype/) provides a `deode` python package that runs the [Destination Earth on Demand Extremes system](https://github.com/destination-earth-digital-twins).

See the [project's documentation page](https://destination-earth-digital-twins.github.io/deode-prototype-docs) for more information.


## System Requirements

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
  module load LUMI
  ```

### Install Dependencies

* python >=3.9
* **Only for [Developers](#developer-mode-installation):**

    * [`poetry`](https://python-poetry.org). To install/reinstall and configure it, run the following commands in youe shell:
      ```shell
      # Clean eventual previous install
      curl -sSL https://install.python-poetry.org | python3 - --uninstall
      rm -rf ${HOME}/.cache/pypoetry/ ${HOME}/.local/bin/poetry ${HOME}/.local/share/pypoetry
      # Download and install poetry
      curl -sSL https://install.python-poetry.org | python3 -
      # Install needed poetry plugin(s)
      poetry self add 'poethepoet[poetry_plugin]'
      ```

### Optional System Requirements
* [`pygdal`](https://pypi.org/project/pygdal/)

  The python library [`pygdal`](https://pypi.org/project/pygdal/) is needed to use certain parts of the system, especially for climate generation. This library depends on [`gdal`](https://gdal.org/), which is notoriously troublesome as dependency when targeting many systems. The versions of `pygdal` and the system's `gdal`should match. You may try installing it using the following command:
  ```shell
  pip install pygdal=="`gdal-config --version`.*"
  ```
  **N.B.**: Do **not** run this installation command on Atos (`hpc-login`)!

  On Atos, simply run:
  ```shell
  ml gdal
  ```
  If installation is not succesful, please contact the IT support in your organisation or HPC facility.


## Installation

For the time being the recommended installation method is the [developer-mode installation](#developer-mode-installation). Before proceeding, please make sure to have followed the instructions under [system requirements](#system-requirements) and that everything correctly set up.

### Developer Mode Installation

:point_right: For those who need/wish to make changes to `deode`'s
source code, or use code from a different branch than `master`.
```shell
git clone git@github.com:destination-earth-digital-twins/Deode-Prototype.git
cd Deode-Prototype
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
  3. Create a [config file](#the-configuration-file).

### The Configuration File
Before you can use `deode` (apart from the `-h` option), you will need a configuration file written in the
[TOML](https://en.wikipedia.org/wiki/TOML) format. Please take a look at
 the default
 [config.toml](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/deode/data/config_files/config.toml) file, as well as the
 [project's Doc Page](https://destination-earth-digital-twins.github.io/deode-prototype-docs),
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

### Running ecflow suite on ATOS

The following commands will launch a run under ecflow on atos (`hpc-login.ecmwf.int`) using the default experiment:
```shell
ECF_HOST="ecflow-gen-${USER}-001"
ECF_PORT="3141"
deode start suite \
      --config-file $PWD/deode/data/config_files/config.toml
```

Then you can either use the; 
1) scheduler file, depending on whether you're using atos or lumi: deode/data/config_files/include/scheduler/ecflow_atos_bologna.toml. Scheduler file:
[ecfvars]
  ecf_files = "/home/@USER@/deode_ecflow/ecf_files"
  ecf_files_remotely = "/home/@USER@/deode_ecflow/ecf_files"
  ecf_home = "/home/@USER@/deode_ecflow/ecf_home"
  ecf_host = "ecflow-gen-@USER@-001"
  ecf_jobout = "/home/@USER@/deode_ecflow/jobout"
  ecf_port = "3141"

2) Flags. These flags are optional with filled in dummy variables. These can be changed to whatever you like:
```
      --ecf-host $ECF_HOST \
      --ecf-port $ECF_PORT \
      --joboutdir $HOME/deode_ecflow/job \
      --ecf-files $HOME/deode_ecflow/ecf
```

After this, open `ecflow_ui` and add `ecflow-gen-${USER}-001` as the server with port `3141`. The default config will place the working directory under `$SCRATCH/deode`.

### Running ecflow suite on LUMI

The following command launches ecflow on lumi (`user@lumi.csc.fi`) using the default experiment:
```shell
ECF_HOST="217.71.195.251"
ECF_PORT="8443"
deode start suite \
      --config-file $PWD/deode/data/config_files/config_CY48t3_lumi.toml \
      --ecf-host $ECF_HOST \
      --ecf-port $ECF_PORT \
      --joboutdir $HOME/deode_ecflow/job \
      --ecf-files $HOME/deode_ecflow/ecf
```

To get access to Ecflow server one must email ECMWF (samet.demir@ecmwf.int; christina.duma@ecmwf.int), ccing in ulf.andrae@smhi.se directly. All users are concatenated under one superuser: 'de330-prod'.

Only after contacting ECMWF and obtaining a file with a custom password:

export ECF_CUSTOM_PASSWD="/users/adelsaid/deode_ecflow/ecf_pwd"

adelsaid@uan01:/users/adelsaid> cat /users/adelsaid/deode_ecflow/ecf_pwd
5.11.3
de_330 217.71.195.251 8443 {PASSWORD_OBTAINED_FROM_ECMWF}

Then follow these steps to add this to your ecflow:

module load ecflow
ecflow_ui &

"Servers > Manage Servers > Add Server"

Name: de330-prod
Host: 217.71.195.251
Port: 8443
Custom user: de_330
Favourite (Not checked)
Use SSL: (Make sure this is checked!)

### Running the `"forecast"` task from the `hpc-login`'s command line

The command below runs `deode`'s task `"forecast"` using the batch system rules defined in your `config.toml`:
```shell
deode run \
      --config-file $PWD/deode/data/config_files/config.toml \
      --task Forecast \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/forecast.job \
      --output $PWD/forecast.log
```

If you have done the above mentioned default ecflow test the stand alone forecast will pick the input data from the existing run and output the result in the same directories (as defined by the config file).

### Running a stand-alone task with an example config file on LUMI

```shell
try=$((try+1)) ; \
deode run \
      --config-file $PWD/deode/data/config_files/config_CY48t3_lumi.toml \
      --task Forecast \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/forecast_try$try.job \
      --output $PWD/forecast_try$try.log
```
