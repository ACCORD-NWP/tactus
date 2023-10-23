![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)
![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
) [![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

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

### Running an `ecflow` suite from server `ecflow-gen-${USER}-001` on atos

The following commands will launch a run under ecflow on atos (`hpc-login.ecmwf.int`) using the default experiment:
```shell
ECF_HOST=`echo ecflow-gen-${USER}-001`
deode start suite \
      --config-file $PWD/deode/data/config_files/config.toml \
      --ecf-host $ECF_HOST \
      --ecf-port 3141 \
      --joboutdir $HOME/deode_ecflow/job \
      --ecf-files $HOME/deode_ecflow/ecf
```

After this, open `ecflow_ui` and add `ecflow-gen-${USER}-001` as the server with port `3141`. The default config will place the working directory under `$SCRATCH/deode`.


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

### Running a stand-alone task with an example config file on LUMI

```shell
deode run \
      --config-file $PWD/deode/data/config_files/config.toml \
      --task Forecast \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/test.job \
      --troika-config $PWD/deode/data/config_files/troika.yml \
      --output $PWD/test.log
```
