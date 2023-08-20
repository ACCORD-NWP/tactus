![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)
![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
) [![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=HMWJ69EY51)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

# DEODE Scripting System

### About

`deode` is a python package that runs the Destination Earth on Demand Extremes system.

See also the [project's Doc Page](https://destination-earth-digital-twins.github.io/deode-prototype-docs) for more information.

## System Requirements

* python >=3.8
* **For
[Developer-Mode Installation](#developer-mode-installation):**

    * [`poetry`](https://python-poetry.org), which can be installed as follows:
        * On Atos (`hpc-login`):

              module load python3/3.8.8-01
              rm -rf ~/.cache/pypoetry/
              curl -sSL https://install.python-poetry.org | python3 -

        * In other platforms, in general:

              curl -sSL https://install.python-poetry.org | python3 -

#### Optional System Requirements
* pygdal

To use certain parts of the system, especially for climate generation, the python package [pygdal](https://pypi.org/project/pygdal/) is needed, which again depends on [gdal](https://gdal.org/) which is notoriously troublesome as dependency when targeting many systems. pygdal should match the gdal version on the system:
```shell
pip install pygdal=="`gdal-config --version`.*"
```

On Atos (`hpc-login`) no special treatment of gdal is required.

## Installation

Before proceeding, please make sure that your system fulfils the appropriate
[system requirements](#system-requirements). For the time being the recommended installation method is the [Developer-Mode Installation](#developer-mode-installation) using poetry.

#### Developer Mode Installation

:point_right: For those who need/wish to make changes to `deode`'s
source code, or use code from a different branch than `master`.

    git clone git@github.com:destination-earth-digital-twins/Deode-Prototype.git
    cd Deode-Prototype
    export PATH=~/.local/bin:$PATH
    poetry install

Installing in "developer mode" means that changes made in any of the package's
source files become visible as soon as the package is reloaded.

## Usage

### The Configuration File

After successful installation, the `deode` command will become available in the poetry virtual environment. Load the virtual environment by
```
poetry shell
```
to make the `deode`command available. However, before you can use `deode` (apart from the `-h` option), you will need a configuration file written in the
[TOML](https://en.wikipedia.org/wiki/TOML) format.

Please take a look at the default
[config.toml](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/deode/data/config_files/config.toml) file,
as well as the [project's Doc Page](https://destination-earth-digital-twins.github.io/deode-prototype-docs), for more information about the configuration file.

### Command line options

After completing the setup, you should be able to run

    deode [opts] SUBCOMMAND [subcommand_opts]

where `[opts]` and `[subcommand_opts]` denote optional command line arguments
that apply, respectively, to `deode` in general and to `SUBCOMMAND`
specifically.

**Please run `deode -h` for information** about the supported subcommands
and general `deode` options. For info about specific subcommands and the
options that apply to them only, **please run `deode SUBCOMMAND -h`** (note
that the `-h` goes after the subcommand in this case).


## Examples

### Example running a ecflow suite from server ecflow-gen-${USER}-001 on atos

Assuming you have successfully installed deode the following will launch a run under ecflow.
Log into hpc-login.ecmwf.int, load python and ecflow and start the poetry shell

```
module load python3/3.8.8-01
module load ecflow
export PATH=~/.local/bin:$PATH
poetry shell
```

Now you can start the default experiment by

```
ECF_HOST=`echo ecflow-gen-${USER}-001`

deode -config_file $PWD/deode/data/config_files/config.toml \
      start suite \
      --ecf_host $ECF_HOST \
      --ecf_port 3141 \
      --joboutdir $HOME/deode_ecflow/job \
      --ecf_files $HOME/deode_ecflow/ecf
```


You can now open ecflow_ui and add ecflow-gen-${USER}-001 as the server with port 3141. The default config file will locate the working directory under `$SCRATCH/deode`


### Example using poetry and run the forecast task from hpc-login command line

The example below shows how to run deode/task/forecast.py using the batch system rules defined in your config.toml. The example assumes you're running the command from the Deode-Prototype root directory and have the appropriate input data in place.

```
module load python3/3.8.8-01
poetry shell
deode -config_file $PWD/deode/data/config_files/config.toml \
      run --task Forecast \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/forecast.job \
      --output $PWD/forecast.log
```
