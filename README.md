![Linting](https://github.com/DEODE-NWP/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)
![Tests](https://github.com/DEODE-NWP/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
) [![codecov](https://codecov.io/github/DEODE-NWP/Deode-Prototype/branch/develop/graph/badge.svg?token=HMWJ69EY51)](https://codecov.io/github/DEODE-NWP/Deode-Prototype)

# DEODE Scripting System

### About

`deode` is a python package that runs the Destination Earth on Demand Extremes system.

See also the [project's Doc Page](https://github.com/DEODE-NWP/Deode-Prototype/tree/develop/docs) for more information.

## System Requirements

* python >=3.8
* **Only for
[Developer-Mode Installtion](#developer-mode-installation):**

    * [`poetry`](https://python-poetry.org), which can be installed as follows:
        * On Atos (`hpc-login`):

              module load python3/3.8.8-01
              rm -rf ~/.cache/pypoetry/
              curl -sSL https://install.python-poetry.org | python3 -

        * In other platforms, in general:

              curl -sSL https://install.python-poetry.org | python3 -


## Installation

Before proceeding, please make sure that your system fulfils the appropriate
[system requirements](#system-requirements). If you plan to just use the code
without modifying it, please follow one of the installation methods presented
in the [Regular Installation section](#regular-installation). However, if you
need/wish to modify the code in any way, then please proceed as indicated in the
[Developer-Mode Installtion section](#developer-mode-installation).


### Regular Installation
#### Regular Installation from PyPi
:point_right: Easiest method if you just want to use the code and don't want to
look at the source code at all.

    pip install deode



#### Regular Installation Directly From The Git Repo

:point_right: Similar to a [regular installation from PyPi](#regular-installation-from-pypi),
but retrieves the code from the git repo instead (which is usually updated more
often).

    pip install "git+https://github.com/DEODE-NWP/Deode-Prototype"


#### Regular Installation From Downloaded Source

:point_right: For those who have `deode`'s source code in a local directory,
wish to install it from there, but also don't want to modify any code.

    pip install .


#### Developer Mode Installation

:point_right: For those who need/wish to make changes to `deode`'s
source code, or use code from a different branch than `master`.

    poetry install

Installing in "developer mode" means that changes made in any of the package's
source files become visible as soon as the package is reloaded.

If you have problems installing `poetry`, you can install in development mode using `pip (>= v22.2.2)` as follows:

    pip install -e .


### Installation troubleshooting

If you encounter a following problem when performing installation with pip locally:

    error: https://git.ecmwf.int/scm/ecsdk/troika.git did not send all necessary objects

You might need to downgrade your pip version down to v21.2.

    pip install pip==21.2


### After Installation: Configuration File

After successful installation, a `deode` command will become available in
your environment. However, before you can use `deode` (apart from the `-h`
option), you will need a configuration file written in the
[TOML](https://en.wikipedia.org/wiki/TOML) format.

Please take a look at the
[docs/minimal_config_example.toml](docs/minimal_config_example.toml) file,
as well as the [project's Doc Page](https://github.com/DEODE-NWP/Deode-Prototype/tree/develop/docs), for more information about the configuration file.


`deode` assumes that one of the following (whichever is first encountered)
is your configuration file :

1. A *full file path* specified via the `DEODE_CONFIG_PATH` envvar or on command line using `--config_file DEODE_CONFIG_PATH`
2. A `config.toml` file located in the directory where `deode` is called
3. `$HOME/.deode/config.toml`


## Usage
After completing the setup, you should be able to run

    deode [opts] SUBCOMMAND [subcommand_opts]

where `[opts]` and `[subcommand_opts]` denote optional command line arguments
that apply, respectively, to `deode` in general and to `SUBCOMMAND`
specifically.

* When installing in development mode using `poetry`, you may need to run
`poetry shell` to be able to run `deode` as a command.

**Please run `deode -h` for information** about the supported subcommands
and general `deode` options. For info about specific subcommands and the
options that apply to them only, **please run `deode SUBCOMMAND -h`** (note
that the `-h` goes after the subcommand in this case).


## Examples
### Example running a ecflow suite from server ecflow-gen-${USER}-001

Log into hpc-login.ecmwf.int

```
# Load python and ecflow and start the poetry shell
module load python3/3.8.8-01
poetry shell


# Run experiment
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

The example below shows how to run deode/task/forecast.py using the batch system rules defined in your config.toml. The example assumes you're running the command from the Deode-Prototype root directory. 

```
> module load python3/3.8.8-01
> poetry shell
> deode run --task Forecast \
      -config_file $PWD/deode/data/config_files/config.toml \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/forecast.job \
      --ooutput $PWD/forecast.log


```
