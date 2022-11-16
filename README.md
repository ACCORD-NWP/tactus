![Linting](https://github.com/DEODE-NWP/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)
![Tests](https://github.com/DEODE-NWP/Deode-Prototype/actions/workflows/test.yaml/badge.svg
) [![codecov](https://codecov.io/github/DEODE-NWP/Deode-Prototype/branch/master/graph/badge.svg?token=HMWJ69EY51)](https://codecov.io/github/DEODE-NWP/Deode-Prototype)

# DEODE Scripting System


**Table of Contents**

[[_TOC_]]


### About

`deode` is a python package that ...

See also the [project's Wiki](https://source.coderefinery.org/deode/deode-prototype/-/wikis/home) for more information.

### System Requirements

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


### Installation

Before proceeding, please make sure that your system fulfils the appropriate
[system requirements](#system-requirements). If you plan to just use the code
without modifying it, please follow one of the installation methods presented
in the [Regular Installation section](#regular-installation). However, if you
need/wish to modify the code in any way, then please proceed as indicated in the
[Developer-Mode Installtion section](#developer-mode-installation).


#### Regular Installation
##### Regular Installation from PyPi
:point_right: Easiest method if you just want to use the code and don't want to
look at the source code at all.

    pip install deode



##### Regular Installation Directly From The Git Repo

:point_right: Similar to a [regular installation from PyPi](#regular-installation-from-pypi),
but retrieves the code from the git repo instead (which is usually updated more
often).

    pip install "git+https://source.coderefinery.org/deode/deode-prototype"


##### Regular Installation From Downloaded Source

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

:wrench: **Recommendation to contributors:** Before making your first commit to
the repo, please also run the following:

    pre-commit install

This sets up the git hook scripts defined in the
[.pre-commit-config.yaml](.pre-commit-config.yaml) file and only needs to be run
(i) before the first commit, and (ii) after having modified the
[.pre-commit-config.yaml](.pre-commit-config.yaml) file. The
[pre-commit](https://pre-commit.com) package is installed when you run any of
the `poetry install` commands listed above.

#### Installation troubleshooting

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
as well as the [project's Wiki](https://source.coderefinery.org/deode/deode-prototype/-/wikis/home), for more information about the configuration file.


`deode` assumes that one of the following (whichever is first encountered)
is your configuration file :

1. A *full file path* specified via the `DEODE_CONFIG_PATH` envvar
2. A `config.toml` file located in the directory where `deode` is called
3. `$HOME/.deode/config.toml`


### Usage
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

### Example running a ecflow suite from server ecflow-gen-${USER}-001

Log into hpc-login.ecmwf.int

```
#!/usr/bin/bash

# Load python
module load python3/3.8.8-01
module load ecflow

# Clone DEODE prototype in ~/projects

mkdir -p ~/projects
cd ~/projects
[ -d Deode-Prototype ] || git clone git@github.com:DEODE-NWP/Deode-Prototype.git
cd ~/projects/Deode-Prototype

# Install deode with poetry if not done
poetry install
poetry shell

# Run experiment 
ECF_HOST=`echo ecflow-gen-${USER}-001`

deode -loglevel debug \
-config_file \
$HOME/projects/Deode-Prototype/docs/task_config_example.toml \
start suite \
--name test_deode \
--ecf_host $ECF_HOST \
--ecf_port 3141 \
--submit $HOME/projects/Deode-Prototype/ecflow-gen.json \
--logfile $HOME/test/log \
--joboutdir $HOME/test \
--ecf_files $HOME/projects/Deode-Prototype/ecf \
--start_command "ssh $ECF_HOST ecflow_start.sh -p 3141"

```

You can now open ecflow_ui and add ecflow-gen-${USER}-001 as the server with port 3141

### Example using poetry and run the forecast task from hpc-login command line

The example below shows how to run deode/task/forecast.py using the batch system rules defined in ecflow-gen.json.

```
> module load python3/3.8.8-01
> module load troika
> poetry shell
> deode -loglevel debug run --task Forecast \
 --template $PWD/ecf/stand_alone.py \
 --job $PWD/forecast.job \
 --submit background.json \
 --troika_config $PWD/config.yml \
 -o $PWD/forecast.log

```
