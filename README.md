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

    * [`poetry`](https://python-poetry.org), which can be installed by running

            curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3

        * On Atos (`hpc-login`), please load a python module with the appropriate version **before** installing `poetry`

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

:wrench: **Recommendation to contributors:** Before making your first commit to
the repo, please also run the following:

    pre-commit install

This sets up the git hook scripts defined in the
[.pre-commit-config.yaml](.pre-commit-config.yaml) file and only needs to be run
(i) before the first commit, and (ii) after having modified the
[.pre-commit-config.yaml](.pre-commit-config.yaml) file. The
[pre-commit](https://pre-commit.com) package is installed when you run any of
the `poetry install` commands listed above.


### After Installation: Configuration File

After successful installation, a `deode` command will become available in
your environment. However, before you can use `deode` (apart from the `-h`
option), you will need a configuration file written in the
[TOML](https://en.wikipedia.org/wiki/TOML) format.

Please take a look at the
[docs/minimal_config_example.toml](docs/minimal_config_example.toml) and
[docs/more_complete_config_example.toml](docs/more_complete_config_example.toml)
files, as well as the [project's Wiki](https://source.coderefinery.org/deode/deode-prototype/-/wikis/home), for more information about the configuration file.


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

### Example with pip for hpc-login

```
#!/usr/bin/bash

# Load python
module load python3/3.8.8-01
module load ecflow

# Clone DEODE prototype in ~/projects

mkdir -p ~/projects
cd ~/projects
[ -d Deode-Prototype ] || git clone git@github.com:DEODE-NWP/Deode-Prototype.git

# Add to your ~/.bashrc
export PATH=$HOME/.local/bin:$PATH

cd ~/projects/Deode-Prototype
python3 -m pip install --upgrade pip --user
pip3 install --user -e .

# Put your config in ~/.deode/config.toml
# Minimum config
mkdir -p ~/.deode
cat > ~/.deode/config.toml << EOF
[general]
data_rootdir = "SOME_PATH"
outdir = "/tmp/deode_output"

[general.assimilation_times]
start = "2020-08-15 00:00:00+00:00"
end = "2020-08-16 21:00:00+00:00"
cycle_length = "3H"
EOF

# Find a port for your user id
USERID=`id -u`
ECF_PORT=$(( $USERID + 1500 ))
echo "ECF_PORT=$ECF_PORT"

# Start deode suite
deode -loglevel debug start suite --name test_deode --ecf_host hpc-login --ecf_port $ECF_PORT --submit $HOME/projects/Deode-Prototype/hpc-login.json --logfile $HOME/projects/Deode-Prototype/log --joboutdir $HOME/test --ecf_files $HOME/projects/Deode-Prototype/ecf
```

You can now open ecflow_ui and add hpc-login as the server with your port (value of $ECF_PORT)
