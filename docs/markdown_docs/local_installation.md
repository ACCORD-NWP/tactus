# Local installations

In  the following we have gathered instructions for all known platforms. If a platform is missing please add instructions.

## Adding a new host

In  the following we have gathered instructions for all known platforms. In the standard case a host/platform can be recognized either through the host name or by identifying a specific environment variable. This is configured in `deode/data/config_files/known_hosts.yml`. In the example below we see how `atos_bologna` and `lumi` are regonized via a hostname regular expression whereas `freja` is recognized from a specific environment variable. A hostname can also be forced by setting the DEODE_HOST environment variable which overrides all settings in the known_hosts.yml file.

```
atos_bologna :
  hostname : "ac\\d-\\d\\d\\d"
lumi :
  hostname : "uan\\d\\d"
freja:
  env:
   SNIC_RESOURCE: "freja"
```

Any new host should be added in the same way and the names for the configuration files for `platform`, `scheduler` and submission should be named using the given hostname.

## Setup ecflow

The ecflow server setup is defined in `deode/data/config_files/include/scheduler/ecflow_@HOST@.toml`. For your local installation you might add the proper configurations, e.g. `ecflow_freja.toml`:
```toml
[scheduler.ecfvars]
  ecf_files = "/nobackup/smhid20/users/@USER@/deode_ecflow/ecf_files"
  ecf_files_remotely = "/nobackup/smhid20/users/@USER@/deode_ecflow/ecf_files"
  ecf_home = "/nobackup/smhid20/users/@USER@/deode_ecflow/jobout"
  ecf_host = "le1"
  ecf_jobout = "/nobackup/smhid20/users/@USER@/deode_ecflow/jobout"
  ecf_out = "/nobackup/smhid20/users/@USER@/deode_ecflow/jobout"
  ecf_port = "_set_port_from_user(10000)"
  ecf_ssl = "0"
  hpc = "freja"
```

Note there are two functions available for the detection of `ecf_port` and `ecf_host` that might help to detect correct values for these two variables. `_set_port_from_user()` sets a user-id related ecf_port while `_select_host_from_list()` finds the active ecf_host from a list of possible hostnames (used in `ecflow_atos_bologna.toml`). Both functions are defined in `deode/scheduler.py`

## linda

Linda is the SMHI RedHat linux environment. In the following it's described how to install tactus to run the simple test suite with ecflow.

### Fetch and install the micromamba environment, and tactus

```
"${SHELL}" <(curl -L micro.mamba.pm/install.sh)
micromamba self-update
micromamba create -n tactus_3.10 python=3.10 ecflow poetry
micromamba activate tactus_3.10
git clone git@github.com:ACCORD-NWP/tactus.git
cd tactus
poetry install
```

### Platform dependent config files

* Rules for archiving: tactus/data/config_files/include/archiving/linda.toml
* Platform dependent paths: tactus/data/config_files/include/platform_paths/linda.toml
* Ecflow settings: tactus/data/config_files/include/scheduler/ecflow_linda.toml
* Job submission rules: tactus/data/config_files/include/submission/linda.toml

We also have to make sure the host is recognized by adding a rule in `tactus/config/known_host.yaml`

### Hack ecflow_start.sh

Here we want to resolve hostname to localhost and to force ecflow to use this we change the ecflow server start script, located by `which ecflow_start.sh` so that
```
hostname="localhost"
```

### Explaining the config

For this example we use a simple config file `tactus/data/config_files/modifications/test_ecflow.toml`. First we select a simplfied ecflow suite definition and call it `test_ecflow`

```
[general]
  case = "test_ecflow"
[suite_control]
  suite_definition = "TestSuiteDefinition"
```

This suite has four tasks
 * PrepRun: The usual preparatory work

 * CollectLogsTest: Collect the log output of PrepRun

```
[collectlogs.test]
  joboutdir = "@ECF_OUT@/@CASE@"
  tarname = "Test"
  task_logs = "@WRK@"
```

 * ArchivTest: "Archives" the result of the log collection in a new folder called duplicate


```
[archiving.test.copy.logs]
  active = true
  inpath = "@LOGS@"
  outpath = "@LOGS@/duplicate"
  pattern = "*.tar.gz"
  exclude = "*duplicate*"
```

 * PostMortem: Cleans up after the run. Here we have configured the cleaning to remove the duplicated log

```
[cleaning.PostMortem.test]
  active = true
  dry_run = false
  path = "@LOGS@/duplicate"
```

### Start the run

Now we're ready to launch the run

```
tactus case tactus/data/config_files/modifications/test_ecflow.toml --start-suite
```

Launch the ecflow viewer by

```
ecflow_ui &
```

Output will be written to
* ~/deode_wrk
* ~/deode_ecflow
* ~/ecflow_server

### Remove all results

All traces of the run can be removed by

```
tactus remove test_ecflow.toml --execute-removal
```


## freja

Freja is the SMHI research cluster operated by NSC. For more details see https://nsc.liu.se/systems/freja

### Installing under mamba

Get the code
```
git clone git@github.com:destination-earth-digital-twins/Deode-Workflow.git
cd Deode-Workflow
```

Create a conda environment and install ecflow, gdal and poetry.
```
$ module purge
$ module load Mambaforge/23.3.1-1-hpc1
$ mamba create -p .conda ecflow gdal=3.5.0 poetry python=3.10.4
...
$ mamba activate .conda/
```

Install deode and all it's dependencies

```
(deode-py3.10) $ poetry install
```

Now we're ready to go!

```
deode-py3.10) $ deode --version
2024-05-20 13:00:19 | INFO     | Start deode v0.5.0 --> "deode --version"
deode v0.5.0
mamba deactivate
```

To load your new environment do

```
$ cd Deode-Workflow
$ mamba activate .conda/
```

Note that for the time being ( until the mamba/poetry usage is better understood ) it's recommended to make this procedure, with a new mamba name, for each new deode clone.


## LEONARDO

LEONARDO is a EuroHPC cluster operated by CINECA. For more details see https://www.hpc.cineca.it/systems/hardware/leonardo/

### Preparations
 * As the ecflow server can be instantiated on any of the four login nodes available on Leonardo (login01, login02, login05, login07), we need to setup an environment variable to keep track of what node we are running on, in the current shell session.
```
export SUBMIT_HOST=$(hostname)
```
For convenience, it is suggested to add the above export line at the end of your own ~/.bashrc file.
 * Each DE330 user on LEONARDO is assigned a port number for their ecflow server. The mapping is defined in `/leonardo_work/DestE_330_25/users/SAN/leonardo_install/leonardo_users.json`. If your user is not defined here please get in contact with Matteo Ippoliti (m.ippoliti@cineca.it).

### Install and activate micromamba

On LEONARDO we install the Deode-Workflow using micromamba. Install micromamba and create the environment for the Deode-Workflow in your actual $HOME (not your user directory within the project), if you have not done so already.
```
"${SHELL}" <(curl -L micro.mamba.pm/install.sh)
micromamba self-update
micromamba create -y -p ${HOME}/micromamba-wf conda python=3.10.10 gdal=3.6.2 ecflow poetry
```
when prompted during the installation, you can confirm all the default directories and answer yes to all entries.

Activate the environment by
```
source $HOME/micromamba-wf/bin/activate
```

### Get and install the Deode-Workflow
Get the code
```
git clone git@github.com:destination-earth-digital-twins/Deode-Workflow.git
cd Deode-Workflow
```

Activate your micromamba environment as above then install deode and all it's dependencies

```
poetry install
poetry self update
```
Acitvate poetry by
```
poetry env activate
poetry shell
```
and execute the source command given.

Now we're almost ready to go! As the ecflow server should run on `login02` and can currently only be started on the node your on make sure you're login to `login02` before proceeding.

```
(deode-py3.10) (base) [uandrae0@login02 leonardo]$ deode --version
2025-03-19 15:28:20 | INFO     | Start deode v0.13.0 --> "deode --version"
deode v0.13.0
```
Continue and try to run an experiment with e.g.
```
deode case ?deode/data/config_files/configurations/cy49t2_arome --start-suite
```

### Access the ecflow server with port forwarding
The ecflow_ui can be executed on the login node of leonardo but it's faster to run the gui locally. To open up the port for ecflow_ui do locally

```
ssh YOUR_USER@SUBMIT_HOST -C -N -L PORT:SUBMIT_HOST:PORT
```
where YOUR_USER is your LEONARDOD user name and PORT is the assigned port number in the above mentioned file.

## Belenos
Belenos is the Météo-France computing cluster for research. On this platform, the Deode-Workflow can be installed using Micromamba.

### Installing under micromamba
Get the code
```
git clone git@github.com:destination-earth-digital-twins/Deode-Workflow.git
cd Deode-Workflow
```
Create a micromamba environment and install python, ecflow and gdal.
```
"${SHELL}" <(curl -L micro.mamba.pm/install.sh)
source ~/.bashrc
micromamba self-update
micromamba create -y -p ${HOME}/micromamba-wf conda python=3.10.* gdal=3.6.2 ecflow
```
Install deode and all its dependencies.
```
cd deode/Deode-Workflow/
source $HOME/micromamba-wf/bin/activate
pip install -e . --no-cache --prefer-binary
```
Then we have a setup:
```
(base) [coutandn@belenoslogin1 ~]$ deode --version
2026-03-03 14:15:47 | INFO     | Start deode v0.24.0 --> "deode --version"
deode v0.24.0
```

### Ecflow server
The ecflow server can run on any login node `belenosloginN`, where N ranges from 0 to 3. The port number is computed by `_set_port_from_user`, which adds the user's UID to an offset (default=0).
Edit the configuration file : `deode/data/config_files/include/scheduler/ecflow_belenos.toml` and set `ecf_host` to the name of your server.

```
[scheduler]

[scheduler.ecfvars]
  case_prefix = ""
  ecf_deode_home = "@DEODE_HOME@"
  ecf_files = "@HOME@/deode_ecflow/ecf_files"
  ecf_files_remotely = "@HOME@/deode_ecflow/ecf_files"
  ecf_home = "@HOME@/deode_ecflow/jobout"
  ecf_host = "belenoslogin0.belenoshpc.meteo.fr"
  ecf_jobout = "@HOME@/deode_ecflow/jobout"
  ecf_out = "@HOME@/deode_ecflow/jobout"
  ecf_port = "_set_port_from_user('0',)"
  ecf_ssl = "0"

[scheduler.ecfvars.troika]
  config_file = "@ECF_DEODE_HOME@/data/config_files/troika.yml"
```
