# Local installations

In  the following we have gathered instructions for all known platforms. If a platform is missing please add instructions.

## Adding a new host

In  the following we have gathered instructions for all known platforms. In the standard case a host/platform can be recognized either through the host name or by identifying a specific environment variable. This is configured in `tactus/data/config_files/known_hosts.yml`. In the example below we see how `atos_bologna` and `lumi` are regonized via a hostname regular expression whereas `freja` is recognized from a specific environment variable. A hostname can also be forced by setting the TACTUS_HOST environment variable which overrides all settings in the known_hosts.yml file.

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

The ecflow server setup is defined in `tactus/data/config_files/include/scheduler/ecflow_@HOST@.toml`. For your local installation you might add the proper configurations, e.g. `ecflow_freja.toml`:
```toml
[scheduler.ecfvars]
  ecf_files = "/nobackup/smhid20/users/@USER@/tactus_ecflow/ecf_files"
  ecf_files_remotely = "/nobackup/smhid20/users/@USER@/tactus_ecflow/ecf_files"
  ecf_home = "/nobackup/smhid20/users/@USER@/tactus_ecflow/jobout"
  ecf_host = "le1"
  ecf_jobout = "/nobackup/smhid20/users/@USER@/tactus_ecflow/jobout"
  ecf_out = "/nobackup/smhid20/users/@USER@/tactus_ecflow/jobout"
  ecf_port = "_set_port_from_user(10000)"
  ecf_ssl = "0"
  hpc = "freja"
```

Note there are two functions available for the detection of `ecf_port` and `ecf_host` that might help to detect correct values for these two variables. `_set_port_from_user()` sets a user-id related ecf_port while `_select_host_from_list()` finds the active ecf_host from a list of possible hostnames (used in `ecflow_atos_bologna.toml`). Both functions are defined in `tactus/scheduler.py`

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
* Job submission rules: tactus/data/config_files/include/submission/linda.toml. Here all jobs are running in the background.

We also have to make sure the host is recognized by adding a rule in `tactus/config/known_host.yaml`

## freja

Freja is the SMHI research cluster operated by NSC. For more details see https://nsc.liu.se/systems/freja

### Installing under mamba

Get the code
```
git clone git@github.com:ACCORD-NWP/tactus.git
cd tactus
```

Create a conda environment and install ecflow, gdal and poetry.
```
$ module purge
$ module load Mambaforge/23.3.1-1-hpc1
$ mamba create -p .conda ecflow gdal=3.5.0 poetry python=3.10.4
...
$ mamba activate .conda/
```

Install tactus and all it's dependencies

```
(tactus-py3.10) $ poetry install
```

Now we're ready to go!

```
tactus-py3.10) $ tactus --version
2024-05-20 13:00:19 | INFO     | Start tactus v0.5.0 --> "tactus --version"
tactus v0.5.0
mamba deactivate
```

To load your new environment do

```
$ cd tactus
$ mamba activate .conda/
```

Note that for the time being ( until the mamba/poetry usage is better understood ) it's recommended to make this procedure, with a new mamba name, for each new tactus clone.


## Belenos
Belenos is the Météo-France computing cluster for research. On this platform, the tactus can be installed using Micromamba.

### Installing under micromamba
Get the code
```
git clone git@github.com:ACCORD-NWP/tactus.git
cd tactus
```
Create a micromamba environment and install python, ecflow and gdal.
```
"${SHELL}" <(curl -L micro.mamba.pm/install.sh)
source ~/.bashrc
micromamba self-update
micromamba create -y -p ${HOME}/micromamba-wf conda python=3.10.* gdal=3.6.2 ecflow
```
Install tactus and all its dependencies.
```
cd tactus/tactus/
source $HOME/micromamba-wf/bin/activate
pip install -e . --no-cache --prefer-binary
```
Then we have a setup:
```
(base) [coutandn@belenoslogin1 ~]$ tactus --version
2026-03-03 14:15:47 | INFO     | Start tactus v0.24.0 --> "tactus --version"
tactus v0.24.0
```

### Ecflow server
The ecflow server can run on any login node `belenosloginN`, where N ranges from 0 to 3. The port number is computed by `_set_port_from_user`, which adds the user's UID to an offset (default=0).
Edit the configuration file : `tactus/data/config_files/include/scheduler/ecflow_belenos.toml` and set `ecf_host` to the name of your server.

```
[scheduler]

[scheduler.ecfvars]
  case_prefix = ""
  ecf_tactus_home = "@TACTUS_HOME@"
  ecf_files = "@HOME@/tactus_ecflow/ecf_files"
  ecf_files_remotely = "@HOME@/tactus_ecflow/ecf_files"
  ecf_home = "@HOME@/tactus_ecflow/jobout"
  ecf_host = "belenoslogin0.belenoshpc.meteo.fr"
  ecf_jobout = "@HOME@/tactus_ecflow/jobout"
  ecf_out = "@HOME@/tactus_ecflow/jobout"
  ecf_port = "_set_port_from_user('0',)"
  ecf_ssl = "0"

[scheduler.ecfvars.troika]
  config_file = "@ECF_TACTUS_HOME@/data/config_files/troika.yml"
```
