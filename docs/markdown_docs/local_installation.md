[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Prototype)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-prototype-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

# Local installations

In  the following we have gathered instructions for all known platforms. If a platform is missing please add instructions.

## Adding a new host

In  the following we have gathered instructions for all known platforms. A host/platform can be recognized either through the host name or by identifying a specific environment variable. This is configured in `deode/data/config_files/known_hosts.yml`. In the example below we see how `atos_bologna` and `lumi` are regonized via a hostname regular expression whereas `freja` is recognized from a specfici environment variable.

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


## freja 

Freja is the SMHI research cluster operated by NSC. For more details see https://nsc.liu.se/systems/freja

### Installing under mamba

Get the code
```
git clone git@github.com:destination-earth-digital-twins/Deode-Prototype.git 
cd Deode-Prototype
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
$ cd Deode-Prototype
$ mamba activate .conda/
``` 

Note that for the time being ( until the mamba/poetry usage is better understood ) it's recommended to make this procedure, with a new mamba name, for each new deode clone.


### Contact

Have a chat with Ulf Andrae, FoU-Met
