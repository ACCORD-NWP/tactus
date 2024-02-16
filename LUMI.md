[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Prototype)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-prototype-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

# DEODE Scripting System (LUMI)

## About

The [DEODE Scripting System](https://github.com/destination-earth-digital-twins/Deode-Prototype/) provides a `deode` python package that runs the [Destination Earth on Demand Extremes system](https://github.com/destination-earth-digital-twins).

See the [project's documentation page](https://destination-earth-digital-twins.github.io/deode-prototype-docs) for more information.

## Setting up Ecflow on LUMI


3) For securing the communications, in this server the use of ssl connection was introduced, please follow the instructions below to correctly configure it:

a. Create, if not already present, the directory .ecflowrc/ssl
$ mkdir -p $HOME/.ecflowrc/ssl

b. Rename the file server.ssl, received by e-mail, to server.crt and save it under
<YOUR_LUMI_HOME>/.ecflowrc/ssl/ path

e.g. if your home is located at /users/demirsam/ then you need to copy that file to
/users/demirsam/.ecflowrc/ssl/

4) A few more environment variables need to be defined. Please do the following exports:

$ export ECF_PORT=8443
$ export ECF_USER=de_330
$ export ECF_SSL=1

5) Once done, use the following command to check the connection:

$ ecflow_client --ping --host 217.71.195.251

If successful, you will see:
ping server(217.71.195.251:8443) succeeded in 00:00:00.238896  ~238 milliseconds

### Special setup for LUMI-Ecflow interfacing (Do not proceed to other steps til this is complete)


1. To get access to Ecflow server from LUMI, email ECMWF (cristina.duma@ecmwf.int with samet.demir@ecmwf.int; bojan.kasic@ecmwf.int in cc), providing them with: 
a) Your **LUMI username**.
b) Your **LUMI public key**.

Only after contacting ECMWF can you proceed. They will give you the following:

a) id_rsa_troika.pub - public key file for troika
b) server.ssl - an ssl certificate file
c) 217.71.195.251.ecf.custom_password - ecf custom password file


You can put these in the following locations:

a) This file will be mapped to: /users/lrb_465000527_efprd/. (Ask a super-user for more info on this)
b) /users/adelsaid/.ecflowrc/ssl/server.crt
c) /users/adelsaid/deode_ecflow/217.71.195.251.ecf.custom_password

The custom password file should look like this:

```shell
export ECF_CUSTOM_PASSWD="/users/adelsaid/deode_ecflow/ecf_pwd"

adelsaid@uan01:/users/adelsaid> cat /users/adelsaid/deode_ecflow/ecf_pwd

5.11.3
de_330 217.71.195.251 8443 {PASSWORD_OBTAINED_FROM_ECMWF}
```

Only once these steps are completed can you move onto the next steps.

### Running ecflow on LUMI

To bring up the Ecflow User Interface:

```shell
module load ecflow
ecflow_ui &
```

Then, on the ecflow_ui itself, go to: "Servers > Manage Servers > Add Server" and make sure you set the following:

```
Name: de330-prod
Host: 217.71.195.251
Port: 8443
Custom user: de_330

And make sure you check these flags:
"Use SSL”:  (check)
"Add server to current view":  (check)
```

### Running ecflow on LUMI (after setup only)

Logging into LUMI:

`ssh -X adelsaid@lumi.csc.fi`

make sure you have set:

```shell
ECF_HOST="217.71.195.251"
ECF_PORT="8443"
ECF_CUSTOM_PASSWD="/users/USER/deode_ecflow/ecf_pwd"
ECF_SSL=1
ECF_USER="de_330"
```

then the following command launches DEODE on LUMI

```shell
deode start suite
```

This will start the suite, and default to `--config-file $PWD/deode/data/config_files/config.toml`. You can change this by calling it as follows:

```shell
deode start suite --config-file $PWD/deode/data/config_files/config_CY48t3_lumi.toml
```

The ecflow_scheduler has variables living inside `deode/data/config_files/include/scheduler/ecflow_lumi.toml` as follows:

```toml
[ecfvars]
  ecf_files = "/users/@USER@/deode_ecflow/ecf_files"
  ecf_files_remotely = "/home/ecflow-user/deode_ecflow/ecf_files"
  ecf_home = "/home/ecflow/deode_ecflow/ecf_files"
  ecf_host = "217.71.195.251"
  ecf_jobout = "/users/adelsaid/deode_ecflow/jobout"
  ecf_out = "/scratch/project_465000527/adelsaid/deode_ecflow/ecf_files/"
  ecf_port = "8443"
  ecf_ssl = "1"
  ecf_user = "de_330"
  ecf_remoteuser = "ecflow-user"
```

Which can be overriden within the scheduler file itself or by calling the optional flags from command line 
```shell
      deode start suite \
      --ecf-host $ECF_HOST \
      --ecf-port $ECF_PORT \
      --ecf-user $ECF_USER \
      --ecf-remoteuser $ECF_RUSER \ (for lumi)
      --ecf-home $ECF_HOME \
      --ecf-files $ECF_FILES \
      --ecf-files-remotely $ECF_FILES_REMOTELY \ (for lumi)
      --joboutdir $HOME/deode_ecflow/job \
      --ecf-files $HOME/deode_ecflow/ecf
```

The additional flags specific to LUMI can also be utilised (above) in this fashion. However, the default values should suffice.

## LUMI - Standalone task example (forecast)

```shell
deode run \
      --config-file $PWD/deode/data/config_files/config_CY48t3_lumi.toml \
      --task Forecast \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/forecast.job \
      --output $PWD/forecast.log
```

### Contacts

support@lumi-supercomputer.eu 
Henrik Nortamo: henrik.nortamo@csc.fi

