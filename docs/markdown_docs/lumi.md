[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Prototype)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-prototype-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

## About
The [DEODE Scripting System](https://github.com/destination-earth-digital-twins/Deode-Prototype/) provides a `deode` python package that runs the [Destination Earth on Demand Extremes system](https://github.com/destination-earth-digital-twins).

See the [project's documentation page](https://destination-earth-digital-twins.github.io/deode-prototype-docs) for more information.

## LUMI 
### Getting on LUMI
To run the project on LUMI, you need to have a LUMI account and add your SSH key to it. Please check the [online instructions on how to do that](https://docs.lumi-supercomputer.eu/firststeps/SSH-keys/). Once you have your SSH key added, you can login to LUMI by running `ssh -i <your-private-key> <username>@@lumi.csc.fi`. Once logged in, create a SSH key and add that to your GitHub account so you can clone the repo. Then, please follow the download, install and usage instructions provided in the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file.

### Ecflow:  Initial setup

** Do not proceed to other steps til this is complete **

1. First step is to email ECMWF (cristina.duma@ecmwf.int with samet.demir@ecmwf.int; bojan.kasic@ecmwf.int in cc), providing them with: 

a) Your **LUMI username**.

b) Your **LUMI public key** (usually under: /users/USERNAME/.ssh/id_rsa.pub)

And ask to be 'whitelisted' to be granted access to the Ecflow server from LUMI.

2. You need to copy the following **encrypted** files from this location: (/pfs/lustrep4/projappl/project_465000527/shared/LUMI_ECFLOW/)

a) server.ssl.gpg - an ssl certificate file

b) 217.71.195.251.8443.ecf.custom_password.gpg - ecf custom password file

c) id_rsa_troika.pub.gpg - public key for troika

3. Decrypt them with the following command:
```shell
gpg filename
```
using the **password: 'peterpiper'**.

4. It is recommend that you then put these files in the following locations:

a) /users/USERNAME/.ecflowrc/ssl/server.crt

b) /users/USERNAME/ecflow_server/217.71.195.251.8443.ecf.custom_password

**Note** The public key: 'id_rsa_troika.pub' needs to be uploaded to my.csc.fi or myaccessid provider.

5. Then map custom password and SSL variable as follows:

```shell
export ECF_CUSTOM_PASSWD="/users/USERNAME/ecflow_server/217.71.195.251.8443.ecf.custom_password"
export ECF_SSL=1
```

Which should look like this:
```shell
adelsaid@uan01:/users/adelsaid> cat /users/USERNAME/ecflow_server/217.71.195.251.8443.ecf.custom_password
5.11.3
de_330 217.71.195.251 8443 {PASSWORD_OBTAINED_FROM_ECMWF}
```
Note: For now, the above file is the **same for every user**.

### Testing Ecflow server connection

Only once these steps are completed, test your connection by pinging:
```shell
ecflow_client --ping --host 217.71.195.251
```
If there are any obvious errors, rechart your steps by starting again. 

Another litmus test that is sufficient but not necessary to Ecflow working well is logging into the ecflow server:
```shell
ssh ecflow-user@217.71.195.251
```

Only once you have a working connection should you attempt to setup Ecflow UI below.

### Setting up Ecflow UI

1. To bring up the Ecflow User Interface issue the following 'module load' (ml) commands:

```shell
ml use /scratch/project_465000527/SW/modules
ml ecflow
ecflow_ui &
```

2. On ecflow_ui itself, go to: "Servers > Manage Servers > Add Server" and set the following:

```
Name: de330-prod
Host: 217.71.195.251
Port: 8443
Custom user: de_330

And make sure you check these flags:
"Use SSL”:  (check)
"Add server to current view":  (check)
```

### Troubleshooting

3. If your server (de330) is gray in colour, that means something has failed. To get an idea of the problem, click on 'Panel' then 'Add info panel' which will divide the ecflow_ui screen and give you a message.
   
4. If you make any changes after solving issues with your Ecflow functionality, you will need to reset ecflow_ui. To do this you must close it, and reissue the command `ecflow_ui &`

### Running ecflow on LUMI (after setup)

1. Logging into LUMI:

```shell
ssh -X adelsaid@lumi.csc.fi
```
(use the -A flag if you have any problems with the -X flag above).

2. Ensure you:
    
    a) Have correct environment:
    ```shell
    ml use /scratch/project_465000527/SW/modules`
    ml cray-python/3.10.10
    ml ecflow
    ```
    b) ECF variables as in step 6 above. Then: `ecflow_ui &`
    
    c) Ensure you are in the correct folder ($DEODE) where you did `git clone`
    
    d) `Poetry install`
    
    e) `Poetry shell`

3. Then the following command launches DEODE on LUMI
```shell
deode start suite --config-file $PWD/deode/data/config_files/config_CY48t3_lumi.toml
```

Note: if you forget to use the --config-file flag, it will default to `--config-file $PWD/deode/data/config_files/config.toml` most likely causing errors.

3. (FYI) The ecflow_scheduler variables live inside `deode/data/config_files/include/scheduler/ecflow_lumi.toml`:

```toml
[ecfvars]
  ecf_files = "/users/@USER@/deode_ecflow/ecf_files"
  ecf_files_remotely = "/home/ecflow-user/deode_ecflow/ecf_files"
  ecf_home = "/home/ecflow/deode_ecflow/ecf_files"
  ecf_host = "217.71.195.251"
  ecf_jobout = "/users/@USER@/deode_ecflow/jobout"
  ecf_out = "@SCRATCH@/deode_ecflow/ecf_files/"
  hpc = "lumi"
```

which are set to default values. These can be changed in the file as required.

### Standalone task example (forecast)

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

henrik.nortamo@csc.fi

