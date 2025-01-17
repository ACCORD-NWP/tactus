[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Workflow)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-workflow-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Workflow/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Workflow)

# LUMI 

## Introduction

Two ecflow servers are provided on LUMI:

1. de330-dev: development server at 217.71.194.208 (**normal use**) 
2. de330-prod : production server at 217.71.194.199 (**production, authorized users only**) 

We provide on this page a step-by-step instructions guide for installing Deode-Workflow and communicate with these ecflow servers on LUMI.


## Step 1. Getting on LUMI

### 1.1 SSH to LUMI
To access LUMI, you need to have a LUMI account, and login via SSH. 
Please follow the [online instructions from lumi](https://docs.lumi-supercomputer.eu/firststeps/SSH-keys/). 
Once you have your SSH key added, you can login to LUMI by running:

```
 ssh -i <your-private-key> -X <username>@lumi.csc.fi
 ```


### 1.2 Setup Github access

Once logged in, it is usefull to configure the Github access.
Create a new SSH key pair, and add that to your GitHub account so you can clone the Deode repository.
Please follow the [online instructions from Github](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account)

## Step 2. Request access to ecflow server

### 2.1 Generate a new SSH keys pair without passphrase, using the RSA protocol

We will create a key with default name for RSA by using the following command:

```shell
 ssh-keygen -t rsa 
 ```
Hit Enter when a password is requested to creeate a key without passphrase.

 This will create private key ~/.ssh/id_rsa  and  the public key ~/.ssh/id_rsa.pub.

### 2.2 Send the public key to ECMWF

Send an email to <cristina.duma@ecmwf.int> (with <samet.demir@ecmwf.int>, <bojan.kasic@ecmwf.int> and <ulf.andrae@smhi.se> in cc), providing them with: 

-  Your **LUMI username** (<user name> in the rest of this document).

-  The public key **id_rsa.pub** (usually under: /users/<user name>/.ssh/id_rsa.pub)

-  Ask to be 'whitelisted' to be granted access to the **dev ecflow server for LUMI** identified by the IP address:  **217.71.194.208**.


### 2.3 Verification step: ssh ecflow servers

When the access is granted, you should be able to connect to the ecflow server using ssh from the terminal:

-  Dev server
```shell
ssh ecflow-user@217.71.194.208
Linux de330-ecflow-dev 5.10.0-28-cloud-amd64 #1 SMP Debian 5.10.209-2 (2024-01-31) x86_64

The programs included with the Debian GNU/Linux system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Debian GNU/Linux comes with ABSOLUTELY NO WARRANTY, to the extent
permitted by applicable law.
Last login: Wed Apr 24 12:48:29 2024 from 193.167.209.165
```

-  Production server (optional)
```shell
ssh ecflow-user@217.71.194.199
```

## Step 3. Configure access to ecflow server from ecflow_client

### 3.1 Retrieve the encrypted ecflow certificates

 You need to copy the following **encrypted** files from this location (/pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/) into a temp directory.


- SSL certificates
    - 217.71.194.208.8443.crt.gpg (prod server)
    - 217.71.194.199.8443.crt.gpg (dev server)

- ecf custom password file (for prod and dev servers)
    - ecf.custom_passwd.gpg 

-  troika public keys (for both servers, both are needed)
    - id_rsa_troika_DEV.pub.gpg 
    - id_rsa_troika_PROD.pub.gpg 

```shell
mkdir tmp; cd tmp
cp /pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/217.71.194.208.8443.crt.gpg .
cp /pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/217.71.194.199.8443.crt.gpg .
cp /pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/ecf.custom_passwd.gpg .
cp /pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/id_rsa_troika_DEV.pub.gpg .
cp /pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/id_rsa_troika_PROD.pub.gpg .
```

**Note: ecf.custom_passwd is the same for every user**.


### 3.2 Decode the encrypted files:

Use the gpg command using the **password: peterpiper**.
```shell
cd tmp
gpg 217.71.194.208.8443.crt.gpg 
gpg 217.71.194.199.8443.crt.gpg
gpg ecf.custom_passwd.gpg
gpg id_rsa_troika_DEV.pub.gpg
gpg id_rsa_troika_PROD.pub.gpg 
```

### 3.3 Copy decrypted files to their current location:


- SSL certificates to `/users/<user name>/.ecflowrc/ssl/`

- Password file to `/users/<user name>/.ecflowrc/`

- Troika public keys uploaded to `my.csc.fi` (or myaccessid)


```shell
mkdir -p '${HOME}/.ecflowrc/ssl'
cp 217.71.194.208.8443.crt ${HOME}/.ecflowrc/ssl/
cp 217.71.194.199.8443.crt ${HOME}/.ecflowrc/ssl/
cp ecf.custom_passwd ${HOME}/ecf.custom_passwd
```

The files id_rsa_troika_PROD.pub and id_rsa_troika_DEV.pub need to be uploaded to my.csc.fi or myaccessid provider separately. Once this is done, the files themselves aren't needed.


### 3.4 Create soft links for **both** certificate files:

```shell
ln -sf ~/.ecflowrc/ssl/217.71.194.199.8443.crt ~/.ecflowrc/ssl/de330-ecflow-prod.8443.crt
ln -sf ~/.ecflowrc/ssl/217.71.194.208.8443.crt ~/.ecflowrc/ssl/de330-ecflow-dev.8443.crt
```

### 3.5 Define the following variables:

```shell
export ECF_CUSTOM_PASSWD="/users/<user name>/.ecflowrc/ecf.custom_passwd"
export ECF_HOST=217.71.194.208
export ECF_PORT=8443
export ECF_USER=de_330
export ECF_SSL=1
```

For convenience, you can insert these line in you ~/.bashrc file.
In that case, make sure to source your ~/.bashrc (or re-login) before going furter, to ensure these variables are correctly defined.

### 3.6 Initialize the modules

This step needs to be done each time you want to start ecflow server
```
ml use /scratch/project_465000527/jasinskas/scl/modules/
ml cray-python/3.10.10
ml scl-ecflow_23
```

### 3.7 Verification test: ping the ecflow server

At this point, you should be able to perform a ping to the different ecflow servers from the ecflow_client:

- Development server
```shell
ecflow_client --ping --host 217.71.194.208 --port 8443
ping server(217.71.194.208:8443) succeeded in 00:00:00.147990  ~147 milliseconds
```
- Production server (optional)
```shell
ecflow_client --ping --host 217.71.195.251 --port 8443
ping server(217.71.195.251:8443) succeeded in 00:00:00.171897  ~171 milliseconds
```

**Note: Only once you have a working connection should you attempt to setup the Ecflow User Interface below.**

## Step 4. Configure  Ecflow UI

### 4.1 Start ecflow_ui

Bring up the Ecflow User Interface by calling ecflow_ui (after loading the correct modules if not already done in previous step):

```shell
ml use /scratch/project_465000527/jasinskas/scl/modules/
ml cray-python/3.10.10
ml scl-ecflow_23
ecflow_ui &
```

### 4.2 Add the servers in Ecflow UI

In ecflow_ui, go to: "Servers > Manage Servers > Add Server" and enter the following information:

- Dev server

```
Name: de330-dev
Host: 217.71.194.208
Port: 8443
Custom user: de_330

And make sure you check these flags:
"Use SSL”:  (check)
"Add server to current view":  (check)
```

- Production server (optional)

```
Name: de330-prod
Host: 217.71.194.199
Port: 8443
Custom user: de_330

And make sure you check these flags:
"Use SSL”:  (check)
"Add server to current view":  (check)
```

## Step 5. Setup Deode

Please follow the download, install and usage instructions provided in the [Deode-Prototype installation instructions](https://github.com/destination-earth-digital-twins/Deode-Workflow/blob/develop/README.md).

In short:
```
# Reinstall poetry (optional)
curl -sSL https://install.python-poetry.org | python3 - --uninstall
rm -rf ${HOME}/.cache/pypoetry/ ${HOME}/.local/bin/poetry ${HOME}/.local/share/pypoetry
# Download and install poetry
curl -sSL https://install.python-poetry.org | python3 -


# Checkout Deode
git clone git@github.com:destination-earth-digital-twins/Deode-Workflow.git
cd Deode-Workflow
# install dependencies
poetry install
```

## Step 6. Running Deode


### Running a complete suite

The following commands prepare the environment (execute them one time before starting to use Deode Workflow)

```shell
# Do these steps only once
poetry shell 
ml use /scratch/project_465000527/jasinskas/scl/modules/
ml pyeccodes_23
ml scl-ecflow_23
```
The following command slaunches DEODE suite, using AROME, on LUMI

```
deode case ?deode/data/config_files/configurations/cy48t3_arome -o cy48t3_arome.toml --start-suite
```

### Standalone task example (forecast)

To run a standalone "Forecast" task, first you need to change the 'SCHOST' field from "lumi-batch" to "lumi" under [parallel], as shown below:

```shell
[parallel]
  NPROC = 16
  SCHOST = "lumi"
  tasks = ["Forecast", "e927", "Pgd", "Prep", "c903"]
  WRAPPER = "srun"

```

in: deode/data/config_files/include/submission/lumi_CY48t3.toml.

This also applies to any task one wishes to run standalone. Then simply run the following comment below:

```shell
deode run --config-file config_CY48t3_lumi.toml --task Forecast
```


## Configuration 

### Ecflow server selection

By default, all users will use the **dev server**.

Every user can choose to run on the de330-dev or de330-prod ecflow servers. To do this, you only need to change the ecf_host variable in deode/data/config_files/include/scheduler/ecflow_lumi.toml:

```
[ecfvars]
  ecf_host = "217.71.194.208"
```

As of today, two ecflow servers are integrated with LUMI: 
- Development Server: 217.71.194.208 (**normal use**) 
- Production Server: 217.71.194.199 (**production only**)

**Note: Please do not use the production server unless authorised to do so**

**Should you require access to the prod server, you must be whitelisted by requesting permission from the ECMWF with someone senior in cc (Ulf Andrae - SMHI or Xiaohua Yang - DMI) . This is detailed in step 2 above**


### Ecflow scheduler

 The ecflow scheduler variables live inside: 
```shell
deode/data/config_files/include/scheduler/ecflow_lumi.toml
```
which look like:
```toml
[ecfvars]
  ecf_files = "/users/@USER@/deode_ecflow/ecf_files"
  ecf_files_remotely = "/home/ecflow-user/deode_ecflow/ecf_files"
  ecf_home = "/home/ecflow/deode_ecflow/ecf_files"
  ecf_host = "217.71.194.208"
  ecf_jobout = "/users/@USER@/deode_ecflow/jobout"
  ecf_out = "@SCRATCH@/deode_ecflow/ecf_files/"
  hpc = "lumi"
```
and are preset to default values. These can be changed in the file as required.

### Data

At the time of writing this, there is no streamlined way to obtain data from sources such as MARS etc, as it is with ATOS. We currently have a shared local directory on LUMI for this project:
```shell
/scratch/project_465000527/de_33050_common_data/
```
spend a few minutes just familiarising yourself with what's there. 

IFS data for the boundaries (HRES and ATOS_DT) is currently stored here:
```shell
/scratch/project_465000527/de_33050_common_data/deode_ref/IFS
```

Should you require any extra data you will need to manually download this from ATOS yourself:
```shell
scp -r /home/snh02/work/dev-CY46h1_deode/climate/DEODE_LARGE adelsaid@lumi.csc.fi:/scratch/project_465000527/adelsaid/
```
for example.

If you foresee that the data you download will be needed in the long term, contact a super user (Ulf Andrae, Denis Haumont, Trygve Aspelien) to assist you in getting it into the common area mentioned above, where it should be safer in the long term.



### Production user (lrb_465000527_efprd)

There is a dedicated production user on LUMI. To access this, follow the steps below:

1. Email ulf.andrae@smhi.se to ask for access to the production user. Without this step, you cannot gain access to the production user. 
2. You need to **add** your public RSA (or otherwise) key to my.csc.fi 
3. At KNMI we also use **myaccessid** : https://mms.myaccessid.org/fed-apps/profile/settings_sshkeys - if you have another equivalent to this please also ensure your ssh public key is added there and that your my.csc.fi account is linked to it (separate step).
4. Wait an hour or two before attempting to connect: 
```shell
ssh lrb_465000527_efprd@lumi.csc.fi
```
5. To get started, as you would on your own user:
```shell
 cd $DEODE
 poetry shell
 deode case ?deode/data/config_files/configurations/cy48t3_arome -o cy48t3_arome.toml --start-suite
```

6. If you have any further issues please email ulf.andrae@smhi.se or servicedesk@csc.fi.


## Troubleshooting


### 1. If you can not perform an ssh to the ecflow servers

If you have an error similar to this one when performing the verification test above:

```
ssh ecflow-user@217.71.194.199
The authenticity of host '217.71.194.199 (217.71.194.199)' can't be established.
ECDSA key fingerprint is ----
Are you sure you want to continue connection (yes/no/[fingerprint])? yes
Warning: Permanently added '217.71.194.199' (ECDSA) to the list of known hosts.
ecflow-user@217.71.194.199: Permission denied (publickey).
```
Please check that:
1. You have been whitelisted by ECMWF according to your `~/.ssh/id_rsa.pub` key (NOT `id_rsa_troika.pub`)

2. ~~You have added the contents of `~/.ssh/id_rsa.pub` to your csc.fi profile (or myaccessid if you use that)~~.


### 2. If you can not start ecflow UI

Graphics are needed to see the Ecflow graphic UI.
Make sure that you used the -X (or -A option) to ssh to LUMI

```shell
ssh -i <your-private-key> -X <username>@lumi.csc.fi
```
(use the -A flag if you have any problems with the -X flag above). 


### 3. If you can not connect to your ecflow servers

Once Ecflow UI is loaded, if your server (de330) is gray in colour, that means something has failed. To get an idea of the problem, click on 'Panel' then 'Add info panel' which will divide the ecflow ui screen and give you a message.
   

Please check that:
1. You have a correct environment:

    ```shell
    $ module list
    Currently Loaded Modules:
    1) perftools-base/24.03.0   4) cray-dsmml/0.3.0      7) PrgEnv-cray/8.5.0      10) init-lumi/0.2     (S)  13) libfabric/1.15.2.0                    16) partition/L         (S)  19) scl-ecflow_23
    2) cce/17.0.1               5) cray-mpich/8.1.29     8) ModuleLabel/label (S)  11) craype-x86-rome        14) craype-network-ofi                    17) LUMI/24.03          (S)
    3) craype/2.7.31.11         6) cray-libsci/24.03.0   9) lumi-tools/24.05  (S)  12) craype-accel-host      15) xpmem/2.8.2-1.0_5.1__g84a27a5.shasta  18) cray-python/3.10.10 (H)

    Where:
    S:  Module is Sticky, requires --force to unload or purge
    ```


2. You have correct ECF variables 
    
    ```shell
    $  env | grep ECF

    ECF_CUSTOM_PASSWD=/users/<user name>/.ecflowrc/ecf.custom_passwd
    ECF_HOST=217.71.194.208
    ECF_PORT=8443
    ECF_USER=de_330
    ECF_SSL=1
    ```

3. You have a correct password file:
    
    The password file **must look exactly like this**

    ```shell
    $ cat /users/<user name>/.ecflowrc/ecf.custom_passwd
    5.11.3
    de_330 217.71.194.208 8443 {PASSWORD_OBTAINED_FROM_ECMWF}
    de_330 217.71.194.199 8443 {PASSWORD_OBTAINED_FROM_ECMWF}
    ```

4. You have correct certificates

    ```shell
    $ ls ~/.ecflowrc/ecf.custom_passwd
    $ ls ~/.ecflowrc/ssl/217.71.194.208.8443.crt
    $ ls ~/.ecflowrc/ssl/217.71.194.208.8443.crt
    $ ls ~/.ecflowrc/ssl/de330-ecflow-dev.8443.crt
    $ ls ~/.ecflowrc/ssl/de330-ecflow-prod.8443.crt
    ```

5. You have reset ecflow_ui

    If you make any changes after solving issues with your Ecflow functionality, **you must reset ecflow_ui**. To do this: close it, and reissue the command `ecflow_ui &`.

### 4. If you can not start a deode suite

Please check that:

1. You are inside the $DEODE folder, in which you cloned the Deode repository

2. You did `poetry install` inside your $DEODE folder.

3. You spawned `poetry shell` inside your $DEODE folder.



## Contacts

support@lumi-supercomputer.eu 

henrik.nortamo@csc.fi

