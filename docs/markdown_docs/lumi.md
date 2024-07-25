[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Prototype)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-prototype-docs/)


[![Linting](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml/badge.svg
)](https://github.com/destination-earth-digital-twins/Deode-Prototype/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Prototype)

# LUMI 
## Getting on LUMI
To run the project on LUMI, you need to have a LUMI account and add your SSH key to it. Please check the [online instructions on how to do that](https://docs.lumi-supercomputer.eu/firststeps/SSH-keys/). Once you have your SSH key added, you can login to LUMI by running `ssh -i <your-private-key> <username>@@lumi.csc.fi`. Once logged in, create a SSH key and add that to your GitHub account so you can clone the repo. Then, please follow the download, install and usage instructions provided in the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file.

## Running DEODE on LUMI

The following command launches DEODE, using AROME, on LUMI
```shell
deode case ?deode/data/config_files/configurations/cy48t3_arome -o cy48t3_arome.toml --start-suite
```

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

### Ecflow server selection

By default, all users will use the **dev server**.

Every user can choose to run on the de330-dev or de330-prod ecflow servers. To do this, you only need to change the ecf_host variable in deode/data/config_files/include/scheduler/ecflow_lumi.toml:

```
[ecfvars]
  ecf_host = "217.71.194.208"
```

1. 217.71.194.199 (this is the de330-prod server for **production only**) 
2. 217.71.194.208 (this is the de330-dev server for **normal use**) 

These are the 2 ecflow servers integrated with LUMI currently, 'Develop' (dev-330) and 'Production' (prod-330).

**Should you require access to the prod server, you must be whitelisted by requesting permission from the ECMWF with someone senior in cc (Ulf Andrae - SMHI or Xiaohua Yang - DMI) . This is detailed in step 1 in the Ecflow Server Setup >> Initial setup section below.**

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

### Standalone task example (forecast)

To run a standalone "Forecast" task, first you need to change the 'SCHOST' field from "lumi-batch" to "lumi" under [parallel], as shown below:

```shell
[parallel]
  NPROC = 16
  SCHOST = "lumi"
  tasks = ["Forecast", "e927", "Pgd", "Prep", "c903"]
  WRAPPER = "srun"

```

in: deode/data/config_files/include/submiossion/lumi_CY48t3.toml.

This also applies to any task one wishes to run standalone. Then simply run the following comment below:

```shell
deode run \
      --config-file $PWD/deode/data/config_files/config_CY48t3_lumi.toml \
      --task Forecast \
      --template $PWD/deode/templates/stand_alone.py \
      --job $PWD/forecast.job \
      --output $PWD/forecast.log
```

## Ecflow Server Setup

###  Initial setup

**Do not proceed to other steps til this is complete**

1. Whitelisting: Permission to servers. First step is to email ECMWF,  cristina.duma@ecmwf.int (with samet.demir@ecmwf.int, bojan.kasic@ecmwf.int and ulf.andrae@smhi.se in cc), providing them with: 

a) Your **LUMI username** (USERNAME in the rest of this document).

b) Your **LUMI public key** (usually under: /users/USERNAME/.ssh/id_rsa.pub)

And ask to be 'whitelisted' to be granted access to the **dev ecflow server for LUMI** identified by the IP address:  **217.71.194.208**.

2. You need to copy the following **encrypted** files from this location: (/pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW/)

```shell
adelsaid@uan02:/pfs/lustrep4/projappl/project_465000527/shared_dev/LUMI_ECFLOW> ls
217.71.194.208.8443.crt.gpg  217.71.194.199.8443.crt.gpg  ecf.custom_passwd.gpg	id_rsa_troika_DEV.pub.gpg   id_rsa_troika_PROD.pub.gpg
```
a) 217.71.194.208.8443.crt.gpg 217.71.194.199.8443.crt.gpg (ssl certificates for prod and dev servers)

b) ecf.custom_passwd.gpg (ecf custom password file for prod and dev servers)

c) id_rsa_troika_DEV.pub.gpg and id_rsa_troika_PROD.pub.gpg (troika public keys for both servers, both are needed)

3. Decrypt the files with the following command:
```shell
gpg filename
```
using the **password: peterpiper**.

4. Then put them in the following locations:

a) 2 certificate files: /users/USERNAME/.ecflowrc/ssl/

b) Password file: /users/USERNAME/.ecflowrc/ecf.custom_passwd

c) 'id_rsa_troika_PROD.pub' and 'id_rsa_troika_DEV.pub' need to be uploaded to my.csc.fi or myaccessid provider separately. Once this is done, the files themselves aren't needed.

5. Finally, create soft links for **both** certificate files:

```shell
ln -sf ~/.ecflowrc/ssl/217.71.194.199.8443.crt ~/.ecflowrc/ssl/de330-ecflow-prod.8443.crt
ln -sf ~/.ecflowrc/ssl/217.71.194.208.8443.crt ~/.ecflowrc/ssl/de330-ecflow-dev.8443.crt
```

So now your file arrangement should look like:
```shell
(deode-py3.10) adelsaid@uan03:/scratch/project_465000527/adelsaid/git/github/draelsaid/Deode-Prototype> ls -l ~/.ecflowrc/ssl/
total 8
-rw-rw---- 1 adelsaid adelsaid 1103 Apr 24 17:58 217.71.194.208.8443.crt
-rw-rw---- 1 adelsaid adelsaid 1107 Oct 18  2023 217.71.194.208.8443.crt
lrwxrwxrwx 1 adelsaid adelsaid   54 Apr 29 11:00 de330-ecflow-dev.8443.crt -> /users/adelsaid//.ecflowrc/ssl/217.71.194.208.8443.crt
lrwxrwxrwx 1 adelsaid adelsaid   54 Apr 29 11:00 de330-ecflow-prod.8443.crt -> /users/adelsaid//.ecflowrc/ssl/217.71.194.199.8443.crt
```
and
```shell
adelsaid@uan02:/scratch/project_465000527/adelsaid/git/github/draelsaid/Deode-Prototype> ls ~/.ecflowrc/
ecf.custom_passwd  ssl
```

6. Then map the following ECF variables:

```shell
adelsaid@uan02:/scratch/project_465000527/adelsaid/git/github/draelsaid/Deode-Prototype> env | grep ECF
ECF_CUSTOM_PASSWD=/users/adelsaid/.ecflowrc/ecf.custom_passwd
ECF_HOST=217.71.194.208
ECF_PORT=8443
ECF_USER=de_330
ECF_SSL=1
```

which you can enter in the ~/.bashrc file for convenience:
```shell
export ECF_CUSTOM_PASSWD="/users/USERNAME/.ecflowrc/ecf.custom_passwd"
export ECF_HOST=217.71.194.208
export ECF_PORT=8443
export ECF_USER=de_330
export ECF_SSL=1
```

The password file **must look exactly like this**:
```shell
adelsaid@uan02:/users/adelsaid> cat /users/USERNAME/.ecflowrc/ecf.custom_passwd
5.11.3
de_330 217.71.194.208 8443 {PASSWORD_OBTAINED_FROM_ECMWF}
de_330 217.71.194.199 8443 {PASSWORD_OBTAINED_FROM_ECMWF}
```
**Note: ecf.custom_passwd is the same for every user**.

### Testing Ecflow server connection

Only once these steps are completed, test your connection by pinging to **each server** (you have to load the ecflow module first as described in the next section):
```shell
adelsaid@uan02:/scratch/project_465000527/adelsaid/git/github/draelsaid/Deode-Prototype> ecflow_client --ping --host 217.71.194.199 --port 8443
ping server(217.71.194.199:8443) succeeded in 00:00:00.171897  ~171 milliseconds
adelsaid@uan02:/scratch/project_465000527/adelsaid/git/github/draelsaid/Deode-Prototype> ecflow_client --ping --host 217.71.194.208 --port 8443
ping server(217.71.194.208:8443) succeeded in 00:00:00.147990  ~147 milliseconds
```
If there are any obvious errors, rechart your steps by starting again. 

Another litmus test for Ecflow working is logging into the ecflow server:
```shell
adelsaid@uan03:/users/adelsaid> ssh ecflow-user@217.71.194.208
Linux de330-ecflow-dev 5.10.0-28-cloud-amd64 #1 SMP Debian 5.10.209-2 (2024-01-31) x86_64

The programs included with the Debian GNU/Linux system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Debian GNU/Linux comes with ABSOLUTELY NO WARRANTY, to the extent
permitted by applicable law.
Last login: Wed Apr 24 12:48:29 2024 from 193.167.209.165
ecflow-user@de330-ecflow-dev:~$ 
```

and

```shell
adelsaid@uan03:/users/adelsaid> ssh ecflow-user@217.71.194.199
```

**Note: Only once you have a working connection should you attempt to setup the Ecflow User Interface below.**

### Ecflow UI

1. To bring up the Ecflow User Interface you need the following modules, by doing 'module load' (ml):

```shell
ml use /scratch/project_465000527/jasinskas/scl/modules/
ml cray-python/3.10.10
ml scl-ecflow_23
```

then
```shell
ecflow_ui &
```

2. On the ecflow_ui itself go to: "Servers > Manage Servers > Add Server" and set the following:

```
Name: de330-prod
Host: 217.71.194.199
Port: 8443
Custom user: de_330

And make sure you check these flags:
"Use SSL”:  (check)
"Add server to current view":  (check)
```

And the dev server:

```
Name: de330-dev
Host: 217.71.194.208
Port: 8443
Custom user: de_330

And make sure you check these flags:
"Use SSL”:  (check)
"Add server to current view":  (check)
```

**Note: Please do not use the production server unless authorised to do so**

### Troubleshooting

1. Once Ecflow is setup on LUMI you need to login as follows:

```shell
ssh -X USERNAME@lumi.csc.fi
```
(use the -A flag if you have any problems with the -X flag above). Graphics are needed to see the Ecflow graphic UI.

2. Once Ecflow UI is loaded, if your server (de330) is gray in colour, that means something has failed. To get an idea of the problem, click on 'Panel' then 'Add info panel' which will divide the ecflow ui screen and give you a message. Retrace the steps above and ensure
   
3. If you make any changes after solving issues with your Ecflow functionality, **you must reset ecflow_ui**. To do this: close it, and reissue the command `ecflow_ui &`.

4. Here is a checklist of things you need to ensure you have for the ecflow ui to run:
    
    a) Correct environment:
    ```shell
    ml use /scratch/project_465000527/jasinskas/scl/modules/
    ml scl-ftn16_23
    ml scl-ecflow_23
    ```
    b) Correct ECF variables 
    ```shell
    export ECF_CUSTOM_PASSWD="/users/USERNAME/.ecflowrc/ecf.custom_passwd"
    export ECF_HOST=217.71.194.208
    export ECF_PORT=8443
    export ECF_USER=de_330
    export ECF_SSL=1
    ```
    c) You are inside the correct folder ($DEODE) where you did `git clone`
    
    d) You did `poetry install` inside your $DEODE folder.
    
    e) You spawned `poetry shell` inside your $DEODE folder.

5. (FYI) The ecflow scheduler variables live inside: 
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

6. If you get an errors related to publickey, similar to this:
```shell
ssh ecflow-user@217.71.194.199
The authenticity of host '217.71.194.199 (217.71.194.199)' can't be established.
ECDSA key fingerprint is ----
Are you sure you want to continue connection (yes/no/[fingerprint])? yes
Warning: Permanently added '217.71.194.199' (ECDSA) to the list of known hosts.
ecflow-user@217.71.194.199: Permission denied (publickey).
```
check:

a) You have been whitelisted by ECMWF according to your `~/.ssh/id_rsa.pub` key (NOT `id_rsa_troika.pub`)

b) You have added the contents of `~/.ssh/id_rsa.pub` to your csc.fi profile (or myaccessid if you use that).


## Contacts

support@lumi-supercomputer.eu 

henrik.nortamo@csc.fi

