(cleaning-of-experiment)=
# Cleaning of experiment
 The forecast suite  produces a lot of files in the working and output folders. To avoid increased usage of file storage, it is needed to have a cleaning feature, which allows configurable options to delete files.

Each forecast experiment has a start and end dates and cycle length. These settings define the experiment timeline and used folders:

![drawing](../docs/figs/experiment_timeline.jpg)

working folder - contains the workings folder of each suite task: wrk="@CASEDIR@/@YYYY@@MM@@DD@_@HH@@mm@/"

archive folder - contains the forecast result folders: archive="@ARCHIVE_ROOT@/@ARCHIVE_TIMESTAMP@"="@ARCHIVE_ROOT@/@YYYY@/@MM@/@DD@/@HH@/"

These folder paths are parameterized by settings in the config file and its include setting files. Here  for working folder the time parameters are from the experiment start date.
For the archive folder the time parameters are specific for each cycle.
Regular usage of suite forecast for different start dates produces a series of specific folders and files in the experiment working and archive folders.

## Cleaning task to clean experiment files . 
It is added an option in the suite_control setting section of the config file, which allows to add or not an ecflow cleaning task.
```shell
do_cleaning = true 
```
To use the cleanig task it is created a ecflow task which is the last task in the forecast suite. It is triggered after successfully execution of the previous suite tasks.

Cleaning settings are grouped in the cleaning include file, which is referred as a setting in the include setting section of the config file.
```shell
cleaning = "include/cleaning.toml"
```
## Cleaning settings are separeted to general default settings and folder specific settings:

![cleaning scheme](../docs/figs/cleaning_image.jpg)

```shell
defaults
  active = true  - turn on or turn off the cleaning task
  dry_run = true  - testing mode - to see which files would be deleted without their actual deletion. This could be seen in the task log file.
  include  = "(.*)" - regular expression which specifies which files to delete. "(.*)" is regular expesion which matches all files
  cleaning_delay = "P1D" 
  cleaning_max_delay = "P2D"
  step = "PT6H"
```
cleaning_delay and cleanig_max_delay are period settings which define the cleaning time interval according to the expiriment start date

step - defines the time moments in the cleaning interval when to clean files

All of the default settings are required. But these values could be changed in the specific folder settings.

## Settings for the working folder. 
Required settings are "active" and "path". "path" defines the working folder path - the root folder which contains all working files and folders. 
Example value of the working path: "/ec/res4/scratch/bgmt/deode/CY48t3_AROME/20230916_0000/"
```shell
[wrk]
  active = true
  dry_run = true
  include  = "ELS*"
  path = "@WRK@"
```
## Settings for the archive folder. 
Required settings are "active" and "path". 
Example value of the archive folder path is "/ec/res4/scratch/bgmt/deode/CY48t3_AROME/archive/2023/09/16/00/"
```shell
[archive]
  active = true
  dry_run = true
  include = "GRIBDEOD*"
  path = "@ARCHIVE@"
  cleaning_delay = "P1D"
  step = "PT6H"
```
The cleaning settings allow to define the cleaning time interval and step and which files to delete.
