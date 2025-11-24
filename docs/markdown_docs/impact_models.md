# Impact models

Impact models can be connected to the workflow in various ways. Here we describe the current implementation and how the hydrological model EHYPE have been connected. This may differ between different models, and in development and operations. For development this method is convenient but for operations, where various components may be distributed over several hosts, AVISO will be the main triggering platform.

## General design

The methods dealing with connecting an impact model is defined in `deode/tasks/impact.py`. To add a new model we'd first have to introduce a new section in `impact.toml`.

```
[impact.ehype]
  active =  false                       # activates the specific model 
  arguments = "@ARCHIVE@/ehype.json"    # are arguments given to the impcat model start command
  config_name = "@ARCHIVE@/ehype.json"  # config_name is a (potential) config file to give to the impact model
  path = "@EHYPE_PATH@"                 # path is the path to the impact model start command
  task = "StartImpactModels"            # task is the task in the workflow where the impact model should start from. Has to be defined in impact.py.
```
In addition a `communicate` section can be used to inform the impact model about date/time, location of data (disk/fdb), file names, output frequency and simlar things. For EHYPE we have the following keys:

```
[impact.ehype.communicate]
  archive_1 = "@ARCHIVE@"
  archive_2 = "@ECFS_PREFIX@/archive/@ARCHIVE_TIMESTAMP@"
  basetime = "@BASETIME@"
  file_template = "GRIBPF*h00m00s"
  forecast_range = "@FORECAST_RANGE@"
  name = "@CASE@_@YMD@_@HH@"
  nwp_model = "@CSC@"
  ouput_freq = "@FULLPOS_OUTPUT_FREQ@"
```

The communication section can include nested sections as well, depending on the needs of the impact model. 

Once this is defined we need to write a function in impact.py that executes the impact model start command. For ehype it's simply

```
    def ehype(path, args):
        cmd = f"{path}/deploy_suite.sh {args}"
        BatchJob(os.environ, wrapper="").run(cmd)
```

Finally we need to declare that this particular impact model is available on the host we're running on. This is done by specifying the path to the installation in the `platform` config section. I.e. for atos we currently have

```
[platform.impact]
  ehype = "/home/snh02/DE_Impact/EHYPE/forecast/pyflow/utils"
```

With this is in place the impact model is ready for execution, from the workflow perspective.

## EHYPE

The configuration settings for EHYPE are described in the example below. The model itself consists of two parts: One continous run to maintain a good initial state and the on-demand part triggered by the above mentioned settings. The activation above will trigger a separate suite following the settings in the communicate section. On atos EHYPE is installed under the sn02 user and execution is available for any user. Results will be found under `$SCRATCH/$USER/DE_Impact/EHYPE`.

## Deode-EPS-Upscaling
The configuration settings for the [Deode-EPS-Upscaling](https://github.com/destination-earth-digital-twins/Deode-EPS-Upscaling) model is documented in the README of the repo. The model can be activated by including the config file `deode/data/config_files/modifications/eps/eps_upscaling.toml` when launching a run.
The activation will trigger a runtime installation of the model into a temporary directory, and launch a separate ecflow suite following the settings in the communicate section. The resulting upscaled fields will be stored at the path `@ARCHIVE_ROOT@/@ARCHIVE_TIMESTAMP@/eps_upscaling/`.
