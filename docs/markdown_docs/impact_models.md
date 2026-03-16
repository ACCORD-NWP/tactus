# Impact models

Impact models can be connected to the workflow in various ways. Here we describe the current common practice implementation and give some examples about how it can be used for impact models like the hydrological model EHYPE, or postprocessing `impact models` like EPS upscaling or nwp2windpower.

This may differ between different models, and in development and operations. For development this method is convenient but for operations, where various components may be distributed over several hosts, AVISO will be the main triggering platform.

## General design
The methods dealing with connecting an impact model is defined in `deode/tasks/impact.py`. This `BaseImpactModel` basically provides logic for commonly used functionality. To activate this one need to define a couple of configuration keys.

For example, to configure an impact model `my_impact_model` one has basically three locations to provide configurations:
1. The task specific section `[impact.my_impact_model.StartImpactModels]`
2. The platform specific `platform.impact.my_impact_model` installation key (or section)
3. The general impact model section `[impact.my_impact_model]`

Note: when a key is defined in multiple of these sections, the first defined value will be taken.

To launch a new impact model, let's call it `my_impact_model`, we'd first have to introduce a new `[impact]`-section:
```toml
[impact.my_impact_model]
  active = true                                   # activates the specific model
  config_name = "@ARCHIVE@/my_impact_model.toml"  # config_name is a (potential) config file to give to the impact model
  task = "StartImpactModels"                      # the task in the workflow where the impact model should start from
```

Note: at the moment supported filetypes for the given `config_name` are the same as supported by Deode-Workflow configurations, i.e. `.toml`, `.json`, `.yaml` and `.xml`-files.

The second section that we need to prove is a platform-specific information. This can be done by registering `my_impact_model` in `platform.impact`. This can be done by just specifying a key in that section, e.g.:
```toml
[platform.impact]
  my_impact_model = "just-something"
```
or specifying a section with platform specific configurations e.g.:
```toml
[platform.impact.my_impact_model]
  path = "/perm/@USER@/DE_Impact/MyImpactModel"
```

To use `my_impact_model` we finally need to create a dummy dataclass for this impact model in `impacts.py`:
```python
@dataclass()
class MyImpactModel(BaseImpactModel):
    """MyImpactModel specific methods."""

    name = "my_impact_model"
```

## Installation
Next to registering the impact model into the workflow one needs to specify where the plugin can be found. This can be done in two ways: providing a a pre-installed `path` or configuring an on-demand checkout.

### Pre-installed plugins
A possiblity is to have the plugin pre-installed and configured at a specific location. This specific location can be configured using the `path` parameter.

The platform section `[platform.impact.ehype]` can be used to provide the platform specific `path`, e.g. for EHYPE:
```toml
[platform.impact.ehype]
  path = "/home/snh02/DE_Impact/EHYPE/forecast/pyflow/utils"
```

### On-demand installed plugins
On some hpc-systems the compute nodes have internet access and one can prefer for an on-demand installation of plugins using git when starting the impact model. To configure that one can provide the following configuration:
```toml
[impact.my_impact_model.git]
  active = true               # Activate to use git path generation in stead of other logic
  branch = "main"             # The branch of the repo the check-out
  remote_url = "git@github.com:destination-earth-digital-twins/my_impact_model.git" # The repo url
  remove_dir = false          # Should cleaning-task remove dir
```
Note, during check-out of the specified branch the main deode toml configuration is changed and a new key `impact.my_impact_model.git.dir` is inserted with the path where the repository is checked-out. Next to that the key `impact.my_impact_model.git.remove_dir` is set to `True` such that the cleaning task knows that this repo folder can be removed.

The path where this git repository is checked out is a random subfolder if the impact model configured `path` field.

## Launching a plugin
### Using a generic runner
Finally, the StartImpactModels needs a handle to run the impact model. A basic solution is to just provide a shell script in the plugin folder and let StartImpactModels run that script (maybe with providing additional arguments). For example, the next code snippet shows the configuration how that launches the `deploy_suite.sh` script in the `path` with the argument provided in `arguments`:
```toml
[impact.my_impact_model]
  arguments = "--config-file @IMPACT.MY_IMPACT_MODEL.CONFIG_NAME@"
  runner = "deploy_suite.sh"
```

So, the configuration to launch the EHYPE impact model on atos bologna is given by
```toml
[impact.ehype]
  active = true                            # activates the specific model
  config_name = "@ARCHIVE@/ehype.json"     # config_name is a (potential) config file to give to the impact model
  runner = "@EHYPE_PATH@/deploy_suite.sh"  # The handle to run the impact model

[impact.ehype.StartImpactModels]
  arguments = "@IMPACT.EHYPE.CONFIG_NAME@" # are arguments given to the impcat model start command

[macros.select.ehype]
  gen_macros = [{ehype_path = "platform.impact.ehype.path"}]

[platform.impact.ehype]
  path = "/home/snh02/DE_Impact/EHYPE/forecast/pyflow/utils"
```

### Using a custom runner
If a plugin should be launched via a specific command wrapper, provide a `runner` key. For example, the verification plug-in can be configured like this:
```toml
[platform.impact.verification]
  path = "/hpcperm/snh02/DE_Verification/plugins/harpverify"
  runner = "pixi run"

[impact.verification.StartImpactModels]
  arguments = [
    "deode case --config-file @ARCHIVE_ROOT@/config.toml @VERIFICATION_PATH@/harpverify_plugin.toml -o @WRK@/verification_config.toml --case-name verification_for_@CASE@",
    "deode start suite --config-file @WRK@/verification_config.toml -f @WRK@/verification.def",
  ]

```
Note: instead of a single `arguments` string, one can also specify a list of argument strings. In that case `StartImpactModel` uses `runner` to execute all arguments in sequence.

### Using a Deode-Workflow task
Finally, one can also skip providing a `runner`. In that case it is also not allowed to provide `arguments`. This enables plugins to reuse the git on-demand installation logic while handling their own interaction in a specific implementation of `BaseImpactModel.run`. An example is the wildfire plugin, which implements its own run logic in `deode/tasks/wildfires.py`.


## Communication
In addition a `communicate` section can be used to inform the impact model about date/time, location of data (disk/fdb), file names, output frequency and simlar things. For EHYPE we have the following keys:

```toml
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

To communicate full configuration sections to the impact model configuration file one can use the magic keys `.COPY` and `.COPYALL`. For example the following statement copies the `[domain]` section without its subsections and the `[json2tab]` with all its subsections to the configuration of the impact model:
```toml
[impact.my_impact_model.communicate.domain.COPY]
[impact.my_impact_model.communicate.json2tab.COPYALL]
```

With this is in place the impact model is ready for execution, from the workflow perspective.

## EHYPE

The configuration settings for EHYPE are described in the example above. The model itself consists of two parts: One continous run to maintain a good initial state and the on-demand part triggered by the above mentioned settings. The activation above will trigger a separate suite following the settings in the communicate section. On atos EHYPE is installed under the snh02 user and execution is available for any user. Results will be found under `$SCRATCH/$USER/DE_Impact/EHYPE`.

## Deode-EPS-Upscaling
The configuration settings for the [Deode-EPS-Upscaling](https://github.com/destination-earth-digital-twins/Deode-EPS-Upscaling) model is documented in the README of the repo. The model can be activated by including the config file `deode/data/config_files/modifications/eps/eps_upscaling.toml` when launching a run.
The activation will trigger a runtime installation of the model into a temporary directory, and launch a separate ecflow suite following the settings in the communicate section. The resulting upscaled fields will be stored at the path `@ARCHIVE_ROOT@/@ARCHIVE_TIMESTAMP@/eps_upscaling/`.


## NWP to WindPower
The configuration settings for the [NWP2WindPower](https://github.com/destination-earth-digital-twins/nwp2windpower/) plugin are documented in the README of that repo. The postprocessing can be activated by including the config file `deode/data/config_files/modifications/nwp2windpower.toml` when launching a run.
The activation will trigger a runtime installation of the model into a temporary directory, and launch a separate ecflow suite following the settings in the communicate section. The resulting upscaled fields will be stored at the path `@SCRATCH@/DE_Impact/Wind/@CASE@`.
