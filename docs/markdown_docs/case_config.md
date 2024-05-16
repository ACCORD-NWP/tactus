# Configure cases

## The case setup

The Deode-workflow is designed to be highly configurable and driven from a single config file. The `deode case` functionality offers a way to reduce the number of lengthy config files by building the final config file from smaller sections of configuration settings. A number of pre-defined configurations are available under `deode/data/config_files/configurations`

Example usage would be:
`deode case ?deode/data/config_files/configurations/cy48t3_alaro_gpu_lumi -o test.toml`

where ? is a file includer operator where all the arguments are defined line by line. I.e. `deode/data/config_files/configurations/cy48t3_alaro_gpu_lumi` contains a list of arguments to be evaluated. In this case we have

```
--config-file
deode/data/config_files/config.toml
deode/data/config_files/modifications/CY48t3.toml
deode/data/config_files/modifications/alaro.toml
deode/data/config_files/include/accelerator_device/lumi_gpu.toml
deode/data/config_files/modifications/submission/lumi_CY48t3_gpu_large_domain.toml
```

When the first config file, `config.toml`, has been read the appropriate files for the current host is included for `scheduler` `platform`and `submission`.

The following configuration files will be read and added in order of appearence. If we check the various files we find that e.g. `alaro.toml` only contains the bare minimum changes on top of the default configuration file required to run the ALARO CSC.

The processed configuration output file, here `test.toml`, is self contained from a config point of view. All configuration settings (also defaults from json schema) are in the generated configuration file. 

The produced config file, `test.toml` is now used to start a run by

```
deode start suite --config-file test.toml
```

