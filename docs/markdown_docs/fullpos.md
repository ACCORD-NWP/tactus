# Configure selection of fields from fullpos
Describes how to select fields and output settings for fullpos. The scripting allows to select fields and levels without having to worry about how the namelist should be constructed, this is done by the system.

## Overview
The fullpos output is controlled by the following settings
- Output interval is set by `output_settings.fullpos` in the general section of the main config file. The interval is specified using the duration syntax and can be give down to a resolution of minutes.
- Field selection and settings for different output times is done in the fullpos main selection, `deode/namelist_generation/CYCLE/master_selection.yml`

Currently it's only implemented support for a single domain using the native geometry of the domain used for the model run.

## The fullpos config files
There are three fullpos config file types under `deode/namelist_generation/fullpos/CYCLE/`
- rules.yml sets `LEVEL_MAP` and `PARAM_MAP` and tells how settings in the selection part should be translated to settings in `NAMFPC`. Normally this does not have to be changed.
- namfpc_header.yml where we specify the non-field related settings in NAMFPC
- master_selection.yml where all fields and levels are specified per output type as defined by fullpos

The settings done in the `selection` part will be used to fill `NAMFPC` and the correct level numbers will be filled in the final namelist.

### NAMFPC settings

In the section we can add general fullpos settings as in the example below.

```
NAMFPC:
    NFPGRIB: 141
    CFPDOM: "${domain.name}"
    LFPCAPEX: True
    LFPMOIS: False
    L_READ_MODEL_DATE: True
    NFPCAPE: 5
```

### Selection
In the selection section we specify all fields to be produced by fullpos. Following the time selection logics we define the subsection as an arbitrary number of `xxtddddhhmm` choices. The default setup contains a general selection and on for t=0, i.e. `xxt00000000`.

The logics follows the namelists used by fullpos. For 2D fields we specify the field names in the appropriate sections. For 3D fields there are two ways. We can specify the fields directly as in here:

```
  NAMFPDYH:
   CL3DF:
     - 'CLOUD_FRACTI'
     - 'GRAUPEL'
   RFP3H:
     - 20.0
     - 50.0
```

If there is a wish for different levels for different fields we can specify this as

```
  NAMFPDYP:
   VOR:
    CL3DF:
     - 'ABS_VORTICITY'
    RFP3P:
     - 5000.0
     - 10000.0
   DIV:
    CL3DF:
     - 'DIVERGENCE'
    RFP3P:
     - 70000.0
     - 80000.0
```

Where `VOR` and `DIV` in the example above are just arbitrary names used to separate the different choices. Note that it's not possible to combine grouped and none grouped settings.

### Combine selection files

A number of selection files can be combined by extending the `selection` in the fullpos part of the config file

```
[fullpos]
 selection = ["rules", "namfpc_header", "master_selection" ,"my_additions"]
```

This allows to define different combination of output depending on other configuration choices.


## Example

The fullpos namelists can be generated from command line using

```
deode --config-file deode/data/config_files/config.toml show namelist -t master -n forecast_bdmodel_ifs
```

This will produce the namelist for the forecast and as many xxt* files as has been defined in the fullpos yaml file.

