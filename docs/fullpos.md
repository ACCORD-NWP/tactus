# Configure selection of fields from fullpos
Describes how to select fields and output settings for fullpos. The scripting allows to select fields and levels without having to worry about how the namelist should be constructed, this is done by the system.

## Overview
The fullpos output is controlled by the following settings
- Output interval is set by `output_interval_fp` in the general section of the main config file. The interval is specified using the duration syntax and can be give down to a resolution of minutes.
- Field selection and settings for different output times is done in `deode/namelist_generation/fullpos_namelist.yaml`

Currently it's only implemented support for a single domain using the native geometry of the domain used for the model run.

## The fullpos config file
The config file `deode/namelist_generation/fullpos_namelist.yaml` consists of four main parts
- `NAMFPC` where we specify the non-field related settings
- `selection` where all fields and levels are specified per output type as defined by fullpos
- `LEVEL_MAP` and `PARAM_MAP` tells how settings in the selection part should be translated to settings in `NAMFPC`. Normally this does not have to be changed.

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

Where `VOR` and `DIV` in the example above are just arbitrary names used to separate the different choices.


## Example

A simple example on how the namelists are generated can be found at the end of `deode/fullpos.py` 

```
import fullpos

fp = Fullpos("test")

namfpc, selections = fp.construct()
for k, v in namfpc["NAMFPC"].items():
    print(k, v)
for k, v in selections.items():
    print(k)
    for kk, vv in v.items():
        print(kk, vv)
```

The fullpos namelists can be generated from command line using

```
deode -config_file deode/data/config_files/config.toml show namelist -t master -n forecast
```

This will produce the namelist for the forecast and as many xxt* files as has been defined in the fullpos yaml file.
