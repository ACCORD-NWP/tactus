# The DEODE config file
This is a file generated from the default config and the main schema file using `dedoe doc config > docs/config.md`, do not edit!

# GeneralSectionModel
Model for the 'general' section.
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |case|Experiment name|harmonie_arome_CY46h1||string|
  |cnmexp|Experiment short name|DEOD||string|
  |realization|Placeholder for future ensemble or similar need|-1||number|
  |tstep|Model time step|75||number|
  |loglevel|Logger output level|info|critical, error, debug, info, warning, CRITICAL, ERROR, DEBUG, INFO, WARNING|string|
  |csc|CSC|HARMONIE-AROME|AROME, ALARO, HARMONIE-AROME|string|
  |cycle|IAL cycle|CY46h1|CY48t3, CY46t1, CY46h1|string|
  |surfex|SURFEX switch|True||boolean|
  |bdint|Boundary interval|PT3H||string|
  |bdcycle|Boundary model cycle interval|PT12H||string|
  |bdmax|Max number of parallel boundary interpolation tasks|12||number|
  |forecast_range|Forecast range|PT12H||string|
  |keep_workdirs|Do not remove working directories|True||boolean|
  |create_static_data|Activate the generation of PGD and monthly climate files in the suite|True||boolean|
  |accept_static_namelists|Allow usage of static namelists as input for the tasks. The namelist should be located in the directory specified by platform.namelists and named with the convention namelist_{kind}_{task} where kind could be surfex or master and task forecast/pgd or similar. For the extraction of namelist see [namelists.md](../docs/namelists.md).|False||boolean|
  |nproma||-32|||

## GeneralSectionModel > OutputSettings
Output settings. Specified as duration for any of interval, endtime:interval, starttime:endtime:interval or a list of these options
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |history|Output list for history files|('PT1H:PT3H:PT1H', 'PT3H:PT6H:PT3H', 'PT6H')|||
  |fullpos|Output list for fullpos files|PT24H:PT15M|||
  |surfex|Output list for surfex files|('PT3H:PT1H', 'PT9H:PT24H:PT3H')|||

## GeneralSectionModel > Times
Model for the 'general.times' section.
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |basetime||2023-02-19T00:00:00Z|||
  |validtime||2023-02-19T00:00:00Z|||
  |start|Start|2023-02-19T00:00:00Z||string|
  |end|End|2023-02-19T03:00:00Z||string|
  |cycle_length|Cycle length defined as duration|PT3H||string|

# FileTemplates
Templates for file names
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |duration_grib||@LLLH@h@LM@m@LS@s|||
  |duration_fa|Macros for duration representation of FA files, note that this is given by the IAL code.|@LLLH@:@LM@:@LS@||string|
  |history|Name template for history files|ICMSH@CNMEXP@+@DURATION_FA@||string|
  |surfex|Name template for surfex history files|ICMSH@CNMEXP@+@DURATION_FA@.sfx||string|
  |fullpos|Name template for fullpos files. Note that these are in GRIB2 by default.|GRIBPF@CNMEXP@@DOMAIN@+@DURATION_FA@||string|

# MacroSection
Model for the 'domain' section.
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |os_macros|Environment variables used as macros|('USER', 'HOME', 'PWD')||array|
  |group_macros|Turns a full config section to a macro|('platform', 'system')||array|
  |gen_macros|Turns any string config to a macro, use a dict to rename an attribute.|('general.cnmexp', 'general.cycle', 'file_templates.duration_grib', 'file_templates.duration_fa', {'surfex_template': 'file_templates.surfex'}, {'case': 'general.case'}, {'domain': 'domain.name'}, {'rrr': 'general.realization'})||array|

# VerticalLevels
Options for vertical levels
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |nlev|Number of vertical full levels|65||number|
  |ahalf|Level definition for the a coefficient of half levels|(0.0, 2000.0, 4000.21287319, 6002.09662113, 7911.25838577, 9633.01049417, 11169.37146237, 12522.57753978, 13695.00149653, 14689.11546998, 15507.49052823, 16154.69697732, 16632.12471208, 16940.1494996, 17082.34869816, 17065.28164099, 16898.18367797, 16592.58939571, 16161.90395878, 15620.9434055, 14985.46502362, 14271.70773051, 13495.95994372, 12674.1690991, 11821.60314859, 10952.5704262, 10080.20053763, 9216.28565403, 8371.17893039, 7553.74479607, 6771.35457397, 6029.92021691, 5333.95880836, 4686.68074804, 4090.09511346, 3545.1264511, 3051.73811264, 2609.05813936, 2215.50455766, 1868.90774223, 1566.6282106, 1305.66882073, 1081.85503306, 890.47596795, 727.74548529, 590.17748096, 474.5876798, 378.08857614, 298.07947335, 232.23312781, 178.48015386, 134.9920744, 100.16369201, 72.59529482, 51.07508967, 34.5621649, 22.17022046, 13.15225964, 6.8864131, 2.86306141, 0.67344356, 0.0, 0.0, 0.0, 0.0, 0.0)||array|
  |bhalf|Level definition for the b coefficient of half levels|(0.0, 0.0, 0.0, 0.0, 0.00095468, 0.0038257, 0.00862327, 0.01535782, 0.02404046, 0.03468314, 0.04729839, 0.06195102, 0.07868187, 0.09744325, 0.11815586, 0.14071098, 0.16497348, 0.19078554, 0.21797086, 0.24633925, 0.27569119, 0.30582244, 0.33652825, 0.36760726, 0.39886479, 0.43011564, 0.46118624, 0.49191624, 0.52215946, 0.55178443, 0.58067442, 0.60872709, 0.63585388, 0.66197911, 0.68703898, 0.71098036, 0.73375964, 0.75534143, 0.77569737, 0.79480486, 0.81264598, 0.82920633, 0.84454, 0.85875505, 0.87191802, 0.88409276, 0.89534045, 0.90571965, 0.91528643, 0.92409452, 0.93219549, 0.93963895, 0.94647277, 0.95274328, 0.95849551, 0.9637734, 0.96862008, 0.97307803, 0.97718944, 0.9809964, 0.98454132, 0.98786727, 0.99102462, 0.9940651, 0.99703923, 1.0)||array|

# DomainSection
Model for the 'domain' section.
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |name|Name of your domain|DEMO_60x80_2500m||string|
  |nimax|Number of grid points longitudinally|49||number|
  |njmax|Number of grid points latitudinally|69||number|
  |xloncen|Domain center longitude|4.9||number|
  |xlatcen|Domain center latitude|51.967||number|
  |xlon0|Reference longitude|0.0||number|
  |xlat0|Reference latitude|52.5||number|
  |xbeta||0.0|||
  |xdx|Domain longitudinal length in meters|2500.0||number|
  |xdy|Domain latiudinal length in meters|2500.0||number|
  |ilone|Longitudinal extension zone|11||number|
  |ilate|Latitudinal extension zone|11||number|
  |nbzong|Latitudinal coupling zone width|8||number|
  |nbzonl|Longitudinal coupling zone width|8||number|
  |gridtype|Select truncation type|linear|linear, quadratic, cubic, custom|string|

# System
DEODE system specific path settings
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |wrk|Location of date specific working directory|@SCRATCH@/deode/@CASE@/@YYYY@@MM@@DD@_@HH@@mm@||string|
  |archive|Location of date specific archive|@ARCHIVE_ROOT@/@YYYY@/@MM@/@DD@/@HH@/||string|
  |bddir|Location of input boundaries|/scratch/sism/DEOL/@YYYY@/@MM@/@DD@/@HH@||string|
  |bdfile_template|Name template for input boundary files|PFIFSDEOL+@LLLL@||string|
  |bddir_sfx|Location of input intial file for surfex|/home/snh02/work/dev-CY46h1_deode/boundaries/HRES/@YYYY@/@MM@/@DD@/@HH@||string|
  |bdfile_sfx_template|Name template for input initial surfex file for prep|fc@YYYY@@MM@@DD@_@HH@+@LLL@||string|
  |climdir|Location of generated/used climate files (PGD/monthly files)|@SCRATCH@/deode/@CASE@/climate/@DOMAIN@||string|
  |bdclimdir|Location of host model climate files used for e927/c903|/home/snh02/work/dev-CY46h1_deode/climate/DEODE_LARGE||string|
  |bindir|Location of binaries|/home/snh02/work/dev-CY46h1_deode/bin/2023-03-29||string|
  |fullpos_config_file|Path to the fullpos yml config file|@DEODE_HOME@/deode/namelist_generation_input/@CYCLE@/fullpos_namelist.yml||string|

# Platform
DEODE platform specific path settings
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |deode_home|Path to the deode code, normally no need to change|set-by-the-system||string|
  |scratch|Path used as macro for e.g. wrk and other directories.|/scratch/@USER@||string|
  |archive_root|Path to the main archive directory|@SCRATCH@/deode/@CASE@/archive||string|
  |static_data|Macro used in other platform paths|/hpcperm/snh02/DEODE||string|
  |climdata|Macro used in other platform paths|@STATIC_DATA@/climate||string|
  |rrtm_dir|Path to RRTM input data|@STATIC_DATA@/rrtm/@CYCLE@||string|
  |ncdir|Path to ECrad NetCDF data|@STATIC_DATA@/ncdir||string|
  |e923_data|Path to e923 input data|@STATIC_DATA@/climate/E923_DATA||string|
  |namelists|Path to static namelists|@DEODE_HOME@/deode/data/namelists/@CYCLE@||string|
  |ecosg_data_path|Path to ECOCLIMAP SG data|@CLIMDATA@/ECOCLIMAP-SG||string|
  |ecoclim_data_path|Path to the ECOCLIMAP-II data|@CLIMDATA@/ecoclimap/@CYCLE@||string|
  |gmted2010_data_path|Path to gmted2010 input|@CLIMDATA@/GMTED2010||string|
  |soilgrid_data_path|Path to solgrid input data|@CLIMDATA@/SOILGRID||string|
  |pgd_data_path|Path to PGD input data|@CLIMDATA@/PGD||string|

# Troika
DEODE system specific path settings
  |Key|Description|Default|Options|Type|
  |---|---|---|---|---|
  |config_file|Troika config file|@DEODE_HOME@/deode/data/config_files/troika.yml||string|
