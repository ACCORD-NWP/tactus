# The DEODE config file
This is a file generated from the default config and the main schema file using `dedoe doc config > docs/config.md`, do not edit!

# GeneralSectionModel
Model for the 'general' section.
  |Key|Description|Default|Options|
  |---|---|---|---|
  |case|Experiment name|deode_case||
  |cnmexp|Experiment short name|DEOD||
  |realization||-1||
  |tstep|Model time step|75||
  |loglevel|Logger info level|info|critical, error, debug, info, warning, CRITICAL, ERROR, DEBUG, INFO, WARNING|
  |csc|CSC|HARMONIE-AROME|AROME, ALARO, HARMONIE-AROME|
  |cycle|IAL cycle|CY46h1|CY48t3, CY46t1, CY46h1|
  |surfex|SURFEX switch|True||
  |bdint|Boundary interval|PT3H||
  |bdcycle|Boundary model cycle interval|PT12H||
  |bdmax||12||
  |forecast_range|Forecast range|PT12H||
  |output_interval_his|Output list for history files. Specified as duration for any of interval, endtime:interval, starttime:endtime:interval or a list of these options|('PT1H:PT3H:PT1H', 'PT3H:PT6H:PT3H', 'PT24H:PT6H')||
  |output_interval_fp|Output list for history files. Specified as duration for any of interval, endtime:interval, starttime:endtime:interval or a list of these options|PT24H:PT15M||
  |output_interval_sfx|Output list for history files. Specified as duration for any of interval, endtime:interval, starttime:endtime:interval or a list of these options|('PT3H:PT1H', 'PT9H:PT24H:PT3H')||
  |output_interval_sfx_sel||PT1H||
  |keep_workdirs|Do not remove working directories|True||
  |create_static_data||True||
  |accept_static_namelists||False||
  |nproma||-32||

## times
Model for the 'general.times' section.
  |Key|Description|Default|Options|
  |---|---|---|---|
  |basetime||2023-02-19T00:00:00Z||
  |validtime||2023-02-19T00:00:00Z||
  |start|Start|2023-02-19T00:00:00Z||
  |end|End|2023-02-19T03:00:00Z||
  |cycle_length|Cycle Length|PT3H||

# Macros
Model for the 'domain' section.
  |Key|Description|Default|Options|
  |---|---|---|---|
  |os_macros|Environment variables used as macros|('USER', 'HOME', 'PWD')||
  |group_macros|Turns a full config section to a macro|('platform', 'system')||
  |gen_macros|Turns any string config to a macro, use a dict to rename an attribute.|('general.cnmexp', 'general.cycle', {'case': 'general.case'}, {'domain': 'domain.name'}, {'rrr': 'general.realization'})||

# vertical_levels

  |Key|Description|Default|Options|
  |---|---|---|---|
  |nlev||65||
  |ahalf||(0.0, 2000.0, 4000.21287319, 6002.09662113, 7911.25838577, 9633.01049417, 11169.37146237, 12522.57753978, 13695.00149653, 14689.11546998, 15507.49052823, 16154.69697732, 16632.12471208, 16940.1494996, 17082.34869816, 17065.28164099, 16898.18367797, 16592.58939571, 16161.90395878, 15620.9434055, 14985.46502362, 14271.70773051, 13495.95994372, 12674.1690991, 11821.60314859, 10952.5704262, 10080.20053763, 9216.28565403, 8371.17893039, 7553.74479607, 6771.35457397, 6029.92021691, 5333.95880836, 4686.68074804, 4090.09511346, 3545.1264511, 3051.73811264, 2609.05813936, 2215.50455766, 1868.90774223, 1566.6282106, 1305.66882073, 1081.85503306, 890.47596795, 727.74548529, 590.17748096, 474.5876798, 378.08857614, 298.07947335, 232.23312781, 178.48015386, 134.9920744, 100.16369201, 72.59529482, 51.07508967, 34.5621649, 22.17022046, 13.15225964, 6.8864131, 2.86306141, 0.67344356, 0.0, 0.0, 0.0, 0.0, 0.0)||
  |bhalf||(0.0, 0.0, 0.0, 0.0, 0.00095468, 0.0038257, 0.00862327, 0.01535782, 0.02404046, 0.03468314, 0.04729839, 0.06195102, 0.07868187, 0.09744325, 0.11815586, 0.14071098, 0.16497348, 0.19078554, 0.21797086, 0.24633925, 0.27569119, 0.30582244, 0.33652825, 0.36760726, 0.39886479, 0.43011564, 0.46118624, 0.49191624, 0.52215946, 0.55178443, 0.58067442, 0.60872709, 0.63585388, 0.66197911, 0.68703898, 0.71098036, 0.73375964, 0.75534143, 0.77569737, 0.79480486, 0.81264598, 0.82920633, 0.84454, 0.85875505, 0.87191802, 0.88409276, 0.89534045, 0.90571965, 0.91528643, 0.92409452, 0.93219549, 0.93963895, 0.94647277, 0.95274328, 0.95849551, 0.9637734, 0.96862008, 0.97307803, 0.97718944, 0.9809964, 0.98454132, 0.98786727, 0.99102462, 0.9940651, 0.99703923, 1.0)||

# Domain
Model for the 'domain' section.
  |Key|Description|Default|Options|
  |---|---|---|---|
  |name|Name of your domain|DEMO_60x80_2500m||
  |nimax|Number of grid points longitudinally|49||
  |njmax|Number of grid points latitudinally|69||
  |xloncen||4.9||
  |xlatcen||51.967||
  |xlon0||0.0||
  |xlat0||52.5||
  |xbeta||0.0||
  |xdx|Domain longitudinal length in meters|2500.0||
  |xdy|Domain latiudinal length in meters|2500.0||
  |ilone|Longitudinal extension zone|11||
  |ilate|Latitudinal extension zone|11||
  |nbzong||8||
  |nbzonl||8||
  |gridtype|Select truncation type|linear|linear, quadratic, cubic, custom|

# system

  |Key|Description|Default|Options|
  |---|---|---|---|
  |wrk||@SCRATCH@/deode/@CASE@/@YYYY@@MM@@DD@_@HH@@mm@||
  |archive||@ARCHIVE_ROOT@/@YYYY@/@MM@/@DD@/@HH@/||
  |bddir||/scratch/sism/DEOL/@YYYY@/@MM@/@DD@/@HH@||
  |bdfile_template||PFIFSDEOL+@LLLL@||
  |bddir_sfx||/home/snh02/work/dev-CY46h1_deode/boundaries/HRES/@YYYY@/@MM@/@DD@/@HH@||
  |bdfile_sfx_template||fc@YYYY@@MM@@DD@_@HH@+@LLL@||
  |climdir||@SCRATCH@/deode/@CASE@/climate/@DOMAIN@||
  |bdclimdir||/home/snh02/work/dev-CY46h1_deode/climate/DEODE_LARGE||
  |bindir||/home/snh02/work/dev-CY46h1_deode/bin/2023-03-29||

# platform

  |Key|Description|Default|Options|
  |---|---|---|---|
  |deode_home||set-by-the-system||
  |archive_root||@SCRATCH@/deode/@CASE@/archive||
  |scratch||/scratch/@USER@||
  |rrtm_dir||@STATIC_DATA@/rrtm/46h1||
  |ncdir||/home/snh02/work/cy46t1/arome/data||
  |e923_data||@STATIC_DATA@/climate/E923_DATA||
  |namelists||@DEODE_HOME@/deode/data/namelists/CY46h1/||
  |static_data||/hpcperm/hlam/data||
  |climdata||/hpcperm/hlam/data/climate||
  |ecosg_data_path||@CLIMDATA@/ECOCLIMAP-SG||
  |gmted2010_data_path||@CLIMDATA@/GMTED2010||
  |soilgrid_data_path||@CLIMDATA@/SOILGRID||
  |pgd_data_path||@CLIMDATA@/PGD||

# troika

  |Key|Description|Default|Options|
  |---|---|---|---|
  |config_file||@DEODE_HOME@/deode/data/config_files/troika.yml||

# metadata

  |Key|Description|Default|Options|
  |---|---|---|---|
  |source_file_path||/home/a000864/dev/Deode-Prototype/deode/data/config_files/config.toml||
