# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

[Full Changelog](https://github.com/destination-earth-digital-twins/Deode-Prototype/compare/...HEAD)


## [Unreleased](https://github.com/destination-earth-digital-twins/Deode-Prototype/tree/HEAD)

### Added
- Add DDH config example. [\#1433](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1433) (@uandrae)
- Add ecflow mirror support between a host and target LAM. [\#1425](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1425) (@uandrae)

### Changed
- Remove average u and v from air quality fullpos output [\#1421](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1421)(@KristianHMoller)
- Disable cleaning of ecf_files by PostMortem [\#1421](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1421)(@KristianHMoller)
- Do not allow `.` or leading digit in case names as it breaks the sqlite extraction. [\#1244](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1244) (@uandrae)

### Fixed
- Fixed small bug in datetime_utils when deciding which decadal PGDs to make [\#1442](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1442)(@adeckmyn)
- Correct AQ archive path when running as a plugin [\#1421](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1421)(@KristianHMoller)
- Correct C903 input for CY46h1. [\#1436](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1436)(@uandrae)
- Increase nodes for PGD to prevent failures in ALARO when the domain is located too far east. [\#1434](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1434)(@kastelecn)
- Fix paths for eps-upscaling impact model [\#1430](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1430) (@mafdmi)

## [0.23.0] - 2025-11-24

### Added
- Introduce nzsfilter config option for orography filtering [\#1409](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1409) (@uandrae)

### Changed
- Produce more LBCs in one call of MASTERODB.[\#1414](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1414)(@kastelecn)
- Replace all config symlinks by files to allow installation as a package. [\#1419](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1419) (@uandrae)

### Fixed
- Fix broken pgd namelist for cy48t3. [\#1429](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1429) (@kastelecn)
- Update outdated documentation (cy48t3 -> cy49t2 for the test cases). [\#1427](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1427)(@kastelecn) 
- Fix IOmerge task missing output files [\#1411](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1411) (@uandrae)

## [0.23.0] - 2025-11-24

### Added
- Added installation instructions for Belenos [\#1396](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1396) (@nicolasCtd).
- Allow to disable max_static_data_tasks and max_interpolation_tasks by setting them <0. [\#1388](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1388) (@uandrae)
- Add Deode-EPS-Upscaling impact model [\#1252](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1252/) (@mafmdi)
- Introduce plugin launcher, like for AQ. [\#1243](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1243)(@uandrae)
- Add .toml to enable use of new high-res ECOCLIMAPSG+ML data [\#1390](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1390)(@PanuMaaFMI)

### Changed
- Optimized SPP settings for Harmonie-AROME [\#1417](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1417) (@pirkkao) 
- Optimized SPP settings for AROME [\#1412](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1412) (@cwastl) 
- Change operational archive path.[\#1387](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1387)(@kastelecn)
- Delete only finished suite in Clean\_old\_data. [\#1379](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1379) (@kastelecn) 
- Switch off ecfs archiving of GRIB2 data in operations [\#1405](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1405)(@uandrae)
- Use default binary for PREP when initializing from offline SURFEX [\#1401](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1401)(@trygveasp)
  
### Fixed
- Correct GMTED reading for longitudes > 120 deg. [\#1402](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1402)(@natalieth)
- Do not coupled hydrometors for harmone-arome and arome. [\#1376](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1376)(@kastelecn)
- Erroneous implementation of max_ecf_tasks. [\#1388](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1388) (@uandrae)
- Workaround for missing SST update in CY50 IAL: disable upd_sst_sic [\#1352](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1352) (@jacobsnoeijer)
- Fix cycle50 binary path [\#1378](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1378) (@pardallio)
- Disabled surfex for ALARO cy50 [\#1378](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1378) (@pardallio)
- Couple NAMMCC.LMCCECSST to upd_sst_sic for harmonie-arome and arome cy50[\#1378](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1378) (@pardallio)
- Fix triggers by flattening list [\1386](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1386) (@uandrae)

### Changed
- Split up archiving.toml into a lumi and atos specific file. [\#1399](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1399)

## [0.22.0] - 2025-10-21

### Added
- Add custom topography input to PGD. [\#1372](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1372)(@BolliPalmason)
- Add new dynamic option in namelist for forecast. [\#1366](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1366)(@kastelecn)
- Use offline SURFEX EUR run for Prep [\#1233](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1233)(@BolliPalmason)

### Fixed
- Added a condition not to run lumi workflow on pull request event and atos build wheel job [\1341](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1341)(@milennimh)
- Fix lam to lam coupling [\#1375](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1375)(@uandrae)
- Fix marsprep on LUMI. [\#1374](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1374)(@kastelecn)
- Fix strip_off_mount_path to support multiple underscores in prefix.[\#1364](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1364)(@jacobsnoeijer)
- Update lumi operational settings.[\#1363](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1363)(@kastelecn)
- Create surfex grib files only in case of AQ case.[\#1358](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1358)(@kastelecn)
- CY50: change FA_SFX2CLIM in upper case [\1343](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1343) (@dhaumont)
- CY50: Reanable NFPGRIB as workaround is not needed anymore on CY590 [\1344](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1344/)(@jacobsnoeijer)
- Fix GRIB keys for SQLite extraction eps vs deterministic. Also flexible use of multiple parameter lists. [\1335] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1335/)(@adeckmyn)
- Fix namelist update functionality by ensure namelist names are always in upper case.[\1336](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1336/)(@uandrae)
- Set correct LRED defaults for AROME (true) and HARMONIE-AROME (false) [\1329](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1329/)(@rolfhm)
- Remove duplicate entries of FA field names in eccodes definitions [\1357](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1329/)(@sbnielsen)

### Added 
 - Add different namelist switches for optional new physics [\#1326](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1326) (@jacobsnoeijer) 
 - Support to use EU-scale Open street map data for urban parameters and surface fractions [\#1349](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1349) (@natalieth)
 - Use offline SURFEX EUR run for Prep [\#1233](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1233)(@BolliPalmason)
 - Introduce static files for ecrad and a namelist example [\#1355](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1355) (@uandrae)
 - Add support for conversion to julian days in datetime utils, useful for delayed start of plugins [\#1290](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1290) (@svianaj)

### Changed
 - Change AROME namelist settings [\#1356](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1356) (@uandrae)
 - Change polling frequency for mirrors of globalDT, to reduce load on ecf-servers [\#1365](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1365)(@FlorianW-ZAMG)
 - Use official ecCodes 2.41.0 installation on LUMI. [\#1333](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1333)(@KristianHMoller)
 - Set ECF\_TRIES to 15 dor PgdFilterTownFrac on LUMI. [\#1360](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1360)(@kastelecn)
 - Activate FDB archiving for EPS in operations. [\#1340](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1340) (@uandrae)
 - Write pysurfex json config files to both work and climate directory to ensure correct usage and picking up confing changes. [\#1334](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1334) (@uandrae)
 - Exclude parts of e923 (climate generation) that is not strictly required without assimilation. Avoids crashes for sea domains. [\#1328](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1328) (@uandrae)
 - Cy50t1: Fix PGD and C903 [\#1331](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1331) (@dhaumont)

### Fixed
 - Fix lost array indexing in the static namelist reader. [\#1332](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1332) (@uandrae)

## [0.21.0] - 2025-09-25

### Added
- Introduce archiving to the databridge[\#1321](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1321)(@uandrae)

### Changed
- Added deode run with the default configuration to atos workflow. Added job to check if the deode run is completed [\#1000](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1000)(@milennimh)
- Reenable sst interpolation for lumi runs [\1313](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1313/) (@jacobsnoeijer)

### Fixed
- Created a verbose version of the ICMGG_maskland MARS request that can be processed on LUMI's MARS version [\1313](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1313/)(@jacobsnoeijer)
- Issue with merging and archiving of sqlite files in operational ensemble runs [\#1316](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1316)(@mafdmi)
- Fixes for the  operational settings. [\#1219](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1319)(@kastelecn)

## [0.20.0] - 2025-09-18

### Added
- Add link to documentation for LUMI setup in general README [\#1318](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1318)(@tbnc)
- CY50t1 introduction [\#1280](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1280)(@dhaumont)
- Add "resolution" to sfcdir and mars settings for is6g/irok [\#1263](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1263)(@kastelcn,@j-fannon)
- Use pysurfex in surfex related tasks [\#1210](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1210)(@trygveasp)
- Add support for E927 and alaro -> alaro coupling in CY49. [\#1165](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1165)(@uandrae)
- Add 2m dewpoint temperature to gribmodify and standard output [\#1279](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1279)(@sbnielsen)
- Add ecflow mirror functionality -> allows mirror of globalDT and activate in operations on atos. [\#1107](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1107), [\#1284](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1284) (@FlorianW-ZAMG, @uandrae)

### Changed
- Changes binary path defaults to include compiler and precision as R64 or R32 changes way of setting precision ("R32"/"R64" vs "-sp"/"") [\1288](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1288) (@pardallio)
- Move StartImpact outside of the ensemble loop [\1302](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1302) (@uandrae)
- Create one single point to define gl-binary and make ial/gl version Deode-configurable [\1288](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1288) (@jacobsnoeijer)
- Disables sst interpolation for lumi runs [\#1276](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1276)(@pardallio)
- Merge boundary interpolation tasks in one [\#1165](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1165)(@uandrae)

### Fixed
- Fixed bddir\_sst file template to allow runs with bdshift>0. [\#1308](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1308)(@kastelecn)
- Restore lost recreation of surfex files [\#1303](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1303)(@uandrae)
- Correct usage of bddir_sst path [\#1300](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1300)(@uandrae)
- Fix names of the inputs for e927 and Prep.[\#1298](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1298)(@kastelecn)
- Make sure unit tests runs on atos. Fixes ecflow connection problem and location of the troika file. [\#1277](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1277)(@uandrae)
- Introduce tile fraction normalization to tile averaging. [\#1275](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1275)(@KristianHMoller)

## [0.19.0] - 2025-08-14

### Added
- Added GNU config files for ATOS and LUMI [\#1265](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1265)(@pardallio)
- Updated FDB parameters for archiving of ensemble forecasts [\#1222](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1222)(@KristianHMoller)
- Add first operational ensemble config modification files. Add 750m domain template. [\#1215](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1215) (@mafdmi)

### Fixed
- Fixed eccodes version to 2.41 in pyproject.toml [\#1261](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1261)(@KristianHMoller)
- Fix MergeSQLites class/task to comply with new way of referencing parameter lists (separate lists for sfc and upper air variables)  [\#1253](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1253)(@svianaj)
- Simplified bdmembers/bdmember. [\#1204](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1221)(@mafdmi)

## [0.18.1] - 2025-07-22

### Fixed
- Sort list of empty dirs in CleanOldData. [\#1251](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1251)(@kastelcn)

## [0.18.0] - 2025-07-16

### Added
- Turn on SST/SIC update by default for CY49 and CY46 HARMONIE-AROME [\#1241](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1241) (@jacobsnoeijer)
- Make CY49 the default cycle [\#1239](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1239) (ealerskans, @mafdmi)
- Add option to update SST/SIC during simulation from host model [\#1231, \#1236, \#1238, \#1240](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1231) (@jacobsnoeijer, @BertvanUlft)
- Add path in config to prep input file to allow better control of file location. [\#1212](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1212)(@uandrae)
- Add merging of sqlite files in cases of more than 1 ensemble member. [\#1193](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1193)(@mafdmi)
- Add georef-calculation and determination of stream-type to deode case command [\#1213](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1213)(@johtoblan)
- Introduce first CSC dependent EPS settings. [\#1217](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1217)(@uandrae)
- Add sqlite extraction for Z&Q and limit extraction of all upper air variables to the locations of TEMP stations [\#1235](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1235])(@svianaj)
- Introduce destineFaModelSource for handling output settings of ensemble members as well as sub-hourly output. [\#1211](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1211)(@sbnielsen, @KristianHMoller)

### Fixed
- Fix cleaning for EHYPE data, add cleaning of empty directories. [\#1227](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1227) (@kastelecn)
- Correct per member generation of input file for prep. [\#1214](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1214)(@uandrae) 
- Specify tagged version 0.1.2 or above of pyfdb in pyproject.toml. [\#1205](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1205)(@KristianHMoller) 
- Allow using duration notation for both general.times.start and end. [\#1209](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1209) (@uandrae)
- Set ensemble member number in namelist by default. [\#1216](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1216)(@uandrae) 
- Fix grib2 encodings of radar product to allow ensemble production. [\#1219](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1219) (@sbnielsen)
- Fixed issue with the user having to specify all members when using the dict syntax in eps configuration. [\#1220](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1220)(@mafdmi)
- Fix bug in creation of merged sqlite files which contained member 0 in all data columns [\#1223](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1223])(@svianaj)

## [0.17.0] - 2025-06-13

### Added
- Add option to write output only at first output time to creategrib and gribmodify. [\#1156](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1156)(@KristianHMoller)
- Added FDB paths and expver for LUMI. [\#1181](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1181) (@KristianHMoller)

### Changed
- Clarify procedure for creating a hotfix [\#1076](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1076)(@KristianHMoller)
- Updated to using ecCodes 2.41 on ATOS. [\#1187](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1187)(@KristianHMoller)
- Remove param 231049 from FDB exclusion. [\#1187](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1187)(@KristianHMoller)

### Fixed
- Avoid generating a task when checking for active impact models. [\#1196](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1196) (@uandrae)
- Allow FDB archiving without excluding fields. [\#1198](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1198) (@KristianHMoller)
- Correct fullpos merge method. Removes dependency of merge order. [\#1195](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1195) (@uandrae)
- Fix c903 climate file for lagged boundary conditions (bdshift). [\#1194](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1194) (@adeckmyn)
- Fix arome nudging settings to remove spikes in norms of pressure-departure. [\#1180](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1180) (@kastelecn)
- Synced develop with master [\#1186](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1186) (@mafdmi)
- Correct time unit when changing productDefinitionTemplateNumber. [\#1156](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1156)(@KristianHMoller)
- Output surface geopotential only at analysis time. [\#1156](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1156)(@KristianHMoller)
- Remove duplicated radiation parameters from AQ output. [\#1156](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1156)(@KristianHMoller)
- Fix GRIB2 encoding for tile attribute properties. [\#1156](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1156)(@KristianHMoller)
- Fix lagged boundary conditions (bd_shift). [\#1175](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1175) (@adeckmyn)
- Moved incorrectly placed entries in changelog. [\#1192](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1192)(@KristianHMoller)
- Corrected paths in config files for running demo cases as an ensemble. [\#1191](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1191) (@mafdmi)
- Fixed pytest removing data on ATOS. [\#1043](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1043) (@mafdmi)
- Make config file saved to wrk dir for every task. Removed member env in standalone task. [\1155](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1155) (@mafdmi)
- Revert test command in tests.yaml workflow. [\#1201](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1201) (@mafdmi)

## [0.16.1] - 2025-05-23

### Fixed
- Reverted referencing pyproject.toml relative to package directory, since, when installing deode as a package, it does not include pyproject.toml file. [\#1177](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1177) (@mafdmi)

## [0.16.0] - 2025-05-21

### Added
- Json schema check on expver for FDB archiving.[\#1168](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1168) (@uandrae)
- Add posibility to run ALARO cy49t2 with surfex.[\#1128](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1128) (@kastelecn)
- Add more control of expver for FDB archiving.[\#1120](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1120) (@uandrae)
- Introduce fixed global orography on gaussian grid for marsprep speedup. [\#1143](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1143) (@adeckmyn)
- Set start date for snow data. [\#1097](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1097) (@kastelecn)
- Introduce subtasks for CreateGrib and AddCalculatedFields to reduce time to solution.[\#1138](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1138) (@uandrae)
- Introduced EPS in the workflow. By default, every run is now treated as an ensemble. [\#1031](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1031)
- Introduce caching of parameters to speedup the search in GRIB files in gribmody.py.[\#1136](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1136) (@uandrae)
- Add generation and storage of expanded config file for dcmbd usage.[\#1126](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1126) (@uandrae)

### Changed
- Switch on FDB archiving in operations and updated the FDB documentation. [\#1168](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1168) (@uandrae)
- Make log collection settings configurable. [\#1167](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1167) (@uandrae)
- Use module related environment variable for FDB5_HOME on atos_bologna.[\#1120](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1120) (@uandrae)
- Deactivate TOMS computations to allow execution of forecast model in SP.[\#1135](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1135) (@uandrae)
- Allow archiving to be called from any task. Allows log collection to run after archiving.[\#1117](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1117) (@uandrae)
- Rename PreCleaning to RunPrep as it also handles storing of config files.[\#1126](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1126) (@uandrae)
- Update GRIB encoding of root depth parameter for AQ.[\#1134](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1134) (@KristianHMoller)
- Simplifies config files, having one for each cyle, one for each csc, and one for each host, instead of the combinations of all of these.[\#1088](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1088) (@pardallio)


### Fixed
- Restore gathering of task specific log files. [\#1167](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1167) (@uandrae)
- Fix LBC filenames in case of mode = "restart". [\#1162](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1162) (@kastelecn)
- Added @MEMBER_STR@ to archive outpath where missing. [\#1147](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1147) (@mafdmi)
- Made host resolvement for impact models happen run-time instead of when generating config. [\#1148](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1148) (@mafdmi)
- Fix error with parent node for "CreateGribStatic" task. [\#1152](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1152) (@mafdmi)
- Revert fetching ecFlow variables from ecFlow server. [\#1142](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1142) (@mafdmi)

## [0.15.0] - 2025-04-24

### Added
- Add wfp configuration for all cscs and event-type for windfarm [\#999](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/999) (@jacobsnoeijer)
- Prepare for activation of FDB archiving in operations and add a new FDB archiving task. [\#1096](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1096). FDB archiving switch off due to memory problems in [\#1127](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1126). (@uandrae)
- Add grib conversion of static files [\#1035](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1035) (@uandrae)
- Modified gribmodify to use a json file for configuration [\#1049](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1049) (@KristianHMoller)
- Updated gribmodify to do patch averaging for AQ needs [\#1049](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1049) (@KristianHMoller)
- Added config files for running CY49 in leonardo [\#1064](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1064) (@pardallio)
- Added config files for running CY49 in SP [\#1064](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1064) (@pardallio)

### Changed
- Move Ecflow limit from E923 Family to StaticData family. [\#1121] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1121) (@kastelecn)
- Change walltime for Forecast task on LUMI for cy48t3 [\#1113](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1113) (FlorianW-ZAMG)
- Change bindir for CY49t2 to match new name convention [\1122](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1122)(@uandrae)
- Change default balance between PROCs used for Forecast and IO for default (small domain) configuration [\1114](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1114)(@jacobsnoeijer)
- Change default behaviour of the deode show namelist command. Now produces unparsed namelists. [\#1099](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1099) (@uandrae)
- Let the IOmerge tasks cleanup the Forecast working directories[\#1102](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1102) (@uandrae)

### Fixed
- Fix trigger bug introduced in #1102. [\#1125](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1125) (@uandrae)
- Fix forecast archive bug introduced in #1102. [\#1124](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1124) (@uandrae)
- Fix name of suite to ignore in cleaning. [\#1116](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1116) (@kastelecn)
- Fixed some details in Mars requests depending on Mars client version and data origin [\#1066] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1066) (@adeckmyn)
- Remove erroneous macro warnings for non-existent impact model [\#1091](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1091) (@uandrae)
- Made git branch structure figures appear in documentation. Resolved sphinx warnings [\#1077](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1077) (@mafdmi)
- Make sure the IOmerge tasks fails if the Forecast task fails [\#1090](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1090)(@uandrae)
- Updated documentation of FDB as georef is added, also remove backgroundProcess = "99" as default grib_set [\#1094](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1094) (@johtoblan)

## [0.14.0] - 2025-03-27

### Added
- Check on output frequency for fields calculated in gribmodify [\#1085](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1085) (@uandrae)
- Added documentation for installation on leonardo. [\#1062](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1062)(@uandrae)
- Added setup for running CY48t3_arome and CY49t2_arome on Leonardo, for both 60x80 and large domains. [\#1057](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1057)(@mippoliti-cin)
- Uniform the fullpos namelists for all CSC's in cy49 [\#1028](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1028) (@egregow)
- Add function for evaluate relative dates from config file. [\#1085](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1058#pullrequestreview-2710477467) (@kastelecn)

### Changed
- Update surfex namelist settings for CY49. [\#1050](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1050)(@uandrae)
- Add support to configure simulations for eventtype eclipse [\#1063](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1063)(@jacobsnoeijer)
- Change macro configuration to be more flexible [\#1032](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1032)(@uandrae) 

### Fixed
- Restore problematic deode_home and ecf_host changed in v0.13.0  [\#1068](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1068)(@uandrae)
- Use 32 bit space filling curve for geohash [\#1042](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1042) (@johtoblan)
- Reduce ntasks for C903 on lumi. [\#1080](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1080) (@kastelecn)
- Add snowfields to IFSENS MARS request [\#1055](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1055)(@uandrae)
- Specify timeIncrement to hours for 60M precipitation variables in faFieldNames.def[\#1052](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1052)(@KristianHMoller)

## [0.13.2] - 2025-03-21

### Changed
- Reintroduced calculation of wind gust to gribmodify and removed from fullpos [\#1070](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1070) (@KristianHMoller)

## [0.13.1] - 2025-02-27

### Changed
- Use gl binary built with correct eccodes version [\#1029](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1029) (@uandrae))

## [0.13.0] - 2025-02-26

### Added
- Add possibility to set start to yesterday. [\#1047](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1047#pullrequestreview-2677820249) (@kastelecn)
- Print deode version and config file used for each task [\#1011](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1011) (@uandrae)
- Configure dependabot for frequent dependency updates. [\#989](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/989) (@observingClouds)
### Changed
- Set lumi_DT as default ifs selection on LUMI, seperate option for running on debug partition. [\#1034](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1034)(@kastelen)
- Name convention for LBC families [\#984](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/984)(@uandrae) 
- Increase walltime for CreateGrib on atos_bologna [\#1009](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1009) (@uandrae)
- Use bddir_sfx for MARS latlon file for more flexibilty on output location [\#1012](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1012) (@uandrae)
- Sqlite extraction increased & limited to standard pressure levels for RH,S,T,D [\#1013] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1013) (@svianaj)
- Updated station list for sqlite extraction with new ones from WOW 2023-2024 period [\#1030] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1030) (@svianaj)


### Fixed
- Remove BDMEMBER from all config files. [\#1024](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1024)(@kastelecn)
- Restored lost bdcycle setting [\#984](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/984)(@uandrae) 
- Fix json schema for impact models. [\#1020](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1020)(@kastelecn) 

## [0.12.1] - 2025-02-21

###Fixed
- Fix operational setting for ehype. [\#1015](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1015) (@kastelecn)

## [0.12.0] - 2025-02-21

### Added
- Add possibility to retrieve several EPS members at the same time.[#988](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/988)(@kastelecn)
- Introduce possibility to generate ensemble config file. [\#865](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/865) (@mafdmi)
- Introduce possibility for multiple search path for config files. [\#776](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/776) (@uandrae)
- Added support for CMake compiled binaries on LUMI [\#980](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/980) (@pardallio)

### Changed
- Clean Marsprep slurm settings for atos_bologna [\#1001](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1001) (@uandrae)
- Changed binary location o LUMI to CI/CD compiled binaries [\#980](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/980) (@pardallio)
- Update ehype config settings. [\#967](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/967)(@uandrae)

### Fixed
- Correct EcCodes module inconsistency for atos_bologna [\#1006](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/1006) (@uandrae)

## [0.11.0] - 2025-02-13

- Add Harmonie-Arome configurations and update namelist creation for cy49t2 [\#969](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/969) (@romick-knmi)
- Detach selection of orography filtering method from CSC. [\#960](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/960)(@uandrae)
- Namelist changes for c903 CY49t2, retrieve snow parameters on SOIL levels from mars.[\#978](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/978)(@kastelecn)
- Add large domain submission rules for CY49 on atos_bologna [#945](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/945) (@uandrae)
- Add synthetic satellite fields for cy49t2 [#954](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/954) (@FlorianW-ZAMG)
- Add albedo to AQ event output. [\#920](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/920) (@sbnielsen)
- Add documentation for archiving with FDB on ATOS [\#768](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/768) (@johtoblan)
- Track fields not archived to fdb[\#754](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/754) (@uandrae, @sbnielsen, @KristianHMoller)
- Add geohash algorithm to identify starting area in FDB [\#973](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/973) (@johtoblan)
- Add model version mapping to FDB grib_set parameters [\#975](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/975) (@KristianHMoller)

### Changed
- Update AROME dynamics settings [\#966](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/966)(@uandrae)
- Update fullpos selection for CY49 [\#941](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/941)(@uandrae)
- Increased walltime from 15 to 30 minutes for AddCalculatedFields [\#948](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/948) (@KristianHMoller)
- Updated FA2GRIB2 translations [\#754](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/754) (@uandrae, @sbnielsen, @KristianHMoller)
- Add EcCodes as a python dependency [\#937](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/937) (@johtoblan)
- Make version of poethepoet more flexible [\#950](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/950) (@johtoblan)
- Updated README.md for poetry >= v2.0.0 shell plugin installation. [\#935](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/935) (@BertvanUlft)
- Externalise sqlite_utils to grib2sqlite. [\#949] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/949) (@adeckmyn)
- Make submission groups more flexible. [\#867](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/867) (@uandrae)
- Add pandas 2.0 compatible versions to allowed dependencies. [\#942](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/942) (@observingClouds)

### Fixed
- Correct orography truncation for linear grid. [\#960](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/960)(@uandrae)
- Correct merge of fullpos selection [\#941](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/941)(@uandrae)
- Load ecmwf-toolbox/2024.11.0.0 for AddCalculateFields [\#958](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/958)(@KristianHMoller)
- Semi-functional settings for running DW on LUMI (GMK binary, Cray 16).[\#947](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/947)(@kastelecn)
- Remove erroneous domain specification from flooding event [\#943](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/943) (@uandrae)
- Remove 10wdir from gribmodify.[\#934](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/934) (@kastelecn)
- Add FA2GRIB translation for gust wind speed, makes the gribmodify part obsolete. [\#928](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/928) (@uandrae)
- Bump python version to 3.10 in github linting action. [\#933](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/933) (@johtoblan)

## [0.10.0] - 2024-12-17

### Added
- Add namelists to create needed output for postprocessing [\#923](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/923) (@egregow)
- Add unit test for standalone template file [\#890](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/890) (@pardallio)
- Add wind fields to grib output. [\#916](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/916) (@kastelecn)
- Add lightning to ALARO. [\#907](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/907) (@kastelecn)
- Output surface geopotential (CY46h1) using gl to separate file in air quality events [\#854](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/854) (@sbnielsen)

### Fixed
- Fixed get_decadal_list [#924](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/924)(@natalieth)
- Fixed fullpos.selection for subhourly output and added CLSMEAN.RAD.TEMP to alaro fullpus namelist. [\#893](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/893) (@kastelecn)
- Fixes to make `deode case -e` produce a correct config file [\#897](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/897) (@uandrae)
- Only run gl if there is an actual gl namelist [\#854](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/854) (@sbnielsen)
- Correctly mask surfex values where missing in CY48t3 [\#854](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/854) (@sbnielsen)
- Output surface geopotential (CY48t3) and land-sea-mask (CY48t3 and CY46h1) at analysis instead of forecast in air quality events [\#854](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/854) (@sbnielsen)

### Changed
- Updated git branch structure of repository to standardize branch names and workflows. [\#885](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/885) (@mafdmi)
- Additional fixes for ALARO lightning, finall fixes for ALARO namelist, add graupel to total precipitation for all CSC. [\#915](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/915)(@kastelecn)
- Add MF\_87 vertical levels for ALARO (instead of MF\_90). [\#914](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/914) (@kastelecn)
- Updated docs with accept/decline procedure for reviewers; all accepting reviewers now have to approve before merge. [\#872](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/872) (@mafdmi)
- Updated air quality event output in cy46h1 to use surfex parameters from tile 3 instead of 1 to fix issue since transition to 3 patches. [\#912](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/912) (@sbnielsen) 

- Added WOW's station lists and others for verification [\#860](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/918) (@svianaj)


## [0.9.1] - 2024-11-22

### Fixed
- Archiving of config file in operational setting [\#896](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/896) (@FlorianW-ZAMG)

## [0.9.0] - 2024-11-20

### Added
- Add mars settings for expver iit7 (eps example) [\#879](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/870) (@kastelecn)
- Add xml dumper impact models configs. [\#837](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/837) (@uandrae)
- Intercept standard logging in loguru log handler. [\#855](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/855)
- Add end time for expver i4ql (ATOS\_DT) [\#866](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/866) (@kastelecn)
- Airquality event type configuration file. [\#818](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/818) (@sbnielsen)
- Temporary SURFEX grib2 definitions for CY6h1 and CY48t3. [\#818](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/818) (@sbnielsen)
- Canopy water and land-sea-mask to fullpos aq selection. [\#818](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/818) (@sbnielsen)

### Changed
- Replace batches with ecflow limit for LBC tasks. [\#850](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/850)(@kastelecn)
- deodemakedirs will use default linux permission unless unixgroup is specified. [\#879](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/879) (@trygveasp)
- Refactored the DEODE ecFlow suite generation scripts. Bumped python version to 3.10 in both test environment and pyproject.toml. [\#831](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/831) (@mafdmi)
- Changed impact model config layout [\#838](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/838) (@uandrae)
- Updated operational settings [\#859](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/859) (@kastelecn)
- Added LACE's station lists for verification [\#860] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/860) (@svianaj)
- Creategrib task has been changed so that it is more flexible and can handle several conversions. [\#818] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/818) (@sbnielsen)
- Grib definitions of momentum fluxes and canopy water. [\#818] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/818) (@sbnielsen)
- Updated descriptions for setting up environment and installation in README.md. [\#894] (https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/894) (@ole-dmi)

### Fixed
- Added find_value function to fn_steplist to decode string into list. [\#883](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/883) (@pardallio)
- Added empty modification submission files for alaro on atos\_bologna to silent erroneous warnings. [\#887](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/887) (@kastelecn)
- Not abort on missing logs directory. [\#878](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/878) (@trygveasp)
- Fixed bug in clean\_old\_data.toml. [\#882](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/882) (@kastelecn)
- Set NPATCH=3 explicitly for HARMONIE-AROME. [\871](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/871) (@uandrae)
- Made it impossible to parse any string to output_settings. Only empty string, string of ISO8601 format, or list of strings of ISO8601 format are possible. [\#830](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/830) (@mafdmi)
- Bug that made it impossible to merge namelists with more than one hybrid level with master selection in CY48t3 [\#818](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/818) (@sbnielsen)
- Change the behaviour of `deode run` to only require the `--task` argument. The other arguments will default to names based on the task name. [\#836](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/836) (@uandrae)
 
## [0.8.0] - 2024-10-31

### Added
- Added a operational config toml to be activated after further inspection. [\#829](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/829) (@uandrae)
- Added the same macros to `general.times.[start|end|validtime]` as for `basetime`. [\#829](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/829) (@uandrae)
- Add settings for suite CleanOldData, which clean scratch data, suites in ecflow server and IFS data. It include cron. [\#825](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/825) (@kastelecn)
- Add posibility to read, and remove, suite definition file. [\#799](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/799) [\#832](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/832) (@uandrae)
- Add functionality for user macros and to expand macros in config file. [\#824](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/824) (@uandrae)

### Changed
- Changed the test procedure to require using `deode/data/config_files/modifications/test_settings.toml`. [\#829](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/829) (@uandrae)
- Renamed the Norrkoping domain to 500m tempalate and changed useage accordingly. [\#829](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/829) (@uandrae)
- Change mars setings to work on lumi with lumi\_DT selection. [\#817](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/817) (@kastelecn)
- Remove used suite definition file by default. [\#799](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/799) (@uandrae)
- Change default ifs.selection to point to the valid DT on the current host. With this the default selection on atos is coming from expver=iekm	  . [\#826](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/826) (@uandrae)

### Removed
- Removed the requirement to set `general.times.end`. [\#829](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/829) (@uandrae)
- Removed the requirement to set `domain.[xlat0|xlon0]`. [\#829](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/829) (@uandrae)


## [0.7.1] - 2024-10-17

### Added
- Add total precipitation to grib and add total rain and snow to grib (ALARO). [\#804](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/804) (@kastelecn)


## [0.7.0] - 2024-10-09

### Added
- Introduce a general method for starting impact models in general and EHYPE in particular [\#793](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/793) (@uandrae)
- Automatic name convention for config file output from `deode case` [\#785](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/785) [\#798](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/798) (@uandrae)
- Allow host specification by environment variable DEODE_HOST [\#774](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/774) (@uandrae)

### Changed
- Updated runtime thresholds for CreateGrib task to 2 hours [#809](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/809) (@tbnc)
- Separate fullpos selections by CSC where required [#808](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/808) (@uandrae)
- Updated binary paths to include the "latest" installations of IAL and gl [#807](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/807) (@pardallio)
- Change fullpos selection syntax from a list to a dictionary of lists to allow better merge functionality [#792](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/792) (@uandrae)
- Remove domain name from the stored grib files [#788](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/788) (@uandrae)
- Change URL for troika [#795](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/795) (@uandrae)
- Change default ifs\_delection to ATOS\_DT [#775](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/775) (@kastelecn)

### Fixed
- Various configuration settings lost in translation [\#813](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/813) (@uandrae)
- Store config file with a fixed name rather than the user one [\#808](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/808) (@uandrae)
- Repeat calls to ecflow host fixing failures due to unreacheable ecflow host [\#805](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/805) (@uandrae)
- Fix lost target run functionality [\#803](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/803) (@uandrae)
- Correct usage of start date in the case name [\#801](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/801) (@uandrae)
- Correct setting of ECCODES_DEFINITION_PATH depending on ECcodes version [\#766](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/766) (@uandrae)
- Correct notation and activation of the CreateGrib task [\#770](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/770) (@uandrae)
- Respect input types in namelist config parsing [#784](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/784) (@uandrae)
- Removed duplicated parsing [#783](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/783) (@uandrae)
- Fix ecf_host selector not selecting the correct naming convention on Atos [#781](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/781) (@adam-otruba)

##[0.6.2] - 2024-09-26

### Fixed
- Fix ecf\_host selector not selecting the correct naming convention on Atos [\#781](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/781) (@adam-otruba)

## [0.6.1] - 2024-09-24
- change binary version for CY48t3
- set ATOS\_DT as default ifs.selection

##[bugfix\_v0.6.2] - 2024-09-26

### Fixed
- Fix ecf\_host selector not selecting the correct naming convention on Atos [\#781](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/781) (@adam-otruba)

## [bugfix\_v0.6.1] - 2024-09-24
- change binary version for CY48t3
- set ATOS\_DT as default ifs.selection


## [0.6.0] - 2024-09-19

### Added
- Add missing unit tests for creategrib [\#770](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/770) (@uandrae)
- Add switch LWTHRESHMOIST in CY46h1 following implementation in [HARMONIE repo](https://github.com/destination-earth-digital-twins/Harmonie/pull/37) [\#757](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/757) (@natalieth)
- Introduce the possibility for mulitple simultaneous archiving methods [\#752](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/762)(@uandrae)
- Use of config files and mod files shipped with Deode-Workflow is now possible, when installing Deode-Workflow as a package in another repository [\#671](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/671)(@mafdmi)
- Changes default archiving storage on ATOS to ec: [\#753](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/753)(@FlorianW-ZAMG)
- Added FDB-archiving on LUMI with pyfdb. [\#577](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/577)
- Introduced CY49t2 namelist and config files for all three CSCs [#698](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/698) (@uandrae)
- Updated documentation of ecflow server settings [#659](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/659) (@FlorianW-ZAMG)
- Mars works on LUMI, added selection LUMI\_DT [#647](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/647) (@kastelcn)
- Introduced IOmerge task[s] that can run the merging of IOSERVER output in parallel while the forecast is running, rather than after [#677](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/677) (@adeckmyn)
- Introduced a ecf_host selection function to handle the old and new ecflow server name conventions on atos[#675](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/675) (@uandrae)
- Introduced a short description of available tasks [#658](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/658) (@uandrae)
- Introduction of Leonardo machine [#645](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/645) (@dhaumont)
- Adding 49t2 configuration files [#643](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/643) (@dhaumont)
- Thenamelisttool integration to convert a namelist from one cycle to another [\#613](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/613) (@dhaumont)
- Added option to pick binaries from the different repositories in the same task. [\#630](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/630) (@kastelecn)
- Functionality to run CY46h1 on LUMI. [\#562](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/562) (@tbnc)
- Introduce config files for SMHI laptop. [\#606](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/606) (@uandrae)
- Introduce config files for SMHI laptop. [\#606](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/606) (@uandrae)
- Introduce config files for the SMHI HPC freja@NSC. [\#595](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/595) (@uandrae)
- Additional way to initialise a module environment. [\#597](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/597) (@uandrae)
- Function for setting ecf_port by userid. [\#598](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/598) (@uandrae)
- Possibility for case setup and configurations. [\#557](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/557) (@trygveasp)
- Possibility for cleaning of experiment. [\#587](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/587) [\#637](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/637) (@milennimh, @uandrae)
- Add mean winds grib2 definitions. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)
- Add continous integration workflow for testing installation process on Atos and LUMI [\#437](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/437) (@khintz)

### Changed

- Introduce OmegaConf for namelist configuration handling. Move namelist data to /data sub-directory. [\#759](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/759) (@adeckmyn)
- Move bdcycle ro mars\_settings, fixes for mars on LUMI [\#765](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/765) (@kastelecn)
- Force user to set expver manually for FDB archiving [\#763](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/763) (@uandrae)
- Updates path to Cycle 48t3  and 46h1 binaries to comply with CI/CD [\#760](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/760) (@pardallio)
- Centralise definition of various package related directories [\#758](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/758)(@uandrae)
- Changed default DEODE_HOME to ./Deode-Workflow/deode [\#671](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/671)(@mafdmi)
- Updates path to Cycle 48t3 binaries to comply with CI/CD [\#755](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/755) (@pardallio)
- Documentation for binary seclection [\#736](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/736) (@uandrae)
- Update HARMONIE-AROME to harmonie-46h1.1 binaries. [\#693](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/693) (@uandrae) 
- Always use the class name as the task name to be consistent with submission settings. [\#701](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/701) (@kastelecn, @uandrae)
- Make io-merge processing more verbose and configurable w.r.t waiting times. [\#694](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/694) (@uandrae) 
- Geopotential z in latlon grid for surfex input retrieves from ICMSH global file instead of global\_sfcdir. [\#697](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/697) (@kastelecn)
- Changed location of the binary fa\_sfx2clim. [\#672](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/672) (@kastelecn) 
- Update the default case name to include the domain name. [\#655](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/655) (@uandrae)
- Adapt settings to binary /scratch/project_465000527/ospaniel/executable_cy48t3/ works. [\#618](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/618) (@kastelecn)
- Externalise the selection of static input data to the forecast. [\#619](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/619) (@uandrae)
- Improve error message on erroneous command line usage. [\#629](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/629) (@uandrae)
- README content and cosmetics corrections. [\#600](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/600) (@uandrae)

### Fixed
- Creation of remote directories with scp [\#763](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/763) (@uandrae)
- Adjust sqlite template and path to fix missing ecfs archiving [\#748](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/748) (@uandrae)
- Erroneous submission section on leonardo [\#736](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/736) (@uandrae)
- Changing LUMI's common_de330 data area. [\#738](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/738) (@draelsaid)
- Removes platform-specific namelist settings from the Cy46h1 master namelist file following Jul 2024 binaries update. [#742](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/742) (@tbnc)
- Fix mars retrieve for latlon z. [#687](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/687) (@kastelecn)
- Correct the namelist update functionality. [\#673](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/673) (@uandrae)
- Fix bug in SQLite extraction at other times than midnight [\#680](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/680) (@adeckmyn)
- Minor change to CY46h1/master_namelists.yml for simulated radiance calculation [\#670](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/670) (@fbaordo)
- Restore large domain settings and arome -> arome config template. [\#642](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/642) (@uandrae)
- Make sure schema files are included for validation when creating new config files. [\#601](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/601) (@uandrae)
- Allow macro parsing of path to troika. [\#596](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/596) (@uandrae)
- Handle shift in forcing model start time. [\#627](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/627) (@uandrae)
- Allow macro parsing of path to troika. [\#596](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/596) (@uandrae)
- Introduce support for scp as a provider method in the file manager. [\#579](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/579) (@uandrae)
- Include missing json schema validation for troika. [\#594](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/594) (@uandrae)
- Amendments to [\#557] with updated documentation and changes to allow all tests to run on both atos and LUMI. [\#586](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/586) (@uandrae)
- Updated grib2 tablesVersions to 32 in faFieldName.def. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)
- Updated grib2 definitions of following fields: SURFLIFTCONDLEV, SURFEQUILIBRLEV, SURFFREECONVLEV. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)
- Remove scaleFactorOfFirstFixedSurface=0 for all fields with typeOfFirstFixedSurface=1 in grib2 definitions in agreement with eccodes standards. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)
- Change CI-HPC workflow to only run on develop and master branch to adhere to security concerns. [\#681](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/681) (@khintz)
- Fix bug where if do-cleaning was False in suite-configuration, suites could not be started [\#665](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/665) (@johtoblan)
- Replace all occurences of "prototype" in documentation with "workflow" and fix all the resultant broken code and links [\#737](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/737) (@adam-otruba)
- Replace unnecessarily external links within documentation with internal links [\#737](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/737) (@adam-otruba)

### Removed
- Obsolete config file for CY48t3 submission on lumi [\#736](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/736) (@uandrae)
- All occurences of output variables with stepType!=instant, i.e. accumulated/min/max, at t=0 to conform to fdb strict grib encoding standards[\#590](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/590)(@uandrae)


## [0.5.0] - 2024-05-06

### Added
- Option for sub hour and sub minute bdint[\#565](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/565)(@kastelecn)
- Suites and tasks treated as plug-ins. [\#526](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/526) (@trygveasp)
- Add option to run marsprep before each c903/Prep task separately. [\#546](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/546)
- Add extra variable, mean radiant temperature in both CY46h1 and CY48t3. [\#548](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/548) and [\#537](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/537)

### Changed
- Updated CY48t3 binaries and switch on hybrid parallelisation. [\#548](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/548) (@kastelecn)
- Updated CY46h1 binaries and switch on hybrid parallelisation. [\#541](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/524) (@uandrae)
- Separate the subhourly fullpos output conig to allow more fine grained control. [\#524](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/524) (@uandrae)
- Improve task detection. Avoids erroneous log messages [\#498](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/498) (@uandrae)

### Fixed
- Recover lost coverage [\#575](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/575) (@trygveasp)
- Correct path construction in the sql extraction [\#566](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/566) (@uandrae)
- Better error message in case of missing initial files [\#560](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/560) (@uandrae)
- Decrease NPROC and NPROC\_IO for forecast on the small domain to work with \_target domain. [\#558](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/558) (@kastelecn)
- Use old microphysics routine on LUMI as a temperorary workaround, fixes issue [\#536](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/536). [\#547](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/547) (@uandrae)
- Correct task recognition for Forecast. Makes the usage in the submission config more intuitive. [\#549](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/549) (@uandrae)
- Fix issue [\#483](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/483), broken AQ output selection. [\#524](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/524) (@uandrae)
- Fix SQLite issues [\#485](https://github.com/destination-earth-digital-twins/Deode-Prototype/issues/485) (Failure when no obs. stations in domain) and [\#521](https://github.com/destination-earth-digital-twins/Deode-Prototype/issues/521) (Memory leak). [\#520](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/520) (@adeckmyn)


## [0.4.0] - 2024-03-01

### Breaking changes

- Deode new calls (flags removed) [\#416](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/416) (@draelsaid)

### Added

- Implemented new de330-dev server (for more information see docs/markdown/lumi.md) [\#510](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/576)(@draelsaid) 
- Implemented e923Update to improve the ALARO forecast (for more information see doc/markdown/e923\_update.md) [\#510](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/510)(@kastelecn) 
- Add ALARO forecast [\#261](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/261) (@adeckmyn)
- Sqlite improvements [\#413](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/413) (@adeckmyn)
- Add namelists and config files for ALARO forecast [\#261](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/261) (@adeckmyn)
- Allow recursive references in the namelist config [\#412](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/412) (@uandrae)
- SQLite extraction: add parameters requiring multiple GRIB fields, fix speed issues [\#413](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/413) (@adeckmyn)
- Add fullpos radiation selection for solar renewable output streams [\#425](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/425) (@sbnielsen)
- Add Paris RDP global DT experiment MARS request [\#430](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/430) (@uandrae)
- Add option to use Open Street Map data to create PGD for Paris region [\#434](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/434) (@natalieth)
- Add submission files for experiments with t3999 (Increasing the number of nodes to increase memory) [\#456](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/456) (@kastelecn)
- LUMI Ecflow suite [\#464](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/464) (@draelsaid)
- Add renewables wind selection [\#482](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/482) (@uandrae)
- Add submission settings for c903 on LUMI [\#497](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/497) (@kastelecn)
- Config updates that brings the default setup for AROME@CY48t3 running under ECFLOW on LUMI [\#502](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/502) (@uandrae)
- Added poethepoet as part of the pyproject.toml, such that we can specify the version for local and github [\#496](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/496) (@johtoblan)
- Dummy FDB archiving methods [#495](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/495) (@uandrae)
- Submission of bash wrapper scripts [#492](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/492) (@trygveasp)
- Introduce troika config for lumi prod/dev users and support for macro parsing of the troika file [\#491](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/491) (@uandrae)

### Changed

- Update Alaro to work properly [\#403](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/403) (@kastelecn)
- Use OpenMP in CY48t3 forecasts on large domains [\#427](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/427) (@FlorianW-ZAMG)
- Extend submission MODULE scope [\#432](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/432) (@uandrae)
- Add more parameters to be extracted for verification [\#439](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/439) (@uandrae)
- Add SQLite extraction of wind direction at pressure levels to config file [\#447](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/447) (@adeckmyn)
- Fixed setup for forecasts using the global DT at 12UTC for boundary data [\#442](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/442) (@tbnc)
- ALARO Forecast on LUMI - CPU and GPU version [\#452](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/452) (@dhaumont)
- Update platform paths [\#454](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/454) (@uandrae)
- Change the start date of the HRES data [\#455](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/455) (@kastelecn)
- Added MARS settings for January 2017 period (winter AQ runs) using the global DT for boundary data [\#463](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/463) (@tbnc)
- Add IWIDTH paranmeter to sfx namelist for CY48t3 [\#473](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/473) (@kastelecn)
- Reading surface fields from sfcdir with higher resolution [\#476](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/476) (@kastelecn)
- Activate ccsds packing for CY48t3 [\#479](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/479) (@uandrae)
- Speeding up LUMI>Ecflow server file copy [\#542](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/542) (@draelsaid)

### Removed

- Remove mars_expver variable [\#406](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/406) (@tbnc)
- Remove duplicated ArchiveStatic [\#448](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/448) (@uandrae)
- Remove msdeode as default account and simplify account setting [\#487](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/487) (@uandrae)

### Fixed

- Replace hardcoded value for NRTFP3S with ${vertical_levels.nlev} [\#411](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/411) (@kastelecn)
- Avoid STDOUT/STDERR decoding errors [\#426](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/426) (@uandrae)
- Correct sqlite_model_name [\#429](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/429) (@uandrae)
- Fix/pgd multidecades issue and climate file generation for longer periods [\#443](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/443) (@FlorianW-ZAMG)
- fix missing satellite fields in grib-files at full hours [\#444](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/444) (@FlorianW-ZAMG)
- Correct MARS config and json settings [\#446](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/446) (@uandrae)
- Submission schema typo: Set Default value of lfttw to True [\#475](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/475) (@kastelecn)
- reintroduce wind.u/v.phy in faFieldName.def, accidentally removed [\#477](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/477) (@FlorianW-ZAMG)
- Fix setting of ECCODES_DEFINITION_PATH [\#480](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/480) (@uandrae)
- Separate scheduler and stand alone arguments in bash script used in submission [\#501](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/501) (@trygveasp)
- Correct erroneous usage of bdshift and fixed marsprep unit testing [#489](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/489) (@uandrae)
- Fix wind direction in SQLite extraction: use uvRelativeToGrid [\#449](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/449) (@adeckmyn)

### Maintenance

- Update documentation [\#576](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/576) (@draelsaid)
- Update documentation [\#441](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/441) (@uandrae)
- Lumi - improve submission settings [\#457](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/457) (@dhaumont)
- Add pull-request template [\#465](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/465) (@leifdenby)
- Draelsaid/latest lumi [\#467](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/467) (@draelsaid)
- Fix typo and add info on ecflow on LUMI in README [\#472](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/472) (@tbnc)
- LUMI: Ecflow Job Submission, Troika etc [\#478](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/478) (@draelsaid)
- LUMI: deode flag removal and readme testing [\#493](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/493) (@draelsaid)
- LUMI readme updates [\#503](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/503) (@draelsaid)
- LUMI: Minor updates to readme [\#506](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/506) (@FlorianW-ZAMG)


## [0.3.0] - 2023-11-28

### Added

- Configurable archiving on ecfs, default switched on
- First sqlite extraction for verification. Default switched on but with a limited set of parameters
- Support for reading IFSENS data 

### Changed

- Extraction from MARS configurable from config. More data streams added and corrected resolution for global DT data.
- Changed config structure. E.g. model timestep is now found in the `domain` part.
- Updated config settings. E.g. settings for large domains running on atos. Boundary settings reflecting availability of HRES.
- Json schema validation and documentation updates.
- Change reflecting stricter linting.

### Fixed

- Stop writing log files to the user's home directory. This was reintroduced by mistake.
- Correct windfarm fullpos activation
- Remove explicit python code from the submission files
- Fixes to allow to run the forecast as a stand alone task for the default config


## [0.2.0] - 2023-10-27

### Added

- Adjustment of coupling zone with depending on resolution
- Introduce support for cold_start, start, restart mode using `suite_control.mode` see documentation for usage. Replaces the cold_start flag.
- Fullpos output templates for air quality and hydrology applications. Add gust to the general fullpos output.
- Speedup of the PGD generation by setting `pgd.one_decade = true`. Using false retains to old behaviour.
- Allow setting of unix group for all created directories using `platform.unix_group`. The current default for atos is `msdeode`.
- `poetry devtools` command

  To help developers with tasks such as linting and pre-push checks

### Changed

- New default binaries for CY46h1.
- General improvements in documentation about installation, development practices and configuration.
- Remove less frequently used settings from the example config files and update the json schema files, and documentation accordingly. Impose stricter output frequency settings.
- Minimum required python version is now `v3.9`
  - Requires reinstaling the environment. See instructions in the [README](README.md) file.
- `deode doc config` command now shows info defined in the schema only.

  Users can run `deode show config` when they wish to know what is used in the config.

- Migrate most linting checks from `flake8` to [`ruff`](https://docs.astral.sh/ruff/). It is faster.

### Removed

- `deode toml-formatter` command
  Use a separate library for this

### Fixed

- Correct the unit testing of MARS tasks.
- Updated GRIB2 definitions to conform to WMO standards.
- Surface scheme settings for CY48t3. Fixes problems with erroneous soil data input and crashes in PREP when coupling AROME to AROME.
- Configuration fixes for the default AROME to AROME coupling.
- Format of online config doc in github pages.
- Fixes throughout doc files and online docs.


## [0.1.0] - 2023-10-11

### Added

v0.1.0 of `deode` is able to perform forecast generation for an arbitrary European domain
tailored to high-resolution simulations (with AROME CY48t3 and HARMONIE-AROME
CY46h1) on the ECMWF ATOS HPC. Basic functionality for archiving on ECFS@ATOS is availiable but switched off by default.
