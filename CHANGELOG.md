# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

[Full Changelog](https://github.com/destination-earth-digital-twins/Deode-Prototype/compare/...HEAD)


## [Unreleased](https://github.com/destination-earth-digital-twins/Deode-Prototype/tree/HEAD)


### Added
- Functionality to run CY46h1 on LUMI. [\#562](https://github.com/destination-earth-digital-twins/Deode-Workflow/pull/562) (@tbnc)
- Introduce config files for SMHI laptop. [\#606](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/606) (@uandrae)
- Introduce config files for SMHI laptop. [\#606](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/606) (@uandrae)
- Introduce config files for the SMHI HPC freja@NSC. [\#595](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/595) (@uandrae)
- Additional way to initialise a module environment. [\#597](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/597) (@uandrae)
- Function for setting ecf_port by userid. [\#598](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/598) (@uandrae)
- Possibility for case setup and configurations. [\#557](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/557) (@trygveasp)
- Possibility for cleaning of experiment. [\#587](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/587) (@milennimh)
- Add mean winds grib2 definitions. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)

### Changed
- Improve error message on erroneous command line usage. [\#629](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/629) (@uandrae)
- README content and cosmetics corrections. [\#600](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/600) (@uandrae)

### Fixed
- Allow macro parsing of path to troika. [\#596](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/596) (@uandrae)
- Introduce support for scp as a provider method in the file manager. [\#579](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/579) (@uandrae)
- Include missing json schema validation for troika. [\#594](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/594) (@uandrae)
- Amendments to [\#557] with updated documentation and changes to allow all tests to run on both atos and LUMI. [\#586](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/586) (@uandrae)
- Updated grib2 tablesVersions to 32 in faFieldName.def. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)
- Updated grib2 definitions of following fields: SURFLIFTCONDLEV, SURFEQUILIBRLEV, SURFFREECONVLEV. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)
- Remove scaleFactorOfFirstFixedSurface=0 for all fields with typeOfFirstFixedSurface=1 in grib2 definitions in agreement with eccodes standards. [\#585](https://github.com/destination-earth-digital-twins/Deode-Prototype/pull/585) (@sbnielsen)

### Removed
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
