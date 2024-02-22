# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

[Full Changelog](https://github.com/destination-earth-digital-twins/Deode-Prototype/compare/...HEAD)

## [Unreleased](https://github.com/destination-earth-digital-twins/Deode-Prototype/tree/HEAD)
### Added
- Dummy FDB archiving methods (#495)
- Submission of bash wrapper scripts (#492)
- Introduce troika config for lumi prod/dev users and support for macro parsing of the troika file (#491)
### Fixed
- Correct erroneous usage of bdshift and fixed marsprep unit testing (#489)
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
