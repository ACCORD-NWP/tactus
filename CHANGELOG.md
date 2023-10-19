# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

[Full Changelog](https://github.com/destination-earth-digital-twins/Deode-Prototype/compare/...HEAD)

## [Unreleased](https://github.com/destination-earth-digital-twins/Deode-Prototype/tree/HEAD)
### Changed
- `deode doc config` command now shows info defined in the schema only.
  Users can run `deode show config` when they wish to know what is used
  in the config.
- Migrate most linting checks from `flake8` to [`ruff`](https://docs.astral.sh/ruff/). It is faster.
### Fixed
- Format of online config doc in github pages.


## [0.1.0] - 2023-10-11
### New features

v0.1.0 of `deode` is able to perform forecast generation for an arbitrary European domain
tailored to high-resolution simulations (with AROME CY48t3 and HARMONIE-AROME
CY46h1) on the ECMWF ATOS HPC. Basic functionality for archiving on ECFS@ATOS is availiable but switched off by default.
