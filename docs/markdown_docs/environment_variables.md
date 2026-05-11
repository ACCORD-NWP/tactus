
[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/ACCORD-NWP/tactus)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://ACCORD-NWP.github.io/tacus-docs/)

[![Linting](https://github.com/ACCORD-NWP/tactus/actions/workflows/linting.yaml/badge.svg)](https://github.com/ACCORD-NWP/tactus/actions/workflows/linting.yaml)
[![Tests](https://github.com/ACCORD-NWP/tactus/actions/workflows/tests.yaml/badge.svg)](https://github.com/ACCORD-NWP/tactus/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/ACCORD-NWP/tactus/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/ACCORD-NWP/tactus)

# Tactus environment variables

The following environment variables can be used to control various behaviour.

- TACTUS_LOGLEVEL sets the logger log level. Select e.g. between info and debug. For more options see the [loguru](https://loguru.readthedocs.io/) documentation.
- TACTUS_HOST allows to override the automatic detection of host. The host is used to pick up specific configuration settings. See e.g. `tactus show host`
- TACTUS_CONFIG_DATA_DIR sets additional search paths for config files. This is useful when using tactus as a package. To see paths in use run `tactus show paths`.
