
[![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/destination-earth-digital-twins/Deode-Workflow)
[![Github Pages](https://img.shields.io/badge/github%20pages-121013?style=for-the-badge&logo=github&logoColor=white)](https://destination-earth-digital-twins.github.io/deode-workflow-docs/)

[![Linting](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/linting.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/linting.yaml)
[![Tests](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/tests.yaml/badge.svg)](https://github.com/destination-earth-digital-twins/Deode-Workflow/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/github/destination-earth-digital-twins/Deode-Workflow/branch/develop/graph/badge.svg?token=4PRUK8DMZF)](https://codecov.io/github/destination-earth-digital-twins/Deode-Workflow)

# DEODE environment variables

The following environment variables can be used to control various behaviour.

- DEODE_LOGLEVEL sets the logger log level. Select e.g. between info and debug. For more options see the [loguru](https://loguru.readthedocs.io/) documentation.
- DEODE_HOST allows to override the automatic detection of host. The host is used to pick up specific configuration settings. See e.g. `deode show host`
- DEODE_CONFIG_DATA_PATH sets additional search paths for config files. This is useful when using DEODE as a package. To see paths in use run `deode show paths`.
