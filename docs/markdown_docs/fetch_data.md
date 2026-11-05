# Fetching static data from IAL-data

The static data needed to run IAL (climate files, RRTM and ecRad data, RTTOV coefficients, ...) are maintained in the [IAL-data](https://github.com/ACCORD-NWP/IAL-data) repository. Only the metadata of the files are stored in git; the data themselves are stored in an ECMWF object store bucket and retrieved with [`dvc`](https://dvc.org).

The `IALDataFetch` task clones IAL-data and pulls its data onto a given location. It can run in its own suite through the `tactus fetch-data` command, or as part of the main tactus suite, similar to the compilation.

## Requirements

`dvc` and `dvc-s3` are installed together with tactus.

The data are read anonymously over https, so no credentials are needed to fetch them. `dvc-s3` is only needed by maintainers pushing data to the bucket. The task always uses the `dvc` installed with tactus, i.e. next to the python interpreter running it.

## Quick start

```
tactus fetch-data --data-dir /path/to/IAL-data
```

This will:

1. Build a config from `config.toml` + host-specific overrides + `fetch_data_suite.toml`
2. Set the location where IAL-data is cloned and its data pulled to `/path/to/IAL-data`
3. Generate a case named `IAL_data_fetch`
4. Start the suite (`FetchDataSuiteDefinition`), unless `-d` (equivalent to `--dry-run`) is given

The data then end up under `/path/to/IAL-data/data`, with the same layout as `@STATIC_DATA@` (`climate`, `ncdir`, `rrtm/@CYCLE@`, `satellite`, ...).

### Command line arguments

| Argument | Config key | Description |
| --- | --- | --- |
| `--data-dir` | `fetch_data.ial_data_dir` | Where IAL-data is cloned and its data pulled. Relative paths are made absolute |
| `--ial-data-version` | `fetch_data.ial_data_git_version` | Branch/tag/commit of IAL-data to check out |
| `--ial-data-repo` | `fetch_data.ial_data_git_repo` | IAL-data repository to clone |
| `--target` | `fetch_data.dvc_targets` | Only pull this path of IAL-data, e.g. `data/ncdir`. Can be repeated |

Arguments not given keep the config values. The remaining arguments (`--output`, `--case-name`, `--dry-run`, `--keep-def-file`, `--expand-config`) behave as for `tactus case`.

For example, to only fetch the RRTM data of CY50t2 and the ecRad data:

```
tactus fetch-data --data-dir $SCRATCH/IAL-data --target data/rrtm/CY50t2 --target data/ncdir
```

## Running as part of the main suite

To fetch the data in a regular run, add the `fetch_data.toml` modification to your case command, e.g.

```
tactus case ?tactus/data/config_files/configurations/cy50t2_arome tactus/data/config_files/modifications/fetch_data.toml [...]
```

or set `suite_control.fetch_data = true` in your config. The `FetchData` family then runs after `PrepRun`, in parallel with the `Compilation` family when compilation is also active. The following nodes (static data, and the time dependent part and final cleaning when they trigger on `PrepRun`) wait for both families, in the same way as for the compilation alone.

`fetch_data.toml` also sets `platform.static_data = "@IAL_DATA_DIR@/data"`, where the `@IAL_DATA_DIR@` macro is `fetch_data.ial_data_dir`. The run hence reads its static data from what it fetched, i.e. by default from `@CASEDIR@/IAL-data/data`. Setting `suite_control.fetch_data = true` alone fetches the data without changing `platform.static_data`.

Note that IAL-data does not (yet) contain all the data under the host static data directories. On ATOS, `BATOR` (data assimilation), `WFP_input_files` (wind farms), `climate_fields_mir` and `climate/OSM_SFX8_1_EU` (`pgd.use_osm`) are missing, so configurations needing them cannot use `fetch_data.toml` yet.

## Configuration

| Key | Default | Description |
| --- | --- | --- |
| `suite_control.fetch_data` | `false` | Add the `FetchData` family to the main suite |
| `fetch_data.ial_data_dir` | `@CASEDIR@/IAL-data` | Where IAL-data is cloned and its data pulled. Available as the `@IAL_DATA_DIR@` macro |
| `fetch_data.ial_data_git_repo` | `git@github.com:ACCORD-NWP/IAL-data.git` | IAL-data repository |
| `fetch_data.ial_data_git_version` | `develop` | Branch/tag/commit to check out |
| `fetch_data.dvc_targets` | `[]` | Paths, relative to the clone, to pull. Empty pulls everything (~30GB) |
| `fetch_data.dvc_jobs` | `0` | Number of parallel downloads, `0` uses the dvc default |

## The `IALDataFetch` task

* If `fetch_data.ial_data_dir` is not a git clone yet, IAL-data is cloned into it. The directory may exist if it is empty; a non-empty directory that is not a git clone is an error.
* If it is already a clone, it is updated with `git fetch`.
* The configured version is checked out and, if it is a branch, fast-forwarded to the remote.
* `dvc pull` is run on the configured targets. Directory targets are expanded to all the `.dvc` files below them, as IAL-data tracks each file separately. Files already present and unchanged are not downloaded again, so the task can be rerun to update the data.

On ATOS the task has a walltime of one hour.

## Troubleshooting

If `dvc pull` reports that the files exist *neither locally nor on remote*, check that the python running dvc can verify the object store certificate: dvc reports SSL errors as missing files. Pointing `SSL_CERT_FILE` to a CA bundle, e.g. `export SSL_CERT_FILE=$(python -c "import certifi; print(certifi.where())")`, solves it.
