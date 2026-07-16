# Running a Compilation

Tactus exposes a dedicated `compile` subcommand that builds a configuration aimed at compiling IAL and starts (optionally) the compilation suite.

## Quick start

```
tactus compile --ial-tag develop
```

This will:

1. Build a config from `config.toml` +  host-specific overrides + `compile_suite.toml`
2. Set `compile.ial_git_branch` to `develop`
3. Generate a case named `IAL_develop_compile`
4. Start the compilation suite (`CompilationSuiteDefinition`) because `-d` (equivalent to `--dry-run` is not passed as an argument)

## What the command does

The `compile` subcommand is a specialization of `tactus case`. It:

* Sets `compile.ial_git_branch` from `--ial-tag` (default: `develop`)
* Always merges the following modification files on top of the user-supplied config:
  * `tactus/data/config_files/modifications/@HOST@.toml`
  * `tactus/data/config_files/modifications/compile_suite.toml`
* Forwards everything else (output path, start-suite flag, keep-def-file, expand-config) to `tactus case`

## Required configuration

For the compilation suite to run end-to-end, the following keys are read by the task classes documented below:

| Key | Used by | Notes |
| --- | --- | --- |
| `compile.ial_git_repo` | `IALClone` | Required if cloning IAL |
| `compile.ial_git_branch` | `IALClone`, macro `@IAL_TAG@` | Set via `--ial-tag` |
| `compile.git_token` | `IALClone`, `TactusBundleCreate` | Optional; enables HTTPS token auth |
| `compile.ial_dir` | `IALClone`, `TactusBundleCreate` | Local IAL checkout path |
| `compile.bundle_file` | `TactusBundleCreate` | ECBundle YAML |
| `compile.dir` | `TactusBundleCreate`, `TactusBundleBuild` | Bundle working directory |
| `compile.arch` | `TactusBundleBuild` | Build architecture |

See the sections below for the full configuration surface of each task.

---

# Bundle Compilation Tasks

This module provides three compilation-related task classes:

* `IALClone` — clones the IAL (IFS/Arpege Library) Git repository
* `TactusBundleCreate` — creates or updates an ECBundle source bundle
* `TactusBundleBuild` — builds the bundle and optionally caches compiled artifacts

These tasks are designed to work within the Tactus framework and use `ecbundle` for source management and compilation orchestration.

---

# Overview

The workflow is typically:

1. **Clone IAL repository** (optional)

   * Fetch IAL sources from a Git repository onto a local directory
2. **Create/update the bundle**

   * Clone/update repositories defined in a bundle YAML
   * Optionally merge in local bundle overrides
3. **Build the bundle**

   * Configure and compile all sources
   * Install binaries into the configured install directory
   * Optionally reuse cached builds

---

# Tasks

## `IALClone`

Clones the IAL Git repository into a local directory and checks out the configured branch.

### Purpose

* Clones the IAL source repository
* Checks out the configured branch
* Skips cloning if the target directory already exists

### Configuration Keys

| Key                       | Description                                            |
| ------------------------- | ------------------------------------------------------ |
| `compile.ial_git_repo`    | URL of the IAL Git repository (supports `[TOKEN]` placeholder) |
| `compile.ial_git_branch`  | Branch to check out after cloning                      |
| `compile.git_token`       | Git token substituted into `[TOKEN]` placeholder       |
| `compile.ial_dir`         | Local destination directory for the IAL clone          |

### Token Substitution

If the repository URL contains the placeholder `[TOKEN]`, it is replaced with the value of `compile.git_token` before cloning. This allows the token to be embedded into HTTPS Git URLs, e.g.:

```text
https://[TOKEN]@github.com/ecmwf/ial.git
```

becomes:

```text
https://<actual-token>@github.com/ecmwf/ial.git
```

### Behavior

* If `compile.ial_dir` already exists: the clone step is skipped and an info message is logged.
* Otherwise the task executes:

```bash
git clone <ial_git_repo> <ial_dir>
cd <ial_dir>; git checkout <ial_git_branch>
```

---

## `TactusBundleCreate`

Creates or updates an ECBundle source tree.

### Purpose

* Reads the configured bundle YAML
* Optionally merges in an update bundle YAML (for local IAL overrides or other customizations)
* Executes:

```bash
ecbundle create
```

---

### Configuration Keys

| Key                          | Description                                                                                  |
| ---------------------------- | -------------------------------------------------------------------------------------------- |
| `compile.dir`                | Directory where bundle sources are created (defaults to `@CASEDIR@/bundle`)                  |
| `compile.git_token`          | Optional GitHub token                                                                        |
| `compile.bundle_file`        | ECBundle YAML file (defaults to `@TACTUS_HOME@/data/compilation/@CYCLE@/bundle.yml`)         |
| `compile.bundle_update`      | If `True`, merges an additional update YAML on top of the base bundle file                   |
| `compile.update_bundle_file` | YAML file used to override/extend the base bundle when `compile.bundle_update` is enabled    |
| `compile.ial_dir`            | Optional local IAL source override (exported as `IAL_DIR` environment variable)              |

---

### Bundle Update Mechanism

When `compile.bundle_update` is enabled, the task:

1. Loads the original bundle YAML
2. Loads the update bundle YAML
3. Merges them via `merge_dicts(..., overwrite=True, remove_none=True)`:

   * `overwrite=True` — values in the update file override values in the original
   * `remove_none=True` — keys explicitly set to `None` in the update file are removed from the merged result
4. Writes the merged result to:

```text
@CASEDIR@/bundle-local-ial.yaml
```

YAML formatting is preserved using `ruamel.yaml` with:

* `preserve_quotes = True`
* indentation: mapping=4, sequence=4, offset=2
* line width: 4096

Example transformation:

#### Before (original)

```yaml
ial-source:
  git: github.com/ecmwf/ial.git
  version: 1.2.0
```

#### Update YAML

```yaml
ial-source:
  git: ~
  version: ~
  dir: /path/to/local/ial
```

#### After (merged)

```yaml
ial-source:
  dir: /path/to/local/ial
```

---

### IAL_DIR Environment Variable

Before invoking `ecbundle`, the task exports:

```bash
IAL_DIR=<substituted compile.ial_dir>
```

This allows the bundle YAML to reference `${IAL_DIR}` for local IAL source overrides.

---

### Git Authentication

#### SSH Mode (default)

If no Git token is provided:

```python
os.environ["GITHUB"] = "git@github.com:"
```

Repositories are cloned using SSH access.

#### Token Mode

If `compile.git_token` is set:

```bash
--github-token <TOKEN>
```

is passed to `ecbundle`.

> [!WARNING]
> Updating the remote repository while keeping the same branch/version may fail if the local branch is already tracking a different remote.
>
> Example error:
>
> ```text
> + git remote add eeec494cba20d4c7ae560cc38b7a8b14 git@github.com:/uandrae/IAL
> ERROR: Branch feature/toolchain-flags was already tracking origin/feature/toolchain-flags. Manual intervention needed.
> ERROR: Could not download or update ial-source ...
> ```
>
> This happens because Git refuses to change the upstream tracking configuration automatically when the branch already tracks another remote.
>
> In this case, remove the existing source directory or manually reconfigure the branch tracking before rerunning the bundle creation step.

---

### Generated Command

```bash
cd <compile_dir>; ecbundle create [--github-token <TOKEN>] --bundle <bundle_file> --update
```

---

## `TactusBundleBuild`

Builds an ECBundle source tree.

### Purpose

* Builds source repositories produced by `TactusBundleCreate`
* Supports cached builds keyed by a deterministic source hash
* Supports multiple architectures
* Supports precision selection (`prec` / `R32`)
* Supports Ninja builds
* Supports clean rebuilds
* Backs up the resolved `bundle.yml` next to the build output

---

### Configuration Keys

| Key                   | Description                                                                                  |
| --------------------- | -------------------------------------------------------------------------------------------- |
| `compile.dir`         | Bundle source directory (output of `TactusBundleCreate`)                                     |
| `compile.arch`        | Build architecture configuration (defaults to `source/ial-source/bundle/arch/ecmwf/hpc2020`) |
| `compile.ninja`       | Enable Ninja builds (defaults to `false`)                                                    |
| `compile.skip_build`  | Skip build if install already exists (defaults to `false`)                                   |
| `compile.clean_build` | Clean build directory before compiling (defaults to `false`)                                 |
| `compile.cache`       | Enable cached builds (defaults to `false`)                                                    |
| `compile.cache_dir`   | Cache storage directory (defaults to `@REFERENCE_DATA@/bundle_cache`)                        |
| `task.args.prec`      | Precision selector: `prec` (double) or `R32` (single). Defaults to `prec`.                   |

---

### Precision Modes

| Precision | Effect                                            |
| --------- | ------------------------------------------------- |
| `prec`    | Default double-precision build                    |
| `R32`     | Adds `--without-double-precision` to ecbundle    |

---

### Build Directories

The builder creates:

```text
build/<precision>
install/<precision>
```

Examples:

```text
build/prec
install/prec
```

or:

```text
build/R32
install/R32
```

---

### Bundle Backup

Before building, the task attempts to copy:

```text
<bundle_dir>/source/bundle.yml
```

to the resolved compile/cache directory as `bundle.yml`. This preserves a snapshot of what was actually built. If the source file is missing, the failure is logged and execution continues.

---

### Cached Builds

If `compile.cache` is enabled, a deterministic hash is generated from:

* repository commit hashes
* repository dirty state

This allows identical sources to reuse a previously installed build.

The architecture component of the cache path is derived from the symlink:

```text
<bundle_dir>/<arch>/default
```

If present, it is resolved and the portion after `arch` is used as the cache subpath. Otherwise the raw arch directory is used.

---

### Bundle Hashing

The method:

```python
get_bundle_hash(source_dir)
```

creates a SHA256 hash from a deterministic JSON manifest:

```json
{
  "repositories": {
    "<repo>": {
      "commit": "<sha>",
      "dirty": false
    }
  },
  "dirty": false
}
```

The JSON is serialized with `sort_keys=True` and compact separators to guarantee reproducibility.

---

#### Dirty Repositories

A repository is considered dirty if it contains:

* modified files
* staged changes
* untracked files

When any repository is dirty, the hash is suffixed:

```text
<hash>-dirty
```

Non-Git directories inside `source/` are skipped (logged as `[SKIP]`).

If the `source` directory itself is missing, the hash falls back to:

```text
unknown
```

---

### Cache Layout

Cached builds are stored as:

```text
<cache_dir>/<arch>/<bundle_hash>/
├── bundle.yml
├── install/<precision>/
└── build/<precision>/
```

Example:

```text
/cache/linux-gnu/4bc1b3.../install/prec
```

---

### Symlink Management

When cache mode is enabled, the local install path:

```text
@CASEDIR@/install/<precision>
```

becomes a symlink to the cached install directory. Existing symlinks at this location are removed and recreated on each run.

Example:

```text
install/prec -> /cache/linux/<hash>/install/prec
```

---

### Build Command

The generated build command is:

```bash
cd <bundle_dir>; ecbundle build \
  --arch <arch> \
  [--ninja] \
  --forecast-only \
  [--clean] \
  [--without-double-precision] \
  --install-dir=<install_dir> \
  --install \
  --build-dir=<build_dir>
```

Optional flags:

| Option                       | Trigger                    |
| ---------------------------- | -------------------------- |
| `--ninja`                    | `compile.ninja=True`       |
| `--clean`                    | `compile.clean_build=True` |
| `--without-double-precision` | `precision == "R32"`       |

---

### Skip Build Logic

If:

```python
compile.skip_build == True
```

and:

```text
<install_dir>/MASTERODB
```

exists, compilation is skipped. The cache symlink is still (re)created so the local install path points to the existing cached binaries.

---

# Notes

* Only Git repositories contribute to the bundle hash; non-Git directories are silently skipped
* Cached builds are architecture-specific and precision-specific
* Symlinks at `@CASEDIR@/install/<precision>` are recreated on every run when caching is enabled
* The `ecbundle` binary is resolved as `<python-bin-dir>/ecbundle`, i.e. it must be installed in the same environment as Tactus
* `IAL_DIR` is always exported from `compile.ial_dir`, regardless of whether `bundle_update` is enabled, so bundle YAMLs can rely on it being set
