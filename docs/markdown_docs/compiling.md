# Bundle Compilation Tasks

This module provides two compilation-related task classes:

* `TactusBundleCreate` — creates or updates an ECBundle source bundle
* `TactusBundleBuild` — builds the bundle and optionally caches compiled artifacts

These tasks are designed to work within the Tactus framework and use `ecbundle` for source management and compilation orchestration.

---

# Overview

The workflow is typically:

1. **Create/update the bundle**

   * Clone/update repositories defined in a bundle YAML
2. **Build the bundle**

   * Configure and compile all sources
   * Install binaries into the configured install directory
   * Optionally reuse cached builds

---

# Tasks

## `TactusBundleCreate`

Creates or updates an ECBundle source tree.

### Purpose

* Reads the configured bundle YAML
* Optionally rewrites IAL source entries to use a local directory
* Executes:

```bash
ecbundle create
```

---

### Configuration Keys

| Key                   | Description                                |
| --------------------- | ------------------------------------------ |
| `compile.dir`         | Directory where bundle sources are created (defaults to `@CASEDIR@/bundle`)|
| `compile.git_token`   | Optional GitHub token                      |
| `compile.bundle_file` | ecbundle YAML file    (defaults to `@TACTUS_HOME@/data/compilation/@CYCLE@/bundle.yml`)                     |
| `compile.ial_dir`     | Optional local IAL source override         |

---

### Local IAL Override

If `compile.ial_dir` is configured:

* The bundle YAML is rewritten
* Any `ial-source` entries:

  * remove `git`
  * remove `version`
  * add `dir`

Example transformation:

#### Before

```yaml
ial-source:
  git: github.com/ecmwf/ial.git
  version: 1.2.0
```

#### After

```yaml
ial-source:
  dir: /path/to/local/ial
```

A temporary bundle file is written to:

```text
@CASEDIR@/bundle-local-ial.yaml
```

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

# `TactusBundleBuild`

Builds an ECBundle source tree.

---

## Purpose

* Builds source repositories
* Supports cached builds
* Supports multiple architectures
* Supports precision selection
* Supports Ninja builds
* Supports clean rebuilds


---

# Configuration Keys

| Key                   | Description                                                                                  |
| --------------------- | -------------------------------------------------------------------------------------------- |
| `compile.arch`        | Build architecture configuration (defaults to `source/ial-source/bundle/arch/ecmwf/hpc2020`) |
| `compile.ninja`       | Enable Ninja builds (defaults to `false`)                                                    |
| `compile.skip_build`  | Skip build if install already exists (defaults to `false`)                                   |
| `compile.clean_build` | Clean build directory before compiling (defaults to `false`)                                 |
| `compile.cache`       | Enable cached builds (defaults to `true`)                                                    |
| `compile.cache_dir`   | Cache storage directory (defaults to `@REFERENCE_DATA@/bundle_cache`)                        |

---

# Build Directories

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

# Cached Builds

If caching is enabled, a deterministic hash is generated from:

* repository commit hashes
* repository dirty state

This allows reuse of identical builds.

---

# Bundle Hashing

The method:

```python
get_bundle_hash(source_dir)
```

creates a SHA256 hash from:

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

---

## Dirty Repositories

If any repository contains:

* modified files
* staged changes
* untracked files

then:

```text
<hash>-dirty
```

is generated.

---

# Cache Layout

Cached builds are stored as:

```text
<cache_dir>/<arch>/<bundle_hash>/
```

Example:

```text
/cache/linux-gnu/4bc1b3.../
```

---

# Symlink Management

When cache mode is enabled:

```text
@CASEDIR@/install/<precision>
```

becomes a symlink to the cached install directory.

Example:

```text
install/prec -> /cache/linux/hash/install/prec
```

---

# Build Command

The generated build command is:

```bash
ecbundle build \
  --arch <arch> \
  --forecast-only \
  --install \
  --install-dir=<install_dir> \
  --build-dir=<build_dir>
```

Optional flags:

| Option                       | Trigger                    |
| ---------------------------- | -------------------------- |
| `--ninja`                    | `compile.ninja=True`       |
| `--clean`                    | `compile.clean_build=True` |
| `--without-double-precision` | `precision == "R32"`       |

---

# Skip Build Logic

If:

```python
compile.skip_build == True
```

and:

```text
<install_dir>/MASTERODB
```

exists, compilation is skipped.

---

# Notes

* Only Git repositories contribute to the bundle hash
* Non-Git directories are skipped
* Cached builds are architecture-specific
* Symlinks are recreated when switching cache targets
