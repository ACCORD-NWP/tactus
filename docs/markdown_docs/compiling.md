# Running a Compilation

Tactus exposes a dedicated `compile` subcommand that builds a configuration aimed at compiling IAL and starts (optionally) the compilation suite.

## Quick start

```
tactus compile --ial-tag develop
```

This will:

1. Build a config from `config.toml` + host-specific overrides + `compile_suite.toml`
2. Set the compile task's IAL git version to `develop`
3. Generate a compilation case (named from `submission.ial_version`, see callout below)
4. Start the compilation suite (`CompilationSuiteDefinition`) because `-d` (equivalent to `--dry-run`) is not passed as an argument

## What the command does

The `compile` subcommand is a specialization of `tactus case`. It:

* Sets `ial.compile.ial_git_version` from `--ial-tag` (default: `develop`)
* Sets `ial.compile.ial_git_repo` from `--ial-repo`, when provided
* Always merges the following modification files on top of the user-supplied config:
  * `tactus/data/config_files/modifications/@HOST@.toml`
  * `tactus/data/config_files/modifications/compile_suite.toml`
* Forwards everything else (output path, start-suite flag, keep-def-file, expand-config) to `tactus case`


## Required configuration

For the compilation suite to run end-to-end, the following keys are read by the task classes documented below:

| Key | Used by | Notes |
| --- | --- | --- |
| `compile.ial_git_repo` | `IALClone` | Required if cloning IAL |
| `compile.ial_git_version` | `IALClone`, macro `@IAL_GIT_TAG@` | Branch/tag/commit hash to check out; intended to be set via `--ial-tag` (see known issue above) |
| `compile.git_token` | `IALClone`, `TactusBundleCreate` | Optional; enables HTTPS token auth |
| `compile.ial_dir` | `IALClone`, `TactusBundleCreate` | Local IAL checkout path |
| `compile.arch_dir` | `TactusBundleCreate` | Passed to `ecbundle create --arch-dir` |
| `compile.bundle_file` | `TactusBundleCreate` | ECBundle YAML |
| `compile.dir` | `TactusBundleCreate`, `TactusBundleBuild` | Bundle working directory |
| `compile.install` | `TactusBundleBuild` | Whether to install into the shared `@INSTALL_DIR@` tree |
| `compile.ial_version` | `TactusBundleBuild` (when `compile.install` is enabled) | Names the shared install location — see callout under `TactusBundleBuild` |
| `submission.arch` | `TactusBundleBuild`, macro `@ARCH@` | Build architecture |
| `submission.compiler` | `TactusBundleBuild` (via `@COMPILER@`), macro `@COMPILER@` | Compiler used to build IAL (`intel` / `gnu` / `""`) |

See the sections below for the full configuration surface of each task.

---

# Bundle Compilation Tasks

This module provides three compilation-related task classes:

* `IALClone` — clones the IAL (IFS/Arpege Library) Git repository
* `TactusBundleCreate` — creates or updates an ECBundle source bundle
* `TactusBundleBuild` — builds the bundle and optionally installs it into a shared, versioned location

These tasks are designed to work within the Tactus framework and use `ecbundle` for source management and compilation orchestration.

---

# Overview

The workflow is typically:

1. **Clone IAL repository** (optional)

   * Fetch IAL sources from a Git repository onto a local directory
   * Check out the configured git version/branch/commit
2. **Create/update the bundle**

   * Clone/update repositories defined in a bundle YAML
   * Optionally merge in local bundle overrides
3. **Build the bundle**

   * Configure and compile all sources
   * Install binaries either locally (per-experiment) or into a shared, versioned install tree

---

# Tasks

## `IALClone`

Clones the IAL Git repository into a local directory and checks out the configured version.

### Purpose

* Clones the IAL source repository
* Checks out the configured version/branch/commit
* Skips cloning if the target directory already exists

### Configuration Keys

| Key                        | Description                                            |
| --------------------------- | ------------------------------------------------------ |
| `compile.ial_git_repo`      | URL of the IAL Git repository (supports `[TOKEN]` placeholder) |
| `compile.ial_git_version`   | Branch/tag/commit checked out after cloning             |
| `compile.git_token`         | Git token substituted into `[TOKEN]` placeholder       |
| `compile.ial_dir`           | Local destination directory for the IAL clone          |

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
* Otherwise the task clones the repository:

```bash
git clone <ial_git_repo> <ial_dir>
```

* In **either case**, the task then always runs a checkout of the configured version:

```bash
cd <ial_dir>; git checkout <ial_git_version>
```

> The checkout runs unconditionally, even when the directory already existed from a previous run, so it also serves to move an existing checkout onto a newly requested `compile.ial_git_version`.

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
| `compile.arch_dir`           | Directory where arch files can be found; passed to `ecbundle create --arch-dir`               |
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
cd <compile_dir>; ecbundle create [--github-token <TOKEN>] --bundle <bundle_file> --update --arch-dir <arch_dir>
```

---

## `TactusBundleBuild`

Builds an ECBundle source tree, either as a local per-experiment install or into a shared, versioned install tree.

### Purpose

* Builds source repositories produced by `TactusBundleCreate`
* Supports multiple architectures and compilers
* Supports precision selection (`prec` / `R32`)
* Supports Ninja builds
* Supports clean rebuilds
* Optionally installs into a shared `@INSTALL_DIR@` tree with a `latest` pointer, instead of a purely local per-experiment install
* Backs up the resolved `bundle.yml` next to the build output

---

### Configuration Keys

| Key                      | Description                                                                                    |
| ------------------------ | ------------------------------------------------------------------------------------------------ |
| `compile.dir`            | Bundle source directory (output of `TactusBundleCreate`)                                         |
| `compile.install`        | Install into the shared `@INSTALL_DIR@` tree instead of only locally (defaults to `false`)       |
| `compile.ial_git_version`| Names the shared install location when `compile.install` is enabled — see callout below          |
| `compile.ninja`          | Enable Ninja builds (defaults to `false`)                                                        |
| `compile.skip_build`     | Skip build if install already exists (defaults to `false`)                                       |
| `compile.clean_build`    | Clean build directory before compiling (defaults to `false`)                                     |
| `submission.arch`        | Build architecture configuration (defaults to `@COMPILER@/default`)                              |
| `submission.compiler`    | Compiler used to build IAL: `intel`, `gnu`, or `""` (defaults to `intel`)                         |
| `task.args.prec`         | Precision selector: `prec` (double) or `R32` (single). Defaults to `prec`.                       |

---

### Precision Modes

| Precision | Effect                                            |
| --------- | ------------------------------------------------- |
| `prec`    | Default double-precision build                    |
| `R32`     | Adds `--without-double-precision` to ecbundle    |

---

### Install Modes

`TactusBundleBuild` supports two install layouts, controlled by `compile.install`:

* **Local install** (`compile.install = false`, the default): binaries are built straight into `@CASEDIR@/install/<precision>`. Nothing outside the case directory is touched.
* **Shared install** (`compile.install = true`): binaries are installed into a shared, version-named tree under `@INSTALL_DIR@`, and the local `@CASEDIR@/install/<precision>` becomes a symlink pointing at that shared location. This lets multiple experiments/cases reuse the same compiled binaries for a given install version (see the callout above regarding which key actually supplies that version name).

#### Shared install layout

When `compile.install` is enabled, the shared install root is:

```text
@INSTALL_DIR@/<compile.ial_version>/<precision>/<compiler>
```

and the actual install directory adds an architecture-derived subpath on top (see **Install Subpath** below):

```text
@INSTALL_DIR@/<compile.ial_version>/<precision>/<compiler>/<install_subpath>
```

A `latest` pointer is also maintained, pointing at the version root (not the full precision/compiler/subpath path):

```text
@INSTALL_DIR@/latest -> @INSTALL_DIR@/<compile.ial_version>
```

---

### Install Subpath

`get_install_subpath()` computes an additional path segment appended to the shared install root, so that different architecture/compiler combinations under the same bundle don't collide:

1. It resolves `<compile.dir>/source/arch/<submission.arch>` — following it as a symlink if it is one.
2. It looks for `submission.compiler` among the components of the resolved path.
3. If found, everything **after** that component is returned as the subpath.
4. If `submission.compiler` does not appear anywhere in the resolved arch path, the method returns `None` — which will produce an invalid install path, so `submission.compiler` must correspond to an actual segment of the resolved `submission.arch` path (e.g. `submission.arch` defaults to `@COMPILER@/default`, which already embeds the compiler name).

---

### Build Directories

The builder creates:

```text
build/<precision>
```

under `@CASEDIR@`, regardless of install mode. The install directory (`exp_bindir`) depends on `compile.install`:

```text
@CASEDIR@/install/<precision>                                                       # local install
@INSTALL_DIR@/<compile.ial_version>/<precision>/<compiler>/<install_subpath>        # shared install
```

---

### Bundle Backup

Before building, the task attempts to copy:

```text
<bundle_dir>/source/bundle.yml
```

to `@CASEDIR@/bundle.yml`. This preserves a snapshot of what was actually built. If the source file is missing, the failure is logged and execution continues.

---

### Pre-build Cleanup

If the build is not being skipped, any existing content at `@CASEDIR@/install/<precision>` is removed before building: it is unlinked if it's a symlink, or fully removed with `shutil.rmtree` if it's a real directory. This runs regardless of install mode, and prevents a fresh build from silently mixing with a stale local install or a stale symlink from a previous run.

---

### Build Command

The build now actually runs (the invocation is no longer commented out). The command assembled and executed is:

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

| Option                       | Trigger                        |
| ---------------------------- | ------------------------------- |
| `--ninja`                    | `compile.ninja=True`            |
| `--clean`                    | `compile.clean_build=True`      |
| `--without-double-precision` | `precision == "R32"`            |

---

### Shared Install Symlinks (`make_install_arch_symlink`)

When `compile.install` is enabled, after building the task:

1. Calls `make_install_arch_symlink()`: if `<compile.dir>/source/arch/<submission.arch>` is itself a symlink (e.g. resolving the `default` segment of `submission.arch`), that same symlink (its target, not the resolved file) is copied into the shared install root as `<install_dir_root>/default`, replacing any existing `default` link there.
2. Refreshes `@INSTALL_DIR@/latest` to point at `@INSTALL_DIR@/<compile.ial_version>`, removing a stale symlink first if one exists.
3. Symlinks the local per-experiment install path `@CASEDIR@/install/<precision>` to the shared install directory computed above.

When `compile.install` is disabled, none of this runs — the local install path *is* the real build output, with no shared-tree bookkeeping.

---

### Skip Build Logic

If:

```python
compile.skip_build == True
```

and:

```text
<install_dir>/bin/MASTERODB
```

exists, the build step is skipped. The shared-install symlink bookkeeping (when `compile.install` is enabled) still runs regardless of `skip_build`, so the local install path is kept pointing at the right shared location.

---

# Notes

* This build path targets `ecbundle==2.5.0` (pinned in `pyproject.toml`)
* All compilation-related configuration lives under the flat `[compile]` section again — the nested `[ial]` / `[ial.compile]` schema explored at one point has been reverted; update any config files or macro references using `ial.*` keys back to `compile.*`
* `@COMPILER@` is sourced from `submission.compiler`, and `@ARCH@` from `submission.arch` — architecture/compiler selection now lives entirely under `submission.*`, separate from the `compile.*` git/build-process settings
* `@IAL_GIT_TAG@` (macro, sourced from `compile.ial_git_version`) is the git branch/tag/commit being compiled; `@IAL_VERSION@` (sourced from `submission.ial_version`) is the installed-binaries version name used elsewhere (e.g. in submission `bindir` defaults) — these are intentionally allowed to differ, since `submission.ial_version` can be `"latest"` with no corresponding git ref; see the known issues above for where this separation isn't fully connected yet
* Host-specific overrides (e.g. `modifications/compile_atos_bologna.toml`) can set `submission.bindir`, a task-specific `MASTERODB` bindir override, and `compile.arch_dir` for a given platform
* The `ecbundle` binary is resolved as `<python-bin-dir>/ecbundle`, i.e. it must be installed in the same environment as Tactus
* `IAL_DIR` is always exported from `compile.ial_dir`, regardless of whether `bundle_update` is enabled, so bundle YAMLs can rely on it being set
