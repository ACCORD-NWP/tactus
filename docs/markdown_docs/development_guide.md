# Development guidelines
Describes best practices and guidelines for development in the Deode-Workflow repository.

## Best practices
- Assignee should not merge before the PR has been reviewed and approved (see below on how to request reviewer(s)).
- Assignee and PR author refer to the same person by default.
- Assignee is responsible for merging.
- Assignee and reviewer may not be the same person.
- Assignee is responsible for ensuring that the PR is up to date with the base branch before merging.
- Assignee should not merge PRs that have failing tests.
- Commits to `master` and `develop` branches should only be via PRs.
- We rely on the good sense of both assignee and reviewer.

### Requesting and accepting/declining reviews
- To request a review of a PR, the assignee invites one or more persons to review.
- The invited reviewers accept/decline the invitation by adding a comment to the PR like “I’m on it” or “I’ll not review”.
- After 48 hours, the assignee removes the reviewers that haven't responded.
- Repeat, if none of the invited reviewers were able to review. Minimum one reviewer is required.


### Checklist for assignee
- ✔️ Make sure your local environment is correctly initialised as described in the [README](https://github.com/destination-earth-digital-twins/Deode-Workflow/blob/develop/README.md) file.
- ✔️ Use forks for your changes
- ✔️ If not up-to-date, update your fork with the changes from the target branch (use `pull` with `--rebase` option if possible).
- ✔️ Describe what the PR contains.
- ✔️ Ensure your PR does not contain separate features.

### Checklist for reviewers
Each PR comes with its own improvements and flaws. The reviewer should check the following:
- ✔️ Is the code readable?
- ✔️ Is the code well tested? (Look at the coverage report)
- ✔️ Is the code documented?
- ✔️ Is the code easy to maintain?

### Checklist for assignee after completed review
- ✔️ Is the PR up to date with the base branch?
- ✔️ Are the tests passing?
- ✔️ Have the reviewers who accepted to review approved the PR?
- ✔️ Is the PR ready to be merged?
- ✔️ Squash commits and merge the PR.

## Local testing
No-one likes to wait for the CI to run tests. It is therefore recommended to run tests locally before pushing to the remote repository, and before creating a PR, but no one will force you to do this: how you work locally is entirely up to you.

For convenience, however, we have added a few commands you can use to check that the code is linted, the tests pass, etc. Some of these are exemplified in the next subsections. Please run **inside of your poetry shell**:
```shell
poetry devtools -h
```
for more information.

Note: devtools lint does not currently work for Python >=3.12 (as it depends on flakeheaven)

### Run and fix toml-formatter errors in place
```shell
toml-formatter check --fix-inplace /PATH/TO/FILE
```

### Run linters and exit with an error if non-linted code is detected
```shell
poetry devtools lint
```

### Run linters and **attempt** to fix eventually encountered errors
```shell
poetry devtools lint --fix
```
This will stop with an error if the encountered issues cannot be fixed.

### Runthe typical checks for things you need to fix prior to a push
```shell
poetry devtools pre-push-checks
```

### Run tests
Tests will run as if they were on current platform, if recognized. If the platform is not recognized a bogus plaform `pytest` is used as defined under `tests/include`. To force the tests to run as on the `pytest` platform export `DEODE_HOST=pytest` before running pytest. Run the tests with
```shell
pytest
```
or
```shell
poetry devtools pytest
```

### Generate and view the documentation to be published to our [docpages](https://destination-earth-digital-twins.github.io/deode-workflow-docs/)

```shell
poetry devtools doc clean
poetry devtools doc build
poetry devtools doc view
  ```
or, combining them all:
```shell
poetry devtools doc
```

## Testing on Atos

The testing procedure above does not test the full functionality together with the IAL code. While waiting for a automated CI/CD system to be in place a few manual steps are required on atos to check the functionality.

- ✔️ Run the default config file, using CY48t3, under ecflow following the instructions in the [README](https://github.com/destination-earth-digital-twins/Deode-Workflow/blob/develop/README.md) file.
- ✔️ Run the following sequence of case configurations, i.e. the three CSC's.
```
for case in \
  cy48t3_arome \
  cy48t3_alaro \
  cy46h1_harmonie_arome \
  cy49t2_arome \
  cy49t2_alaro \
  ; do
  deode case ?deode/data/config_files/configurations/$case deode/data/config_files/modifications/test_settings.toml --start-suite
done
```
Once this has completed test the coupling of AROME -> AROME and HARMONIE-AROME -> HARMONIE-AROME:
```
for case in \
  cy48t3_arome_target \
  cy46h1_harmonie_arome_target \
  ; do
  deode case ?deode/data/config_files/configurations/$case deode/data/config_files/modifications/test_settings.toml --start-suite
done
```
- ✔️ Finally test the stand alone task for the forecast following the instructions in the [README](https://github.com/destination-earth-digital-twins/Deode-Workflow/blob/develop/README.md) file.

## Testing on lumi

On lumi we expect the following configurations to be tested in the same way as above:
```
  cy48t3_arome
  cy48t3_alaro
  cy48t3_alaro_gpu_lumi
  cy46h1_harmonie_arome
```
Note that due to the restrictions for the debug partition on lumi it's only possible to launch one suite at the time.

## Git Branching Structure and Workflow

This section describes the Git branching and tag structure used when developing. In the following we differentiate between **branches in the upstream repository** and **branches in developers' forks**. Forks are used to develop features, bugfixes etc. They are created from the upstream's `develop` branch by forking to a local repo.

### Branching Structure

#### Branches in the Upstream Repository
These branches are centralized and exist in the upstream repository. The branches are never deleted.

**`master` branch**:
   - Represents the production-ready codebase.
   - Contains tagged releases (e.g. `v0.1`, `v0.2`).
   - Hotfixes merged into this branch represent stable updates to deployed versions.

**`develop` branch**:
   - Serves as the main branch for development integration.
   - Receives completed features, bug fixes, and hotfixes from developers' forks.

**`legacy-support/vX.Y` branches** (used only when necessary):
   - Created for maintaining older versions (e.g. `v0.1`) when newer releases (e.g. `v0.2`) are already available.
   - Hotfixes targeting older versions are merged here and tagged (e.g. `v0.1.1`).

#### Branches in Developers' Forks
These branches are created and reside in developers' personal forks of the upstream repository. They are temporary and merged back into the upstream repository when work is completed.

**`feature/<name>` branches**:
   - Created for new feature development.
   - Based on the `develop` branch.
   - Merged back into the upstream's `develop` branch upon completion.

**`bugfix/<name>` branches**:
   - Used for fixing non-critical bugs.
   - Based on the `develop` branch.
   - Merged back into the upstream's `develop` branch upon completion.

**`hotfix/<name>` branches**:
   - For critical fixes to production releases.
   - Based on a tag on `master` branch.
   - Merged back into `master` in the upstream repository.

**`release/vX.Y.Z` branches**:
   - Created to prepare a new release.
   - Based on the `develop` branch.
   - Merged back into the upstream's `develop` branch upon completion.

**`binary-update/vX.Y.Z` branches**:
   - Created to update the binary versions used by the specific version (`vX.Y.Z`) of Deode-Workflow .
   - Based on the `develop` branch.
   - Merged back into `master` in the upstream repository.

<br>

*Git branching structure with hotfix to the latest release:*

![git_branch_structure](/figs/git_branch_structure.svg)

*Git branching structure with hotfix to an older release:*
![git_branch_structure](/figs/git_branch_structure_legacy_support.svg)

### Workflow

If you prefer to branch out from your local fork's `develop`, remember to synchronize your fork's `develop` with `upstream/develop` before starting any of the below workflows.

To be able to work with branches in the upstream repository from within your local fork, you can add the upstream as a remote:
```bash
git remote add upstream <upstream_repository_URL>
```
This is e.g. relevant, when tagging a new release on the upstream's `master` branch (see below), or if you prefer to branch out from the upstream's `develop` branch. In the latter case, please replace `origin/develop` with `upstream/develop` in below commands.

#### Developing New Features
  1. In your fork, create a new `feature/<name>` branch based on `develop`:
     ```bash
     git checkout -b feature/<name> origin/develop
     ```
  2. Implement and commit your changes.
  3. Push the branch to your fork and create a pull request to merge it into the upstream's `develop` branch. Follow the instructions in the PR.

#### Fixing Bugs
  1. In your fork, create a new `bugfix/<name>` branch based on `develop`:
     ```bash
     git checkout -b bugfix/<name> origin/develop
     ```
  2. Implement the bug fix and commit your changes.
  3. Push the branch to your fork and create a pull request to merge it into the upstream's `develop` branch. Follow the instructions in the PR.

#### Creating a Hotfix
 1. Make sure you have fetched any new tags by calling
      ```bash
      git fetch --all --tags
      ```
 2. In your fork, create a `hotfix/<name>` branch based on the desired tag from the upstream repository:
     ```bash
     git checkout -b hotfix/<name> <tag>
      ```
 3. Implement the fix and push the branch to your fork.
 4. Follow step 3-5 from the section "Creating a New Release" to adjust version numbers and changelog.
 5. 
    a. Most likely scenario: If the issue affects the latest release (even if created by a previous release):

     1. Create a pull request to merge the hotfix into the upstream's `master` branch. Follow the best practices on creating a PR, requesting review and merging as outlined in the PR instructions.

    b. If the issue affects an older release, which needs continuous support:

     3. Create a `legacy-support/vX.Y` branch in the upstream repository based on the tag of the older release (e.g. `legacy-support/v0.1`).
     4. Create a pull request to merge the hotfix into the upstream's `legacy-support/vX.Y` branch. Follow the instructions in the PR.
 6. After merging, follow the instructions in the section "Creating a New Release" starting from 9. to create a new release.

#### Creating a New Release
1. Ensure your local fork is up to date with the upstream repository.
2. In your fork, create a new `release/vX.Y.Z` branch based on `develop`:
    ```bash
    git checkout -b release/vX.Y.Z origin/develop
    ```
3. Update the `CHANGELOG.md` with a new **[vX.Y.Z] YYYY.mm.dd** section containing all changes that are released.
4. Update the version number in the `pyproject.toml` file.
5. Commit the changes:
    ```bash
    git add CHANGELOG.md pyproject.toml
    git commit -m "Prepare release vX.Y.Z"
    ```
6. Push the branch to your fork:
    ```bash
    git push origin release/vX.Y.Z
    ```
7. Create a pull request to merge the `release/vX.Y.Z` branch into the upstream's `develop` branch. Follow the instructions in the PR.
8. When the PR to develop has been merged, determine if the release requires updates of binary versions

   a. If yes:
      1. Create a `binary-update/vX.Y.Z` branch in your fork based on the upstream's `develop` branch.
      2. Implement the necessary changes and push the branch to your fork.
      3. Create a pull request to merge the `binary-update/vX.Y.Z` branch into the upstream's `master` branch. Follow the instructions in the PR.

   b. If no:

      1. Create a pull request to merge the upstream's `develop` branch into the upstream's `master` branch. Follow the instructions in the PR.
9. Once the pull request is approved and merged, create a new tag on the `master` branch, while still in your own fork:
    ```bash
    git checkout master
    git pull upstream master
    git tag -a vX.Y.Z -m "Release vX.Y.Z"
    git push upstream vX.Y.Z
    ```
10. Create a new release in GitHub
    1.  The release shall be based on the just created tag.
    2.  Click the 'Releases' section to the right on the repository's homepage near the bottom.
    3.  Click 'Draft a new release'.
    4.  Choose the relevant tag and set target branch to 'master'.
    5.  Set the name of the tag as the title of the release.
    6.  Copy and paste the changelog items relevant to this release into the description of the release.
    7.  Set the release as the latest release.
    8.  Publish the release.

11. If the release is not a legacy-support hotfix release
    1.  Create a pull request to merge the master branch back into the develop branch. Follow the instructions in the PR.
    
    > :warning: **IMPORTANT**: When merging, revert the binary updates implemented in 8.a above, as the develop branch should always refer to the `latest` tag of binaries.


### Summary of Branch Responsibilities

| **Branch Type**              | **Location**               | **Purpose**                                                                                       |
|------------------------------|----------------------------|---------------------------------------------------------------------------------------------------|
| `master`                     | Upstream                   | Holds production-ready releases, tagged with version numbers.                                     |
| `develop`                    | Upstream                   | Main branch for integrating features and bug fixes.                                               |
| `legacy-support/vX.Y`        | Upstream                   | For maintaining older versions and applying hotfixes to legacy releases.                          |
| `feature/<name>`             | Developers' Fork           | For developing new features. Based on `develop`.                                                  |
| `bugfix/<name>`              | Developers' Fork           | For resolving bugs found during development. Based on `develop`.                                  |
| `hotfix/<name>`              | Developers' Fork           | For critical fixes to released versions. Based on `master`.|
| `release/vX.Y.Z`          | Developers' Fork           | To prepare a new release. Based on `develop`.|
| `binary-update/vX.Y.Z`    | Developers' Fork           | To update binary versions, when releasing a new release to master. Based on `develop`.|
