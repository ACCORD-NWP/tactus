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
- ✔️ Ensure your PR does not contain seperate features.

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

## Branches
As of now, the repository has two main branches:
- `master` - This is the main branch. It is the branch that is deployed to production.
- `develop` - This is the branch that is deployed to staging.
As the project grows, we may add more branches, such as an `integration` branch, where we can test the integration of multiple features before merging them to `develop`, and run a simpler pipeline (see image below).

![drawing](../docs/figs/development_guide.png)

### Forks
Forks are used to develop features and bug fixes. They are created from the `develop` branch by forking to a local repo. When a feature is ready, a PR is created to merge it to `develop`. When a bug fix is ready, a PR is created to merge it to `develop` and `master`.
