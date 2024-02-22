# Development guidelines
Describes best practices and guidelines for development in the Deode-Prototype repository.

## Best practices
- Authors of PRs should not merge themself - Instead request a reviewer and an assignee. Assignee is responsible for merging.
- Assignee and reviewer may not be the same person.
- Authors of PRs are responsible for ensuring that the PR is up to date with the base branch before merging.
- Assignees should not merge PRs that have failing tests.
- Commits to `master` and `develop` branches should only be via PRs.
- We rely on the good sense of both author and reviewer.

### Checklist for authors
- ✔️  Make sure your local environment is correctly initialised as described in the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file.
- ✔️  Use forks for your changes
- ✔️ If not up-to-date, update your fork with the changes from the target branch (use `pull` with `--rebase` option if possible).
- ✔️ Describe what the PR contains.
- ✔️ Ensure your PR does not contain seperate features.

### Checklist for reviewers
Each PR comes with its own improvements and flaws. The reviewer should check the following:
- ✔️ Is the code readable?
- ✔️ Is the code well tested? (Look at the coverage report)
- ✔️ Is the code documented?
- ✔️ Is the code easy to maintain?

### Checklist for assignees
- ✔️ Is the PR up to date with the base branch?
- ✔️ Are the tests passing?
- ✔️ Is the PR ready to be merged?
- ✔️ Squash commits and merge the PR.

## Local testing
No-one likes to wait for the CI to run tests. It is therefore recommended to run tests locally before pushing to the remote repository, and before creating a PR, but no one will force you to do this: how you work locally is entirely up to you.

For convenience, however, we have added a few commands you can use to check that the code is linted, the tests pass, etc. Some of these are exemplified in the next subsections. Please run **inside of your poetry shell**:
```shell
poetry devtools -h
```
for more information.

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

### Generate and view the documentation to be published to our [docpages](https://destination-earth-digital-twins.github.io/deode-prototype-docs/)

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

- ✔️ Run the default config file, using CY48t3, under ecflow following the instructions in the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file.
- ✔️ Repeat the same using the config file for CY46h1 and CY48t3\_Alaro, i.e. `deode/data/config_files/config_[CY46h1|CY48t3_Alaro].toml`.
- ✔️ Test the nesting procedure for both CY46h1 and CY48t3 using the config files `deode/data/config_files/config_[CY46h1|CY48t3]_target.toml`. Both of the runs will use data from the first two tests.
- ✔️ Finally test the stand alone task for the forecast following the instructions in the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file.

## Branches
As of now, the repository has two main branches:
- `master` - This is the main branch. It is the branch that is deployed to production.
- `develop` - This is the branch that is deployed to staging.
As the project grows, we may add more branches, such as an `integration` branch, where we can test the integration of multiple features before merging them to `develop`, and run a simpler pipeline (see image below).

<img src="../figs/development_guide.png" alt="drawing" width="400"/>

### Forks
Forks are used to develop features and bug fixes. They are created from the `develop` branch by forking to a local repo. When a feature is ready, a PR is created to merge it to `develop`. When a bug fix is ready, a PR is created to merge it to `develop` and `master`.
