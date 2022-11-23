# Development guidelines
Describes best practices and guidelines for development in the Deode-Prototype repository.

## Best practices
- Authors of PRs should not merge themself - Instead request a reviewer and an assignee. Assignee is responsible for merging.
- Assignee and reviewer may be the same person.
- Authors of PRs are responsible for ensuring that the PR is up to date with the base branch before merging.
- Assignees should not merge PRs that have failing tests.
- Commits to `master` and `develop` branches should only be via PRs.
- We rely on the good sense of both author and reviewer.

### Checklist for authors
- ✔️ If not up-to-date, pull in the target branch.
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
No-one likes to wait for the CI to run tests. It is therefore recommended to run tests locally before pushing to the remote repository, and before creating a PR, but no one is enforced to this. How you work locally is entirely up to you.

Here is a list of tests being run in the pipeline, that you can run locally from the root of the repo.

**Linting:**
- `flakeheaven lint`

**Tests:**
- `pytest --cov=./ tests/` *(In the pipeline we run multiple versions of python)*



## Branches
As of now, the repository has two main branches:
- `master` - This is the main branch. It is the branch that is deployed to production.
- `develop` - This is the branch that is deployed to staging.
As the project grows, we may add more branches, such as an `integration` branch, where we can test the integration of multiple features before merging them to `develop`, and run a simpler pipeline (see image below).

<img src="development_guide.png" alt="drawing" width="400"/>

### Forks
Forks are used to develop features and bug fixes. They are created from the `develop` branch by forking to a local repo. When a feature is ready, a PR is created to merge it to `develop`. When a bug fix is ready, a PR is created to merge it to `develop` and `master`.

## Run on LUMI
To run the project on LUMI, you need to have a LUMI account and add your SSH key to it. You can find instructions on how to do that [here](https://docs.lumi-supercomputer.eu/firststeps/SSH-keys/). Once you have your SSH key added, you can login to LUMI by running `ssh -i <your-private-key> <username>@@lumi.csc.fi`. Once logged in create a SSH key and add that to your GitHub account, so you can clone the repo. Then follow these steps (some of which can be put in your `.profile`)
```sh
# Clone the repo
git clone git@github.com:DEODE-NWP/Deode-Prototype.git

# Load modules
module load LUMI/22.06
module load cray-python/3.9.12.1

# Install poetry
curl -sSL https://install.python-poetry.org | python -
export PATH="/users/$USER/.local/bin:$PATH"

# Install project
cd Deode-Prototype && poetry install
# Poetry will create a virtual environment for the project and install all dependencies in this environment. 
# It will be placed in something like: "/users/$USER/.cache/pypoetry/virtualenvs/<deode-<hash>>/bin/:$PATH"

# To activate the environment run 
poetry shell

# Check that the project runs
deode -h

# Run a stand-alone task with an example config file
deode -config_file $PWD/docs/task_config_example.toml run --task Forecast --template $PWD/ecf/stand_alone.py --job $PWD/test.job --submit background.json --troika_config $PWD/config.yml -o $PWD/test.log
```
