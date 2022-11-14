# Development guidelines
Describes best practices and guidelines for development in the Deode-Prototype repository.

## Best practices
- Authors of PRs should not merge themself - Instead request a reviewer and an assignee. Assignee is responsible for merging.
- Authors of PRs are responsible for ensuring that the PR is up to date with the base branch before merging.
- Assignees should not merge PRs that have failing tests.
- Assignee and reviewer may be the same person.
- Commits to `master` and `develop` branches should only be via PRs.

## Branches
As of now, the repository has two main branches:
- `master` - This is the main branch. It is the branch that is deployed to production.
- `develop` - This is the branch that is deployed to staging.
As the project grows, we may add more branches, such as an `integration` branch, where we can test the integration of multiple features before merging them to `develop`, and run a simpler pipeline (see image below).

![](development_guide.png)

### Forks
Forks are used to develop features and bug fixes. They are created from the `develop` branch by forking to a local repo. When a feature is ready, a PR is created to merge it to `develop`. When a bug fix is ready, a PR is created to merge it to `develop` and `master`.
