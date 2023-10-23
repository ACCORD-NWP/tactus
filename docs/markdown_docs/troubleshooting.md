# Troubleshooting

Welcome to our troubleshooting tips. Before proceeding, please make sure you have carefully read the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file and, if applicable, the [development guide](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/docs/markdown_docs/development_guide.md).

## Cannot run `poetry`
Make sure your system fulfills all [system requirements](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md#system-requirements).
If you cannot execute `poetry` even so, you may need to reinstall and reconfigure it as instructed in the [README](https://github.com/destination-earth-digital-twins/Deode-Prototype/blob/develop/README.md) file.

## `Command not found` when trying to run `deode` or some related command
Try running `poetry update`.

## `ImportError` or `ModuleNotFoundError` when running the package's executable
Try running `poetry update`.

## `poetry update` or `poetry update` fail
Try removing the package's `.venv` directory and run the command again. If it still doesn't work, see ["Cannot run `poetry`"](#cannot-run-poetry) and try once more.

## Failing linting checks
You can run `poetry devtools lint --fix` locally to fix some of the linting issues. You will need to solve the remaining issues manually, but the output of the linting tools usually tell you what is wrong and which place in the code you should look.


## Failing tests
It is always recommended to run the test suite locally address any encountered issue *before* pushing to you pull request.

## Failing CI checks on github
Please run `poetry devtools pre-push-checks` and fix any eventually encountered error *before* you push your commits to update the pull request.

### Failing coverage checks on CI
You ned to add unit tests covering reasonably well the changes you are making to the code.
