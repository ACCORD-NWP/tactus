# Troubleshooting

Welcome to our troubleshooting tips. Before proceeding, please make sure you have carefully read the [README](https://github.com/ACCORD-NWP/tactus/blob/develop/README.md) file and, if applicable, the [development guide](https://github.com/ACCORD-NWP/tactus/blob/develop/docs/markdown_docs/development_guide.md).

## Cannot run pixi
Make sure your system fulfills all [system requirements](https://github.com/ACCORD-NWP/tactus/blob/develop/README.md#set-up-environment).
If you cannot execute `pixi`, reinstall it following the instructions in the [README](https://github.com/ACCORD-NWP/tactus/blob/develop/README.md).

## *Command not found* when trying to run *tactus* or some related command

Try running:

`pixi install`

## *ImportError* or *ModuleNotFoundError* when running the package's executable

Try running:

`pixi install`

## *pixi install* fail

Try running `pixi clean` and then re-run `pixi install`. If it still doesn't work, see [Cannot run pixi](#cannot-run-pixi) and try once more.

## Failing linting checks

You can run `pixi run lint` locally to fix some of the linting issues. You will need to solve the remaining issues manually, but the output of the linting tools usually tells you what is wrong and where to look.

Note: keep your local Python/Pixi environment aligned with the supported project Python range in `pyproject.toml`.

## Failing tests
It is always recommended to run the test suite locally address any encountered issue *before* pushing to you pull request.

## Failing CI checks on github
Please run `pixi run pre-push-checks` and fix any encountered error *before* you push your commits to update the pull request.

### Failing coverage checks on CI
You ned to add unit tests covering reasonably well the changes you are making to the code.
