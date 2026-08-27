# Tactus test-runner

The tactus test-runner runs a number of configurations as defined by a config file.

We currently have the following cycle specific config files under the directory `tactus/data/test`

 - atos_bologna_[CY49t2,CY50t2].toml : Complete set of tests for atos_bologna
 - case_definitions_[CY49t2,CY50t2].toml : Definition of all test cases
 - test_macros.toml : Some macro definitions
 - modifs_atos_bologna_[CY49t2,CY50t2].toml : Platform dependent config modifications
 - modifs_reference_[checker,generator].toml : Setting for the reference checker

The main difference between CY49t2 and CY50t2 tests is that the latter includes a compilation step whereas the former uses DEODE binaries on ATOS.

## Check
To list available and activated cases do

```
tactus test -c tactus/data/tests/atos_bologna_CY50t2.toml -l
```

## Create config files
```
tactus test -c tactus/data/tests/atos_bologna_CY50t2.toml -m
```
This will create a directory according to the tag or branched used and create all config files in this directory.

## Launch the suites
```
tactus test -c tactus/data/tests/atos_bologna_CY50t2.toml -r
```
Failures in suites can be treated like any failure. I.e. by changing the relevant code or config and replace/relaunch the suite in full or parts as appropriate. The config files can be regenerated while suites are running if required. The config file generation and launch can be combined by running with `-m -r` in one go.

## Check the outcome of the reference checker
For each suite namelists, logs and results are checked for a number of selected tasks using the 
[reference checker](#reference-checker). The status and progress of this can be checked by running
```
tactus test -c tactus/data/tests/atos_bologna_CY50t2.toml
```
For more information per suite add a `-v`

## Generate new reference files
To generate new reference files run the full test setup with and additional `-g`, i.e.
```
tactus test -c tactus/data/tests/atos_bologna_CY50t2.toml -m -r -g
```
This will create new reference files in the `generated_references` directory. Ask the local tactus administrator to copy this to the correct location.


## Remove the tests from disk and ecflow
After successful runs and assessment the tested cases can be cleaned from disks and ecflow with the standard tactus `remove` functionality
```
tactus remove your_test_tag_configs/your_test_tag*.toml --execute-removal -f
```
Where `your_test_tag` is the name derived from your current branch. Read more about the remove command in the [cleaning documentation section](#cleaning-of-experiment).



## About the config files
The config file has a four main sections: general, case, modifs and ial. Here we explain the usage of each

### General

The general section defines the selection of cases and possible compiler extensions. If tag is not set it's taken from the used tactus branch or tag. In extra we can define extra config files to include.

```
[general]
  reference_date  = "-P1D"
  tag = "my_label_"
  extra = []
  selection = [
    "cy49t2_alaro",
    "cy49t2_alaro_target",
  ]
```
Leaving out the selection section will run all defined cases. Check with `-l` how it works.
To test different compilers we can add the compiler section. Here we define the section as active, configurations patterns to exclude and possible extra config files.
```
[general.compiler.gnu_]
  active = true
  exclude = ["cy48t2", "cy46h"]
  extra = ["tactus/data/config_files/modifications/submission/atos_bologna_gnu.toml"]

```

In the configuration step the generation of new files are done in parallel. The maximum number of threads can be controlled by setting  e.g.
```
[general]
  max_workers = 1
```

### Case

Here we define the config settings per case.

- base gives the config to start from
- host defines the ecflow mirror dependency
- extra is extra config files to add for this specific case
- case.X.modifs.Y allows to modify abitrary config settings for this case only

```
[cases.cy49t2_alaro_eps]
  host = "alaro"
  base = "cy49t2_alaro"
  extra = [
    "tactus/data/config_files/include/eps/eps_7members.toml",
    "tactus/data/config_files/include/eps/alaro.toml",
  ]

[cases.cy49t2_alaro_eps.modifs.eps.general]
  members = "0:3"
```

### Modifs

Here we define global modifications to the default config files. Works the same way as for the config modifications mentioned above.

```
[modifs.archiving.FDB.fdb.fpgrib_files]
  active = false
```
