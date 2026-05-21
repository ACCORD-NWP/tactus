# Tactus test-runner

The tactus test-runner runs a number of configurations as defined in the config file atos_bologna.toml

We currently have the following config files under the directory `tactus/data/test`

 - atos_bologna.toml : Complete set of tests for atos_bologna
 - case_definitions.toml : Definition of all test cases
 - test_macros.toml : Some macro definitions
 - modifs_atos_bologna.toml : Platform dependent config modifications

## Check

```
tactus test -c config_files/atos_bologna.toml -l
```

## Run
```
tactus test -c config_files/atos_bologna.toml
```

This will create a directory according to the tag and create all config files in this directory. For each config a tactus ecflow run will be launched. To only prepare config files without running tactus do:

```
tactus test -c config_files/atos_bologna.toml -d
```

## Clean

After successful runs and assessment the tested cases can be cleaned from disks and ecflow with the standard tactus `remove` functionality
```
tactus remove /scratch/$USER/tactus/your_test_tag_\*/archive/config.toml --execute-removal -f

```
Read more about the remove command in the cleaning documentation section.


## About the config files

The config file has a four main sections: general, case, modifs and ial. Here we explain the usage of each

### General

The general section defines the selection of cases and possible compiler extensions. If tag is not set it's taken from the used tactus branch or tag. In extra we can define extra config files to include.

```
[general]
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

To rerun the tests with the same dates as those used when testing prior to tagging set the reference date as
```
[general]
   reference_date  = "YYYY-MM-DD"
```
on atos and
```
[modifs.general.times]
   end = "YYYY-MM-DDT00:00:00Z"
   start = "YYYY-MM-DDT00:00:00Z"
```
on in `config_files/modifs_lumi.toml` on lumi. Note though that fdb on lumi only stores data for the most recent weeks.

### Case

Here we define the config settings per case.

- base gives the config to start from
- host defines the forcing run for a target run
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
