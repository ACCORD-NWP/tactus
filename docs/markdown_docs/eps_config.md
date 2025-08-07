# EPS Configuration by design

The Deode-Workflow treats every run as an ensemble run. A deterministic run is thus just a special case of an ensemble with only one member. Furthermore, an ensemble in Deode-Workflow is not limited to consist of a control member and a number of perturbed members. Instead, think of it as an ensemble of configurations, where each member "perturb" the default configuration. One can e.g. think of running an ensemble of
- a set of different model configurations (e.g. different physics options, different
  initial conditions, different boundary conditions, etc.)
- a set of different CSCs
- a set of different domains
- a set of different domain resolutions and extends
- a set of different time settings
- etc.

Basically any configuration setting can be "perturbed", i.e. configured differently for each member, so it's up to your imagination how to use the ensemble design to your needs.

## Running a minimal ensemble

To get hands-on running an ensemble, the Deode-Workflow ships with a bunch of example eps include files. To enable one of them, one has to include the file in the configuration or on the commandline. The example eps include files are available in the
`deode/data/config_files/include/eps/` directory. 
E.g. to run a 1 control + 2 perturbed members (using IFSENS boundary data) ensemble, one can use the `deode/data/config_files/include/eps/eps_3members_IFSENS_common_mars_prep` and do
    
```bash
deode case --config-file deode/data/config_files/config.toml deode/data/config_files/include/eps/eps_3members_IFSENS_common_mars_prep.toml --start-suite
```
or include the following in the configuration file

```
--config-file
deode/data/config_files/config.toml
deode/data/config_files/include/eps/eps_3members_IFSENS_common_mars_prep.toml
```

and do

```bash
deode case --config-file <path_to_config_file> --start-suite
```

The `eps_3members_IFSENS_common_mars_prep.toml` file contains the following settings:

```toml
[general.times]
  start = "-P1D"

[boundaries.ifs]
  bdmembers = [0, 1, 2]
  selection = "IFSENS"

[eps.general]
  members = "0:3"

[eps.member_settings.boundaries.ifs]
  bdmember = [0, 1, 2]
```

Translated into words, including this file in the main config file will make the `deode case` command produce a config file with three members:

```toml
[general.times]
  start = "-P1D"

[eps.general]
  members = [0, 1, 2]

[eps.member_settings.system]
  # This section comes from the default eps include file deode/data/config_files/include/eps/eps_default.toml
  wrk = "@CASEDIR@/@YYYY@@MM@@DD@_@HH@@mm@/@MEMBER_STR@"

[eps.member_settings.boundaries.ifs]
  bdmember = [0, 1, 2]

[eps.members]

[eps.members.0.boundaries.ifs]
  bdmember = 0

[eps.members.1.boundaries.ifs]
  bdmember = 1

[eps.members.2.boundaries.ifs]
  bdmember = 2
```


> **_NOTE:_**:
> The general.times.start setting is set to "-P1D" by default, to make the example able to run out-of-the-box, since IFSENS data is only available in mars for the past two weeks.

The three members (0, 1, and 2) will use the lateral boundary data from corresponding IFSENS member. Note, that member 0 of the IFSENS is the control member, so the EPS member 0 will use the control member data. This is a quite bare bones example, since they only differs in what boundary data they use.

## Adjusting which member uses which IFSENS member data

Let's instead say, that you wanted the member 2 to use the IFSENS member 0 boundary data, i.e. the control member data, and member 0 and 1 to use the IFSENS member 1 and 2 boundary cata, respectively. You can do this by setting `eps.member_settings.boundaries.ifs = [1, 2, 0]`, e.g.

```toml
[general.times]
  start = "-P1D"

[boundaries.ifs]
  selection = "IFSENS"
  bdmembers = [0, 1, 2]

[eps.general]
  members = "0:3"

[eps.member_settings.boundaries.ifs]
  bdmember = [1, 2, 0]
```

Running the `deode case` command will result in a config file with the following `[eps]` section:

```toml
[general.times]
  start = "-P1D"

[eps.general]
  members = [0, 1, 2]

[eps.member_settings.system]
  # This section comes from the default eps include file deode/data/config_files/include/eps/eps_default.toml
  wrk = "@CASEDIR@/@YYYY@@MM@@DD@_@HH@@mm@/@MEMBER_STR@"

[eps.member_settings.boundaries.ifs]
  bdmember = [1, 2, 0]

[eps.members]

[eps.members.0]
  bdmember = 1

[eps.members.1]
  bdmember = 2

[eps.members.2]
  bdmember = 0
```

> **_NOTE:_**:
> The general.times.start setting is set to "-P1D" by default, to make the example able to run out-of-the-box, since IFSENS data is only available in mars for the past two weeks.

## Common or member specific mars data retrieval
By default, mars data is retrieved for all members simultaneously to optimize the mars requests. This is controlled by the `suite_control.member_specific_mars_prep = false` setting. As shown in above example, all what one has to do, to retrieve the correct members, is to set the `bdmembers` and `bdmember` settings:

```toml
[general.times]
  start = "-P1D"

[boundaries.ifs]
  selection = "IFSENS"
  bdmembers = [0, 1, 2]

[eps.general]
  members = "0:3"

[eps.member_settings.boundaries.ifs]
  bdmember = [0, 1, 2]  # or bdmember = "@MEMBER@"
```

> **_NOTE:_**:
> The general.times.start setting is set to "-P1D" by default, to make the example able to run out-of-the-box, since IFSENS data is only available in mars for the past two weeks.

One can ask why we need both a `boundaries.ifs.bdmembers` and a `eps.member_settings.boundaries.ifs.bdmember` setting. The reason is that the `bdmembers` setting is used to determine which boundary members we should retrieve IFSENS data for, whereas the `bdmember` setting is used to determine which IFSENS member data to use for each member.

To instead retrieve mars data individually for each member, one has to
1. set the `member_specific_mars_prep` setting to `true` in the `[suite_control]` section.
2. set the `bdmembers` and `bdmember` setting individually for each member either in the `[boundaries.ifs]` section or in the `[eps.member_settings.boundaries.ifs]` section. In both cases, one can use macros to define the members. Only in the latter case, one can set them to explicit numbers.

i.e.
```toml
[suite_control]
  member_specific_mars_prep = true

[boundaries.ifs]
  bdmembers = ["@MEMBER@"]
  bdmember = "@MEMBER@"
  selection = "IFSENS"

[eps.general]
  members = "0:3"
```
or
```toml
[suite_control]
  member_specific_mars_prep = true

[boundaries.ifs]
  selection = "IFSENS"

[eps.general]
  members = "0:3"

[eps.member_settings.boundaries.ifs]
  bdmembers = [["@MEMBER@"]]  # or bdmembers = [[0], [1], [2]]. Note the extra brackets. They area necessary to make bdmembers be a list after member settings expansion
  bdmember = "@MEMBER@"   # or bdmember = [0, 1, 2]
```

To make it clear why one cannot set explicit numbers in the `[boundaries.ifs]` section while `member_specific_mars_prep = true`, if one e.g. did
```toml
[suite_control]
  member_specific_mars_prep = true

[boundaries.ifs]
  bdmembers = [0, 1, 2]
  bdmember = 0
  selection = "IFSENS"

[eps.general]
  members = "0:3"
```
it would result in every member retrieving data for member 0, 1, and 2, but only using member 0 data.

## Member specific static data
By default, the static data is generated one time for all members. To generate static data for each member individually, one has to set the `member_specific_static_data` setting to `true` in the `[suite_control]` section, and adjust the `climdir` to have a "@MEMBER_STR@" directory.

i.e.
```toml
[suite_control]
  member_specific_static_data = true
[system]
  climdir = "@CASEDIR@/climate/@DOMAIN@/@MEMBER_STR@"
```

## Construct ensemble from modification files
Sometimes, one wants to create an ensemble with a lot of specific settings for each member. As an example, one could think of an ensemble of CSCs. Every CSC comes with its own specific configurations, and to type them all into e.g. a `eps_3csc_members.toml` file is tedious. Instead, one can use predefined toml modification files and reference them in a `[eps.member_settings.modifications]` section. Let's take a simple example first.

Let's imagine we have tree different modification toml files:

- `modifications/mod1.toml`:
```toml
[general.output_settings]
  fullpos = "PT1H"
```
- `modifications/mod2.toml`
```toml
[general.output_settings]
  fullpos = "PT2H"
```
- `modifications/mod3.toml`
```toml
[general.output_settings]
  fullpos = "PT3H"
```

If we add a section `[eps.member_settings.modifications]` to e.g. the `eps_3members_IFSENS.toml` file from [Running a minimal ensemble](#running-a-minimal-ensemble), and at the same time imagine that we have defined a general member setting for the `fullpos` setting:
```toml
[general.times]
  start = "-P1D"

[boundaries.ifs]
  selection = "IFSENS"
  bdmembers = [0, 1, 2]

[eps.general]
  members = "0:3"

[eps.member_settings.general.output_settings]
  fullpos = "PT6H"

[eps.member_settings.boundaries.ifs]
  bdmember = [0, 1, 2]

[eps.member_settings.modifications]
  example_mod = ["modifications/mod1.toml", "modifications/mod2.toml", "modifications/mod3.toml"]
```

then the three modification files will be merged into the resulting config file for the respective members and overwrite any existing setting. E.g. the resulting eps section for member 0 will include settings from `"modifications/mod1.toml"`, for member 1 from `"modifications/mod2.toml"` and for member 2 from `"modifications/mod3.toml"`. The resulting config file will look like this:

```toml
[general.times]
  start = "-P1D"

[eps.general]
  members = [0, 1, 2]

[eps.member_settings.system]
  # This section comes from the default eps include file deode/data/config_files/include/eps/eps_default.toml
  wrk = "@CASEDIR@/@YYYY@@MM@@DD@_@HH@@mm@/@MEMBER_STR@"

[eps.member_settings.boundaries.ifs]
  bdmember = [0, 1, 2]

[eps.member_settings.general.output_settings]
  fullpos = "PT6H"

[eps.member_settings.modifications]
  example_mod = ["modifications/mod1.toml", "modifications/mod2.toml", "modifications/mod3.toml"]

[eps.members]

[eps.members.0.general.output_settings]
  fullpos = "PT1H"

[eps.members.0.boundaries.ifs]
  bdmember = 0
  
[eps.members.1.general.output_settings]
  fullpos = "PT2H"

[eps.members.1.boundaries.ifs]
  bdmember = 1
  
[eps.members.2.general.output_settings]
  fullpos = "PT3H"

[eps.members.2.boundaries.ifs]
  bdmember = 2
  
```
> **_NOTE:_**:
> - It's not important what the keys in the modification section are called. They are just used to label the different modification files.
> - The settings in the modification files will overwrite any existing value for that setting. 
> - The general.times.start setting is set to "-P1D" by default, to make the example able to run out-of-the-box, since IFSENS data is only available in mars for the past two weeks.

Now to the 3 CSC ensemble example. To set this up, one needs to have the following in the eps config file (NOTE: the exact modification file paths may change with the Deode-Workflow version): 

```toml
[suite_control]
  member_specific_static_data = true

[eps.general]
  members = "0:3"

[eps.member_settings.modifications]
  csc_modification = ["modifications/arome.toml", "modifications/harmonie_arome.toml", "modifications/alaro.toml"]
  cycle_modification = ["modifications/CY48t3.toml", "modifications/CY46h1.toml", "modifications/CY48t3.toml"]
  submission_modification = ["modifications/submission/@HOST@_CY48t3.toml", "modifications/submission/@HOST@_CY46h1.toml", "modifications/submission/@HOST@_CY48t3_alaro.toml"]
  vertical_levels_modification = {2 = "include/vertical_levels/MF_87.toml"}
```

The `member_specific_static_data = true` setting is needed since the static data is generated differently for each CSC. Running the `deode case` command with this eps config file will result in a config file with all the modifications merged into the member specific sections.


## Configuring EPS in general terms

This section describes how to configure an ensemble in Deode-Workflow in general terms. All configuration described below should be applied to a `.toml` file, that is included when running the `deode case` command, as described in [Running a minimal ensemble](#running-a-minimal-ensemble). 

In the `[eps.general]` section, one can set the members that should be part of the ensemble. The `members` setting can either be

1. an integer,
2. a list of integers or
3. a string sequence of integers or slices separated by commas

E.g. `members = [1, 3, 5]` will include members 1, 3, and 5 in the ensemble, 
whereas `members = "0, 1, 2, 4:10:2"` will include members 0, 1, 2, 4, 6, and 8. Setting `members = 1` will include only member 1.
The format of the string slices follows the Python slice notation, i.e. `start:stop:step`.
The default value for `step` is 1, so `start:stop` is equivalent to `start:stop:1`.

To adjust the default member settings to ones needs, one can set member specific settings
for basically any existing settings of the Deode-Workflow config file. There are various
ways to do this, but in any case the settings shall be placed under the
`[eps.member_settings]` section in the eps include file with the full original
config section string appended to "`eps.member_settings`". E.g. to adjust the
`[general.output_settings]` section member specifically, one would add the section

```toml
[eps.member_settings.general.output_settings]
fullpos = "..."
```

to the eps include file.

> **_NOTE:_**:
> - Only deviating settings relative to the `[eps.member_settings]` section
>   are saved to the resulting config file.
> - No validation of the [eps.member_settings.*] sections is done at this point.
> - The original `[eps.member_settings]` section is included in the resulting config file for reference.

The various ways of setting member specific settings are:


### 1. Single value -> all members get the same setting
```toml
[eps.member_settings]
parameter = "value"
```
*Result:*
```toml
[eps.members.0]
  parameter = "value"
[eps.members.1]
  parameter = "value"
...
```

### 2. List of values -> first member gets first item, second member gets second item, etc. (with "circular boundary condition")
```toml
[eps.member_settings]
parameter = ["value1", "value2", "value3", "..."]
```

*Result:*
```
[eps.members.0]
  parameter = "value1"
[eps.members.1]
  parameter = "value2"
...
[eps.members.N]
  parameter = "valueN"
[eps.members.N+1]
  parameter = "value1"
...
```
### 3. Dict of mbr/value pairs -> a given member get the value of the mbr key
```toml
[eps.member_settings]
parameter = {0 = "value1", 1 = "value2", "2:5:2" = "value3", ...}
```

*Result:*
```toml
[eps.members.0]
  parameter = "value1"
[eps.members.1]
  parameter = "value2"
[eps.members.2]
  parameter = "value3"
[eps.members.4]
  parameter = "value3"
```
> **_NOTE:_**
> - "m:n:s" keys are interpreted as slices like for the general members setting, that is `{"2:10:2" = "value3"}` assigns `"value3"` to members 2, 4, 6, and 8.
> - The `[eps.general.members]` setting limits parameter slices. E.g. if `[eps.general.members] = "0:10"`,
>   ```toml
>     [eps.member_settings]
>     parameter = {"6:16" = "value1"}
>   ```
>   will set `parameter = "value1"` only for members 6, 7, 8, and 9. 
> - For members with no mbr/value pair, the default is used. I.e. in the above example, members 0-5 will get the default value.
> - In cases, where a mbr key is duplicated in the dictionary (including string slices), the last value is used. E.g. in
>  ```toml
>    [eps.member_settings]
>    parameter = {"2:4" = "value1", 3 = "value2"}
>  ```
>  `parameter` will be set to `"value2"` for member 3.

### 4. Python subclass of `deode.eps.custom_generators.BaseGenerator`. Generates member settings based on list of members.

E.g. to generate random boolean values for each member, one could define a generator class like

```python
@pydantic_dataclass
class BoolGenerator(BaseGenerator[bool]):
    """Example generator class to generate random boolean values."""

    def __iter__(self):
        for _ in self.members:
            yield random.choice([True, False]) 
```

and set the parameter to a string that points to the given class object like

```toml
[eps.member_settings]
parameter = "deode.eps.custom_generators.BoolGenerator"
```

*Result:*
```toml
[eps.members.0]
  parameter = true
[eps.members.1]
  parameter = false
[eps.members.2]
  parameter = false  
...
```