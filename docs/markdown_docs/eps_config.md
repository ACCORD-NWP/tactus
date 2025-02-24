# EPS setup

## Enabling EPS

To enable EPS, one has to include an EPS include file in the configuration or
on the commandline. A default EPS include file is provided in the
`deode/data/config_files/include/eps/eps_members.toml` file. 
Thus, to run the default EPS setup, one can do
    
```bash
deode case --config-file deode/data/config_files/config.toml deode/data/config_files/include/eps/eps_members.toml
```
or include the following in the configuration file

```
--config-file
deode/data/config_files/config.toml
deode/data/config_files/include/eps/eps_members.toml
```

and do

```bash
deode case --config-file <path_to_config_file>
```

The default EPS include file contains the following settings:

```toml
[eps.general]
  control_member = 0
  members = "0:3"
  run_continously = false

[eps.member_settings.boundaries.ifs]
  selection = {"1:3" = "IFSENS"}

[eps.member_settings.general]
  realization = "-1"
```

Translated into words, including this file in the main config file will make the
`deode case` command produce a config file with three members, where the first
member is the control member. The three members will use the lateral boundary data
from corresponding IFSENS member.

*Resulting config file:*

> **_NOTE:_**:
> - Only deviating settings relative to the above `[eps.member_settings]` section
>   are saved.
> - No validation of the [eps.member_settings.*] sections is done at this point.
> - The original `[eps.member_settings]` section is included in the resulting config file for reference.

```toml
[eps.general]
  control_member = 0
  members = [0, 1, 2]
  run_continously = false

[eps.member_settings.boundaries.ifs]
  selection = "IFSENS"

[eps.member_settings.general]
  realization = "-1"

[eps.members.0.general]
  realization = 0

[eps.members.1.general]
  realization = 1
  selection = "IFSENS"

[eps.members.2.general]
  realization = 2
  selection = "IFSENS"
```

## Configuring EPS

In the general EPS settings section, one can set the control member as well as
the members that should be part of the ensemble. The `members` setting can either be

1. a list of integers or
2. a string sequence of integers or slices separated by commas

E.g. `members = [1, 3, 5]` will include members 1, 3, and 5 in the ensemble, 
whereas `members = "0, 1, 2, 4:10:2"` will include members 0, 1, 2, 4, 6, and 8.
The format of the string slices follows the Python slice notation, i.e. `start:stop:step`.
The default value for `step` is 1, so `start:stop` is equivalent to `start:stop:1`.

The control member can be set to any integer. If the control member is not part of the (expanded) members list, it will be added to the ensemble automatically.


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

to the eps include file. The various ways of setting member specific settings are:


#### 1. Single value -> all members get the same setting
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

#### 2. List of values -> first member gets first item, second member gets second item, etc. (with "circular boundary condition")
```toml
[eps.member_settings]
parameter = ["value1", "value2", "value3", "..."]
```

*Result:*
```toml
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
#### 3. Dict of mbr/value pairs -> a given member get the value of the mbr key
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

#### 4. Python subclass of `deode.eps.custom_generators.BaseGenerator`. Generates member settings based on list of members.

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