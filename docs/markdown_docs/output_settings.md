# Configure selection of timings in output settings

Describes examples of timings in output settings for `[general.output_settings]` in the config file, see [output settings](#property-config-general-output-settings).

Output timings must be specified as follows:

- to configure a single output interval across the entire forecast length (e.g. outputs every hour, every 30 minutes and so on): a single string with format `"PTnC"`, where `n` is a positive integer number and `C` can be hours, `H`, or minutes, `M`. 
- to configure different output intervals across the forecast length: a list of strings. Each string must be of the format `"starttime:endtime:interval"`, where each of `starttime`, `endtime`, `interval` have the format `PTnC` as described above. More explicitly, the string `starttime:endtime:interval` instructs to generate forecast outputs with frequency `interval` (hour or minute) from the forecast step `starttime` to the forecast step `endtime`.
  
## Example

Here we explain output intervals through an example:

```
  fullpos = ["PT0H:PT6H:PT15M", "PT6H:PT9H:PT30M", "PT9H:PT24H:PT1H"]
  history = "PT1H"
  nrazts = "PT1H"
  surfex = ["PT0H:PT9H:PT10M", "PT9H:PT24H:PT3H"]
```

With the above settings:

- `fullpos` output will be created with a 15 minutes interval from the start until six hours, then with a 30 minutes interval until nine hours, then with a 1 hour interval onwards.
- `history` output will be created with a 1 hour interval.
- `nrazts` instantaneous fluxes will be reset hourly.
- `surfex` output will be created with a 10 minutes interval from the start until nine hours, then with a 3 hour interval until 24 hours.
 
