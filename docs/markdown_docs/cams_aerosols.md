# CAMS aerosols in the lateral boundaries

Tactus can add near-real-time aerosols (and ozone) from the CAMS global forecast to the lateral boundaries, so that the forecast model reads and couples them. Enable it by adding the modification file `modifications/nrt_aerosols.toml` when creating the case.

or by setting

```toml
[nrt_aerosols]
  active = true
```

in your own modification file. The defaults are in the `[nrt_aerosols]` section, defined in `include/nrt_aerosols.toml`. The CAMS MARS selection is `[mars.CAMS_NRT]` in `include/mars_settings.toml`.

This is only available for CY49t2; for other cycles the suite generation stops with an error. It needs MARS access (CAMS data have `class=MC`), and the submission settings for the new tasks are only defined for `atos_bologna`.

## What happens

CAMS has no ensemble, so the CAMS part is done once per cycle, in the `CamsAerosols` family next to the member families:

- `CamsAerosols/MarsprepCams`: retrieves the CAMS forecast on the native CAMS grid (spectral fields, surface fields, humidity, the 11 CAMS aerosols and ozone) into `nrt_aerosols.bddir`. Per step it writes `ICMSH_0+STEP`, `ICMGG_0+STEP` and `ICMUA_0+STEP`, named as the c903 input definition expects. It runs in parallel with the regular `InputData` family.
- `CamsAerosols/LBCs/<batch>/C903Cams`: runs c903 on the CAMS data with the `c903_main_cams` and `c903_domain_cams` namelists, for the same batches of boundary times as the regular boundaries. It writes aerosol-only boundary files (SEASALT1-3, DUST1-3, OM1-2, BC1-2, SULF, O3) to `nrt_aerosols.intp_bddir`, which is shared by all members.

In the LBC families of every member, after the regular `C903` (or `E927`):

- `MergeCamsLbc`: adds the aerosol fields to the member's boundary files in `system.intp_bddir` with `lfitools lfi_alt_merge`/`lfi_alt_pack`. It waits for the `C903Cams` tasks of the same boundary times. The member's boundary file is given first, so its fields (surface pressure, orography, ...) are kept and only the aerosols are added. The task stops with an error if a boundary file already holds aerosols; rerun `C903` for that LBC family first.

The forecast namelist gets the sections `f4_lbc_aerosols_cams` (12 aerosols read from the initial file and coupled at the boundaries, with `LPT`, `LPC` and `LINTLIN`) and `f4_lbc_aerosols_cams_AROME` (radiation uses the coupled aerosols). The c903 namelists read the aerosols as gridpoint fields (`LGP`). They are selected through the `@LBC_AEROSOLS@` macro, which is `cams` when `nrt_aerosols.active` and `nrt_aerosols.forecast_namelist` are both true and `none` otherwise.

To add the aerosols to the boundaries only, and keep the forecast namelist unchanged, set:

```toml
[nrt_aerosols]
  active = true
  forecast_namelist = false
```

## Settings

| Setting | Default | Description |
| --- | --- | --- |
| `nrt_aerosols.active` | `false` | Switch the CAMS aerosols on |
| `nrt_aerosols.selection` | `"CAMS_NRT"` | MARS selection, see `[mars.CAMS_NRT]` in `include/mars_settings.toml` |
| `nrt_aerosols.bdshift` | `"PT0H"` | Use an older CAMS forecast, a multiple of the CAMS cycle length (12h). E.g. `"PT12H"` for a real-time setup, where the latest CAMS forecast is not yet available |
| `nrt_aerosols.forecast_namelist` | `true` | Add the CAMS sections to the forecast namelist. If `false`, the aerosols are only added to the boundaries |
| `mars.CAMS_NRT.start_date` | `"-P188D"` | First date with hourly CAMS data in MARS, relative to today |
| `nrt_aerosols.bddir` | `@SCRATCH@/@SYS_NAME@/IFS/@CAMS_SELECTION@/@YYYY@/@MM@/@DD@/@HH@` | Retrieved CAMS data (macro `@BDDIR_CAMS@`) |
| `nrt_aerosols.intp_bddir` | `@CASEDIR@/@YYYY@@MM@@DD@_@HH@@mm@/cams_lbc` | Aerosol-only boundary files from `C903Cams`, shared by all members |

Not all CAMS fields are archived every hour. In `[mars.CAMS_NRT]`, `SH_wind_interval` (spectral vorticity and divergence, 3-hourly) and `GG_snow_interval` (multi-layer snow and albedo fields, 12-hourly) set the archiving interval. In between, the fields of the previous archived step are used with the step relabelled. These fields are not written by the aerosol-only c903, so this does not affect the aerosol boundaries.

## Limitations

- Only CY49t2 with `nrt_aerosols.forecast_namelist=true`.
- Not supported together with `boundaries.do_slaf` or `suite_control.do_glprep`.
