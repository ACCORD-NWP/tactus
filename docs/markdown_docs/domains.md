# Domain configuration

## Define new domains

The domain you use for your run needs to be defined in a toml file and placed in include/domains folder and few test domains are provided.

We recommend you use tools like domain_maker.py (EpyGraM) or [Domain Creation Tool](https://hirlam.github.io/HarmonieSystemDocumentation/dev/ExperimentConfiguration/ModelDomain/) (follow the link) to define your domain.

The parameters you can put into your domain configuration can be found in [the config/domain section](#property-config-domain).

Some of the parameters are optional and derived automatically if not provided:

- **ilone** / **ilate**: If not provided will default to 11 gridpoints.
- **nbzonl** / **nbzong**: If not provided will be derived automatically from resolution (xdx/xdy) but if domain very small, i.e. less than 250 gridpoints in either direction, then the parameter defaults to 8 gridpoints.

Finally you'll need to update _domain_ under section [include] in your main config file and point it to your new domain toml file.

## List of domains

| File                              | name                      | gridtype  | nimax | njmax |  xdx |  xdy | xlatcen | xloncen | tstep | ilate | ilone | nbzong | nbzonl | xbeta | xlat0 | xlon0 |
| --------------------------------- | ------------------------- | --------- | ----- | ----- | ---- | ---- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `500m_template.toml`              | `500m_template`           | cubic     |  1489 |  1489 |  500 |  500 | 58.66 | 16.0 | CSC | — | — | — | — | — | — | — |
| `750m_template.toml`              | `750m_template`           | cubic     |   989 |   989 |  750 |  750 | 58.66 | 16.0 | CSC | — | — | — | — | — | — | — |
| `BG1_3000x3000_500m.toml`         | `BG1_3000x3000_500m`      | linear    |   978 |   978 |  500 |  500 | 42.63 | 24.64 | 30 | 11 | 11 | 8 | 8 | 0.0 | 50.5 | 20.0 |
| `DEMO_60x80_2500m.toml`           | `DEMO_60x80_2500m`        | linear    |    49 |    69 | 2500 | 2500 | 51.967 | 4.9 | 75 | — | — | — | — | — | — | — |
| `DEMO_80x90_1000m.toml`           | `DEMO_80x90_1000m`        | cubic     |    69 |    79 | 1000 | 1000 | 52.0 | 4.9 | 30 | — | — | — | — | — | — | — |
| `DEOL.toml`                       | `DEOL`                    | linear    |   709 |   709 | 9000 | 9000 | 50.0 | 8.0 | 75 | — | — | — | — | — | 50.0 | 8.0 |
| `DINI20A_1920x1620_2000m.toml`    | `DINI20A_1920x1620_2000m` | quadratic |  1909 |  1609 | 2000 | 2000 | 55.5 | -2.0 | 50 | — | — | — | — | — | 55.5 | -8.0 |
| `DINI25A_1536x1296_2500m.toml`    | `DINI25A_1536x1296_2000m` | quadratic |  1525 |  1285 | 2500 | 2500 | 55.5 | -2.0 | 60 | — | — | — | — | — | 55.5 | -8.0 |
| `DKCOEXP_648x648_2500m.toml`      | `DKCOEXP_648x648_2500m`   | linear    |   637 |   637 | 2500 | 2500 | 56.3 | 9.9 | 75 | — | — | — | — | — | 56.3 | 0.0 |
| `EEur_NAfr_1200x1500_2500m.toml`  | `SMSW`                    | cubic     |  1141 |  1429 | 2500 | 2500 | 41.25 | 26.75 | 75 | 11 | 11 | 16 | 16 | 0.0 | 42.5 | 26.0 |
| `IG20B_1350x1600_2000m.toml`      | `IG20B_1350x1600_2000m`   | cubic     |  1339 |  1589 | 2000 | 2000 | 55.5 | -36.0 | 60 | — | — | — | — | — | 72.0 | -36.0 |
| `IG20C_1728x1920_2000m.toml`      | `IG20C_1728x1920_2000m`   | cubic     |  1717 |  1909 | 2000 | 2000 | 70.0 | -39.5 | 60 | — | — | — | — | — | 70.0 | -40.0 |
| `IRELAND150_50x50_15000m.toml`    | `IRELAND150_50x50_15000m` | linear    |    39 |    39 | 15000 | 15000 | 53.5 | -8.5 | 300 | — | — | — | — | — | 53.5 | 0.0 |
| `NETHERLANDS_1500x1500_750m.toml` | `NETHERLANDS_750m`        | linear    |  1489 |  1489 |  750 |  750 | 51.967 | 4.9 | 20 | — | — | — | — | — | 52.5 | 0.0 |
| `NL_1000x1000_750m.toml`          | `large`                   | linear    |   989 |   989 |  750 |  750 | 52.5 | 4.9 | 20 | — | — | — | — | — | 52.5 | 4.9 |
| `large_central_eu.toml`           | `large_central_eu`        | linear    |   989 |   989 |  750 |  750 | 48.7 | 19.1 | CSC | — | — | — | — | — | — | — |
| `paris_osm.toml`                  | `paris_osm`               | linear    |   189 |   189 |  500 |  500 | 48.85 | 2.35 | 15 | — | — | — | — | — | 48.85 | 2.35 |
