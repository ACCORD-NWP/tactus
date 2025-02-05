# Some notes about using the FDB archive
## Getting an expver
on ATOS run the following commands (Requires the user to be in unix group `ifs`):

```bash
module load pifsenv
getNewId -g d1.on-demand-extremes-dt
```

Note, expver is case-sensitive, so the expver `aaaa` and e.g. `Aaaa` is not the same.


## Preparing the fdb tool
This is to use the fdb command line tool, if you want to read, write or wipe the data on FDB.

On ATOS:
```
export FDB_HOME=/home/fdbdev/destine/
export FDB5_HOME=/usr/local/apps/mars/versions/6.33.19.9/
```

On LUMI:
```
module use /appl/local/climatedt/modules
```

## How to list available data
To list available data in fdb, use the fdb list (Here with head -10 to only read the 4 first entries):
```
fdb list class=d1,expver=aaaa | head -10
```
`[OUTPUT]`
```
Listing for request
retrieve,
	class=d1,
	expver=aaaa


{class=d1,dataset=on-demand-extremes-dt,expver=aaaa,stream=oper,date=20230916,time=0000}{type=fc,levtype=sfc}{step=6,param=129}
{class=d1,dataset=on-demand-extremes-dt,expver=aaaa,stream=oper,date=20230916,time=0000}{type=fc,levtype=sfc}{step=6,param=130}
{class=d1,dataset=on-demand-extremes-dt,expver=aaaa,stream=oper,date=20230916,time=0000}{type=fc,levtype=sfc}{step=6,param=134}
{class=d1,dataset=on-demand-extremes-dt,expver=aaaa,stream=oper,date=20230916,time=0000}{type=fc,levtype=sfc}{step=6,param=146}
```

## How to retrieve archived data from FDB:
`fdb read request.mars out.grib`

Where the request.mars file looks something like this (Constructed from last line of output of the fdb list above):
```
retrieve,
    class = d1,
    dataset = on-demand-extremes-dt,
    expver = aaaa,
    stream = oper,
    date = 20230916,
    time = 0000,
    type = fc,
    levtype=sfc,
    step = 6,
    param = 146
```

## How to set expver in DEODE Workflow
expver is set in the `[fdb.grib_set]` section of the `archiving.toml` file like this:
```
[fdb.grib_set]
  [...]
  tablesVersion = "32"
  expver = "aaaa"
```
```{note}
The combination of `class`, `dataset`, `expver`, `stream`, `date` and `time` must be unique for each user (user `A` can only archive to fdb folders owned by user `A`). This is fixed by using the expver generator explained above.
```

You also need to enable fdb archiving by setting the `active` variable for the relevant archiving tasks to `true`. Example for `[archiving.hour.fdb.grib2_files]`:

```
[archiving.hour.fdb.grib2_files]
  active = true
  inpath = "@ARCHIVE@"
  pattern = "GRIBPF*"
```

## How to exclude variables from being archived
This is done by adding values to the `archiving.toml` configuration file under the `[fdb.negative_rules]` section:

```
[fdb]

[fdb.negative_rules]
  typeOfLevel = ["unknown", "not_found"]
```
This will exclude all grib-messages where `typeOfLevel` is `unknown` OR `not_found`. The values to filter can either be a single value or list.

```{note}
Currently on ATOS there is a bug where ECCODES will give an error: `ECCODES ERROR   :  Unable to get typeOfLevel as string (Key/value not found)`, but still run as intended.
```


## How to remove data
Use `fdb wipe`

To list out what data will be deleted:
```
fdb wipe class=d1,dataset=on-demand-extremes-dt,expver=JoLa,stream=oper,date=20230916,time=0600
```
And to actually delete it (add the --doit flag):
```
fdb wipe class=d1,dataset=on-demand-extremes-dt,expver=JoLa,stream=oper,date=20230916,time=0600 --doit
```

```{note}
This is a bit buggy on LUMI as the as the archive.lock file created while clearing the fdb-directory is not removed and as such you can not archive again into the same folder (defined by the combination class,dataset,expver,stream,date,time) without removing this archive.lock file.
```


## Notes about georef
We are using a [geohashing algorithm](https://github.com/tammoippen/geohash-hilbert) with Base64 string-representation of the hash, and a Hilbert space-filling curves instead of Z-order space-filling curves
