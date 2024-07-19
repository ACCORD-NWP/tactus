# Some notes about using the FDB archive on LUMI

## How to exclude variables from being archived
Currently this is done by adding values to the `archiving.toml` configuration file under the `[fdb.negative_rules]` section:

```
[fdb]

[fdb.negative_rules]
  typeOfLevel = ["unknown", "not_found"]
```
This will exclude all grib-messages which have typeOfLevel as unknown OR not_found. The values to filter can either be a single value or list.

## How to retrieve archived data from FDB:
`fdb-read request.mars out.grib`

Where the request.mars file looks something like this:
```
retrieve,
    class = d1,
    expver = deod,
    dataset = on-demand-extremes-dt,
    stream = oper,
    [...]
```
To see the relevant fields to fill in, use `fdb-list class=d1,expver=deod` (and you can further refine the fdb list by adding more keywords)

## How to set expver
expver is set by using the 'cnmexp'-variable in the config `[general]` section. The default expver is DEOD.
NB The combination class,dataset,expver,stream,date,time must be unique for each LUMI-user (user `langvatn` can only archive to fdb-folders owned by user `langvatn`)

## How to remove data
Use the fdb-wipe functionality, this is a bit buggy right now, as the archive.lock file after clearing the fdb-directory is not removed and as such you can not archive again into the same folder (defined by the combination class,dataset,expver,stream,date,time) without removing this archive.lock file.