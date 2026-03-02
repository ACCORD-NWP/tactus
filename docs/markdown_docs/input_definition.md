# JSON configured input definition
For different tasks like C903, E923, E927, forecast (and surfex) a separate json config file can be specified that defines the input files.

The basic structure of such a json-file is given by
```
  "group_name": {
    "path": "source_folder_path",
    "files": ["file1", "file2"]
  }
```

where `group_name` is the name of the group of this files, represented as a dictionary. This dictionary contains at least the following keys:
- `path` gives the folder where the file(s) can be found
- `files` gives a list (or dictionary) of files to link into the work directory of the given task

If `files` are given as dictionary then one can specify a different target file name compared to a source file name. So for example
```
  "group_name": {
    "path": "source_folder_path",
    "files": {
        "target_file_name" : "source_file_name", 
    }
  }
```
will link `target_file_name` in the workdir of this task to the file `source_folder_path / source_file_name`.


## Additional features for E923
### Implicit decompression
When target file names ends with `.Z` it is assumed that these files are compressed and automatic decompression of these files is applied.

### Automatic month number replacement
E923 monthly might iterate over one or more months, a build-in macro `@MM@` will resolve to the actual month number.


### Copy files in stead of linking
By default the files will be linked. By specifying the optional key `provider_id` one can specify any supported provider_id that can be handled by the filemanager, for example 
```
  "group_name": {
    "path": "source_folder_path",
    "provider_id": "copy",
    "files": ["file1", "file2"]
  }
```
will create a real copy of the file into the workdir of the task.

### Remove links when finished
To keep the workdir clean between different parts one can specify that links should be removed after executing the specified part; i.e.
```
  "part2": {
    "path": "source_folder_path",
    "remove_links": true,
    "files": ["file1", "file2"]
  }
```
will link file1 and file2 into the workdir and when `part2` is finished the linked will be removed.

### Parameterized files
Optionally it is possible to specify parameterized filenames. The values for the parameter can be specified as a list with the key `param`. Iterating over this `param` list the the macro `@PARAM@` in the target/source filename will be replaced by the value. So 
```
  "group_name": {
    "param": ["a", "b", "c"],
    "files": {
      "target_@PARAM@": "source_@PARAM@"
    }
  }
```
is shorthand notation for
```
  "group_name": {
    "files": {
      "target_a": "source_a",
      "target_b": "source_b",
      "target_c": "source_c"
    }
  }
```

## Additional features for surfex

TODO: surfex seem to have also additional (undocumented) features in processing the input_definition json-file
