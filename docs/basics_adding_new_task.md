In order to start adding a new task to the deode prototype, a *file* containing your task needs to be created in the `$DEODE_PROTOTYPE/deode/tasks` folder, where **$DEODE_PROTOTYPE** points to the full name to the folder in which the deode prototype is installed.

In the `tasks` folder, the `discover_task.py` file looks for the task specified in the ```deode run``` command. The task name should be the name of the **class** of your task, as it's in the class names that discover_task.py gets the tasks available. For ease of use, the *file* name containing your class can also be named the same as the class.

A created task **class** usually inherits one of the two classes, **Task** or **BinaryTask** from the `base.py` file inside the `tasks` folder, the difference being the **BinaryTask** class inherits from the **Task** class and defines its own versions of the base functions of the **Task** class - `execute` , `prep` `post` and `run`. In the case the new tasks inherit the **BinaryTask** class, the task will need to have the settings defined in the **BinaryTask** version of the functions defined in the config.toml file used to run it. In the case the new tasks inherit the **Task** class, one can define their own `execute` , `prep` , `post`  and `run` functions and in both cases define new ones, and the config.toml file might not need all of the options it otherwise would.

In the case the newly created task inherits from **BinaryTask**, the config.toml file used to run the task needs to have a [task] directive.
The [task] directive then needs to contain a [task.taskname] directive, where `taskname` is the name of our task. Inside that [task.taskname] directive, the *wrapper* and *command* settings need to be set to their appropriate values. The *wrapper* setting sets a wrapper for the command needed to be run, most commonly commands `time` or `srun`. The *wrapper* setting comes from the **BatchJob** class included from `batch.py` inside `base.py`. The **BinaryTask** class reads the wrapper from the config.toml file and creates an object of the **BatchJob** type using that wrapper.
```
wrapper = self.get_task_setting("wrapper")
self.batch = BatchJob(os.environ, wrapper)
```
In the `execute` function of the **BinaryTask** the *command* setting is read from our config.toml file, and the `self.batch.run(cmd)` function is called.
```
cmd = self.get_task_setting("command")
self.batch.run(cmd)
```
The `run` function of the **BatchJob** class unites these two settings before running them as one in our task.
```
cmd = self.wrapper + " " + cmd
```

In order to copy files, inside the `task.py` file, the `fmanager` directive from the `deode/toolbox.py` file needs to be used. To create the input for the task, the `fmanager.input` function needs to be called.
```
def input(
    self,
    target,
    destination,
    basetime=None,
    validtime=None, # noqa
    check_archive=False,
    provider_id="symlink",
)
```
The `fmanager.input` only has two non-optional arguments and providers for *symlink, copy*, *move* and *ECFS*.
The paths used with `fmanager` should be put into macros and not hardcoded.
To link for example MASTERODB, one needs to call the function the following way:
`self.fmanager.input("@BINDIR@/MASTERODB", "MASTERODB")`
To copy or move, one only needs to add a provider_id:
`self.fmanager.input("@BINDIR@/MASTERODB", "MASTERODB", provider_id="copy")`

To handle output data, the `fmanager.output` function needs to be called after the running of the code in a similar way as the input. Example:
`self.fmanager.output("ICMSH@CNMEXP@+000?", "@OUTDIR@/ICMSH@CNMEXP@+000?")`. The same providers are available for output as for input.

In order to setup submission options, they need to be added to the `config.toml` file needs. One can either add to an already existing submit type, like the default `serial`, or create their own submission type. To add SLURM options, a name of the option along with the option itself needs to be added inside the `submission.submissiontype.BATCH` directive:
```
[submission.parallel.BATCH]
WALLTIME = "#SBATCH --time=00:11:00"
QOS = "#SBATCH --qos=np"
NTASKS = "#SBATCH --ntasks=128"
CORES = "#SBATCH --cpus-per-task=4"
MULTITHREAD = "#SBATCH --hint=nomultithread"
NODES = "#SBATCH -N 4"
NAME = "#SBATCH --job-name=fcast_task"
ACCOUNT = "#SBATCH -A msdeode"

```
To add environment variables, run modules or arbitrary commands, they need to be added to the `submission.submissiontype.ENV` directive
```
[submission.parallel.ENV]
MODULE = "print('My beautiful module')"
OS = "import os"
```
One can also specify these in a task.exceptions directive:
```
[submission.task_exceptions.Newtask.BATCH]
WALLTIME = "#SBATCH --time=00:11:00"
QOS = "#SBATCH --qos=np"
NTASKS = "#SBATCH --ntasks=128"
CORES = "#SBATCH --cpus-per-task=4"
MULTITHREAD = "#SBATCH --hint=nomultithread"
NODES = "#SBATCH -N 4"
NAME = "#SBATCH --job-name=fcast_task"
ACCOUNT = "#SBATCH -A msdeode"

[submission.task_exceptions.Newtask.ENV]
MODULE = "print('My beautiful module')"
OS = "import os"
```


After that is all done, the new task can be ran with:
```
deode -config_file=/your/config.toml -loglevel debug run --task yourtask  --template $PWD/deode/templates/stand_alone.py  --job $PWD/yourtask.job  --troika_config $PWD/config.yml  -o $PWD/yourtask.log
```

If `config_file` is specified under `[troika]` in config.yml, one can skip the --troika_config argument.
