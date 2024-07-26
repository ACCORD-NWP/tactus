# Adding a New Task

Follow the instructions below to add a new task to the system.

## Code requirements

In order to start adding a new task to the deode workflow, a *file* containing your task needs to be created in the `$DEODE_PROTOTYPE/deode/tasks` folder, where **$DEODE_PROTOTYPE** points to the full name to the folder in which the deode workflow is installed.

In the `tasks` folder, the `discover_task.py` file looks for the task specified in the ```deode run``` command. The task name should be the name of the **class** of your task, as it's in the class names that discover_task.py gets the tasks available. For ease of use, the *file* name containing your class can also be named the same as the class.

A created task **class** usually inherits the **Task** class from the `base.py` file inside the `tasks` folder The new tasks inherit the **Task** class, one can define their own `execute` , `prep` , `post`  and `run` functions and possibly define new ones.

Inside that [task.taskname] directive, the *wrapper* and *command* settings could be set to their appropriate values if they are used by the task. The *wrapper* setting sets a wrapper for the command needed to be run, most commonly commands `time` or `srun`. The *wrapper* setting comes from the **BatchJob** class included from `batch.py` inside `base.py`.

```
wrapper = self.get_task_setting("wrapper")
self.batch = BatchJob(os.environ, wrapper)

cmd = self.get_task_setting("command")
self.batch.run(cmd)
```
The `run` function of the **BatchJob** class unites these two settings before running them as one in our task.
```
cmd = self.wrapper + " " + cmd
```

## Handling output

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

## Submission rules for a task

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
deode --config-file=/your/config.toml run --task yourtask  --template $PWD/deode/templates/stand_alone.py  --job $PWD/yourtask.job  --troika-config $PWD/config.yml  -o $PWD/yourtask.log
```

If `config_file` is specified under `[troika]` in config.yml, one can skip the `--troika-config` argument.
