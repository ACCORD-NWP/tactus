# Computational parameters in config file

The default `config.toml` file, parameters in the `submission` groups refer to a coarse-resolution run in a small domain and a short lead time.

When running at higher resolution on real-sized domains, it is suggested to take care of and adjust the following parameters:

- WALLTIME: maximum wallclock time allowed by task. It can be raised in `[submission.task_exceptions.{e927,Forecast,Pgd,Prep}.BATCH]`. Raising it is suggested especially during climate generation, e.g., in `[submission.task_exceptions.Pgd.BATCH]`.
- NPROC: number of compute processes (can be also adjusted within `[submission.task_exceptions.{e927,Forecast,Pgd,Prep}]`).
- NODES: number of compute nodes.
- NTASKS: number of MPI tasks.

The parameter values encoding instructions in the `BATCH` groups are specific to the SLURM scheduler.
