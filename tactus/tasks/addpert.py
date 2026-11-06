"""Addpert."""

import os

from tactus.boundary_utils import Boundary
from tactus.datetime_utils import as_datetime, as_timedelta, cycle_offset
from tactus.tasks.base import Task
from tactus.tasks.batch import BatchJob


class Addpert(Task):
    """Addpert, add SLAF perturbation to unperturbed boundary file."""

    def __init__(self, config):
        """Construct Addpert object.

        Args:
            config (ParsedConfig): Configuration
        """
        Task.__init__(self, config, __class__.__name__)

        self.basetime = as_datetime(self.config["general.times.basetime"])
        self.boundary = Boundary(config)
        self.target = (
            f"{self.platform.get_system_value('intp_bddir')}"
            + "/"
            + f"{self.config['file_templates.interpolated_boundaries.archive']}"
        )
        self.binary = self.get_binary("ADDPERT")
        self.name = (
            f"{self.name}_{self.boundary.min_index}-{self.boundary.max_index}"
        ).upper()

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Which member am I?
        me = int(self.config.get("task.args.me", "0"))

        # Create namelist file (static)
        slafk = self.config.get("boundaries.slafk", "0.0")
        with open("fort.4", "w") as nl:
            nl.write("&NAMPERT\n")
            for i in range(1, 5):
                nl.write(f"  CLFNAME({i})='FILE{i}',\n")
            nl.write(f"  Z_MULT={slafk}\n")
            nl.write("/\n")
            nl.close()

        # Loop over boundary files for this batch
        for bd_index, bd_time in self.boundary.bd_index_time_dict.items():
            validtime = as_datetime(bd_time)
            # Do all SLAF parts
            for i in range(3):
                doer = int(self.config.get(f"task.args.doer{i}", "0"))
                part = int(self.config.get(f"task.args.part{i}", "0"))
                bdshift = self.boundary.bdshift
                bd_basetime = self.boundary.bd_basetime
                if i > 0:
                    bdshift += as_timedelta(
                        self.config.get(f"task.args.bdshift{i}", "PT0H")
                    )
                    bd_basetime = self.basetime - cycle_offset(
                        self.basetime,
                        self.boundary.bdcycle,
                        bdcycle_start=self.boundary.bdcycle_start,
                        bdshift=bdshift,
                    )
                # Find the path to the actual file
                this_target = self.target.replace("@NNN@", f"{bd_index:03}")
                real_target = self.platform.substitute(
                    this_target, basetime=bd_basetime, validtime=validtime
                )
                if part > 0:
                    real_target += f"_slaf{part}"
                if doer != me:
                    real_target = real_target.replace(f"mbr{me:03d}", f"mbr{doer:03d}", 1)
                    if i == 0:
                        # In order to save the unperturbed file (link) below
                        self.fmanager.input(
                            real_target,
                            this_target,
                            basetime=bd_basetime,
                            validtime=validtime,
                        )
                tmp_file = f"FILE{i + 1}"
                if os.path.exists(tmp_file):
                    os.remove(tmp_file)
                self.fmanager.input(
                    real_target,
                    tmp_file,
                    basetime=bd_basetime,
                    validtime=validtime,
                )

            # Copy output file
            self.fmanager.input(
                "FILE1",
                "FILE4",
                basetime=self.boundary.bd_basetime,
                validtime=validtime,
                provider_id="copy",
            )

            # Run binary
            batch = BatchJob(os.environ, wrapper=self.wrapper)
            batch.run(self.binary)

            # Save unperturbed target before overwriting target with FILE4
            self.fmanager.output(
                this_target,
                this_target + "_unpert",
                basetime=self.boundary.bd_basetime,
                validtime=validtime,
                provider_id="move",
            )
            self.fmanager.output(
                "FILE4",
                this_target,
                basetime=self.boundary.bd_basetime,
                validtime=validtime,
                provider_id="move",
            )
