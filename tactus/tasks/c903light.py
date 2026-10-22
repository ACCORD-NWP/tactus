"""C903Light."""

from tactus.boundary_utils import Boundary
from tactus.datetime_utils import as_datetime
from tactus.logs import logger
from tactus.tasks.base import Task


class C903Light(Task):
    """C903Light, just symlink the target file to one produced elsewhere."""

    def __init__(self, config):
        """Construct C903Light object.

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
        self.name = (
            f"{self.name}_{self.boundary.min_index}-{self.boundary.max_index}"
        ).upper()

    def execute(self):
        """Run task.

        Define run sequence.

        """
        # Get target file name
        duo = self.config.get("task.args.duo", "0:0")
        doer, i = [int(j) for j in duo.split(":", 1)]
        me = int(self.config.get("task.args.me", "0"))

        # Loop over boundary files for this batch
        for bd_index, bd_time in self.boundary.bd_index_time_dict.items():
            validtime = as_datetime(bd_time)
            target = self.target.replace("@NNN@", f"{bd_index:03}")
            # Find the path to the actual file
            real_target = self.platform.substitute(
                target, basetime=self.boundary.bd_basetime, validtime=validtime
            )
            real_target = real_target.replace(f"mbr{me:03d}", f"mbr{doer:03d}", 1)
            if i > 0:
                real_target += f"_slaf{i}"
            if "target_suffix" in self.config["task.args"]:
                target += self.config["task.args.target_suffix"]
            logger.info(f"real_target={real_target}")

            self.fmanager.input(real_target, target)
            logger.info(f"     target={target}")
