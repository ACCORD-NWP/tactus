"""Impact model classes."""


import os
import sys
from dataclasses import dataclass
from pathlib import Path

import tomlkit

from deode.host_actions import SelectHost
from deode.logs import logger
from deode.os_utils import deodemakedirs
from deode.tasks.impacts import BaseImpactModel


@dataclass()
class WildFire(BaseImpactModel):
    """WildFire specific methods."""

    name = "wildfire"

    def run(self):
        """Starts the WildFire suite."""
        # Let BaseImpactModel handle the path preparation
        tmp_path = super().run()

        config_name = self.platform.substitute(self.config.get("config_name"))

        logger.info(" Load config from: {}", config_name)
        config_ = tomlkit.loads(Path(config_name).read_text())

        # expands ecf variables
        ecf_host = config_["ecfvars"]["ECF_HOST"]
        ecf_host = self.platform.substitute(ecf_host)
        ecf_host = self.platform.evaluate(ecf_host, object_=SelectHost)
        config_["ecfvars"]["ECF_HOST"] = ecf_host

        troika_config_file = config_["ecfvars"]["TROIKA_CONFIG"]
        endpart = troika_config_file.split(")", 2)[-1]
        path = str(self.platform.evaluate(troika_config_file, object_="deode.os_utils"))
        config_["ecfvars"]["TROIKA_CONFIG"] = path + endpart

        path = self.platform.substitute(config_["workdir"])
        deodemakedirs(path, unixgroup=config_["unix_group"])

        path = self.platform.substitute(config_["archive"])
        deodemakedirs(path, unixgroup=config_["unix_group"])

        # adds path to wf suite
        suite_path = os.path.join(str(tmp_path), "IPMA-FIRE")
        sys.path.append(suite_path)

        # updates path to cloned repo in the original deode config file
        config_["repo_home"] = str(tmp_path)

        # updates ecflow server variables
        ecf_files = os.path.join(suite_path, "wf-suite/ecf_files")
        config_["ecfvars"]["ECF_FILES"] = ecf_files

        ecf_include = os.path.join(suite_path, "wf-suite/include")
        config_["ecfvars"]["ECF_INCLUDE"] = ecf_include

        # writes wildfire config file to disk
        logger.info(" Dump modified config to: {}", config_name)
        with open(config_name, mode="w", encoding="utf-8") as f_h:
            f_h.write(tomlkit.dumps(config_))

        # creates and loads wildfire applications suite
        import wf_suite

        wf_suite.load_suite(config_)
