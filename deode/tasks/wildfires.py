"""Impact model classes."""


import os
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

import tomlkit
from git import Repo

from deode.os_utils import deodemakedirs

from ..host_actions import SelectHost
from .impacts import ImpactModel


@dataclass()
class WildFire(ImpactModel):
    """WildFire specific methods."""

    name = "wildfire"

    def run(self):
        """Starts the WildFire suite."""
        # loads config file
        config_ = self.load()

        # expands ecf variables
        ecf_host = config_["ecfvars"]["ECF_HOST"]
        ecf_host = self.platform.substitute(ecf_host)
        ecf_host = self.platform.evaluate(ecf_host, object_=SelectHost)
        config_["ecfvars"]["ECF_HOST"] = ecf_host

        troika_config_file = config_["ecfvars"]["TROIKA_CONFIG"]
        endpart = troika_config_file.split(")", 2)[-1]
        path = str(self.platform.evaluate(troika_config_file, object_="deode.os_utils"))
        config_["ecfvars"]["TROIKA_CONFIG"] = path + endpart

        # builds the path to clone the repo of the wildfire application
        path = self.platform.substitute(config_["repo_root"])
        deodemakedirs(path, unixgroup=config_["unix_group"])
        tmp_path = Path(
            tempfile.NamedTemporaryFile(
                prefix="wildfires_workflow_", dir=path, delete=True
            ).name
        )

        # creates relevant directories
        deodemakedirs(tmp_path, unixgroup=config_["unix_group"])

        path = self.platform.substitute(config_["workdir"])
        deodemakedirs(path, unixgroup=config_["unix_group"])

        path = self.platform.substitute(config_["archive"])
        deodemakedirs(path, unixgroup=config_["unix_group"])

        # clones the wildfire application repo
        remote_url = config_["remote_url"]
        branch = config_["branch"]
        Repo.clone_from(remote_url, tmp_path, branch=branch)

        # adds path to wf suite
        suite_path = os.path.join(str(tmp_path), "IPMA-FIRE")
        sys.path.append(suite_path)

        # updates path to cloned repo in the original deode config file
        self.config["communicate"]["repo_home"] = str(tmp_path)
        config_["repo_home"] = str(tmp_path)
        deode_config = tomlkit.loads(Path(config_["deode_config_file"]).read_text())
        deode_config["impact"]["wildfire"]["communicate"]["repo_home"] = str(tmp_path)
        with open(config_["deode_config_file"], mode="w", encoding="utf-8") as f_h:
            f_h.write(tomlkit.dumps(deode_config))

        # updates ecflow server variables
        ecf_files = os.path.join(suite_path, "wf-suite/ecf_files")
        config_["ecfvars"]["ECF_FILES"] = ecf_files

        ecf_include = os.path.join(suite_path, "wf-suite/include")
        config_["ecfvars"]["ECF_INCLUDE"] = ecf_include

        # writes wildfire config file to disk
        self.dump(config_)

        # creates and loads wildfire applications suite
        import wf_suite

        wf_suite.load_suite(config_)
