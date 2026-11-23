"""Unit tests for the CAMS aerosols in the lateral boundaries."""

from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest
import tomlkit

from tactus.boundary_utils import Boundary, use_cams_aerosols
from tactus.derived_variables import derived_variables, set_times
from tactus.mars_utils import previous_archived_step
from tactus.namelist import NamelistGenerator
from tactus.tasks import marsprep_cams
from tactus.tasks.boundaries import C903Cams, MergeCamsLbc
from tactus.tasks.marsprep_cams import MarsprepCams


def make_config(default_config, tmp_directory, start, cams=""):
    """Return a config with CAMS aerosols and the given start time.

    Args:
        default_config (ParsedConfig): Default config
        tmp_directory (str): Temporary directory
        start (str): Start time
        cams (str): Extra settings in nrt_aerosols

    Returns:
        ParsedConfig: The config
    """
    config = default_config.copy(
        update=tomlkit.parse(
            f"""
            [general]
                cycle = "CY49t2"
                csc = "AROME"
            [general.times]
                start = "{start}"
                end = "{start}"
                forecast_range = "PT6H"
            [boundaries]
                bdint = "PT1H"
            [nrt_aerosols]
                bddir = "{tmp_directory}/cams/@YYYY@@MM@@DD@@HH@"
                intp_bddir = "{tmp_directory}/lbc/cams"
                available = true
                {cams or "active = true"}
            [system]
                wrk = "{tmp_directory}/wrk"
                intp_bddir = "{tmp_directory}/lbc"
            """
        )
    )
    config = config.copy(update=set_times(config))
    return config.copy(update=derived_variables(config))


@pytest.fixture(name="cams_config")
def fixture_cams_config(default_config, tmp_directory):
    """Config with CAMS active, starting at 06 UTC."""
    return make_config(default_config, tmp_directory, "2026-08-28T06:00:00Z")


class TestHelpers:
    """Test the helper functions."""

    @pytest.mark.parametrize(
        ("step", "interval", "expected"),
        [(0, 3, 0), (2, 3, 0), (3, 3, 3), (13, 12, 12), (11, 12, 0)],
    )
    def test_previous_archived_step(self, step, interval, expected):
        assert previous_archived_step(step, interval) == expected


class TestCamsBoundary:
    """Test the boundary times for CAMS."""

    def test_uses_cams_cycle(self, cams_config):
        boundary = Boundary(cams_config, cams=True)
        assert boundary.method == "c903"
        assert boundary.bdmodel == "cams"
        # CAMS runs at 00 and 12 UTC, so 06 UTC uses the 00 UTC forecast
        assert boundary.bdcycle == timedelta(hours=12)
        assert boundary.bd_basetime == datetime(2026, 8, 28, 0, tzinfo=timezone.utc)
        assert boundary.bd_offset == timedelta(hours=6)

    def test_bdshift(self, default_config, tmp_directory):
        config = make_config(
            default_config,
            tmp_directory,
            "2026-08-28T06:00:00Z",
            cams='bdshift = "PT12H"',
        )
        boundary = Boundary(config, cams=True)
        assert boundary.bd_basetime == datetime(2026, 8, 27, 12, tzinfo=timezone.utc)

    def test_invalid_bdshift(self, default_config, tmp_directory):
        config = make_config(
            default_config,
            tmp_directory,
            "2026-08-28T06:00:00Z",
            cams='bdshift = "PT6H"',
        )
        with pytest.raises(ValueError, match="multiple of bdcycle"):
            Boundary(config, cams=True)

    def test_regular_boundary_unchanged(self, cams_config):
        boundary = Boundary(cams_config)
        assert boundary.bdmodel == "ifs"
        assert not boundary.cams


@pytest.fixture(name="recent_config")
def fixture_recent_config(default_config, tmp_directory):
    """Config with CAMS active, starting yesterday at 06 UTC.

    CAMS data are only available for recent dates (mars.CAMS_NRT.start_date).
    """
    yesterday = datetime.now(timezone.utc) - timedelta(days=1)
    start = yesterday.strftime("%Y-%m-%dT06:00:00Z")
    return make_config(default_config, tmp_directory, start)


class TestMarsprepCams:
    """Test the CAMS retrieval."""

    def test_old_date_refused(self, default_config, tmp_directory):
        config = make_config(default_config, tmp_directory, "2020-01-01T06:00:00Z")
        with pytest.raises(ValueError, match="No data"):
            MarsprepCams(config)

    @staticmethod
    def run_mars(task, monkeypatch):
        """Replace the MARS call: record the requests and create the targets.

        Args:
            task (MarsprepCams): The task
            monkeypatch (pytest.MonkeyPatch): Monkeypatch fixture

        Returns:
            list: The requests, as dicts
        """
        requests = []

        def fake_run_request_file(req_file_name):
            request = {}
            for line in Path(req_file_name).read_text().splitlines()[1:]:
                key, value = line.strip().rstrip(",").split(" = ", 1)
                request[key] = value
            requests.append(request)
            target = request["TARGET"].strip('"')
            for step in request["STEP"].split("/"):
                Path(target.replace("[STEP]", step)).write_text(target)

        monkeypatch.setattr(task, "_run_request_file", fake_run_request_file)
        # eccodes is mocked in the unit tests
        monkeypatch.setattr(
            marsprep_cams,
            "relabel_step",
            lambda src, step: f"{src}>{step};".encode(),
        )
        return requests

    def test_requests_and_assembly(self, recent_config, monkeypatch, tmp_path):
        monkeypatch.chdir(tmp_path)
        task = MarsprepCams(recent_config)
        task.prepdir = tmp_path / "cams"
        assert task.steps == list(range(6, 13))
        yesterday = datetime.now(timezone.utc) - timedelta(days=1)
        assert task.init_date_str == yesterday.strftime("%Y%m%d")
        assert task.init_hour_str == "00"

        requests = self.run_mars(task, monkeypatch)
        task.execute()

        assert all(req["CLASS"] == "MC" for req in requests)
        assert all(req["STREAM"] == "OPER" for req in requests)
        assert not [req for req in requests if "NUMBER" in req]
        steps = {req["PARAM"]: req["STEP"] for req in requests}
        assert steps["152/130"] == "6/7/8/9/10/11/12"
        # Spectral winds are archived 3-hourly, snow 12-hourly
        assert steps["138/155"] == "6/9/12"
        assert steps["33/238/228038/228141"] == "0/12"
        assert steps["243.128/244.128/245.128"] == "0/12"

        sh7 = (task.prepdir / "ICMSH_0+7").read_text()
        assert sh7.startswith("ICMSH_0+[STEP]ICMSH.Z")
        assert sh7.endswith("ICMSH.archived0+6>7;")
        gg11 = (task.prepdir / "ICMGG_0+11").read_text()
        assert "ICMGG.GG1" in gg11
        assert gg11.endswith("ICMGG.archived0+0>11;ICMGG.archived1+0>11;")
        ua8 = (task.prepdir / "ICMUA_0+8").read_text()
        assert ua8 == "ICMUA_0+[STEP]"
        # Only the final files and the request files are left
        assert not [
            f
            for f in tmp_path.iterdir()
            if f.name.startswith("ICM") and f.suffix != ".req"
        ]

        # Nothing left to do on a rerun
        requests.clear()
        task.execute()
        assert not requests


class TestC903Cams:
    """Test the CAMS c903 settings."""

    def test_settings(self, cams_config, tmp_directory):
        task = C903Cams(cams_config)
        assert task.namelists == {
            "c903_main_cams": "fort.4",
            "c903_domain_cams": "namelist_c903_domain",
        }
        assert task.host_model_dir == "@BDDIR_CAMS@"
        assert task.output_dir == f"{tmp_directory}/lbc/cams"
        assert task.host_model_file("ICMSH_@BDMEMBER@+@L@") == "ICMSH_0+@L@"


class TestMergeCamsLbc:
    """Test the merge of the aerosols into the boundaries."""

    @pytest.fixture(name="merge_task")
    def fixture_merge_task(self, cams_config, monkeypatch, tmp_path):
        monkeypatch.chdir(tmp_path)
        task = MergeCamsLbc(cams_config)
        (tmp_path / "lbc").touch()
        (tmp_path / "aero").touch()
        commands = []
        monkeypatch.setattr(
            "tactus.tasks.boundaries.BatchJob.run",
            lambda _self, cmd: commands.append(cmd),
        )
        outputs = []
        monkeypatch.setattr(
            task.fmanager, "output", lambda src, dst: outputs.append((src, dst))
        )
        return task, commands, outputs

    def test_merge(self, merge_task, monkeypatch):
        task, commands, outputs = merge_task
        fields = {
            "lbc": {"SURFPRESSION", "S001TEMPERATURE"},
            "aero": {"SURFPRESSION", "S001SEASALT1"},
            "merged": {"SURFPRESSION", "S001TEMPERATURE", "S001SEASALT1"},
        }
        monkeypatch.setattr(task, "lfi_fields", lambda f: fields[f])
        task.merge("lbc", "aero", "merged")
        assert "lfi_alt_merge --lfi-file-in lbc aero" in commands[0]
        assert "lfi_alt_pack" in commands[1]
        assert outputs == [("merged", "lbc")]

    def test_missing_fields(self, merge_task, monkeypatch):
        task, _, outputs = merge_task
        fields = {
            "lbc": {"S001TEMPERATURE"},
            "aero": {"S001SEASALT1"},
            "merged": {"S001TEMPERATURE"},
        }
        monkeypatch.setattr(task, "lfi_fields", lambda f: fields[f])
        with pytest.raises(RuntimeError, match="S001SEASALT1"):
            task.merge("lbc", "aero", "merged")
        assert not outputs

    def test_already_merged(self, merge_task, monkeypatch):
        task, commands, _ = merge_task
        monkeypatch.setattr(task, "lfi_fields", lambda _f: {"S001SEASALT1"})
        with pytest.raises(RuntimeError, match="already holds aerosol"):
            task.merge("lbc", "aero", "merged")
        assert not commands

    def test_missing_file(self, merge_task):
        task, _, _ = merge_task
        with pytest.raises(FileNotFoundError):
            task.merge("lbc", "missing", "merged")


class TestCamsNamelists:
    """Test the CAMS namelist sections (as in the Deode test_lgp_lpc)."""

    def generate(self, config, target):
        nlgen = NamelistGenerator(config, "master")
        nlgen.load(target)
        return nlgen.assemble_namelist(target)

    def test_forecast(self, cams_config):
        nml = self.generate(cams_config, "forecast")
        assert nml["NAMGFL"]["NAERO"] == 12
        # Read by sugfl1 when NAERO > 0
        assert "NAMCOMPO" in nml
        for i in range(1, 13):
            for key in ("NCOUPLING", "NREQIN"):
                assert nml["NAMGFL"][f"YAERO_NL({i})%{key}"] == 1
            for key in ("LPT", "LPC", "LINTLIN", "LGP"):
                assert nml["NAMGFL"][f"YAERO_NL({i})%{key}"] is True
        assert nml["NAMGFL"]["YAERO_NL(1)%CNAME"] == "SEASALT1"
        assert nml["NAMPARAR"]["LAERONRTR"] is True
        assert nml["NAMPHY"]["LAERODES"] is False

    def test_forecast_without_cams(self, default_config, tmp_directory):
        config = make_config(
            default_config, tmp_directory, "2026-08-28T06:00:00Z", cams="active = false"
        )
        nml = self.generate(config, "forecast")
        assert "NAERO" not in nml["NAMGFL"]
        assert "LAERONRTR" not in nml.get("NAMPARAR", {})

    def test_forecast_namelist_off(self, default_config, tmp_directory):
        config = make_config(
            default_config,
            tmp_directory,
            "2026-08-28T06:00:00Z",
            cams="active = true\n                forecast_namelist = false",
        )
        assert use_cams_aerosols(config)
        nml = self.generate(config, "forecast")
        assert "NAERO" not in nml["NAMGFL"]
        assert "LAERONRTR" not in nml.get("NAMPARAR", {})
        # The CAMS c903 namelists are still there
        assert self.generate(config, "c903_main_cams")["NAMGFL"]["NAERO"] == 12

    def test_c903(self, cams_config):
        main = self.generate(cams_config, "c903_main_cams")
        assert main["NAMGFL"]["NAERO"] == 12
        assert "NAMCOMPO" in main
        assert main["NAMGFL"]["YAERO_NL(12)%IGRBCODE"] == 210203
        for i in range(1, 13):
            assert main["NAMGFL"][f"YAERO_NL({i})%LGP"] is True
            assert main["NAMGFL"][f"YAERO_NL({i})%LSP"] is False
        domain = self.generate(cams_config, "c903_domain_cams")
        assert domain["NAMFPC"]["CFP3DF"][0] == "SEASALT1"
        assert domain["NAMFPC"]["CFPPHY"] == ["SURFTEMPERATURE", "SURFIND.TERREMER"]


class TestCamsAvailable:
    """CAMS aerosols require nrt_aerosols.available."""

    def test_available(self, cams_config):
        assert use_cams_aerosols(cams_config)

    def test_not_available_raises(self, cams_config):
        config = cams_config.copy(update={"nrt_aerosols": {"available": False}})
        with pytest.raises(ValueError, match="not available"):
            use_cams_aerosols(config)
        with pytest.raises(ValueError, match="not available"):
            derived_variables(config)

    def test_inactive_not_available(self, cams_config):
        config = cams_config.copy(
            update={"nrt_aerosols": {"active": False, "available": False}}
        )
        assert not use_cams_aerosols(config)

    def test_default_config(self, default_config):
        assert not default_config["nrt_aerosols.available"]
