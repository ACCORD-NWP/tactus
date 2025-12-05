"""Archive."""

import glob
import os
import pathlib
import shutil
import tarfile

from tactus.fullpos import flatten_list
from tactus.logs import logger
from tactus.toolbox import FileManager, Platform


class Archive:
    """Handle archiving."""

    def __init__(self, config, datatype=None, include=None, exclude=None):
        """Construct the archive object."""
        self.config = config
        self.platform = Platform(config)
        self.fmanager = FileManager(self.config)

        self.choices = {}
        self.archive_loc = {}
        if datatype is not None:
            archive_types = self.config.get("platform.archive_types", [])
            if include is None:
                include = []
            if exclude is None:
                exclude = []
            try:
                choices_for_type = self.config[f"archiving.{datatype}"].dict()
                logger.info("Setup archiving for type:{}", datatype)
            except KeyError as error:
                logger.error("Could not find archiving.{}", datatype)
                logger.error("Archiving includes: {}", self.config["archiving"])
                raise RuntimeError(f"Could not find archiving type {datatype}") from error
            skipped_types = []

            for archive_type, choices in choices_for_type.items():
                abort = (
                    archive_type not in include
                    and len(include) > 0
                    or archive_type in exclude
                )
                if abort:
                    msg = f"Archive method {archive_type} is not allowed for this task\n"
                    if len(exclude) > 0:
                        msg += f"Excluded methods: {','.join(exclude)}\n"
                    if len(include) > 0:
                        msg += f"Included methods: {','.join(include)}\n"
                    raise RuntimeError(msg)

                d = {
                    name: choice
                    for name, choice in choices.items()
                    if self.trigger(choice["active"])
                }
                if len(d) > 0:
                    if archive_type in archive_types:
                        self.choices[archive_type] = d
                        self.archive_loc[archive_type] = self.platform.get_value(
                            f"archiving.prefix.{archive_type}", ""
                        )
                    else:
                        skipped_types.append(archive_type)

            if len(skipped_types) > 0:
                logger.warning(
                    "Skipped archive types not defined for this host: {}", skipped_types
                )

    def trigger(self, trigger):
        """Return trigger."""
        if isinstance(trigger, bool):
            return trigger
        return self.config[trigger]

    def compress_files(self, pattern, inpath, outpath, tarname):
        """Create tarfile.

           Creates a tarfile for files matching pattern.
           If no files are found no tarfile is created and a warning is
           issued.

        Args:
            pattern (str,list): string of list of patterns to search for
            inpath (str): Full path on the input archive
            outpath (str): relative path on the output archive
            tarname (str, optional): Name of output tarname

        """
        files = []
        for ptrn in pattern:
            search = str(pathlib.PurePath(inpath, self.platform.substitute(ptrn)))
            files.append([x for x in glob.glob(search) if os.path.isfile(x)])
        files = flatten_list(files)

        if len(files) > 0:
            tar = tarfile.open(tarname, "w:gz")
            for filename in files:
                logger.info(" add:{}", filename)
                tar.add(filename, arcname=os.path.basename(filename))
            tar.close()
            self.fmanager.output(
                tarname, pathlib.PurePath(outpath, tarname), provider_id="move"
            )
        else:
            logger.warning(
                f"No files found matching {pattern}, no {tarname} file created"
            )

    def archive(
        self, pattern, inpath, outpath, archive_type=None, newname=None, tarname=None
    ):
        """Send files to the file manager.

        Args:
            pattern (str,list): string of list of patterns to search for
            inpath (str): Full path on the input archive
            outpath (str): relative path on the output archive
            archive_type (str, optional): Archive type. Defaults to None.
            newname (str, optional): Forces a rename of an identified file.
                                     Defaults to None.
            tarname (str, optional): Name of output tarname

        Raises:
            FileNotFoundError: If file not found
            RuntimeError: Erroneous tarname

        """
        out = self.platform.substitute(outpath)
        inp = self.platform.substitute(inpath)

        if newname is not None and isinstance(pattern, str):
            ptrn = self.platform.substitute(pattern)
            try:
                shutil.copy(ptrn, newname)
            except FileNotFoundError as error:
                raise FileNotFoundError(
                    f"Could not find {ptrn}, incorrect pattern"
                ) from error

            _pattern = [pathlib.PurePath(os.getcwd(), newname)]
            logger.info("Copy {} to {}", ptrn, newname)

        elif isinstance(pattern, str):
            _pattern = [pattern]
        else:
            _pattern = pattern

        if archive_type == "compress":
            if tarname is None:
                raise RuntimeError("Please give a name for the output tarfile")
            self.compress_files(_pattern, inp, out, self.platform.substitute(tarname))
        else:
            for ptrn in _pattern:
                search = str(pathlib.PurePath(inp, self.platform.substitute(ptrn)))
                files = [x for x in glob.glob(search) if os.path.isfile(x)]

                for filename in sorted(files):
                    self.fmanager.output(
                        filename,
                        pathlib.PurePath(
                            self.archive_loc[archive_type],
                            out,
                            os.path.basename(filename),
                        ),
                        provider_id=archive_type,
                    )

    def execute(self):
        """Loops over archive choices."""
        for archive_type, choices in self.choices.items():
            logger.info("Archiving type: {}", archive_type)
            for name, choice in choices.items():
                choice.pop("active")
                logger.info("Archiving {} with: {}", name, choice)
                outpath = choice.get("outpath", "")
                newname = choice.get("newname", None)
                tarname = choice.get("tarname", None)
                self.archive(
                    choice["pattern"],
                    choice["inpath"],
                    outpath,
                    archive_type,
                    newname,
                    tarname,
                )
