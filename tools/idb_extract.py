#!/usr/bin/env python3

from __future__ import annotations


import dataclasses
from pathlib import Path
import sys
import os
import struct

# error print
def eprint(*args, **kwargs):
    print("error: ", *args, **kwargs, file=sys.stderr)

# "progress" print
def pprint(*args, **kwargs):
    print(*args, **kwargs, file=sys.stderr)


compressedTempFile = Path("tmp.z")
decompressedTempFile = Path("tmp")

@dataclasses.dataclass
class IdbEntry:
    fname: Path
    tp: str
    perm: str
    filesize: int|None
    cmp_filesize: int|None
    symval: Path|None # for symlinks

    compressedTempFile.unlink(missing_ok=True)
    decompressedTempFile.unlink(missing_ok=True)

    def getFileSize(self) -> int|None:
        if self.cmp_filesize is not None:
           return self.cmp_filesize
        return self.filesize

    def isCompressed(self) -> bool:
        return self.cmp_filesize is not None

    def writeFile(self, data: bytes):
        assert self.tp == "f", self.tp

        filesize = self.getFileSize()
        assert filesize is not None

        assert len(data) == filesize, f"{len(data)} vs {filesize}"

        if self.isCompressed():
            pprint("        Decompressing... ", end="")
            compressedTempFile.write_bytes(data)

            # gzip.decompress doesn't work
            retVal = os.system(f"gunzip {compressedTempFile}")
            if retVal != 0:
                pprint("failed.")
                return

            data = decompressedTempFile.read_bytes()
            compressedTempFile.unlink(missing_ok=True)
            decompressedTempFile.unlink(missing_ok=True)
            pprint("done.")

        self.fname.parent.mkdir(mode=0o755, parents=True, exist_ok=True)
        self.fname.write_bytes(data)
        if self.perm == "0755":
            self.fname.chmod(int(self.perm, 8))

    def makeSymlink(self):
        assert self.tp == "l", self.tp
        assert self.symval is not None

        pprint(f"    symlink: '{self.fname}' -> ", end="")

        self.fname.parent.mkdir(parents=True, exist_ok=True)

        relativeTarget = Path(self.symval)
        self.fname.symlink_to(relativeTarget)

        realTarget = self.fname.parent / relativeTarget
        pprint(f"'{realTarget}'")


def readIdbMetadata(idb_pname: Path) -> dict[Path, IdbEntry]:
    metadata: dict[Path, IdbEntry] = {}

    with idb_pname.open("r", encoding="UTF-8") as f:
        for line in f:
            parts = line.split()
            tp = parts[0]
            perm = parts[1]
            fname = Path(parts[4])
            filesize = None
            cmp_filesize = None
            symval = None

            for part in parts:
                key = None
                value = None

                if "(" in part and ")" in part:
                    key, value = part.split("(")
                    assert value.endswith(")"), line
                    value = value[:-1]

                if key is not None:
                    assert value is not None
                    if key == "size":
                        filesize = int(value)
                    elif key == "cmpsize":
                        if value != "0":
                            cmp_filesize = int(value)
                    elif key == "symval":
                        symval = Path(value)

            metadata[fname] = IdbEntry(
                fname,
                tp,
                perm,
                filesize,
                cmp_filesize,
                symval,
            )

    return metadata


def run(archive_fname: Path, idb_metadata: dict[Path, IdbEntry]):
    pprint(f"Extracting {archive_fname}")

    with archive_fname.open("rb") as f:
        header = f.read(13)
        assert header in (b'im001V500P00\0', b'im001V530P00\0', b"im001V620P02\0", b"im001V630P00\0", b"im001V405P10\0"), header

        while True:
            size_bytes = f.read(2)
            if not size_bytes:
                break

            fname_size, = struct.unpack(">H", size_bytes)
            if fname_size > 500:
                eprint("bad filename size", fname_size, size_bytes)
                exit(1)

            fname = f.read(fname_size)
            assert len(fname) == fname_size, fname

            fname = fname.decode("ascii")
            pprint(f"    {fname}")
            assert not fname.startswith(".")
            assert not fname.startswith("/")

            meta = idb_metadata[Path(fname)]
            meta.writeFile(f.read(meta.getFileSize()))

        pprint("ok\n")


def handleSymlinks(idb_metadata: dict[Path, IdbEntry]):
    pprint(f"Processing symlinks")

    # Handle symlinks
    for sym, meta in idb_metadata.items():
        if meta.tp != "l":
            continue
        meta.makeSymlink()

    pprint(f"Symlinks done")


def main():
    if len(sys.argv) < 2:
        eprint(f"Usage: {sys.argv[0]} FILE_STEM.*, including FILE_STEM.idb file")
        exit(1)

    idb_file: Path|None = None
    files: list[Path] = []
    for fname in sys.argv[1:]:
        if fname.endswith(".idb"):
            idb_file = Path(fname)
        else:
            files.append(Path(fname))

    if idb_file is None:
        eprint(f"Usage: {sys.argv[0]} FILE_STEM.*, including FILE_STEM.idb file")
        exit(1)

    pprint(f"Using .idb file {idb_file}")

    idbmetadata = readIdbMetadata(idb_file)

    for fname in files:
        run(fname, idbmetadata)

    handleSymlinks(idbmetadata)

if __name__ == "__main__":
    main()
