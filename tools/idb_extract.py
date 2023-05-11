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


@dataclasses.dataclass
class IdbEntry:
    tp: str
    perm: str
    filesize: int|None
    cmp_filesize: int|None

def readIdbMetadata(idb_pname: Path) -> dict[str, IdbEntry]:
    metadata: dict[str, IdbEntry] = {}

    with idb_pname.open("r", encoding="UTF-8") as f:
        for line in f:
            parts = line.split()
            fname = parts[4]
            filesize = cmp_filesize = None
            for part in parts:
                key = value = None
                if "(" in part and ")" in part:
                    key, value = part.split("(")
                    assert value.endswith(")"), line
                    value = value[:-1]

                if key == "size":
                    assert value is not None
                    filesize = int(value)
                if key == "cmpsize" and value != "0":
                    assert value is not None
                    cmp_filesize = int(value)
            metadata[fname] = IdbEntry(
                parts[0],
                parts[1],
                filesize,
                cmp_filesize,
            )

    return metadata


def run(archive_fname: Path, idb_metadata: dict[str, IdbEntry]):
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
            pprint(f"{fname}")
            assert not fname.startswith(".")
            assert not fname.startswith("/")
            meta = idb_metadata[fname]
            cmp = False

            if meta.cmp_filesize:
                filesize = meta.cmp_filesize
                cmp = True
            else:
                filesize = meta.filesize

            data = f.read(filesize)
            assert len(data) == filesize, f"{len(data)} vs {filesize}"
            if cmp:
                pprint("Decompressing... ", end="")
                Path("tmp.z").open("wb").write(data)
                os.system("rm -f tmp; gunzip tmp.z")
                data = Path("tmp").open("rb").read()
                pprint("done.")
            Path(fname).parent.mkdir(mode=0o755, parents=True, exist_ok=True)
            Path(fname).open("wb").write(data)
            if meta.tp == "f" and meta.perm == "0755":
                os.system(f"chmod +x {fname}")
        pprint("ok")
        os.system("rm -f tmp")

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
        pprint(f"Extracting {fname}")
        run(fname, idbmetadata)

if __name__ == "__main__":
    main()
