#!/usr/bin/env python3
"""Inspect the verified dataset or read a case with its original licensing and capture."""
import argparse
import json
from pathlib import Path
from assemble import SPLITS, read_split, verify_payloads


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("dataset", type=Path)
    cli.add_argument("--case", help="Print one complete labeled example by id")
    args = cli.parse_args()
    manifest = json.loads((args.dataset / "manifest.json").read_text(encoding="utf-8"))
    verify_payloads(args.dataset, manifest)
    if args.case:
        for split in SPLITS:
            for row in read_split(args.dataset / f"{split}.jsonl.gz"):
                if args.case == row["id"]:
                    print(json.dumps(row, indent=2, ensure_ascii=False))
                    return
        cli.error("Unknown case id")
    print(f"{manifest['scope']} corpus  |  {len(manifest['families'])} split-isolated families")
    for split in SPLITS:
        stats = manifest["splits"][split]
        modes = ", ".join(f"{n} {mode}" for mode, n in stats["modes"].items())
        print(f"{split:6} {stats['records']:4}   {modes}")
    print("Verified three decompressed payload SHA-256 digests.")


if __name__ == "__main__":
    main()
