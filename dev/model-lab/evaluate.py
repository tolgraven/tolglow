#!/usr/bin/env python3
"""Export held-out prompts or score generated text by scenario family without executing it."""
import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path
from assemble import MODES, encode, read_split


def cases(dataset, split, mode):
    return [r for r in read_split(dataset / f"{split}.jsonl.gz")
            if MODES[r["mode"]] == mode and r["mode"] != "shader-reference"]


def prompts(rows):
    return ({"id": r["id"], "messages": r["messages"][:-1]} for r in rows)


def load_predictions(path):
    result = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        row = json.loads(line)
        if row["id"] in result or not isinstance(row["content"], str):
            raise ValueError(f"Duplicate id or non-text completion: {row['id']}")
        result[row["id"]] = row["content"]
    return result


def score(rows, predictions):
    unknown = predictions.keys() - {r["id"] for r in rows}
    if unknown:
        raise ValueError(f"Unknown prediction ids: {sorted(unknown)}")
    groups = defaultdict(Counter)
    for row in rows:
        result = predictions.get(row["id"])
        status = ("missing" if result is None else
                  "exact" if result == row["messages"][-1]["content"] else "different")
        groups[row["family"]][status] += 1
    return {"examples": len(rows), "by_family": dict(groups), "counts": sum(groups.values(), Counter())}


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("dataset", type=Path)
    cli.add_argument("adapter", choices=("show", "shader", "code"))
    cli.add_argument("--split", choices=("valid", "test"), default="valid")
    group = cli.add_mutually_exclusive_group(required=True)
    group.add_argument("--prompts", type=Path)
    group.add_argument("--predictions", type=Path)
    args = cli.parse_args()
    rows = cases(args.dataset, args.split, args.adapter)
    if not rows:
        cli.error("No held-out examples of this type in this dataset")
    if args.prompts:
        with args.prompts.open("x", encoding="utf-8") as out:
            for row in prompts(rows):
                out.write(encode(row) + "\n")
    else:
        print(json.dumps(score(rows, load_predictions(args.predictions)), indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
