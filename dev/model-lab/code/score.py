#!/usr/bin/env python3
"""Score replacement spans by ID without executing model-generated source."""
import argparse
from collections import Counter
import json
from pathlib import Path


def read_rows(path):
    result = {}
    with path.open(encoding='utf-8') as source:
        for line in source:
            row = json.loads(line)
            key = row['id']
            if key in result:
                raise ValueError(f'Duplicate ID in {path}: {key}')
            result[key] = row
    return result


def score(reference, predictions):
    extras = set(predictions) - set(reference)
    if extras:
        raise ValueError(f'Unknown prediction IDs: {sorted(extras)}')
    families, repositories, totals = {}, {}, Counter()
    for key, row in reference.items():
        expected = row['messages'][-1]
        if row['mode'] != 'code/edit' or expected['role'] != 'assistant':
            raise ValueError('Expected code/edit references with final assistant spans')
        repo = row['source']['repo']
        family = row['family']
        prediction = predictions.get(key, {}).get('content')
        if key in predictions and not isinstance(prediction, str):
            raise ValueError(f'Expected string content for {key}')
        status = 'missing' if prediction is None else 'exact' if prediction == expected['content'] else 'different'
        for metric in [totals, families.setdefault(family, Counter()), repositories.setdefault(repo, Counter())]:
            metric['total'] += 1
            metric[status] += 1
    return {'total': dict(totals), 'by_family': families, 'by_repository': repositories}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('reference', type=Path)
    parser.add_argument('predictions', type=Path, help='JSONL rows of {id, content}')
    args = parser.parse_args()
    print(json.dumps(score(read_rows(args.reference), read_rows(args.predictions)), sort_keys=True, indent=2))


if __name__ == '__main__':
    main()
