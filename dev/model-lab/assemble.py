#!/usr/bin/env python3
"""Assemble independent exporters without reassigning their held-out families."""
import argparse
from collections import Counter
import gzip
import hashlib
import json
from pathlib import Path
import shutil
import tempfile


SPLITS = ('train', 'valid', 'test')
PUBLIC = {'MIT', 'EPL-2.0'}
MODES = {'shader': 'shader', 'shader-reference': 'shader', 'code/edit': 'code',
         'show/edn': 'show', 'show/clojure': 'show', 'show/qa': 'show'}


def encode(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(',', ':'))


def digest(data):
    return hashlib.sha256(data).hexdigest()


def messages(row):
    chat = row['messages']
    if [m['role'] for m in chat] not in (['user', 'assistant'], ['system', 'user', 'assistant']):
        raise ValueError(f"unexpected roles for {row['id']}")
    if not all(isinstance(m['content'], str) and m['content'].strip() for m in chat):
        raise ValueError(f"missing content for {row['id']}")
    return chat


def read_split(path):
    with gzip.open(path, 'rt', encoding='utf-8') as src:
        yield from (json.loads(line) for line in src)


def verify_payloads(directory, manifest):
    for split in SPLITS:
        checksum = hashlib.sha256()
        records = 0
        with gzip.open(directory / f'{split}.jsonl.gz', 'rb') as source:
            for line in source:
                checksum.update(line)
                records += 1
        metrics = manifest['splits'][split]
        if checksum.hexdigest() != metrics['jsonl_sha256'] or records != metrics['records']:
            raise ValueError(f'corpus checksum or record count mismatch: {split}')


def public_source(source):
    return source.get('license') in PUBLIC and source.get('scope') != 'owner-local'


def load(inputs, owner_local):
    seen_ids, prompts, families = set(), set(), {}
    splits = {s: [] for s in SPLITS}
    overlaps = {}
    for directory in inputs:
        for split in SPLITS:
            with (directory / f'{split}.jsonl').open(encoding='utf-8') as src:
                for line in src:
                    row = json.loads(line)
                    chat = messages(row)
                    if not public_source(row['source']):
                        owner_source = row['source'].get('license') == 'LicenseRef-OwnerLocal' or row['source'].get('scope') == 'owner-local'
                        if not owner_local or not owner_source:
                            raise ValueError(f"source outside assembly scope: {row['id']}")
                    if row['mode'] not in MODES or not row['family']:
                        raise ValueError(f"unclassified example {row['id']}")
                    prompt = digest(encode(chat[:-1]).encode())
                    if row['id'] in seen_ids or prompt in prompts:
                        raise ValueError(f"duplicate id or prompt: {row['id']}")
                    if families.setdefault(row['family'], split) != split:
                        raise ValueError(f"family crosses splits: {row['family']}")
                    seen_ids.add(row['id'])
                    prompts.add(prompt)
                    overlaps.setdefault(chat[-1]['content'].strip(), set()).add(split)
                    splits[split].append(row)
    # Tiny expected values (e.g. an integer or a cue ID) can legitimately be
    # shared by different questions. Count them, reject repeated long answers.
    cross = [answer for answer, parts in overlaps.items() if len(parts) > 1]
    if any(len(answer) >= 64 for answer in cross):
        raise ValueError('long answer repeated between splits')
    return splits, families, len(cross)


def assemble(inputs, output, owner_local):
    if output.exists():
        raise ValueError(f'choose a new output directory: {output}')
    parts, families, common = load(inputs, owner_local)
    output.parent.mkdir(parents=True, exist_ok=True)
    manifest = {'schema': 1, 'scope': 'owner-local' if owner_local else 'public',
                'families': dict(sorted(families.items())), 'short_answers_shared_across_splits': common, 'splits': {}}
    with tempfile.TemporaryDirectory(dir=output.parent) as temp:
        build = Path(temp) / 'dataset'
        build.mkdir()
        for split, rows in parts.items():
            rows.sort(key=lambda r: r['id'])
            payload = ''.join(encode(r) + '\n' for r in rows).encode('utf-8')
            path = build / f'{split}.jsonl.gz'
            with path.open('wb') as dest, gzip.GzipFile(filename='', fileobj=dest, mode='wb', mtime=0) as zipped:
                zipped.write(payload)
            manifest['splits'][split] = {'records': len(rows), 'jsonl_sha256': digest(payload),
                                         'modes': dict(sorted(Counter(r['mode'] for r in rows).items()))}
        for directory in inputs:
            for notice in sorted(directory.glob('NOTICE-*.txt')):
                target = build / notice.name
                if target.exists() and target.read_bytes() != notice.read_bytes():
                    raise ValueError(f'conflicting notice: {notice.name}')
                shutil.copyfile(notice, target)
        (build / 'manifest.json').write_text(json.dumps(manifest, indent=2, sort_keys=True) + '\n', encoding='utf-8')
        build.rename(output)
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    parser.add_argument('inputs', nargs='+', type=Path)
    parser.add_argument('--owner-local', action='store_true', help='Allow explicitly owner-scoped rows in a local-only artifact')
    args = parser.parse_args()
    result = assemble(args.inputs, args.output, args.owner_local)
    print(result['scope'], {s: v['records'] for s, v in result['splits'].items()}, 'families', len(result['families']))


if __name__ == '__main__':
    main()
