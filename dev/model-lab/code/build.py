#!/usr/bin/env python3
"""Export audited Git blobs and paired Clojure edit tasks without executing source."""
import argparse
from collections import Counter
from dataclasses import dataclass
from difflib import SequenceMatcher
from hashlib import sha256
import json
from pathlib import Path, PurePosixPath
from tempfile import TemporaryDirectory
import re
import subprocess


@dataclass(frozen=True)
class Pin:
    name: str
    start: str
    end: str
    license: str


AFTERGLOW = Pin('afterglow', 'bcdba8b51b6daeb541ea3a3dac8918268b15b2d1',
                '4d96625fc615ba6730275d02484efb9b4c384c34', 'EPL-2.0')
TOLGLOW = Pin('tolglow', '07817ede8b32859977a5d9f16bbcea5605fcac04',
              '8f0765afb5dbae03558c974173300f4d9505a42d', 'LicenseRef-OwnerLocal')
EPL_DIGEST = '0becf16567beb77fa252b7664631dd177c8f9a1889e48995b45379c7130e5303'
EXTENSIONS = {'.clj', '.cljs', '.cljc'}
EXCLUDED = {'generated', 'vendor', 'vendored', 'third_party', 'third-party', 'private',
            'secrets', 'credentials', 'scratch', 'examples', 'fixtures_library'}
SENSITIVE = re.compile(r'-----BEGIN [A-Z ]*PRIVATE KEY-----|\b(?:gh[pousr]_[A-Za-z0-9_]{25,}|'
                       r'sk-(?:proj-|ant-)?[A-Za-z0-9_-]{20,}|AKIA[A-Z0-9]{16})\b|'
                       r'(?i:(?:password|api-key|access-token|client-secret)\s+"[^"\s]{8,}")')


def digest(text):
    return sha256(text.encode('utf-8')).hexdigest()


def git(path, *args):
    return subprocess.check_output(['git', '--no-replace-objects', '-C', str(path), *args])


def source_family(pin, path):
    """Implementation, native/JVM/JS tests and both sides of a package share a family."""
    p = PurePosixPath(path)
    if p.suffix not in EXTENSIONS or any(part.lower() in EXCLUDED for part in p.parts):
        return None
    parts = list(p.with_suffix('').parts)
    if parts[0] not in {'src', 'test', 'test-native'}:
        return None
    parts.pop(0)
    if parts and parts[0] in {'shared', 'studio'}:
        parts.pop(0)
    if len(parts) < 2 or parts.pop(0) != pin.name:
        return None
    if parts[0] == 'studio' and len(parts) > 1:
        parts.pop(0)
    primary = parts[0].removesuffix('_test')
    return 'clojure/' + primary.replace('_', '-')


def tree(path, pin, rev):
    rows = git(path, 'ls-tree', '-rz', rev, '--', 'src', 'test', 'test-native').split(b'\0')
    result = {}
    for row in filter(None, rows):
        info, name = row.split(b'\t', 1)
        mode, kind, oid = info.decode().split()
        name = name.decode()
        if kind == 'blob' and mode in {'100644', '100755'} and source_family(pin, name):
            result[name] = oid
    return result


def read_blobs(repo, oids):
    """Batch the Git object reads. Working-tree edits, filters and symlinks are ignored."""
    ids = sorted(set(oids))
    request = ('\n'.join(ids) + '\n').encode() if ids else b''
    args = ['git', '--no-replace-objects', '-C', str(repo), 'cat-file', '--batch']
    data = subprocess.run(args, input=request, stdout=subprocess.PIPE, check=True).stdout
    result, cursor = {}, 0
    for oid in ids:
        end = data.index(b'\n', cursor)
        actual, kind, size = data[cursor:end].decode().split()
        if actual != oid or kind != 'blob':
            raise ValueError('Unexpected Git object in source snapshot')
        length = int(size)
        if 0 < length <= 200_000:
            try:
                result[oid] = data[end + 1:end + 1 + length].decode('utf-8')
            except UnicodeDecodeError:
                pass
        cursor = end + length + 2
    return result


def permitted(text, license_id):
    if not text or '\0' in text or SENSITIVE.search(text):
        return False
    headers = re.findall(r'(?im)^\s*;+\s*SPDX-License-Identifier:\s*([^\n]+)', text)
    if any(header.strip() != license_id for header in headers):
        return False
    return not re.search(r'(?im)^\s*;+.*(?:all rights reserved|generated file|do not edit|'
                         r'licensed under|copied from|adapted from|derived from)', text)


def rows_at_pin(pin, repo):
    start = tree(repo, pin, pin.start)
    end = tree(repo, pin, pin.end)
    blobs = read_blobs(repo, list(start.values()) + list(end.values()))
    for path, oid in sorted(end.items()):
        body = blobs.get(oid)
        if not permitted(body, pin.license):
            continue
        family = source_family(pin, path)
        source = {'repo': 'tolgraven/' + pin.name, 'commit': pin.end, 'path': path,
                  'git_blob': oid, 'license': pin.license}
        before = blobs.get(start.get(path))
        yield family, source, body, before if permitted(before, pin.license) else None


def edits(pin, family, source, after, before):
    if not before or before == after:
        return
    old, new = before.splitlines(), after.splitlines()
    matcher = SequenceMatcher(None, old, new)
    picked = 0
    for kind, i, j, k, l in matcher.get_opcodes():
        if kind != 'replace' or not (1 <= j-i <= 12 and 1 <= l-k <= 12):
            continue
        answer = '\n'.join(new[k:l])
        # Delimiters and trivial renames provide little evidence of behavior.
        if len(answer.strip()) < 36 or not any('(' in line for line in new[k:l]):
            continue
        prior = '\n'.join(old[max(0, i-3):j+3])
        context = '\n'.join(new[max(0, k-5):k] + [';; <replace-this-span>'] + new[l:l+5])
        prompt = (f"Restore the changed Clojure span in {source['path']}. Use the earlier code and the "
                  'updated context. Return just the replacement lines.\n\nEarlier code\n' + prior +
                  '\n\nUpdated context\n' + context)
        if len(prompt) > 5200 or len(answer) > 1600:
            continue
        origin = dict(source, base_commit=pin.start, old_lines=[i+1, j], new_lines=[k+1, l])
        yield {'id': digest(f"{source['git_blob']}:{k}:{l}")[:24], 'family': family,
               'mode': 'code/edit', 'source': origin, 'messages': [
                   {'role': 'user', 'content': prompt}, {'role': 'assistant', 'content': answer}]}
        picked += 1
        if picked == 8:
            return


def assign_splits(families):
    # Adding local-only data must never move a public holdout into training.
    result = {}
    for family in sorted(set(families)):
        bucket = int(digest('tolglow-code-splits-v1/' + family), 16) % 10
        result[family] = {0: 'test', 1: 'valid'}.get(bucket, 'train')
    return result


def verify_license(repo, pin):
    if pin == AFTERGLOW:
        for rev in (pin.start, pin.end):
            notice = git(repo, 'show', f'{rev}:LICENSE')
            if sha256(notice).hexdigest() != EPL_DIGEST:
                raise ValueError(f'Unrecognized EPL text at {rev}')
        readme = git(repo, 'show', f'{pin.end}:README.md').decode()
        attribution = readme[readme.index('Copyright ©'):].split('##', 1)[0].strip()
        return attribution + '\n\nSource: https://github.com/tolgraven/afterglow/tree/' + pin.end + '\n\n' + notice.decode('utf-8')
    if pin == TOLGLOW:
        return ('Owner-local source use only. The pinned Tolglow tree has no repository-wide '
                'license grant. No permission to redistribute the source corpus or derived '
                'training examples is supplied by this exporter.\n')
    raise ValueError('Source is outside the license allowlist')


def jsonl(path, rows):
    with path.open('w', encoding='utf-8', newline='\n') as out:
        for row in rows:
            out.write(json.dumps(row, ensure_ascii=False, sort_keys=True, separators=(',', ':')) + '\n')


def export(inputs, out):
    if out.exists():
        raise ValueError(f'Output already exists: {out}. Choose an unused directory.')
    sources, tasks, notices, seen_blobs, seen_answers, counts = [], [], {}, set(), set(), Counter()
    for pin, repo in inputs:
        notices[pin.name] = verify_license(repo, pin)
        for family, source, text, old in rows_at_pin(pin, repo):
            key = digest(text.replace('\r\n', '\n'))
            if key in seen_blobs:
                counts['duplicate_sources'] += 1
                continue
            seen_blobs.add(key)
            sources.append({'family': family, 'source': source, 'content': text, 'sha256': key})
            for task in edits(pin, family, source, text, old):
                answer = task['messages'][-1]['content']
                if answer in seen_answers:
                    counts['duplicate_answers'] += 1
                    continue
                seen_answers.add(answer)
                tasks.append(task)
    # Hold out whole task-bearing families; their sources never enter the training index.
    splits = assign_splits(row['family'] for row in sources)
    train_sources = [row for row in sources if splits[row['family']] == 'train']
    train_text = '\0'.join(row['content'] for row in train_sources)
    train_rows = '\0'.join(message['content'] for task in tasks if splits[task['family']] == 'train'
                          for message in task['messages'])
    output = {name: [] for name in ('train', 'valid', 'test')}
    for task in tasks:
        split = splits[task['family']]
        answer = task['messages'][-1]['content']
        if split != 'train' and (answer in train_text or answer in train_rows):
            counts['heldout_answer_in_training'] += 1
            continue
        output[split].append(task)
    summary = {'format': 1, 'inputs': [vars(pin) for pin, _ in inputs],
               'records': {k: len(v) for k, v in output.items()}, 'train_sources': len(train_sources),
               'eligible_sources': len(sources), 'excluded': dict(counts), 'families': splits,
               'scope': 'owner-local' if 'tolglow' in notices else 'EPL-2.0'}
    out.parent.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(dir=out.parent) as staging:
        target = Path(staging) / 'output'
        target.mkdir(mode=0o700)
        for name, rows in output.items():
            jsonl(target / f'{name}.jsonl', rows)
        jsonl(target / 'sources-train.jsonl', train_sources)
        for repo, notice in notices.items():
            (target / f'NOTICE-{repo}.txt').write_text(notice, encoding='utf-8')
        (target / 'manifest.json').write_text(json.dumps(summary, sort_keys=True, indent=2) + '\n', encoding='utf-8')
        target.rename(out)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--afterglow', required=True, type=Path, help='Local Afterglow Git checkout')
    parser.add_argument('--owner-local-tolglow', type=Path, metavar='CHECKOUT',
                        help='Also use your Tolglow source locally, without a redistribution grant')
    parser.add_argument('--out', required=True, type=Path, help='New output directory')
    args = parser.parse_args()
    inputs = [(AFTERGLOW, args.afterglow)]
    if args.owner_local_tolglow:
        inputs.append((TOLGLOW, args.owner_local_tolglow))
    result = export(inputs, args.out)
    print(json.dumps({key: result[key] for key in ('scope', 'records', 'train_sources', 'excluded')}, sort_keys=True))


if __name__ == '__main__':
    main()
