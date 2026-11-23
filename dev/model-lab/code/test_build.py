import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

import build
import score


def git(repo, *args):
    return subprocess.check_output(['git', '-C', str(repo), *args], text=True).strip()


def write(repo, path, value):
    target = repo / path
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(value)


class CodeCorpusTest(unittest.TestCase):
    def test_source_tree_allowlist_and_family(self):
        permitted = ('src/afterglow/effects/shape.clj', 'test-native/afterglow/effects/shape_test.clj')
        self.assertEqual(['clojure/effects'] * 2, [build.source_family(build.AFTERGLOW, p) for p in permitted])
        self.assertEqual('clojure/shape', build.source_family(build.TOLGLOW, 'src/studio/tolglow/studio/shape_test.cljs'))
        for path in ('src/different/lib.clj', 'resources/tolglow/user.clj', 'src/afterglow/vendor/large.clj',
                     'src/afterglow/foo.js', 'dev/afterglow/demo.clj'):
            self.assertIsNone(build.source_family(build.AFTERGLOW, path))
        self.assertFalse(build.permitted(';; SPDX-License-Identifier: MIT\n(ns test)', 'EPL-2.0'))
        self.assertFalse(build.permitted(';; Copied from a separate project', 'EPL-2.0'))
        self.assertFalse(build.permitted('(def client {:api-key "example-super-secret"})', 'EPL-2.0'))

    def test_holdouts_do_not_change_when_the_second_repo_is_added(self):
        families = {'clojure/control', 'clojure/render', 'clojure/registry'}
        base = build.assign_splits(families)
        extended = build.assign_splits(families | {'clojure/rig'})
        self.assertEqual(base, {f: extended[f] for f in families})

    def test_predictions_are_matched_by_id_and_missing_answers_count(self):
        example = {'mode': 'code/edit', 'family': 'clojure/fixture', 'source': {'repo': 'tolgraven/afterglow'},
                   'messages': [{'role': 'assistant', 'content': '(+ x 1)'}]}
        refs = {key: example for key in ['a', 'b', 'c']}
        preds = {'b': {'content': '(+ x 2)'}, 'a': {'content': '(+ x 1)'}}
        self.assertEqual({'total': 3, 'exact': 1, 'different': 1, 'missing': 1}, score.score(refs, preds)['total'])
        with self.assertRaises(ValueError):
            score.score(refs, {'unknown': {'content': 'outside holdout'}})

    def test_pinned_git_objects_tasks_and_notices(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = root / 'git'
            repo.mkdir()
            git(repo, 'init', '-q')
            git(repo, 'config', 'user.name', 'Local corpus test')
            git(repo, 'config', 'user.email', 'test@localhost')
            for n in range(30):
                name = f'src/afterglow/controller{n}.clj'
                write(repo, name, f'(ns afterglow.controller{n})\n(defn combine-{n} [signal]\n  (+ {n} signal))\n')
            write(repo, 'LICENSE', 'test-only verification fixture')
            write(repo, 'README.md', 'Copyright © local test contributors\n')
            git(repo, 'add', '.')
            git(repo, 'commit', '-qm', 'Before')
            start = git(repo, 'rev-parse', 'HEAD')
            for n in range(30):
                name = f'src/afterglow/controller{n}.clj'
                text = f'(ns afterglow.controller{n})\n(defn combine-{n} [signal]\n  (+ {n} (* signal 0.75) (* signal signal)))\n'
                write(repo, name, text)
            write(repo, 'src/afterglow/clone.clj', text)
            write(repo, 'src/afterglow/override.clj', ';; SPDX-License-Identifier: GPL-3.0\n(ns override)')
            git(repo, 'add', '.')
            git(repo, 'commit', '-qm', 'After')
            end = git(repo, 'rev-parse', 'HEAD')
            write(repo, 'src/afterglow/controller1.clj', 'dirty working tree value')
            pin = build.Pin('afterglow', start, end, 'EPL-2.0')
            with patch.object(build, 'AFTERGLOW', pin), patch.object(build, 'EPL_DIGEST', build.digest('test-only verification fixture')):
                first = build.export([(pin, repo)], root / 'first')
                second = build.export([(pin, repo)], root / 'second')
                self.assertEqual(first, second)
                self.assertEqual(1, first['excluded']['duplicate_sources'])
                self.assertTrue(all(first['records'][k] > 0 for k in ('train', 'valid', 'test')))
                self.assertIn('Copyright © local test contributors', (root / 'first/NOTICE-afterglow.txt').read_text())
                staged = (root / 'first/sources-train.jsonl').read_text()
                self.assertNotIn('dirty working tree value', staged)
                self.assertNotIn('override.clj', staged)
                for file in (root / 'first').iterdir():
                    self.assertEqual(file.read_bytes(), (root / 'second' / file.name).read_bytes())
                families = {}
                for split in ('train', 'valid', 'test'):
                    rows = [json.loads(line) for line in (root / 'first' / (split + '.jsonl')).read_text().splitlines()]
                    families[split] = {r['family'] for r in rows}
                    for row in rows:
                        source = row['source']
                        self.assertEqual((start, end), (source['base_commit'], source['commit']))
                        a, b = source['new_lines']
                        chunk = git(repo, 'show', end + ':' + source['path']).splitlines()[a-1:b]
                        self.assertEqual('\n'.join(chunk), row['messages'][-1]['content'])
                self.assertFalse(families['train'] & (families['test'] | families['valid']))
                self.assertFalse(families['test'] & families['valid'])
                with self.assertRaises(ValueError):
                    build.export([(pin, repo)], root / 'first')
                with patch.object(build, 'EPL_DIGEST', 'unrecognized'):
                    with self.assertRaises(ValueError):
                        build.export([(pin, repo)], root / 'bad')
                    self.assertFalse((root / 'bad').exists())


if __name__ == '__main__':
    unittest.main()
