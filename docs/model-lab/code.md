# Pinned Clojure source and edit tasks

The exporter builds a train-only source index and chat examples from actual changes between two repository snapshots. Each edit prompt contains the earlier implementation and surrounding code from the updated file, with one marked span to restore. The expected answer is the verbatim replacement from the identified Git blob. This evaluates code infilling. Alternative correct implementations require a separate behavioral evaluation.

## Build locally

Use Python 3.9 or newer and local Git checkouts containing the commits below. Run these commands from Tolglow. Each output directory must be new.

```sh
python3 dev/model-lab/code/build.py --afterglow ../afterglow --out target/code-epl
python3 dev/model-lab/code/build.py --afterglow ../afterglow --owner-local-tolglow . --out target/code-owner-local
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s dev/model-lab/code -p 'test_*.py'
```

The first command produces the EPL source set. The second includes Tolglow for the repository owner's local experiments. Each runs offline and reads Git objects directly, so uncommitted edits, active branches, ignored files, model credentials and editor configuration stay outside the inputs. A missing Git object identifies a checkout that needs the pin fetched. Publication and training are separate operator decisions.

| Repository | Earlier snapshot | Corpus snapshot | Rights and scope |
| --- | --- | --- | --- |
| `tolgraven/afterglow` | `bcdba8b51b6daeb541ea3a3dac8918268b15b2d1` | `4d96625fc615ba6730275d02484efb9b4c384c34` | EPL-2.0, verified against the pinned license bytes at both commits. Preserve `NOTICE-afterglow.txt` with any exported data. |
| `tolgraven/tolglow` | `07817ede8b32859977a5d9f16bbcea5605fcac04` | `8f0765afb5dbae03558c974173300f4d9505a42d` | The pinned tree has no repository-wide license. `--owner-local-tolglow` includes the owner's local material and writes `NOTICE-tolglow.txt`. No redistribution grant is provided. |

The Afterglow corpus snapshot pins a specific training baseline independently of later runtime and CI updates. Every edit carries its repository, file path, base and target revisions, target blob, source license and 1-based inclusive ranges in both files. The source index preserves file text and its content digest. Model license terms and third-party libraries need their own review when assembling a larger model release.

## Splits and exclusions

Only tracked regular `.clj`, `.cljs` and `.cljc` files in first-party namespace roots under `src`, `test` or `test-native` enter the index. Vendor trees, private and scratch paths, generated files, mismatching per-file license headers, copied-source markers, credential patterns, non-UTF-8 content and files above 200 KB are excluded. The index stores comments and notices with source. Declared dependency jars, Studio vendor assets, fonts, production data, saved shows, chat and recorded performances do not qualify. A pattern scan cannot establish that arbitrary text is free of secrets or determine every contributor's rights.

Implementation namespaces and their tests share a family. A stable hash assigns whole families to training, validation or test in both export modes. The compiler and its child namespaces, for example, share `clojure/compiler` across implementations. Exact copies of files and repeated answer spans are suppressed. Any held-out answer that occurs verbatim in training source or training messages is removed. A semantic rewrite can still resemble code in another family. The exporter limits each changed file to eight bounded replacements and leaves new files available for retrieval only.

| Scope | Training edits | Validation edits | Test edits | Training source files |
| --- | ---: | ---: | ---: | ---: |
| EPL-2.0 | 72 | 5 | 23 | 158 |
| Owner-local paired | 254 | 13 | 92 | 454 |

`train.jsonl`, `valid.jsonl` and `test.jsonl` contain `id`, `family`, `mode`, `source` and role-tagged `messages`. Use only `train.jsonl` for fine-tuning. Exclude the final assistant message from evaluation prompts. `sources-train.jsonl` is the retrieval corpus for the same split. `manifest.json` records exact inputs, realized split counts, family assignments and deduplication results. The combined public release includes the attributed EPL edit examples. Owner-local outputs belong outside source control.

## Score edit predictions

Write one JSON object per generated answer with `id` and `content`, then run `python3 dev/model-lab/code/score.py target/code-epl/test.jsonl predictions.jsonl`. It reports exact, different and missing spans grouped by family and repository. Unknown or duplicate IDs produce an error. Whitespace and punctuation count in this reconstruction score. The scorer never runs predicted forms.
