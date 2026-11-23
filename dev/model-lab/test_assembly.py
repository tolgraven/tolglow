import gzip
import json
from pathlib import Path
import tempfile
import unittest

from assemble import SPLITS, assemble, encode, load, verify_payloads
from evaluate import cases, prompts, score
from prepare import write_split, tokenize
from peft_run import inspect, labeled_examples


def example(split):
    return {"id": split, "mode": "shader", "family": split, "source": {"license": "MIT"},
            "messages": [{"role": "user", "content": "question " + split},
                         {"role": "assistant", "content": "answer " + split}]}


def char_encoder(chat, generate=False):
    return list("".join(m["role"] + m["content"] for m in chat) + ("assistant" if generate else ""))


class AssemblyTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name)
        self.src = self.root / "source"
        self.src.mkdir()
        self.rows = {split: example(split) for split in SPLITS}
        self.write()

    def write(self):
        for split, row in self.rows.items():
            (self.src / f"{split}.jsonl").write_text(encode(row) + "\n")

    def test_scope_and_family(self):
        self.rows["train"]["source"] = {"scope": "owner-local"}
        self.write()
        with self.assertRaisesRegex(ValueError, "scope"):
            load([self.src], False)
        load([self.src], True)
        self.rows["valid"]["family"] = "train"
        self.write()
        with self.assertRaisesRegex(ValueError, "family"):
            load([self.src], True)

    def test_rejects_repeated_prompt_and_long_answer(self):
        self.rows["test"]["messages"][0] = self.rows["train"]["messages"][0]
        self.write()
        with self.assertRaisesRegex(ValueError, "duplicate"):
            load([self.src], False)
        self.rows["test"] = example("test")
        for split in ("test", "train"):
            self.rows[split]["messages"][1]["content"] = "a" * 100
        self.write()
        with self.assertRaisesRegex(ValueError, "answer"):
            load([self.src], False)

    def test_deterministic_bundles(self):
        paths = [self.root / name for name in ("one", "two")]
        for path in paths:
            manifest = assemble([self.src], path, False)
            verify_payloads(path, manifest)
        for split in SPLITS:
            self.assertEqual(*[(p / f"{split}.jsonl.gz").read_bytes() for p in paths])
        with gzip.open(paths[0] / "train.jsonl.gz", "ab") as dest:
            dest.write(b"{}\n")
        with self.assertRaisesRegex(ValueError, "checksum"):
            verify_payloads(paths[0], manifest)

    def test_public_release(self):
        public = Path(__file__).parent / "public"
        manifest = json.loads((public / "manifest.json").read_text())
        verify_payloads(public, manifest)
        self.assertEqual(manifest["scope"], "public")
        self.assertEqual([manifest["splits"][s]["records"] for s in SPLITS], [234, 8, 26])
        self.assertTrue((public / "NOTICE-afterglow.txt").exists())

    def test_eval_prompts_and_coverage(self):
        public = self.root / "public"
        assemble([self.src], public, False)
        rows = cases(public, "valid", "shader")
        self.assertEqual(list(prompts(rows)), [{"id": "valid", "messages": [self.rows["valid"]["messages"][0]]}])
        self.assertEqual(score(rows, {})["counts"]["missing"], 1)
        self.assertEqual(score(rows, {"valid": "answer valid"})["counts"]["exact"], 1)
        with self.assertRaisesRegex(ValueError, "Unknown"):
            score(rows, {"train": "answer train"})

    def test_response_loss_and_context(self):
        dataset = self.root / "public"
        assemble([self.src], dataset, False)
        record = self.rows["train"]
        self.assertEqual(tokenize(record, char_encoder)[1], len("userquestion trainassistant"))
        stats = write_split(dataset, self.root, "train", "shader", 100, char_encoder, False)
        self.assertEqual(stats["assistant_tokens"], len("answer train"))
        encoded = labeled_examples(self.root, "train", char_encoder)[0]
        self.assertEqual(encoded["labels"][-len("answer train"):], list("answer train"))
        self.assertTrue(all(i == -100 for i in encoded["labels"][:-len("answer train")]))
        model = next(m for m in json.loads((Path(__file__).parent / "models.json").read_text())["models"]
                     if m["role"] == "phone-pilot")
        recipe = {"base": model["id"], "revision": model["revision"], "splits": {"train": stats}}
        (self.root / "recipe.json").write_text(encode(recipe))
        inspect(self.root)
        with self.assertRaisesRegex(ValueError, "no usable"):
            write_split(dataset, self.root, "train", "shader", 2, char_encoder, False)
        with self.assertRaisesRegex(ValueError, "split changed"):
            inspect(self.root)


if __name__ == "__main__":
    unittest.main()
