# Local models for lighting control, shaders and code

**JEV** (J E V) is the first local model for show-control branch decisions.

Start on the 48 GB M4 MacBook Pro with **Qwen2.5-Coder-7B-Instruct** and use **Qwen2.5-Coder-1.5B-Instruct** as the first 8 GB iPhone candidate. Both have Apache-2.0 cards, standard causal decoder implementations, native code pretraining and the same verified tokenizer and chat template. That gives a controlled comparison across devices. Their suitability for lighting control still needs generated-answer and physical device measurements. Nine immutable base revisions, source cards and licenses are recorded in the [model inventory](../../dev/model-lab/models.json).

## Candidates by device

| Target | First experiment | Comparison and installation decision |
| --- | --- | --- |
| 8 GB iPhone or small iPad | 4-bit Coder 1.5B, approximately 0.72 GiB in packed parameters alone | Start with a short 1,024-token interactive budget. Try Coder 0.5B if the live preview or thermal pressure leaves insufficient headroom. Compare Qwen3.5-0.8B with the same held-out prompts after confirming support for its hybrid architecture on the chosen Swift runtime. |
| Larger iPad Pro | Trial 1.5B then 7B on the actual device | Reserve memory and GPU time for 120 Hz preview and Push display mirroring. Compare 4B Qwen3.5 for lower memory use. |
| 48 GB M4 MacBook Pro | Prepare separate 7B show, shader and code adapters. Packed weights start at 3.55 GiB. | Compare Coder 14B (6.88 GiB raw packed) for code edits or slower shader authoring. Run live cue entry on 1.5B if the render queue misses budgets. |
| M3 Ultra Mac Studio or equivalent | Qualify the 14B result, then trial Coder 32B (15.26 GiB raw packed) | Use the 32B as an offline candidate and a controlled teacher for additional examples reviewed by humans. Try Qwen3.5-9B and SmolLM3-3B as alternatives; rescore their own tokenizers and chat templates. |
| A100, H100 or GB200 | Train the **same** pinned phone and Mac bases if calibration favors rental | Independently test code and shader adapters on a single device. The quoted GB200 allocation charges for four GPUs even for one occupied GPU. Independent runs can use the other GPUs. |

Raw 4-bit size is parameter count divided by two and then by 2³⁰. Quantization scales, higher-precision components, KV cache, activations, OS, visualizer and workspace buffers all require additional memory. Probe sustained resident memory, hardware thermal state and foreground frame timing on each target. On a memory warning discard idle adapters and reduce context. Bind a selected adapter to a matching base hash and quantization build. [Model cards, configuration and licenses](../../dev/model-lab/models.json) · [MLX Swift LM](https://github.com/ml-explore/mlx-swift-lm) · [Hardware timing, rental and electricity estimates](hardware.md)

## Assemble and inspect data

The checked-in [public dataset](../../dev/model-lab/public) includes 32 executable MIT WebGL2 programs, 136 attributed Three.js stage/reference units and 100 EPL-2.0 Afterglow edit spans. Each compressed row carries its source and split family. Preserve the included notice and the license and attribution in each shader. The owner-local build adds the executed demo show examples and uses the paired code extractor with Tolglow enabled. Private recordings, provider conversations, credentials, custom shows and third-party code outside the approved sources have no role in either build.

| Data | Train | Validation | Test | Distinct families |
| --- | ---: | ---: | ---: | ---: |
| Public bundle in Git | 234 | 8 | 26 | 17 |
| Owner-local combined export | 846 | 96 | 181 | 88 |

The combined audit rejects duplicate IDs and exact prompts, families occurring in multiple splits and long answers repeated across splits. Thirteen short literal answers recur across different owner-local questions. The code extractor also checks held-out spans against indexed training text. Family isolation cannot exclude similar ideas in a base model’s pretraining. The 136 reference units share a training family and serve retrieval by default. [Compact forms and show verification](show.md) · [Source pins and rights](code.md) · [Shaders and stage references](shaders.md)

From the Leiningen project root, inspect the ready-to-use public subset with Python 3.9+. To reproduce the complete owner-local preparation, use the matching Afterglow checkout and root npm lock:

```sh
python3 dev/model-lab/report.py dev/model-lab/public
python3 dev/model-lab/report.py dev/model-lab/public --case glsl/palette/beat-lookup

npm ci
python3 dev/model-lab/shaders/export.py target/model-shaders --with-references
python3 dev/model-lab/code/build.py --afterglow ../afterglow --out target/code-epl
python3 dev/model-lab/assemble.py target/model-public target/code-epl target/model-shaders

lein run -m clojure.main dev/model-lab/show/export.clj target/model-show
python3 dev/model-lab/code/build.py --afterglow ../afterglow --owner-local-tolglow . --out target/code-local
python3 dev/model-lab/assemble.py --owner-local target/model-local target/model-show target/code-local target/model-shaders
```

Fresh output directories prevent mixing releases. The manifest gives SHA-256 of decompressed UTF-8 JSONL. Gzip timestamps and file order are fixed. A rebuild of the owner-local pilot produced train `c35b73b201c3f943cb92180626460074c2a0e60a7a0f41abdf79d0511af34953`, validation `41a02fabf1c55eb9bd3434e6b1f2b4b2c45e02c2aabeed72d411adc8428f332f`, test `3567209795d87ff1e3ff9006d24ad2e15fba15a2c2b3321c0a2cf9fcec49b67a`. Keep the local export and any resulting owner-local adapters within the permitted scope.

## Prepare an adapter

```sh
python3 -m venv target/model-tools
target/model-tools/bin/pip install -r dev/model-lab/tokenizer-requirements.txt
target/model-tools/bin/python dev/model-lab/prepare.py target/model-local target/mac-show \
  --tokenizer-cache target/model-tokenizers --pilot mac-pilot --adapter show
python3 dev/model-lab/peft_run.py target/mac-show
```

Repeat with `phone-pilot` and each of `show`, `shader` and `code` in a new output directory. Public-only users can immediately build `shader` and `code` from `dev/model-lab/public`. Preparation downloads two checksum-locked tokenizer metadata files and performs no inference. It emits chat-formatted splits, example IDs, processed and supervised token counts, excluded examples, input digests, the model revision and MLX configuration. All six tokenizer-only pilot preparations completed with real tokenization. The source examples and both pinned template/tokenizer digests match across pilots.

| Adapter, owner-local source | Admitted train / valid / test | Processed training tokens | Supervised training tokens | 95th percentile / max training length |
| --- | ---: | ---: | ---: | ---: |
| Show authoring, performance and Q/A | 430 / 80 / 86 | 39,317 | 14,643 | 192 / 203 |
| Standalone GLSL | 25 / 3 / 3 | 27,565 | 23,057 | 3,347 / 3,394 |
| Paired Clojure and ClojureScript edits | 254 / 13 / 92 | 99,349 | 12,573 | 599 / 794 |

A 4,096-token window excludes the entire 6,529-token classic 4D noise program. Set `--context 8192` to include it, then repeat the memory/timing calibration. By default the shader adapter fits only executable examples. `--with-references` opts into supervised reconstruction of rendering chunks, which can bias results toward copying source. The source index remains available for retrieval.

### Future fitting commands

Use disjoint output directories for the base-only benchmark and each adapter. The following commands require a deliberate future fitting session. Use a compatible Python environment for each backend. Preserve the install lock and device/driver versions with each resulting artifact.

```sh
# Apple Silicon, example using the mac-pilot base
pip install 'mlx-lm==0.32.2'
hf download Qwen/Qwen2.5-Coder-7B-Instruct \
  --revision c03e6d358207e414f1eca0bb1891e29f1db0e242 --local-dir target/base-7b
mlx_lm.convert --hf-path target/base-7b --mlx-path target/base-7b-q4 -q --q-bits 4
mlx_lm.lora --config target/mac-show/mlx-config.json --train --model target/base-7b-q4 \
  --data target/mac-show --adapter-path target/mlx-7b-show

# BF16-capable CUDA, single device. Install a matching PyTorch CUDA wheel.
pip install -r dev/model-lab/cuda-requirements.txt
python3 dev/model-lab/peft_run.py target/mac-show --train \
  --tokenizer-cache target/model-tokenizers --output target/peft-7b-show
```

Both recipes mask the prompt and use rank eight, the last sixteen decoder layers, microbatch one and a learning rate of 0.0001. The CUDA recipe targets query and value projections on a frozen NF4 base, with checkpointing and validation after one epoch. MLX applies its native 4-bit quantization and LoRA scaling rules and schedules as many training steps as admitted rows. A step count only approximates a pass under a shuffled backend. Preserve per-backend results separately. A CUDA adapter intended for Apple deployment needs a tested conversion or merge/requantization against the original base and a postconversion evaluation. Keep the original reference adapter and record the executable package and tokenizer hashes. [MLX LoRA configuration](https://github.com/ml-explore/mlx-lm/blob/v0.32.2/mlx_lm/LORA.md) · [PEFT quantized training](https://huggingface.co/docs/peft/en/developer_guides/quantization)

The six prospective pilot runs use **166,231 processed training tokens per base**, before padding and evaluation. For each independent run, pass its measured token count and startup overhead to the budget calculator. For example, `python3 dev/model-lab/budget.py estimate --hardware m4-pro-48gb --size 7B --tokens 39317 --setup-minutes 10 --overhead 0.20`. Training duration, model throughput and costs in the hardware guide are estimates. Preparation downloaded no weights and incurred no paid compute. [Detailed estimates from iPhone through GB200](hardware.md)

## Evaluation and release

1. Freeze a public or local manifest and record the base revision, tokenizer, runtime, quantization, adapter and prompt profile. Run the base and the adapter with identical decoding settings. Use validation to choose the rank, prompt and checkpoint. Unlock test only for the selected variant. The packaged test answers permit reproducible scoring and must be kept out of retrieval and the model prompt.
2. Export unlabeled inputs with `python3 dev/model-lab/evaluate.py target/model-local show --split test --prompts target/show-prompts.jsonl`. Generate a separate JSONL file of `{"id":"…","content":"…"}` externally. Run the same command with `--predictions target/show-guesses.jsonl` in place of `--prompts` to report exact, different and missing by family. Repeat with the other modes, using the same training-only source index for baseline and candidate retrieval.
3. Score accepted **behavior** too. For compact commands, run `lein run -m clojure.main dev/model-lab/show/score.clj target/model-show/test.jsonl target/show-guesses.jsonl`. The scorer replays the fixture setup, reads one EDN form and compares the expanded ordinary request on a single immutable show snapshot. Extra Clojure expressions do not execute. For vetted shader candidates in an isolated evaluation worker, run `node dev/model-lab/shaders/check.cjs target/model-shaders /usr/bin/chromium target/shader-guesses.jsonl test` to compile, link and compare uniform/texture probes to numeric oracles. The reference replay passed all 48 held-out EDN commands and all three palette programs. These results validate the scoring path. We have no model-generated quality score yet.
4. Score code replacements using the pinned span evaluator. Review alternative correct edits in a disposable, credential-free, network-isolated checkout and run the original Leiningen tests. Trusted REPL and application patches require source review before execution. Evaluate end-to-end cue selection, beat alignment, undo and isolation by replaying a deterministic synthetic show. Compare model errors by family. Three palette test examples share one upstream and cannot establish shader quality across materials.
5. Before recommending a model, add independently authored held-out shows, adversarially malformed data, realistic overlapping cues, several new GLSL families and human-rated edits. Track safe structured rejection, functional successes, missing actions, median and p95 time to first accepted action, tokens/s, peak RSS, heat and p99 show latency with the visualizer active. Suggested release gates are zero unvalidated physical output commands, all existing regression suites passing, zero new deadline misses in a maintained 40 Hz reference show over a sustained run, and better held-out success than base-only without a material latency regression. Publish trial counts and device details so small cohorts remain visible.

## Route and ship

The selected editing surface supplies the task. A cue/chat action selects the show adapter, a material or shader editor selects the shader adapter, and an application workspace patch selects code. A manual override can choose a verified installed model and compatible adapter. Keep the active adapter resident and cancel superseded generation. Retrieve from user-approved registry schemas and the indexed train-only source. Inject only the relevant effect or Clojure spans under an explicit token budget. Include source names and versions in the proposed change.

Use `tolglow.agents.forms/request` for compact commands, `forms/submit!` for accepted commands, the existing tool declarations such as `publish_effect` and `launch_cue` for structured providers, and existing application-agent workspaces for changes to the program itself. The model proposes bounded changes, curves and schedules ahead of time. A show owner checks schema, epoch and registry versions and schedules work on the musical clock. Afterglow evaluates it on its frame clock. On timeout the current show continues. Shader compilation belongs in a preview context, with last-known-good code retained until compilation and a trial render succeed. [Show forms](show.md) · [Application workspaces and rollback](../application-agents.md) · [Effect building blocks](../effect-blocks.md)

A **future** macOS build can run the adapter in a supervised local model process, stream via the existing provider interface and store model bundles outside the signed app in Application Support alongside a checksum manifest. The iOS/iPadOS deployment needs a Swift MLX runner with supported architectures, downloaded model data, memory-pressure cancellation and sustained device tests; isolate it from the motion and control queues. Restrict arbitrary native code editing to the suitable desktop development workflow and review App Store rules before submission. Ship the model card, licenses, data scope, evaluation, minimum device profile, expected download and a rollback manifest. The device chooser, native inference, weights, provisioning, physical benchmarks and trained adapters remain open. [Apple distribution rules](https://developer.apple.com/app-store/review/guidelines/#software-requirements) · [MLX Swift LM](https://github.com/ml-explore/mlx-swift-lm)
