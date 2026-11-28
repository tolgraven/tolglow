# Native show-control agent: model strategy

**Status: research and proposal. Nothing in this document is implemented and nothing was measured on this machine.**
Umbrella issue [#821](https://github.com/tolgraven/tolglow/issues/821). Repository facts were read at `bce22156` (origin/master, 2026-11-28). External facts were checked against primary sources on 2026-11-28 and are linked where used. Numbers marked **assumed** or **estimated** are arithmetic on other people's measurements; numbers marked **recalled** come from a paper's body and were not re-fetched.

Sibling reports from the parallel planning lanes. They were reconciled into one set in the [native agent index](README.md), which also holds the merged phase plan, the issue map and the consolidated operator decisions:

| Document | Owns |
| --- | --- |
| [interface.md](interface.md) | Observation and action JSON, the look encoding, the Room render and clip pipeline, DSL and code generation, scheduling and cancellation |
| [learning.md](learning.md) (PR #825) | Preference signals, reward heads, DPO-family and offline-RL training, profiles, following, the repository |
| [orchestration.md](orchestration.md) (PR #824) | Proposers, arbitration, horizons, budgets, the local, API and hosted backends |
| This document | Which models, at which sizes, trained how, decoded how, run on what, at what cost, and in which order |

It builds on the existing [local model lab](../../model-lab/README.md) (#323), which already pins nine base revisions, prepares show, shader and code corpora, and costs training from iPhone to GB200. This report does not repeat that work. It changes the question from "which coder model can author shows" to "which models does a real-time show agent need, and how do we get them".

## Recommendation

1. **Do not train a language-capable model from scratch, and do not start with pruning.** Start from small Apache-2.0 or MIT pretrained models and specialise them by distillation and adapters. From-scratch training is right for two small things only: the reward heads over the look encoding, and, much later, a bar-level reflex policy over a closed action vocabulary.
2. **Use several models split by horizon, not one.** The agent's jobs differ by three orders of magnitude in latency budget and in how much language and code they need:

   | Role | Horizon | Model | Where it runs |
   | --- | --- | --- | --- |
   | Floor | every decision frame | The deterministic planner from #217 | In the show owner |
   | **Policy** | one bar to one phrase ahead | A fine-tuned 0.6 to 2B text model emitting the action schema under a grammar | Local sidecar |
   | Planner / teacher | two phrases to a whole night | An API model, or a local 7 to 9B model | API, hosted, or local off the clock |
   | Author | off the clock | A 7 to 14B code model or an API model, writing effects that are validated in preview and then registered | Local or API |
   | Reward and critique | after the fact | Small heads on the look encoding; a frozen vision encoder with a head; a large vision-language model as offline critic | Local, off the lighting deadline |

3. **The policy model reads structured state, not pixels.** The renderer already knows every fixture's colour, level and aim, so pixels add nothing the look encoding cannot carry for *deciding*. Pixels matter for *judging*: fog, beams and overlap. Put the vision models on the reward side, where they can be large and slow.
4. **Constrain every backend to one schema subset.** Locally that is a grammar compiled from the action schema with the current show's valid identifiers baked in. On API models it is the provider's structured output mode. The owner's validation stays the authority.
5. **Run the local models in a supervised sidecar process over loopback HTTP.** Start with llama.cpp's server, because it is the only runtime today that offers per-request JSON-schema grammars, per-request LoRA selection and a prompt cache behind one OpenAI-compatible endpoint. Train with MLX or PEFT as the model lab already plans. Never load weights into the lighting JVM.
6. **Follow a gated path**: API model with the schema, then untuned local models under the grammar, then a distilled policy, then preference adapters, then compression and the reflex model. Each phase has a validity gate, a deadline gate and a preference win-rate gate (section 5).
7. **Budget.** No hardware purchase is needed through phase 3. A teacher corpus costs roughly $5 to $460 in API calls depending on the teacher, a full-corpus fine-tune roughly $10 to $60 of rented GPU per run, and per-profile adapters run overnight on the M4 Pro for electricity only (section 6).

Two findings for the other lanes. Both are now applied in this set ([reconciliation](README.md#reconciliation-record)):

- **MobileCLIP weights cannot ship.** The interface lane's observation example names `mobileclip-s0` as the look-embedding encoder. Apple releases MobileCLIP and MobileCLIP2 weights under its Machine Learning Research Model License, which grants use "exclusively for Research Purposes" and excludes commercial products ([LICENSE_MODELS](https://github.com/apple/ml-mobileclip/blob/main/LICENSE_MODELS)). Use an Apache-2.0 or MIT encoder instead (section 3).
- **Current Claude models reject a forced tool choice.** The orchestration lane's API adapter plans "one forced `propose` tool". On Claude Opus 5.5, Sonnet 5.5, Fable 5.1 and Fable 5.2, `tool_choice` of type `any` or `tool` returns a 400, and non-default `temperature`, `top_p` and `top_k` are rejected on those models and on Haiku 5.5 (Anthropic API reference, cached 2026-11-03). Use structured outputs or a `strict` tool with `auto`, and get candidate diversity from asking for several alternatives in one reply, not from temperature.

## Observed in the repository

- **The agent loop is provider-neutral and already validates.** `tolglow.integrations.agent/run!` runs a bounded streaming tool loop off the render path. `validate-arguments` checks a deliberate subset of JSON Schema (`type`, `properties`, `required`, boolean `additionalProperties`, `items`, `enum`, numeric and length bounds) and fails closed on anything else. Sixteen tools are declared, including `schedule_events` (up to 64 events in one atomic batch), `cancel_scheduled` and `inspect_schedule` from #645.
- **A self-hosted OpenAI-compatible endpoint is already a provider.** `tolglow.integrations.providers` knows `:openai`, `:anthropic`, `:openrouter` and `:self-hosted`. A local llama.cpp, MLX or Ollama server is reachable today with no new code.
- **The model lab exists and stops before fitting.** [docs/model-lab](../../model-lab/README.md) pins Qwen2.5-Coder 0.5B to 32B, Qwen3.5 0.8B, 4B and 9B, and SmolLM3-3B in `dev/model-lab/models.json`. It has 596 verified show records across 29 scenario families, executable scorers for compact forms and shaders, MLX and PEFT launch recipes (rank 8, last 16 layers), and a budget calculator. Its own text says there is "no model-generated quality score yet" and that device memory and thermal measurements are pending.
- **A replayable rehearsal harness exists.** `tolglow.rehearsal` streams a recorded agent through the real `run!` and `show/dispatch-tool!` over recorded passages with captured output, and reviews phrasing, repetition and lateness (#670). It records agent turnaround apart from frame timing. This is the natural bench for any model.
- **A deterministic floor exists.** `tolglow.automation.policy` proposes commands, never mutates the show, and uses the real compiler and frame resolver to predict emission (`tolglow.automation.prediction`). `tolglow.knob-candidates.generate` is a second seeded, explainable generator.
- **Not present:** any local inference, any grammar-constrained decoding, any trained weights, any vision model, any latency measurement of a model against the beat clock.

## The job in numbers

The interface lane's draft examples give the first concrete sizes. A full observation snapshot is about 9.5 kB of JSON, a delta about 1.3 kB, and a phrase plan with guards about 1.9 kB pretty-printed. At roughly three characters per token for numeric JSON that is about **3,200 tokens in for a snapshot, 450 for a delta and 450 to 650 out for a phrase plan** (estimated; tokenizer-dependent).

The orchestration lane's deadline rule is: a request sent *n* beats ahead must be answered in *n* beats minus a commit margin of one beat plus 50 ms. With 4/4 time and the four-bar phrase its tables use:

| Request sent | Budget at 70 BPM | at 128 BPM | at 180 BPM |
| --- | ---: | ---: | ---: |
| One bar ahead | 2.52 s | 1.36 s | 0.95 s |
| One phrase ahead | 12.8 s | 6.98 s | 4.95 s |
| Two phrases ahead | 26.5 s | 14.5 s | 10.3 s |

Three consequences drive everything below.

- **Output tokens are the latency.** Decoding is 5 to 15 times slower per token than prompt processing on Apple Silicon (section 4). A 600-token plan costs four times a 150-token one. The compactness of the action format is a model-strategy decision as much as an interface one.
- **The prompt must be a stable prefix plus a small delta.** Rig, catalogue, schema and goals change rarely; clock, audio, look and schedule change every request. A warm prefix cache turns a 3,200-token prompt into a 450-token one.
- **The task is narrow.** It is a fixed schema over a few dozen operations and the identifiers of one show. That is exactly the regime where small fine-tuned models match large general ones (section 1).

## 1. Train from scratch, or start from an existing model

### The options compared

| Approach | Data needed | Natural-language goals | Writes code | Size | Latency on M4 Pro | Licence |
| --- | --- | --- | --- | --- | --- | --- |
| Small transformer from scratch on Tolglow trajectories | 10⁵ to 10⁷ decision steps that do not exist yet; a simulator can supply rule-made ones | None, unless a pretrained text encoder supplies a goal vector | No | 1 to 50M | Milliseconds | Ours |
| Decision transformer or offline RL over a structured action space | The same logged steps, plus a reward on every step and broad coverage of good and bad behaviour | Same as above | No | 1 to 50M from scratch; or as a fine-tuning objective on a pretrained model | Milliseconds from scratch | Ours |
| Fine-tune or distil an existing small model (0.3 to 4B) | 10² to 10³ examples per operation, which a teacher model and the simulator can produce | Yes, inherited | Data-shaped DSL yes; real Clojure or GLSL poorly below about 7B | 0.2 to 2.5 GB at 4-bit | 0.7 to 5 s per decision (section 4) | The base's: Apache-2.0 or MIT for the recommended ones |
| Prune or trim an existing model to the domain | The same task corpus, plus the larger corpus needed to heal general ability if that is wanted | Kept only as far as the healing data covers | Degrades first | A further 1.5 to 3 times smaller | Faster for sub-1B models where the vocabulary dominates | The base's; pruned weights are derivatives |
| API model with a schema, no training | None | Best | Best | None locally | 1 to 10 s plus network | Provider terms; per-token cost |

### A small transformer from scratch

What from-scratch models can do is well mapped. TinyStories showed that models "below 10 million total parameters" write fluent multi-paragraph English, but only inside a synthetic world built from the vocabulary of a three-year-old (Eldan and Li 2023, [arXiv:2305.07759](https://arxiv.org/abs/2305.07759)). General language ability costs far more: SmolLM2-1.7B was trained on about 11 trillion tokens (Allal et al. 2025, [arXiv:2502.02737](https://arxiv.org/abs/2502.02737)). At any single-GPU throughput the model lab assumes, that is many years of GPU time. A from-scratch model will therefore not understand "deep red, sparse, one spotlight" unless something pretrained turns that sentence into a vector for it.

There is prior work in this exact domain, and it took this route. Skip-BART maps audio to one light's hue and intensity per frame, learned end-to-end from recordings of lighting engineers, and beat rule-based baselines in a human study (Zhao et al. 2025, ICLR 2026, [arXiv:2506.01482](https://arxiv.org/abs/2506.01482)). It needed pre-training and transfer learning "because data is limited", and a later paper notes it yields only one colour per time point ([arXiv:2602.08838](https://arxiv.org/abs/2602.08838)). A search found no published system in which a language model emits scheduled lighting commands; commercial "AI lighting" boxes such as MaestroDMX do not describe their models.

Tolglow has one real advantage here: a seeded, deterministic simulator. The planner, the rehearsal harness and the showcase rig can generate unlimited trajectories faster than real time. But those trajectories contain the planner's behaviour. A model cloned from them is a slower planner.

**Verdict: use from-scratch training for the parts with a closed vocabulary and no language.** That is the tier-1 reward heads in [learning.md](learning.md) now, and a bar-level reflex policy later, when the decision log holds enough human-shaped data. At 128 BPM a show has 1,920 bars an hour, so 10⁵ bar-level steps is about nine six-hour nights at one venue.

### Decision transformers and offline RL

Decision Transformer recasts offline RL as sequence modelling: condition on a desired return and predict the action with a causally masked transformer (Chen et al. 2021, [arXiv:2106.01345](https://arxiv.org/abs/2106.01345)). Its Gym models were a few layers at 128 dimensions (recalled), which shows how small a structured policy can be. Gato put text, images and control in one 1.2B-parameter sequence model by discretising continuous values into 1,024 bins (Reed et al. 2022, [arXiv:2205.06175](https://arxiv.org/abs/2205.06175); figures recalled). The token design carries over directly: one token per operation, per registered cue and per quantised value.

The limits also carry over. Return-conditioned supervised learning returns the optimal policy only "under a set of assumptions" stronger than those of dynamic-programming methods, and the authors show the limits "will also cause issues in practice" (Brandfonbrener et al. 2022, [arXiv:2206.01079](https://arxiv.org/abs/2206.01079)). The assumptions are near-deterministic dynamics and data that covers the returns one asks for. Tolglow's show is deterministic under a seed, which helps. Its reward is not: human preference is sparse, noisy and arrives late, and one venue's logs cover a narrow band of behaviour. Value-based methods such as CQL ([arXiv:2006.04779](https://arxiv.org/abs/2006.04779)) and IQL ([arXiv:2110.06169](https://arxiv.org/abs/2110.06169)) need a learned critic and more data still.

**Verdict: adopt the idea as an objective, not as an architecture.** Prefixing a pretrained policy's context with the outcome to aim for, and weighting logged actions by advantage, are fine-tuning recipes that fit the model lab unchanged. [learning.md](learning.md) specifies them. A pure decision transformer from scratch is the phase-4 reflex candidate.

### Fine-tuning and distilling an existing small model

The evidence that small fine-tuned models reach large-model accuracy on a fixed set of functions is consistent:

| Result | Numbers | Source |
| --- | --- | --- |
| Octopus v2, Gemma-2B, Android functions | 99.5% at 0.38 s against GPT-4 at 98.6% and 1.02 s. With only 100 examples per function, 98.1%. LoRA rank 16 loses 0.5 points against full fine-tuning | Chen and Li 2024, [arXiv:2404.01744](https://arxiv.org/abs/2404.01744) |
| TinyAgent, TinyLlama-1.1B, 16 Mac functions, 80K synthetic examples | 12.7% untuned to 80.1% tuned, against GPT-4-Turbo at 79.1%. 4-bit quantisation kept accuracy (80.4%) and cut latency 30% | Erdogan et al. 2024, [arXiv:2409.00608](https://arxiv.org/abs/2409.00608) |
| FunctionGemma, 270M | 58% to 85% on Mobile Actions after fine-tuning; 126 tokens/s on a phone CPU | [Model card](https://huggingface.co/google/functiongemma-270m-it) |
| Position paper | Small models are "sufficiently powerful, inherently more suitable, and necessarily more economical" for repetitive agent sub-tasks | Belcak et al. 2025, [arXiv:2506.02153](https://arxiv.org/abs/2506.02153) |

All three accuracy results are self-reported on the authors' own task sets. An independent study found small models' multi-turn function calling far weaker than single-turn (Qwen3-1.7B: 55% overall, 17% multi-turn; [arXiv:2511.22138](https://arxiv.org/abs/2511.22138)). The proposal contract is single-turn by design, which is the favourable case.

Distillation is how the examples get made. A strong teacher plays seeded simulated shows through the real owner; the owner rejects invalid actions; the reward heads and critic rank the rest; the student is fine-tuned on what survives (Hinton et al. 2015, [arXiv:1503.02531](https://arxiv.org/abs/1503.02531); rationale-augmented variants need less data, Hsieh et al. 2023, [arXiv:2305.02301](https://arxiv.org/abs/2305.02301)). Operator performances, rewritten as the same commands, are the second and more valuable source.

Code is where small models stop. Qwen2.5-Coder's own report gives Aider pass rates of 14% at 0.5B, 29% at 1.5B and 56% at 7B ([arXiv:2409.12186](https://arxiv.org/abs/2409.12186), tables 16 to 19). Clojure is a low-resource language even for frontier models: the only numbers found are from one unreviewed blog post, which reports 38% on MultiPL-E Clojure for a fine-tuned Qwen3-8B, 49% after reinforcement learning, and 45 to 64% for two frontier API models ([nibzard.com/clojure-phone](https://nibzard.com/clojure-phone), 2026-04-21). The only GLSL benchmark reports that top models fail to produce working shader code about 31% of the time ([ShaderMatch, LLM4Code 2025](https://conf.researchr.org/details/icse-2025/llm4code-2025-papers/13/Evaluating-Language-Models-for-Computer-Graphics-Code-Completion)). So the real-time policy should not write code at all (section 2).

### Compression methods

| Method | What it removes | Evidence | Use here |
| --- | --- | --- | --- |
| Quantisation | Bits per weight | 4-bit kept TinyAgent's accuracy. GPTQ ([arXiv:2210.17323](https://arxiv.org/abs/2210.17323)) and AWQ ([arXiv:2306.00978](https://arxiv.org/abs/2306.00978)) are the post-training methods. Google ships quantisation-aware 4-bit Gemma 4 files | **Always.** 4-bit for inference, the model lab's default. Re-run the validity gate after quantising |
| Task distillation into a smaller family member | Everything the task does not need, by never teaching it | Section above | **The main route to a small model.** Pick the next size down and distil again |
| Layer dropping | Whole blocks | "Minimal degradation of performance until after a large fraction (up to half) of the layers are removed", healed by a small QLoRA fine-tune (Gromov et al. 2024, [arXiv:2403.17887](https://arxiv.org/abs/2403.17887)). ShortGPT reports the same redundancy ([arXiv:2403.03853](https://arxiv.org/abs/2403.03853)) | Phase 4, for the phone. Cheap to try: drop, heal on the task corpus, re-run gates |
| Structured pruning with distillation | Width, heads, MLP neurons, depth | NVIDIA's Minitron derives 8B and 4B models from 15B with "up to 40× fewer training tokens per model compared to training from scratch" ([arXiv:2407.14679](https://arxiv.org/abs/2407.14679)). Sheared-LLaMA prunes 7B to 1.3B and 2.7B for "only 3% of compute" ([arXiv:2310.06694](https://arxiv.org/abs/2310.06694)). LLM-Pruner ([arXiv:2305.11627](https://arxiv.org/abs/2305.11627)) is the gradient-based variant | Not now. Vendors already publish the pruned sizes; doing it ourselves pays only if no published model fits the phone |
| Unstructured sparsity | Individual weights | SparseGPT ([arXiv:2301.00774](https://arxiv.org/abs/2301.00774)), Wanda ([arXiv:2306.11695](https://arxiv.org/abs/2306.11695)) | No. Apple GPU runtimes gain nothing from sparse weights |
| Vocabulary trimming | Embedding rows for tokens the domain never uses | Half the vocabulary "is enough" for a language-specific model (Ushio et al. 2023, [arXiv:2305.15020](https://arxiv.org/abs/2305.15020)). In sub-1B models the vocabulary is most of the model: Gemma 3 270M has "170 million embedding parameters" and "100 million for our transformer blocks" ([Google](https://developers.googleblog.com/en/introducing-gemma-3-270m/)); Qwen2.5-0.5B's published config implies 136M of 494M | Phase 4. JSON punctuation, digits, a few thousand identifiers and plain English fit in 16 to 32 thousand tokens. For a 270M-class model that removes about half the weights, and the output projection over the vocabulary is also most of its per-token arithmetic. The speed gain is an expectation, not a measurement |

### Candidate bases

Facts from model cards and licence texts fetched on 2026-11-28. BFCL is the Berkeley function-calling benchmark, version 4; "official" means the public leaderboard (updated 2026-04-12), "vendor" means the model card. Vendor and official figures are not comparable with each other.

| Model | Size | Licence | BFCL v4 | Notes |
| --- | --- | --- | --- | --- |
| [Qwen3-0.6B, 1.7B, 4B-2507](https://huggingface.co/Qwen/Qwen3-4B-Instruct-2507) | 0.6 / 1.7 / 4B | Apache-2.0 | 23.9 / 28.4 / 35.7 official | Plain transformer, supported everywhere, the only family with measured M4 Pro speeds. The conservative choice |
| [Qwen3.5-0.8B, 2B, 4B, 9B](https://huggingface.co/Qwen/Qwen3.5-2B) | 0.8 / 2 / 4 / 9B | Apache-2.0 | 25.3 / 43.6 / 50.3 / 66.1 vendor | Image and video input built in. Hybrid attention architecture; llama.cpp support landed in February 2026, text-only at merge. The 0.8B and 2B cards warn of thinking loops. Already pinned in the model lab |
| [Gemma 4 E2B, E4B](https://huggingface.co/google/gemma-4-E2B-it) | 2.3 / 4.5B effective | **Apache-2.0** | not listed | Image and audio input; 70 to 1,120 tokens per image. With its per-layer embeddings E2B holds 5.1B parameters, so it is not small in memory |
| [Granite 4.0 350M, 1B; 4.2 3B](https://huggingface.co/ibm-granite/granite-4.2-3b) | 0.35 / 1 / 3B | Apache-2.0 | 19.0 official (350M); 52.4 vendor (4.2-3b) | Dense variants exist specifically for llama.cpp |
| [SmolLM3-3B](https://huggingface.co/HuggingFaceTB/SmolLM3-3B) | 3B | Apache-2.0 | not comparable | Open training recipe; already pinned in the model lab |
| [Phi-4-mini](https://huggingface.co/microsoft/Phi-4-mini-instruct) | 3.8B | MIT | not listed | Card warns it may "hallucinate function names" |
| [FunctionGemma 270M](https://huggingface.co/google/functiongemma-270m-it) | 0.27B | Gemma Terms | 61.6 simple, 25.7 live-multiple (card) | Built to be fine-tuned into a function caller. Licence passes use restrictions downstream and lets Google "restrict (remotely or otherwise) usage" ([terms](https://gemma.ai/resources/terms/)) |
| [LFM3-1.7B](https://huggingface.co/LiquidAI/LFM3-1.7B), LFM2.5 | 1.2 to 2.6B | LFM Open License | 71.4 vendor | Best vendor score at this size. Commercial use by anyone with $10M revenue is unlicensed ([licence](https://www.liquid.ai/lfm-license)), and that would bind users too. LFM3 does not load in mlx-lm yet |
| [Qwen2.5-Coder 1.5B, 7B, 14B](https://qwenlm.github.io/blog/qwen2.5-coder-family/) | as named | Apache-2.0 (the 3B is not) | not listed | The model lab's pilots. Keep them for the author role |
| xLAM-2, Hammer 2.1, Octopus v2 | 0.5 to 8B | CC-BY-NC | 21 to 47 official | Non-commercial. Reference points only |
| Llama 3.2 1B, 3B | 1.2 / 3.2B | Llama Community | 10.8 / 22.0 official | Naming and "Built with Llama" duties on every derivative. No newer small Llama exists |

Three things follow.

- **Licence decides the shortlist before quality does.** The operator wants a public in-app repository of profiles, including adapters trained by users. Only Apache-2.0 and MIT bases allow that without flowing use policies, naming rules or revenue caps down to every user. That removes Gemma 3 and FunctionGemma, Llama, Liquid and the non-commercial specialists, and keeps Qwen, Gemma 4, Granite, SmolLM and Phi.
- **Zero-shot rank is a weak guide.** Fine-tuning moved TinyLlama from 13% to 80%. The choice among sub-2B bases should be made after one small adapter each, on Tolglow's own scenarios.
- **Proposed bake-off:** Qwen3.5-2B as the favourite and Qwen3-1.7B as the conservative control for the phrase-ahead policy; Qwen3-0.6B, Qwen3.5-0.8B and Granite-4.0-1B for the bar-ahead size; Qwen3.5-4B, Granite-4.2-3B and SmolLM3-3B as the larger step; FunctionGemma as a licence-encumbered yardstick for how low the size can go.

## 2. Structured input and output

### Constrained decoding

A grammar makes malformed output impossible by masking tokens that cannot continue a valid string. The engines differ a great deal ([JSONSchemaBench](https://arxiv.org/abs/2501.10868), Geng et al., ICML 2025):

| Engine | Schema coverage (GitHub-medium / Kubernetes) | Speed | Behaviour on unsupported schema features |
| --- | --- | --- | --- |
| llguidance (Guidance) | 0.69 / 0.91 | 6.4 ms per output token against 15.4 ms unconstrained, because forced tokens are fast-forwarded | Raises an error |
| llama.cpp GBNF converter | 0.57 / 0.76 | about 30 ms per token in the benchmark | **Skips them silently** ([grammars README](https://github.com/ggml-org/llama.cpp/blob/master/grammars/README.md)) |
| XGrammar | 0.52 / 0.07 | fast; default in vLLM and SGLang, the likely hosted runtime | n/a |
| Outlines | 0.29 / 0.57 | 3.5 to 8 s to compile a grammar | n/a |

llguidance builds into llama.cpp behind a compile flag, preserves property order, and reports a mask cost of about 50 µs on a 128k vocabulary ([llama.cpp docs](https://github.com/ggml-org/llama.cpp/blob/master/docs/llguidance.md)). mlx-lm's servers have no built-in schema mode; LM Studio adds Outlines on top, and mlx-swift-lm has a guided-generation library.

Does constraining hurt quality? Tam et al. found reasoning drops under strict formats ([arXiv:2408.02442](https://arxiv.org/abs/2408.02442)). A re-run with proper structured generation found small gains instead ([dottxt](https://blog.dottxt.ai/say-what-you-mean.html)), and JSONSchemaBench measured GSM8K rising from 80.1% to as much as 83.8% under constraint. The real mechanism is that naive masking distorts the model's distribution when the grammar fights its token habits (Park et al., [arXiv:2405.21047](https://arxiv.org/abs/2405.21047); Beurer-Kellner et al., [arXiv:2403.06988](https://arxiv.org/abs/2403.06988)). Fine-tuning on the exact canonical format removes that fight, which is one more reason to distil.

**Recommendations for [interface.md](interface.md):**

- **Bake the show into the grammar.** Compile the action schema per show revision with the valid cue IDs, instance IDs, parameter names and group names as enumerations. The model then cannot name a cue that does not exist. This needs an engine that compiles in milliseconds, which rules out Outlines.
- **Do not rely on the grammar for numbers.** llama.cpp's converter ignores numeric bounds on `number`. The existing tools use `minimum` and `maximum` heavily. Bounds stay with `validate-arguments` and the owner.
- **Put a short `note` first.** Output follows schema order, so a `note` of at most 20 tokens before the events gives the model a place to commit to a direction, and gives the decision log its explanation. Do not allow free-form reasoning on the clock. (The field is `note`, not to be confused with a group's `intent` tags.)
- **Canonical form.** Fixed key order (the action schema's declared order, with `note` before the parts; see [interface.md](interface.md#canonical-form)), no optional whitespace, defaults omitted. [learning.md](learning.md) needs this so that preference losses compare decisions, not formatting, and it also shortens output.
- **Spend tokens like money.** Row arrays over objects, short keys, one event per line. Octopus v2 went further and gave each function its own token, cutting context by 95%. Dedicated tokens for operations and for a show's registered cues are a phase-2 option once the format is frozen; they need the embedding layer trained, not just a LoRA.

### One schema subset for every backend

| Backend | Mechanism | Limits |
| --- | --- | --- |
| Local llama.cpp | `json_schema` per request through llguidance | Subset above |
| Local MLX | A guided-generation library, or none | No server-side schema mode in mlx-lm |
| Claude | `output_config.format`, or a tool with `strict: true` and `tool_choice: auto` | No forced tool choice on current models; schema needs `additionalProperties: false` and `required` |
| OpenRouter, OpenAI-compatible | `response_format` of type `json_schema`, or tools | Support varies by upstream provider; route only to providers that honour it |
| Hosted Tolglow | The same grammar through vLLM or SGLang | XGrammar's weaker coverage of exotic schemas |

The action schema should stay inside the intersection: objects with fixed properties, arrays, enumerations, strings, numbers, booleans. No `oneOf` mixed with `properties`, no `patternProperties`, no conditional schemas. That is almost exactly the subset `validate-arguments` already enforces, which is a good sign. A test should assert that every construct in the action schema is accepted by all four.

### Tool-call formats

Each model family has its own trained format: Hermes-style `<tool_call>` JSON for Qwen3, an XML form for Qwen3.5 and Granite 4.2, Pythonic calls for LFM and Llama 3.2, special tokens for FunctionGemma. llama.cpp's server parses about ten of them natively and falls back to a token-heavier generic format ([function-calling docs](https://github.com/ggml-org/llama.cpp/blob/master/docs/function-calling.md)). MCP is a host protocol, not a model output format.

**The policy model should not use tool calling at all.** The proposal contract is one request and one structured reply, so the reply *is* the action document, constrained by the grammar. That sidesteps per-family parsers and makes the base model swappable. Tool calling remains right for the chat agent and for the planner and author roles, where a model inspects, edits and iterates over several turns.

### DSL, code, or a code-writing sub-model

| Output | Checkable before it runs | Schedulable and cancellable | Small-model quality |
| --- | --- | --- | --- |
| Commands as data (the action schema) | Fully, by the owner | Yes | High after fine-tuning |
| Effects as data: templates and the expression tree in the interface lane's `author` example, or the compact EDN forms in [model-lab/show.md](../../model-lab/show.md) | Yes: schema, then compile, then a preview render | The launch is; the definition is published first | Plausible at 2 to 4B under a grammar; unmeasured |
| Clojure or GLSL source | Only by compiling and running it in isolation | No | Poor below about 7B; weak for Clojure at any size |

CodeAct found code actions give "up to 20% higher success rate" than JSON actions for large models on multi-tool tasks (Wang et al. 2024, [arXiv:2402.01030](https://arxiv.org/abs/2402.01030)). That is an argument for letting the *planner* write code, not the policy.

**Recommendation: three tiers of authoring, routed by what is being written.**

1. The policy emits commands only, plus a request to author: a brief, constraints and a deadline in phrases.
2. Effects-as-data go to whichever model is resident and meets the deadline, under a grammar for the definition schema.
3. Real source goes to the author model (a 7 to 14B coder with the model lab's code or shader adapter, or an API model), off the clock. Its output passes the existing gates: schema, compile, a preview render with last-known-good kept, then registration. Only then can the policy launch it by ID. Whether source from an agent may run at all is the orchestration lane's operator decision 7.

A grammar for a Lisp subset is feasible with llguidance's context-free grammars and guarantees balanced, well-formed forms. It does not guarantee meaning. SynCode ([arXiv:2403.01632](https://arxiv.org/abs/2403.01632)) and PICARD ([arXiv:2109.05093](https://arxiv.org/abs/2109.05093)) are the references.

## 3. Seeing the look: vision encoders, small VLMs, and separate reward and policy models

The operator pictured the agent looking at the Room preview directly, while recognising that small models and multimodality rarely go together. That gap has narrowed: Qwen3.5-0.8B and 2B and Gemma 4 E2B all take images natively under Apache-2.0. The question is where pixels earn their cost.

### Encoders that run on device

| Encoder | Image tower | Licence | Text tower for goal matching | Notes |
| --- | --- | --- | --- | --- |
| [MobileCLIP2-S0 / S2 / B](https://github.com/apple/ml-mobileclip) | 11.4 / 35.7 / 86.3M | **Research only** | Yes | 1.5 / 3.6 / 10.4 ms per image in Apple's table (the repository names an iPhone 12 Pro Max for its comparisons). The best speed for accuracy, and unusable in a product |
| [SigLIP 2 B/16](https://huggingface.co/google/siglip2-base-patch16-224) | 86M | Apache-2.0 | Yes | Released at 86M, 303M, 400M and 1B ([arXiv:2502.14786](https://arxiv.org/abs/2502.14786)). The default recommendation |
| [DINOv2 ViT-S/14, B/14](https://github.com/facebookresearch/dinov2) | 21 / 86M | Apache-2.0 | No | Self-supervised features; strong for similarity and novelty, cannot score against a text goal |
| [TinyCLIP](https://github.com/wkcn/TinyCLIP) | 8 to 63M | Not confirmed | Yes | ICCV 2023. 41 to 64% zero-shot ImageNet. Check the licence before use |
| SmolVLM-256M / 500M | 93M SigLIP-B/16 + 135M or 360M LM | Apache-2.0 | It is a VLM | Under 1 GB at inference; 80 tokens/s in a browser on an M4 Max ([arXiv:2504.05299](https://arxiv.org/abs/2504.05299)) |
| Qwen3.5-0.8B / 2B, Gemma 4 E2B | whole model | Apache-2.0 | It is a VLM | Gemma 4: 70 tokens per image at the lowest budget |
| FastVLM | FastViTHD + a 0.5B or larger LM | Apple; not checked | It is a VLM | 85× faster time to first token than LLaVA-OneVision at the same 0.5B LM ([arXiv:2412.13303](https://arxiv.org/abs/2412.13303)). Check for the same research-only terms as MobileCLIP before use |

No Apple Silicon Mac latency was found for these encoders. An 86M ViT-B at 224 pixels is expected to take single-digit to low tens of milliseconds on an M4 Pro, on the GPU or, through Core ML, on the Neural Engine. The Neural Engine route is worth measuring for exactly this model: unlike a language model it has static shapes, and it would keep the encoder off the GPU that Room is using.

### Frozen encoder plus a small head, or one small VLM?

**For the policy: neither needs pixels.** A robot needs vision because it cannot read the world's state. Tolglow's agent can: the renderer's inputs are the ground truth, and the interface lane's look encoding already carries per-group colour, level, aim and spread, a palette, a luma histogram, coarse floor and front maps, and a per-step brightness rhythm. Parisi et al. showed frozen pretrained visual features "can be competitive or even better than ground-truth state representations" for control ([arXiv:2203.03580](https://arxiv.org/abs/2203.03580)). Read the other way, that means vision at best matches good state, and state costs 300 text tokens against a render plus an encoder pass. Pixels also cannot show the policy what it most needs, which is how a *candidate* would look. No frame of that exists at decision time; it has to be predicted from state, as `automation.prediction` already does.

**For the reward: a frozen encoder plus a small head, clearly.** This is how every public preference scorer for generated images is built. LAION's [aesthetic predictor](https://github.com/LAION-AI/aesthetic-predictor) is a linear layer on frozen CLIP embeddings. PickScore is a CLIP-based scorer trained on user choices that "exhibits superhuman performance on the task of predicting human preferences" ([arXiv:2305.01569](https://arxiv.org/abs/2305.01569)); ImageReward ([arXiv:2304.05977](https://arxiv.org/abs/2304.05977)) and HPS v2 ([arXiv:2306.09341](https://arxiv.org/abs/2306.09341)) follow the same pattern. A head of a few megabytes per profile on a shared frozen encoder is also exactly the shape [learning.md](learning.md) wants for portable taste. One caution: zero-shot, small encoders are weak judges, since "larger VLMs trained with more compute and data are better reward models" (Rocamonde et al., ICLR 2024, [arXiv:2310.12921](https://arxiv.org/abs/2310.12921)). The head must be trained on in-domain pairs, and no public model was trained on dark rooms with volumetric beams.

**A single small VLM as policy** is the alternative. It is one model and one runtime, and it would let a backend without the look encoding still see. It costs volatile image tokens that can never be prefix-cached (at least 210 for three cameras on Gemma 4), a vision tower pass per decision, and a dependence on how a 2B model reads a dark frame, which nobody has measured. It is worth one experiment, not the main line.

**A middle path worth testing later:** project the frozen encoder's embedding into a handful of soft tokens for the text policy, the way LLaVA-style models connect a vision tower. The interface lane's example already carries a 32-number look embedding. Small vision-language-action models for robots use this layout and decouple slow perception from fast action with an asynchronous stack (SmolVLA, "comparable to VLAs that are 10× larger", [arXiv:2506.01844](https://arxiv.org/abs/2506.01844)). The gate is simple: it must raise the policy's preference win rate over the same policy without it.

### Separate models for reward and policy

The policy must be small because it answers inside a bar or a phrase. Nothing that judges has that constraint. So split them:

| Model | Size | Runs | Job |
| --- | --- | --- | --- |
| Policy | 0.6 to 2B text | On the clock | Propose |
| Tier-1 reward heads | Under 5M, trained from scratch on look features | At decision time, on every candidate | Rank candidates inside the safe set |
| Tier-2 vision reward | 21 to 86M frozen encoder plus a head | After the show, over replayed clips | Teach tier 1; score what a feature vector flattens |
| Critic | A 9 to 12B local VLM, or an API model with image input | Offline, or at arc cadence | Label clip pairs with a preference and a reason; write critiques the operator can read; bootstrap the reward heads before human pairs exist |

RL-VLM-F is the precedent for the critic: show a vision-language model two observations and a goal, ask which is better, and train a reward function on the answers, because asking for preferences is more reliable than asking for scores, which "can be noisy and inconsistent" (Wang et al., ICML 2024, [arXiv:2402.03681](https://arxiv.org/abs/2402.03681)). Critic labels are a prior, not ground truth. They enter training at reduced weight and only after the critic agrees with held-out operator pairs.

### What the visual path costs

All figures estimated.

| Item | Cost |
| --- | --- |
| Encoder weights | 42 MB (ViT-S) to 172 MB (ViT-B) at 16-bit |
| Encoding one 4-bar clip: 2 frames per beat, 3 cameras, 96 frames | About 1 s at 10 ms per frame |
| Encoding the current look once a bar, 3 cameras | About 30 ms per bar |
| Rendering those frames | **The real cost.** Room is already too slow (#782). Clips are rendered after the show from replay; a low-resolution look renderer is the interface lane's design |
| Local critic, Qwen3.5-9B at 4-bit | About 5.5 GB resident. A 16-frame pair is 1,100 to 4,000 prompt tokens: roughly 5 to 12 s per pair, so 300 to 700 pairs an hour overnight, for electricity |
| API critic at an assumed 4,000 tokens in and 300 out per pair | About $0.0006 per pair on Claude Haiku 5.5, $0.011 on Sonnet 5.5, $0.022 on Opus 5.5. Ten thousand pairs: $6, $110 or $220, half that through the batch interface |
| Image input on the policy (if tested) | 210 or more uncacheable prompt tokens and one vision-tower pass per decision |

**Where it fits in the plan:** the visual path is a parallel track that never blocks the policy. V0, with phase 1: pick the encoder on licence and measured latency. V1, with phase 2: the critic labels replayed clip pairs. V2, with phase 3: the tier-2 head must beat tier 1 on held-out operator pairs, which is already the ship test in #841. V3, phase 4: look tokens or image input on the policy, if they pass their gate.

## 4. On-device inference on Apple Silicon

### Runtimes

| Runtime | Schema-constrained output | LoRA per request | Prompt cache | Server | Verdict |
| --- | --- | --- | --- | --- | --- |
| **llama.cpp `llama-server`** (b12822, 2026-11-28) | Yes: `json_schema` and grammars per request; llguidance optional | Yes: adapters loaded at start, scaled per request | Yes: reuses the common prefix; slots can be saved | OpenAI-compatible; parallel slots | **Serve with this first** |
| **mlx-lm** (0.32.2) | No | The first server takes an adapter path per request; the new server does not yet | Yes | OpenAI-compatible; documented as "not recommended for production" | **Train with this.** Benchmark as the second serving runtime |
| mlx-swift-lm (3.33) | Yes, a guided-generation library | Yes, runtime toggle | Yes | A library, no server | The route to iPhone and iPad, and a candidate for a Swift sidecar |
| Ollama (0.40) | Yes | No: adapters are set when a model is built (not re-verified for 0.40) | Yes | Yes | Fine for a user's own experiments. Not for profiles |
| LM Studio | Yes | Not verified | Engine-dependent | Yes, proprietary app | A user-installed option only |
| Core ML / Neural Engine | n/a | n/a | n/a | n/a | **Not for the language model**; try it for the vision encoder |
| Apple Foundation Models | Yes, guided generation | **No path.** The adapter toolkit's 26.0.0 release was its last and does not work on OS 27 ([Apple](https://developer.apple.com/apple-intelligence/foundation-models-adapter/)) | Managed | Swift API | An optional zero-download backend at most. It cannot carry profiles |

Sources: [llama.cpp server README](https://github.com/ggml-org/llama.cpp/blob/master/tools/server/README.md), [mlx-lm SERVER.md](https://github.com/ml-explore/mlx-lm/blob/main/mlx_lm/SERVER.md), [Ollama structured outputs](https://docs.ollama.com/capabilities/structured-outputs).

Why not the Neural Engine for the language model: Apple's own Core ML port of Llama 3.1 targets the GPU, reaching 33.7 tokens/s on an M1 Max, and explains that decoding is bound by memory bandwidth ([Apple ML research](https://machinelearning.apple.com/research/core-ml-on-device-llama)). The Neural Engine needs static shapes and has many undocumented compile limits ([arXiv:2603.06728](https://arxiv.org/abs/2603.06728)). MLX runs on the GPU and does not use it.

MLX decodes somewhat faster than llama.cpp on the same chip (344 against 297 tokens/s on Qwen3-0.6B, table below). That 10 to 15% does not outweigh the missing schema mode today. It is a reason to re-check each quarter, and to keep the adapter format portable.

### JVM integration

| Option | Crash containment | Overhead | Verdict |
| --- | --- | --- | --- |
| **HTTP over loopback to a supervised sidecar** (`java.net.http`) | A native or Metal crash cannot take down the show | A loopback round trip, small against 0.5 to 10 s of inference | **Recommended.** It is what the model lab, orchestration.md and #833 already assume, and the `:self-hosted` provider works with it today |
| JNI or the foreign-function API against libllama | None: a crash in native code kills the lighting JVM | Lowest | Rejected. The maintained Java binding tracks upstream about 100 builds behind, and libllama's C API changes often |
| Pure-JVM inference (Jlama, llama3.java) | Memory-safe | CPU only, no Metal; competes with the lighting threads | Rejected |
| LangChain4j, Spring AI | As the backend they wrap | A large dependency tree | No benefit over the existing provider layer |

A bundled sidecar binary must be signed and notarised with the app; models live outside the signed bundle with a checksum manifest, as the model lab already says.

### Latency against the musical horizon

Published speeds on the M4 Pro, 4-bit weights:

| Model | Decode, tokens/s | Prompt processing, tokens/s | Source |
| --- | ---: | ---: | --- |
| Qwen3-0.6B | 297 llama.cpp, 344 MLX | not given | [BaseRT](https://huggingface.co/papers/2607.00501), about July 2026 |
| Qwen2.5-0.5B fine-tune, 1k context | 185 | 2,895 | [oMLX community run](https://omlx.ai/benchmarks/zw8qtt8o) |
| Llama-3.2-1B | 230 llama.cpp, 258 MLX | not given | BaseRT |
| Llama-3.2-3B | 102 llama.cpp, 112 MLX | not given | BaseRT |
| Llama-2-7B | 50 | 364 to 440 | [llama.cpp discussion 4167](https://github.com/ggml-org/llama.cpp/discussions/4167), an old build |
| Qwen2.5-7B, 4k context | 49.6 | 397 | oMLX community run, quantisation not stated |
| Qwen2.5-14B, 4k context | 24.5 | 190 | oMLX community run |

Apple gives the M4 Pro 273 GB/s of memory bandwidth and the M4 Max up to 546 ([Apple](https://www.apple.com/newsroom/2024/10/apple-introduces-m4-pro-and-m4-max/)); decode speed scales roughly with that, so a Max is about twice as fast.

From those, an **estimated** time per decision with a warm prefix cache and a 500-token delta. Prompt rates for the 1 to 4B rows are interpolated.

| Size | Decode | Prompt | 150-token reply | 500-token reply | Cold 3,200-token prompt alone |
| --- | ---: | ---: | ---: | ---: | ---: |
| 0.6 to 0.8B | 300 | 2,900 | **0.7 s** | 1.8 s | 1.1 s |
| 1 to 2B | 240 | 1,500 | **1.0 s** | 2.4 s | 2.1 s |
| 3 to 4B | 105 | 800 | 2.1 s | **5.4 s** | 4.0 s |
| 7 to 9B | 50 | 400 | 4.3 s | 11 s | 8.0 s |
| 14B | 24.5 | 190 | 8.8 s | 23 s | 17 s |

Set against the budgets:

- **Bar-ahead** (0.95 to 2.52 s): a model of at most about 2B up to 128 BPM and under 1B at 180 BPM, and only with a terse reply. At 180 BPM even the smallest is marginal. This is the reflex tier, and above about 150 BPM it should fall back to the rules floor or to a from-scratch model.
- **Phrase-ahead** (4.95 to 12.8 s): a 1 to 2B model with a full 500-token plan at any tempo; a 3 to 4B model up to about 128 BPM; a 7 to 9B model only with a terse reply.
- **Two phrases and beyond**: 7 to 14B models and API models. This is the planner and the author.

So the policy is a 0.6 to 2B model. The orchestration lane reached the same conclusion from one third-party figure; these numbers support it across five sizes.

Not included, and not found published anywhere: time to first token with a warm cache in llama.cpp or MLX, the effect of a grammar on these rates (JSONSchemaBench suggests a gain with llguidance), Qwen3.5's hybrid architecture on this chip, thermal throttling over a six-hour night, and **contention with Room on the same GPU**. The nearest evidence is that saturating memory bandwidth slowed an 8B model's decoding by 72% on a base M4 ([NeurIPS 2026 workshop](https://neurips.cc/virtual/2026/171323)). All of these are measurements for #323 and #833.

### Memory on M-series laptops

Raw 4-bit weights are parameters ÷ 2 bytes, as in the model lab. Add the key-value cache per slot and runtime overhead.

| Resident set | 48 GB M4 Pro | 16 GB Mac | 8 GB phone |
| --- | --- | --- | --- |
| Policy, 0.8 to 2B: 0.5 to 1.3 GB plus about 0.3 GB of cache per slot | Yes | Yes | 0.6 to 1B only |
| Vision encoder: 0.05 to 0.2 GB | Yes | Yes | Yes |
| Author or local planner, 7 to 9B: 4 to 5.5 GB; 14B: about 8.5 GB | Yes; load on demand | 7B at most, with nothing else open | No: use an API |
| Local critic, 9 to 12B VLM: 5.5 to 7 GB | After the show | No | No |

On the 48 GB machine the lighting JVM is capped near 3 GB by convention, and Chrome with Room, Live and the OS take a share that has not been measured during a show. A policy model of about 1.5 GB resident and one larger model loaded on demand is comfortable. Keeping a 14B author and a 9B critic resident during a show is not sensible.

### What profiles cost as adapters

[learning.md](learning.md) asks for the adapter shape per device and the cost of training and serving several.

- **Shape.** Rank 8 on the query and value projections of the last 16 layers, as the model lab's recipes use. For a 2B model that is on the order of a few megabytes at 16-bit. A profile's reward heads and style card are smaller still.
- **Each adapter needs its own prefix cache.** An adapter changes activations, so the cache built under one profile is invalid under another. Three profiles answering in one tick means three slots, three caches, and three sequential generations: about 3 s for 150-token replies on a 1 to 2B model. llama.cpp does not batch requests with different adapters together.
- **A cheaper way to follow someone.** Generate candidates once with the local policy and let each followed profile's *reward head* score them. That costs one generation, not three, and works on API backends that cannot load an adapter at all. A followed profile is then a ranker first and a generator only where the budget allows. This fits [learning.md](learning.md)'s portable profile. The orchestration report adopts it and counts a ranker's influence in the dominance ledger ([orchestration.md](orchestration.md#followed-profiles-as-rankers-first)).
- **Adapters are bound to a base hash and a quantisation build.** When the base changes, every adapter is void. The plan across upgrades: keep each profile's ledger and reward heads as the durable artifact, treat the adapter as a cache of them, and re-train it on the new base from the ledger. A profile whose ledger is not available (a followed one) keeps working as a ranker until its author re-publishes.
- **Training and serving formats differ.** An adapter trained with MLX or PEFT must be converted to GGUF for llama.cpp, and the converted adapter re-evaluated. The model lab already flags this for CUDA-trained adapters. It needs one tested conversion path and a post-conversion gate.

## 5. A phased path with gates

Every phase is measured on the same bench: seeded scenarios replayed through the real owner with captured output, built on the rehearsal harness. Three gates recur.

- **Validity.** *Schema-valid*: the reply parses against the action schema. *Owner-accepted*: every command in it passes the owner's validation against the live show (identifiers exist, values are in range, positions are not in the past).
- **Deadline.** *On time*: the share of requests answered inside the budget for their horizon, at 70, 128 and 180 BPM, with Room open and a 40 Hz show running.
- **Preference.** *Win rate*: the share of blind paired comparisons the candidate wins against a named baseline over the same passages, with a confidence interval. Before human pairs exist, the reward heads stand in and the result is labelled as such. The comparison harness and the degenerate-policy suite are [learning.md](learning.md)'s (#840, #843).

Thresholds are starting values.

| Phase | What is built | Gate to pass |
| --- | --- | --- |
| **0. API model plus schema** | The bench. The action schema inside the common subset. An API model as the first proposer through structured output | Schema-valid 100% (constrained). Owner-accepted ≥ 95%. On time ≥ 99% two phrases ahead. Win rate ≥ 60% against the #217 planner alone, interval excluding 50%. If no API model clears this, the observation or action design is wrong, and no training will fix it |
| **1. Untuned local models under the grammar** | The sidecar from #833 with a per-show grammar. The bake-off list from section 1, zero-shot | Schema-valid 100%. Owner-accepted ≥ 80% for at least one model at each of the two policy sizes. p95 latency inside the horizon for its size, with Room open. Lighting frame p99 and Room frame rate not degraded beyond the limits #833 sets. Picks the two or three bases worth an adapter |
| **2. Distilled policy** | A teacher corpus of validated decisions from simulated shows and operator takes. One adapter per shortlisted base; full fine-tuning of the winner if it is under 1B | Owner-accepted ≥ 98%. On time ≥ 99% phrase-ahead at 180 BPM for the phrase size, and ≥ 95% bar-ahead at 128 BPM for the bar size. Win rate ≥ 45% against the teacher (no worse than five points behind) and ≥ 60% against the planner. Zero unvalidated output commands and zero new deadline misses in the reference show, the model lab's release gates |
| **3. Preference adapters** | Per-profile adapters trained with the DPO family and reward-head reranking, as [learning.md](learning.md) specifies | Win rate ≥ 55% against the phase-2 policy on held-out operator pairs, lower bound above 50%. The degenerate-policy suite passes. Validity and deadline gates not regressed. Adapter survives conversion to the serving format |
| **4. Compression and reflex** | Vocabulary trimming, layer dropping and quantisation-aware tuning for phone-class devices. A from-scratch bar-level policy if the decision log is large enough | On an 8 GB phone: under 1 GB resident, on time ≥ 95% phrase-ahead, win rate ≥ 47% against the phase-3 policy (non-inferior). For the reflex model: on time ≥ 99% bar-ahead at 180 BPM and a win rate above the rules floor |
| **Vision track V0 to V3** | Encoder choice, critic labels, the tier-2 reward head, then look tokens on the policy | Section 3 |

Phases 0 and 1 need no training and can start as soon as the proposal contract (#826) and the schemas exist. Phase 2 is the first that spends money.

## 6. Hardware and cost

Training throughput here uses the model lab's assumed ranges in [hardware.md](../../model-lab/hardware.md), which that document says are unmeasured sensitivity bounds. Rental rates are the ones it checked on 2026-11-23: an H100 at $4.89 an hour, an A100 at $1.99. API prices are Anthropic's list prices cached on 2026-11-03 (per million tokens in and out: Haiku 5.5 $0.10 and $0.50, Sonnet 5.5 $2 and $10, Opus 5.5 $4 and $20; see [pricing](https://www.anthropic.com/pricing)) and OpenRouter's listing for Qwen3.5-9B on 2026-11-28 ($0.04 to $0.17 in, $0.06 to $0.25 out; [OpenRouter](https://openrouter.ai/qwen/qwen3.5-9b)).

### The teacher corpus (phase 2)

Twenty thousand decisions at an assumed 3,200 tokens in and 500 out is 64M input and 10M output tokens.

| Teacher | List cost | With the batch interface (half price) | Notes |
| --- | ---: | ---: | --- |
| Claude Haiku 5.5 | about $11 | about $6 | Cheap enough to run several times while the format changes |
| Claude Sonnet 5.5 | about $230 | about $115 | Prefix caching would cut the input term further when calls are sequential |
| Claude Opus 5.5 | about $460 | about $230 | Thinking tokens are billed as output and are not in this estimate |
| Qwen3.5-9B through OpenRouter | $3 to $13 | n/a | A check on whether an open teacher suffices |

Rejected and re-asked decisions add to this. A first corpus of 2,000 to 5,000 decisions costs a tenth to a quarter as much and is the right size for choosing a base.

### Fine-tuning (phases 2 and 3)

| Job | Tokens per epoch | 48 GB M4 Pro | One rented H100 |
| --- | ---: | --- | --- |
| Pilot adapter, 1.5B base, 5,000 decisions at 2,900 tokens | 14.5M | 10 to 45 h, under $0.50 of electricity | 0.5 to 2.7 h, $3 to $13 |
| Full corpus, 1.5B base, 20,000 decisions | 58M | 38 to 180 h: too slow | 2 to 11 h, $10 to $52 |
| Full fine-tune of a sub-1B base on the full corpus | 58M | Not practical | Similar hours at a higher memory setting; budget $20 to $60 |
| One profile's preference adapter: 1,000 pairs at about 3,800 tokens, plus one cached pass of the frozen reference | 3.8M | 2.5 to 12 h: overnight | Minutes, a few dollars |
| A 7B author adapter on the model lab's existing corpora | as costed there | 2.4 to 9.7 h per million tokens | $1.50 to $4 per million tokens |

Two levers shorten the Mac numbers by about three times: train on deltas with a short static prefix instead of full snapshots, and, since the adapter touches only the last 16 layers, cache the earlier layers' activations for the shared prefix.

DPO-family training costs two policy passes per pair, one for each alternative, and one pass of the frozen reference. The reference's log-probabilities do not change, so they are computed once and stored.

### Training from scratch

| Model | Compute | Verdict |
| --- | --- | --- |
| Reward heads, under 5M parameters | Seconds to minutes on the Mac | Do it |
| Reflex policy, 5 to 30M parameters, 100M tokens of structured trajectories | An hour or two on the Mac (estimated; not measured) | Affordable. The cost is the data, not the compute |
| A language-capable model of 0.3 to 2B | Trillions of tokens: many years of a single H100, far beyond this project's budget | Do not |

### Pruning with distillation

General-purpose pruning at Minitron's scale needs tens of billions of teacher-labelled tokens. Task-specific compression needs only the task corpus: a layer-drop or vocabulary-trim experiment is one healing run, the same cost as a pilot adapter.

### Running costs

- **Local**: no marginal cost. The price is memory, heat, battery and the risk to Room's frame rate, all unmeasured.
- **API on the clock**, with the orchestration lane's assumptions (480 phrase-ahead requests an hour at 128 BPM, 3,000 tokens in, 300 out): about $0.22 an hour on Haiku 5.5, $4.30 on Sonnet 5.5 and $8.60 on Opus 5.5 before caching. Only the first is a sensible on-clock proposer; the others belong at arc cadence, where the cost falls by the cadence ratio.
- **Hardware**: nothing to buy through phase 3. An M4 Max would double decode speed. A Mac Studio or a rented GPU matters only if many profiles are trained centrally.

## Alternatives considered

| Alternative | Why not the main line | When it becomes right |
| --- | --- | --- |
| One medium model (7 to 9B) for everything | It answers in 4 to 11 s, which misses phrase-ahead at 180 BPM with a full plan, and it takes 5 GB next to Room | On an M4 Max or later chips; as the local planner and author today |
| The model lab's Qwen2.5-Coder 1.5B as the policy | A coder pretrain is not obviously better at a non-code schema, and it is a generation behind on function calling | It stays in the bake-off as the phone pilot, since its tokenizer and recipes are already verified |
| Train with MLX and serve with MLX | No server-side schema mode today; the new server cannot yet take an adapter per request | When mlx-lm gains both, or behind a Swift sidecar using mlx-swift-lm's guided generation |
| Apple's on-device foundation model | No adapter path on OS 27, an 8k context, and no control over the base | A zero-download fallback proposer for users who install nothing |
| A small VLM as the policy | Uncacheable image tokens and unmeasured judgment of dark scenes | If the look-token experiment shows pixels help the policy |
| A from-scratch decision transformer as the only model | No language, no data yet | The reflex tier, once the decision log is large |
| FunctionGemma or LFM as the base | Licence terms that bind every user of a public profile repository | If the operator decides profiles are personal and never redistributed, or a commercial licence is bought |
| No local model: API only | Cost per hour, network dependence in a venue, and rig data leaving the machine | It is phase 0, and it remains the backend for small machines |

## Contracts with the other lanes

| Lane | This report needs from it | This report gives it |
| --- | --- | --- |
| Interface | A stable prefix and a delta, so the prompt cache works. An action format that stays inside the common schema subset, with canonical serialisation and a terse form near 150 tokens for bar-level replies. Per-show enumerations of valid identifiers. A redistributable encoder in place of MobileCLIP. A low-resolution look renderer for clips | Token and latency budgets per horizon. The schema subset. The author routing by tier |
| Learning | The ledger and reward heads as the durable profile, so adapters can be re-trained on a new base. Win-rate and degenerate-policy harnesses | A base whose licence allows redistributable adapters. Adapter shape and cost. Encoder and critic choices. The cost of DPO-family training |
| Orchestration | Deadlines per request. Health and back-off for the sidecar. A decision on followed profiles as rankers | The runtime choice for #833. Model size per horizon. One prefix cache per adapter. No forced tool choice on Claude |

## Implementation slices

Child issues of #821 in milestone "Roadmap later".

| Slice | Issue |
| --- | --- |
| Bench agent models for validity, musical deadlines and tokens on seeded scenarios, with an API baseline (phase 0) | [#862](https://github.com/tolgraven/tolglow/issues/862) |
| Constrain every backend's output to one action-schema subset with the show's own identifiers | [#863](https://github.com/tolgraven/tolglow/issues/863) |
| Pick the policy base: bake off small Apache and MIT models, untuned and after one adapter (phase 1) | [#864](https://github.com/tolgraven/tolglow/issues/864) |
| Build a validated teacher corpus from simulated shows and distil it into the policy (phase 2) | [#865](https://github.com/tolgraven/tolglow/issues/865) |
| Route effect authoring to a model that can write it and measure which size writes which tier | [#866](https://github.com/tolgraven/tolglow/issues/866) |
| Judge look clip pairs offline with a vision-language critic, used only when it agrees with operators | [#867](https://github.com/tolgraven/tolglow/issues/867) |
| Keep profile adapters valid across training and serving runtimes and across base upgrades | [#868](https://github.com/tolgraven/tolglow/issues/868) |
| Shrink the policy for phone-class devices: quantisation, vocabulary trimming, layer dropping (phase 4) | [#869](https://github.com/tolgraven/tolglow/issues/869) |
| Try a from-scratch bar-level reflex policy once the decision log is large enough (phase 4) | [#870](https://github.com/tolgraven/tolglow/issues/870) |

Existing issues are extended, not duplicated:

| Topic | Issue | What this report adds |
| --- | --- | --- |
| Model inventory, device measurements, training recipes | #323 | Policy-size candidates released since its inventory, the licence filter, and the measurements still missing: warm-cache time to first token, prompt rate, contention with Room, cost of several adapters |
| The local sidecar | #833 | The runtime choice (llama.cpp's server first), one prompt cache per adapter, model size per horizon |
| Look-view encoders | #852 | MobileCLIP's weights are research-only; Apache-2.0 substitutes; the Neural Engine route for the encoder |
| Reward heads and the offline vision tier | #841 | Encoder sizes and the critic as a label source |
| Profile training | #842 | The cost of DPO-family training and the adapter shape |
| Win-rate gating | #843 | The validity and deadline gates that sit beside it |
| Sandboxed generator code | #855 | Which model writes which tier |
| Phase 3, preference adapters | #842, #843, #844 | Owned by the learning lane; no separate issue here |

## Decisions for the operator

The consolidated, deduplicated list across the four reports, with the phase each decision blocks, is in the [index](README.md#operator-decisions).

1. **Is a public profile repository a firm goal?** If yes, the base must be Apache-2.0 or MIT, which excludes FunctionGemma, Liquid and Llama. Proposed: yes, restrict to Apache-2.0 and MIT.
2. **May a paid API model be the teacher?** Its outputs become training data for weights that are then shared. Provider terms on training competing models from outputs need a read before phase 2. Proposed: use an open-weight teacher through OpenRouter for anything that will be redistributed, unless the terms clearly allow it.
3. **One model or several?** Proposed: several, split by horizon. The cost is more moving parts; the gain is a policy that answers in under a second.
4. **Does the policy ever see pixels?** Proposed: no for now. Vision goes to the reward and critic models; look tokens on the policy are a gated experiment.
5. **llama.cpp or MLX for serving?** Proposed: llama.cpp first for its schema mode and per-request adapters, MLX for training, re-check each quarter.
6. **May operator performances and rig data be sent to an API critic or teacher?** Clip frames and observations carry the rig and whatever names the operator wrote. Proposed: local critic by default; API only with the per-show opt-in the orchestration lane already requires.
7. **How much to spend on phase 2.** Proposed: one pilot of about $20 (a Haiku-class or open teacher, 5,000 decisions, adapters trained on the Mac), then a decision on the full corpus with the pilot's gate results in hand.
8. **Is the phone a target for the first release of the local agent?** It drives phase 4 and the choice of a sub-1B base. Proposed: no; the phone uses the hosted or API backend until phase 4.

## Not established

- Every latency and memory figure for this machine. None was measured here.
- Whether any sub-2B model clears the validity gate on Tolglow's schema, untuned or tuned.
- Whether a 500-token phrase plan can be cut to 150 to 200 tokens without losing guards and intents.
- Inference contending with Room on one GPU, thermal behaviour over a night, and warm-cache time to first token.
- How Qwen3.5's hybrid architecture trains with LoRA in MLX and PEFT, and whether its adapters convert to GGUF.
- Whether a critic model's preferences over dark, foggy renders agree with an operator's.
- Small-model quality on the effect-definition schema, and any small-model Clojure baseline.
- The speed gain from vocabulary trimming, which is arithmetic here.
- TinyCLIP's licence; the image-token cost of Qwen3.5; Mac latency for any of the vision encoders.
- Decision Transformer's and Gato's architecture figures and Octopus v2's per-function data sizes were taken from the papers' bodies; the first two were recalled and not re-fetched.
- Vendor benchmark numbers are self-reported. BFCL figures from different sources are not comparable.

## Sources

Linked inline. The external facts came from three passes on 2026-11-28: a survey of model cards, licence texts and the BFCL leaderboard; a survey of runtime release notes, server documentation and published Apple Silicon timings; and direct fetches of the cited arXiv abstracts and repository pages. Repository facts are from `src/tolglow/integrations/agent.clj`, `src/tolglow/integrations/providers.clj`, `src/tolglow/automation/`, `src/tolglow/rehearsal/`, `src/tolglow/knob_candidates/`, `docs/model-lab/`, `docs/scheduler.md` and `dev/model-lab/models.json` at `bce22156`, and from the sibling lanes' drafts and example files as they stood in their worktrees.
