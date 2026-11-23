# Engineering handoff

Snapshot for R83, 23 November 2026 UTC. Branches and PRs are durable resume points. Check their current GitHub state before taking an assignment. The [request register](request-coverage.md) retains each original acceptance condition and the [ledger](work-items.edn) holds implementation evidence.

| Work | Last verified checkpoint | Remaining acceptance |
| --- | --- | --- |
| Antialiasing, HDR, adaptive quality | Matched-camera supersampled comparison, 4K backing store, HDR readback and a motion capture in [Tolglow 118](https://github.com/tolgraven/tolglow/pull/118) | Qualify adaptive geometry resolution, headroom, refresh and actual HDR output on Apple hardware. A 4K backing store alone does not establish 4K geometry. |
| Apple Studio bundles | [Apple Studio branch](https://github.com/tolgraven/tolglow/tree/codex/apple-studio-bundle) and [native project](../native). The Apple Silicon build ran external Clojure through the bundled Graal JVM. | Run the packaged desktop, phone and tablet workflows. Verify editable source staging, PiP, physical AirPlay and 4K. Track native-image constraints separately from JVM and paired-client operation. |
| Push page and mode selection | [Afterglow 72](https://github.com/tolgraven/afterglow/pull/72) and [actual page capture](https://raw.githubusercontent.com/tolgraven/tolglow/c7f1a2de26d7105b0a9f7b69ec19abc966ce8e3a/pages/select-and-browse.gif) | Integrate the matching Tolglow page atlas and check physical feedback with held pads. |
| Push shared scene and wide camera | [Tolglow 119](https://github.com/tolgraven/tolglow/pull/119). Production software capture completed 24 scene frames and released the renderer on exit. | Optimize cold start and warm capture, and qualify on-device latency across lenses. |
| Push parameter history and spatial widgets | [Parameter history branch](https://github.com/tolgraven/tolglow/tree/codex/push-spatial-param-history). Tests caught a namespaced field lost in the display IPC encoder. | Finish release captures, review and verify paired hardware. |
| Visible-page pad colors | [Afterglow 71](https://github.com/tolgraven/afterglow/pull/71) and [Tolglow 117](https://github.com/tolgraven/tolglow/pull/117). Captured MIDI transport exercised inactive previews. | Check physical refresh during page changes and keep host and controller revisions together. |
| Continuous performance profiling | [Afterglow 70](https://github.com/tolgraven/afterglow/pull/70) and [Tolglow 116](https://github.com/tolgraven/tolglow/pull/116). The dense replay contains 20k pixels and 64 moving heads. | Follow published runs, byte-check scalar and optimized outputs, preserve timing outliers, and compare the next master. |
| Local adapter preparation | [Model preparation branch](https://github.com/tolgraven/tolglow/tree/codex/local-model-lab). Independent assignments cover licenses and code extraction, validated GLSL, and hardware economics. | Finish command corpus, deduplication, held-out tests, inference/routing design and recipes. Actual fine-tuning remains outside this assignment. |
| Live Link and Live set integration | [Transport and pause behavior](ableton-link.md), [Live routing](live-routing.md), [audio returns](live-audio-returns.md). | Verify real Live playback/pause, edited clips and arrangement, Rack pads, audio-return tails, Session modulation and dual-host Link/AbleSet. Hardware/Live-dependent follow-ups remain queued regardless of the earlier threads' state. |

The two ongoing performance scopes have separate owners and acceptance criteria. The [work-elimination experiments](performance/render-work-elimination.md) address whether calculations should run at all or can stay on the GPU. The [perceptual scheduling experiments](performance/perceptual-scheduling.md) measure the error when work runs less frequently. Checkpoint both at each merge and reassign their queues if a worker stops.

The distributed compute and active-chain compiler each have an active work item. The [render owner](https://github.com/tolgraven/afterglow/blob/master/src/afterglow/render/compute.clj) already supports local CPU/native GPU evaluation. R78 and R79 require new end-to-end evidence for remote hardware and fused dispatch respectively.

## Reconcile historical work

Tolglow's effects integration is commit [4265696](https://github.com/tolgraven/tolglow/commit/4265696). [Tolglow PR 3](https://github.com/tolgraven/tolglow/pull/3) integrated the full project into master. [Afterglow PR 2](https://github.com/tolgraven/afterglow/pull/2) merged at `1455a31d`. GitHub reports 647,431 additions for Tolglow PR 3, 606,680 for [Tolglow PR 2](https://github.com/tolgraven/tolglow/pull/2), and 14,197 for Afterglow PR 2. These overlapping PR totals cannot identify a 66k-line subagent session or establish the amount of authored code. R25 stays open pending an identifying revision or session diff.

## Resume across interruptions

Before claiming a path, check open PRs, active agents and the ledger. Preserve the branch, last verified SHA, failing command and untested hardware in the handoff. When all slots are occupied, leave the new acceptance check queued. When a slot becomes available, resume a disjoint item. Confirm the intended scope against the actual commit and review before closing it. Continue reporting active scopes by their current descriptive assignments.
