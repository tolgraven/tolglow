# Pause checkpoint — 21 November 2026

Work paused because the user reported 1% remaining allowance and wanted to avoid
credit spending. There is no available usage-reset control. Resume the existing
program rather than starting over. Agents were asked to commit WIP, leave
`RESUME.md` in their worktrees and stop. Unfinished work is not verified delivery.

## Integrated repositories

- Tolglow master: `89dd03a`. Full project layout, OLA plus independent Art-Net,
  packaged Push webview, preset/global widgets, WASD/touch camera/tours, and
  expressive mapping backend are merged. Root checkout is
  `/workspace/tolglow`, branch `codex/studio-master-integration`.
- Afterglow master: `6e82fcf`. Includes MPE mapping and measured mixed-rig dense
  rendering optimization. Root checkout `/workspace/afterglow` is current.
- IMPORTANT: Tolglow's original fetch refspec fetched only `effects`, so
  `git fetch origin master` updated FETCH_HEAD while leaving origin/master stale.
  Root changed remote branches to `*` and explicitly refreshed master. Use
  `git fetch origin master:refs/remotes/origin/master` and verify SHAs on resume.
- GitHub default-branch settings change to master was denied by token permissions;
  code is on master, but repository landing/default branch may still be effects.

## Pending integration

| Work | Branch/worktree | State |
| --- | --- | --- |
| Browser state deltas | `codex/web-state-deltas`, `/workspace/tolglow-state-deltas` | PR12 head9230ca0; repeated ledger-only conflicts resolved, final CI pending. HTTP polling delta transport, not WebSockets. |
| Musical aim-path publication | `codex/interactive-rig-aim-paths`, `/workspace/tolglow-rig-aim-paths` | PR15 head3cb07bc; final focused12tests/84assertions, prior parity502/4225. UI agent has checkpoint. |
| LiDAR calibration | `codex/ios-rig-calibration`, `/workspace/tolglow-lidar-calibration` | PR16 f309e9f; parity487/3817, conflicts against current master. Disarm calibration before persistence/thaw changes owned instance identities. Swift/Xcode/device unverified. |
| Master profiling | `codex/master-performance`, `/workspace/tolglow-performance` | PR17 18515b7. Before/after/repeat evidence and scheduled artifact-only performance workflow. Shared-host outliers prohibit steady40Hz claims. |
| Expressive touch UI | `/workspace/tolglow-expressive-ui` | Agent checkpoint/RESUME.md; frontend147/780 and release148files/zero warnings passed; actual backend/browser QA underway at pause. |
| 3D fixture/path editor | `/workspace/tolglow-rig-editor-ui` | Agent checkpoint/RESUME.md; frontend142/751 and graphics tests passed, actual browser QA underway. Merge root aim follow-up3cb07bc. |
| Effect fault isolation | `/workspace/afterglow-effect-isolation`, `/workspace/tolglow-effect-isolation` | New implementation being tested; preserve WIP and resume from agent notes. |
| Renderer fault attribution | `/workspace/afterglow-render-attribution` | Profiler follow-up, separate from already merged optimization. |
| RDM | `/workspace/tolglow-rdm` | Verified OLA protocol research and implementation WIP; see agent notes. |

Only merge final heads with required checks green. Preserve all ledger entries
when resolving concurrent additions. Artifact attachment tool repeatedly returns
unsupported or hangs; calls were attempted for PRs, do not spend time retrying.

## Latest requirements still needing implementation

**Existing names and fixture authoring forms:** preserve Tolglow/Afterglow
terminology and basic data structures. In `config/patches`, group maps use
`:universe`/`:offset`, or `:start-universe`, `:type`, and `:list`. Repeated
keywords/functions should need only one start address. Optional entry position
forms remain useful. Expand this authoring shorthand to complete validated
fixture definitions before atomic insertion, without a runtime compatibility
layer. Audit `src/tolglow/config.clj:148` and `util/patch-cfg!` around line314.
Support existing declarative fixture definitions, functions, pixel heads, group
IDs and names. Do not execute arbitrary downloaded EDN as code. Footprints must
include every head and coarse/fine channel, not only global channels.

The new renderer already scatters offsets across universes (including fine
channels); end-to-end shorthand/pixel-strip and both output-sink regression
tests are required. Preserve `:start-universe` meaning while avoiding overlap
when a strip consumes multiple universes. No implementation lane was started
for this latest request before pause.

**Ableton Link and upstream parity:** upstream `Deep-Symmetry/afterglow` uses
`main`, observed SHA `bb5dfd1042f7670881245b7e7cd3cb1c4bd7238c`, fetched as
`upstream/main` in Afterglow. Merge base is16b0b4c. Only three commits absent by
ancestry: bfee8dd,4314219 and merge bb5dfd1. Their aset-byte and byte-array type
hint functionality is already present in our channel.clj/show.clj. Existing
carabiner.clj matches upstream and is incomplete: phase alignment/clock hookup
contains TODOs/commented code. New Studio sync explicitly lacks Ableton Link.
Implement an owned Carabiner/Link transport in the shared sync boundary, with
tempo/phase/quantum/start-stop and lifecycle/persistence tests, rather than
claiming the old namespace or MIDI/Beat Link proves Link support. Record a
functional upstream audit; preserve existing fork optimizations and names.

**Effect resilience:** recoverable compile/reload/evaluation/result faults
isolate only the offending instance/version, preserve last-good state/definition,
omit failed contributions/intents, and keep healthy effects advancing. Suppress
repeated bad revisions until edit or explicit `:instance/retry`. Bounded portable
events under variables/runtime are subscribable; logs use an owned off-thread
queue. Cancellation/VM failure and global model/executor corruption remain
distinct. Renderer provenance identifies malformed output without duplicate
healthy-frame rendering; retry resolution using retained evaluation results,
never rerun effects. Profiler and isolation agents agreed `:source` contribution
metadata and `:effect-source` exception data. Tests and integration unfinished.

**RDM:** explicit discovery/read/control from Rig→Fixtures, passive opening.
Independent bounded OLA RPC session, no IO on DMX deadline, runtime/epoch/session
guards, ACK_TIMER/NACK handling, typed PID data and advanced GET/SET. UID binding
and adopting verified physical configuration into the patch are separate,
collision-validated operations. Persist observations/configuration, reconnect
unarmed. Direct ArtRdm remains a separate gate. No physical discovery authorized
by ordinary tests.

## Running Studio and test environment

Studio8500 PID155069 still runs old loaded classes and old compiled frontend.
Output is preview-only, observed show main/epoch0/revision0. It has NOT been
refreshed with merged changes. Export `/api/export` before stopping only this
owned process; build combined frontend, restart and restore through `/api/import`.
Do not lose show state. Other agent QA servers may exist; consult RESUME.md.

Use Clojure CLI `/workspace/.tools/clojure-1.12.7.1681/bin`,
`JAVA_TOOL_OPTIONS=-Duser.home=/workspace/.tools/java-home`,
`CLJ_CONFIG=/workspace/.tools/clojure-config`. `./bin/test parity` includes pure.
Node26 is under `/workspace/.tools/dependency-runtimes`; Chromium `/usr/bin/chromium`.
Until PR12 merges, run compiled frontend tests explicitly with Node because
shadow-cljs autorun can print failed assertions while returning exit0.

The full product roadmap remains in work-items.edn and product documents. This
checkpoint does not replace it or imply AAA preview, physical controller/mobile,
Live host or native iOS qualification is finished.
