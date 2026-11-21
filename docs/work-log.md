# Work log

## 2026-11-21 — Descriptive owners and actual waveform projections

The active workstreams now use descriptive agent names. A single
`room_renderer_fog_fixture_optics` owner handles the entire scene/fog/optics
pipeline; there is no second graphics agent. `studio_push_touch_interface`
owns the combined UI, while `audio_capture_waveform_projections` owns the
read-only backend data. Dependency upgrades, application agents and terminal
reliability each have separate claims. The ledger validates 96 items with
58 completed milestones, 29 planned and 9 active; these counts are scope labels,
not a claim that the product is complete.

Live signal previews now resolve captured instance definitions and current
effective parameter bindings through the actual numeric IR. Captured PCM
produces measured, bounded peak/RMS/frequency columns and authoritative beat/bar
markers. The Push capture worker receives those projections off the lighting
deadline. Initial combined waveform/audio/display gate passed 27 tests and
193 assertions, followed by a focused 11-test/86-assertion gate including actual
Ring parameter edits, compact audio, captured-clock bindings and the new source
layout under `src/tolglow/`.
The final waveform/display gate passed 17 tests /118 assertions, adding continuous
off-deadline projection when no new model view is submitted and preserving the
existing transport lifecycle tests. These focused and aggregate counts overlap.
Further browser and complete host/device wiring remains separately
tracked in [waveform projections](waveform-projections.md).

The isolated PCM summary stage measured median/p99 0.080/0.284 ms for 480-sample
48 kHz blocks, including the immutable 256-column view. Decode, existing analysis,
capture and display/HTTP costs were excluded. Root renamed the independent
physical sender to `output-scheduler`: parallel effect workers remain separate
from the deterministic model coordinator and serial network sender.

## 2026-11-21 — Verified sensors, output policy and workspace checkpoint

The latest root Afterglow parity gate passed **269 tests /2943 assertions**,
including sensors and the pure output policy. The earlier checkpoint **247 /2808** was
committed at`3b7528008191f6c479b6cc2422b27237460ae600`. The earlier replay mismatch
below was resolved only after independent comparison of all36 inputs confirmed
unchanged output/selected instance semantics. Tolglow's later restart-owner
parity run passed **411 /3294**; subsequent source still needs root's final gate.

Sensor middleware passed **11 /71**; actual OSC/service/connections **31 /239**.
The maintained Capacitor Motion8.0.1 package passed three smoke assertions. Native
source passed real host protocol/loopback **4 /10** and six static checks; Apple
build/physical Watch/iPhone tests remain unavailable. [Sensors](sensors.md)
records source revisions, SI axes, permissions, explicit arming and ownership.

The pure [output policy](output-presentation.md) passed **11 /64**, with direct
mode default, timestamp-preserving late frames, bounded history and fixture-aware
fallback. At100 universes, fade median/p99 were1.251/2.000ms; interpolation
1.720/3.457ms, maximum13.65ms. Host publisher integration remains separate; these
stage measurements are not a40Hz whole-show guarantee.

Workspace/terminal tests passed **19 /139**, including actual Git and Docker/PTY.
REPL backend passed **34 /264**, Ring host **2 /19**, and nine real browser REPL
flows. Patchbay four and waveform three browser flows also passed without console
errors. Frontend source checkpoint **100 /538**, optimized142 files/0warnings,
precedes newer sensor/scene work. Bruvbox uses the actual pinned theme and a real
WASM verification, not a similarly named palette. Automatic application-coding
agents and preview/reload activation remain separate incomplete workflows.

Clock/routing and restart ownership are implemented. One renderer owner now owns the new Three scene and shaped
fog work, with no shared visual polishing: initial screenshots did not meet visual acceptance, despite
math/SwiftShader checks. There is no AAA, mobile GPU or physical optics claim.
The ledger validates **94 items:58done,29planned,7active**; scoped module completion
does not close the original broad product roadmap.

## 2026-11-21 — Performance and development-workspace follow-up

The shared instance/category control gate passed **81 tests / 612 assertions**
with reflection checks; real scalar/batch compiler integration passed **2 / 33**.
[System controls](system-controls.md) document private-clock pause, live manual
input response, independent musical AR, zero-opacity write suppression and alpha
compositing across rich/dense/typed/geometry rendering. The full Afterglow pure
checkpoint then ran 209 tests / 2460 assertions with zero failures and one replay
fixture mismatch caused by newly persisted system bookkeeping. That run was not green; the later verified comparison and passing parity checkpoint above supersede it.

Other latest owner-reported scoped gates: X-Touch codec/session **10 / 138**;
modulation **16 / 122**; effect recipes **8 / 153**; registered signals **8 / 105**;
typed patch kernel **16 / 118**; patch editor component **12 / 54**; system-control
UI component **9 / 81**. The latest combined frontend source suite passed
**78 / 434**, with zero warnings; newer performance/REPL browser flows still need
actual verification. Earlier browser gates include six Connections and six
modulation workflows. An actual 960×160 re-frame display capture ran 60 samples,
median 66.8 ms / p95 73.8 ms (14.79 fps) in software Chromium; its JVM boundary
passed five tests / 29 assertions plus five Node tests. This is not physical USB
performance or a 60 fps display claim.

The [development-workspace specification](product/development-workspace.md)
records the Quake/Bruvbox REPL, repository-wide Neovim and scratch forms, captured
locals, branch-isolated Codex/Claude application work, diffs/watchers, scoped
manual/after-turn/preview reload, last-good rollback and real Docker/zsh terminal.
JVM REPL and isolated-worktree service are independent active lanes. Phone motion
and native Watch conducting research/core middleware are now a separate active
lane; sensor permissions, gesture availability and physical verification must be
reported from actual supported APIs.

The ledger checkpoint validates **85 items: 50 done, 29 planned, 6 in progress**.
Broad product requirements remain open where only a kernel or component is done.

## 2026-11-21 — Current implementation checkpoint

The product stack is now **ClojureScript/re-frame/Reagent with Ring/Jetty**.
The optimized frontend build passed with 129 files and zero warnings; its latest
source suite passed **28 tests / 124 assertions**. On the restarted host, nine
session and six authoring browser workflows passed with no console/page errors.
Verified flows include multitouch/cancel and keyboard actions, actual Neovim WASM
editing/publishing, typed macro slot overrides, fixture search/import/create/
patch/remove, rig dragging/keyboard edits and saved truss values reaching WebGL.
A sixth Connections view for sync/audio is a subsequent active lane, not part of
that completed browser checkpoint.

There is **no runtime compatibility layer**. Old source supplies feature and
functional parity oracles; the former bridge experiment was removed. The lane
is now named `parity`; historical results below retain their original labels.
The `tolmaster` merge is `f235148aeca0eed12a2f25c611382351d8b7e38b`, and an earlier
functional runtime/control/device checkpoint was committed as `ccb9768`.
Later source/revision and final combined results are recorded by the integrator.
Beat Link 8.0.0 and required thi.ng dependencies are explicit application pins.

| Scoped result | Latest reported evidence | Practical limit |
| --- | --- | --- |
| Functional control/runtime | Root Afterglow pure checkpoint **158 / 2030** | Current pure lane; final parity/source revision tracked separately |
| Integrated Tolglow | Root pure checkpoint **244 / 1780** | Before the final two host/device construction regressions and sync follow-up |
| Connections resource owner | **16 / 158** | Injected provider/PCM/MIDI factories; no physical ports |
| Studio restoration host | **12 / 80**, existing studio **13 / 65** | Full model/output restore, staged resources, Unicode file replacement, stale imports, controller-open race, audio rebase |
| Full schema2 persistence | **19 / 165** before rig-validator addition | Real registry/compiler/patch validators; no callback execution |
| Reactive REPL | **20 / 99** | Durable source/var handlers, ratoms, pending actions and watcher comparisons; host owns scheduling |
| Provider history/credentials | Manager **16 / 130**, credential store **5 / 31** | Fake keys/local HTTP; real accounts unverified |
| Audio analysis/capture | Analysis **12 / 121**; capture **14 / 94** | Synthetic PCM and owned lifecycle; physical routing unverified |
| Fixture/registry backend | **31 / 288** | OFL actual public HTTP verified; supported GDTF subset only |
| Rig validation | **5 / 52**, with persistence **24 / 217** | Explicit patch/room/truss checks; no real fixture calibration claim |
| Fixture host flow | **3 / 59** | Actual Ring/runtime queue admission and publication |
| Live bridge assets | **15 Node tests passed** | Real loopback protocol plus injected LiveAPI; Max/Live execution unavailable |
| Render/timing/runtime | Combined **66 / 534**; batch **5 / 48**; output **4 / 30** | CPU correctness and capture sinks, not a physical rig |

Counts overlap and must not be summed into a project total. The root integrator
owns final combined runs and revision IDs in [testing](testing.md). Historical
legacy runs that included the removed bridge are not the current suite count.

Schema2 saves active instance state, captured definitions, musical timers,
composition/automation ownership, output buffers converted to portable data,
settings, agent conversations/credential references, fixture cache and durable
reactive state. `prepare-restore` validates/builds candidate resources; the host
commits resource ownership and model/output under its owner lock, then retires
old sessions outside it. Old input epochs are rejected, saved output is published
without an extra effect evaluation, and capture restarts only after clock rebase.
Saved sync settings and `:wanted?` are now owned by the same connections boundary;
full host sync routes and UI are being integrated.

The latest 10,000-target pipeline benchmark includes three effects, response/
envelope middleware, actual engine, mixed rendering, a 16-head projection and
ArtDMX capture. The two-worker byte-copy run reported **11.95/15.90/18.66 ms
p50/p95/p99**, maximum **26.80 ms**, and **11.89 MB/frame** allocation versus
20.20 MB before that copy improvement. It still excludes whole-host scheduling,
network delivery and UI/simulator contention. See [rendering progress](rendering-progress.md)
and the retained [benchmark reports](benchmarks/README.md). Software WebGL passed
eight oracle cases/992 compared components and nine lifecycle groups; it remains
SwiftShader evidence, not physical GPU acceleration.

Audio reuse research, package versions/licenses and actual smoke tests are in
[audio-analysis](audio-analysis.md). Native aubio is a candidate for qualification;
Meyda feature extraction passed its small smoke check, while the tested aubiojs
beat-count gate failed. Mood/phrase inference and deep-learning beat estimates
are not claimed from simple energy/onset features.

Physical Push/lighting/DJ-Link/Live/audio validation and real external providers
remain outstanding. Native Push 3 display/standalone support is not inferred from
Push 2. The preview does not implement the requested AAA optics/volumetric fog.
Robot duck identity/SDK, club presets and phone/LiDAR environment capture remain
research. Narrow delivered v1 modules do not close the broader roadmap items.

## 2026-11-21 — Expanded expressive requirements

The [new performance specification](product/expressive-performance.md) preserves
all prior goals and adds beat-random modulation for all supported parameter types
and cue/category subsets; category colors, pause/removal, opacity and response
shaping; iPad performance gestures; expressive Push 3/Seaboard M automapping;
X-Touch/extender protocol and color/PWM research; rich shared Push widgets with
an evaluated offscreen re-frame/CSS webview; a waveform/signal lighting synth,
normalized derivative/jagginess and audio-derived signals; musical Live patchbay
connections with curves/ranges/envelopes; Link/AbleSet and Session View/racks on
two MacBook Pros; optional afterglow-max hosting; and a sharing website for
versioned artifacts, shows and Live arrangements. These are tracked as separate
scoped implementation/research lanes, not silently marked shipped.

Frontend checkpoint `cff16cd` contains the tested re-frame studio. The Afterglow
functional/timing/render/reactive checkpoint is `8cf0068`, following `f5d7aeb`.
Later active source still requires its own tests and integrated commit evidence.

## 2026-11-21 — Historical architecture foundation

This earlier checkpoint checked out Tolglow `effects` at
`01146ece7731ec06aa1ee6409342373096c8e0dc` and Afterglow `master` at
`16b0b4cac70433e4118038dfbfd304e22d1d9018`. `tolgraven/tolgrAVen` was inspected
as a Neovim WASM reference. Separate agents surveyed the runtime, controls/web,
authoring/integrations, genuine legacy tests and a small explicit-context kernel.
The first foundation had no Tolglow production changes and no new legacy-loop
wiring. That statement describes this earlier checkpoint only.

```text
afterglow/bin/test legacy  -> 47 tests, 379 assertions, 0 failures/errors
tolglow/bin/test legacy    -> 20 tests, 103 assertions, 0 failures/errors
afterglow/bin/test pure    -> 13 tests, 51 assertions, 0 failures/errors
tolglow/bin/test pure      -> 10 tests, 57 assertions, 0 failures/errors
```

Legacy includes pure; the original combined broad lanes were 67 / 482. The
runtime was OpenJDK 21.0.13, Clojure 1.11.4 and CLI 1.11.1.1435. Hosted CI and
local Java 17 were not observed. Real source and dependencies were exercised;
whole namespaces were not replaced with stubs. A vacuous core test was replaced
with meaningful context tests; existing fade/step/color tests were retained.

At that stage an unavailable Beat Link snapshot was replaced only in test
classpath generation, and `Param`/`color-type` were missing on default master.
Subsequent source-based `tolmaster` integration and manifest repair resolved
those immediate blockers. The [branch record](branch-compatibility.md) retains
both the original source audit and current status.

An initially suspected iterator defect was disproved against original source:
zero-arity `conj!` is valid in Clojure 1.11.4. The speculative edit was reverted.
A legacy `:ending` assertion was corrected to its existing truthy keyword
contract without a production change. This remains the characterization policy:
understand behavior before choosing what to preserve or deliberately change.

The original assignment/packing benchmark measured 100/1,000/10,000-head
workloads with 30 warmup and 100 measured frames. It is retained separately in
[benchmark-baseline.md](benchmark-baseline.md). Its 1,000/10,000-head medians of
36.110/366.670 ms describe a different historical stage/workload from the newer
backend reports and must not be casually divided into a speedup figure.

The original foundation was committed locally in Tolglow `19e5c4b` and Afterglow
`882a0ce`; later integration work is tracked separately. No remote pull request
or physical device deployment is implied by these local commits or test runs.
