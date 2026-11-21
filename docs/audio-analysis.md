# Realtime musical audio analysis: reuse and implementation decision

Research checked against upstream sources and package artifacts on **2026-11-21**.
The goal is to align lighting with an existing musical clock, detect kick-like
attacks and energy changes, and expose useful structural hints. These are
separate problems from estimating an unknown tempo/downbeat or recognizing a
listener's emotional interpretation of a track.

## Decision

Build the small **Tolglow-specific state and policy layer**, and reuse established
feature/tracker implementations through replaceable providers. Do not build a
new general beat/downbeat neural model or label RMS as “mood.”

1. **Authoritative musical transport first.** When Live/MIDI/other synchronized
   transport provides beat, meter and phase, keep it authoritative. Align audio
   observations to that clock and measure timing error; kick detections can
   trigger or modulate cues without repeatedly resetting tempo.
2. **Implement a bounded JVM fallback now:** explicit block/sample-rate/time
   input, filtered kick-band onset evidence, adaptive threshold/refractory gate,
   energy statistics, breakdown hysteresis, clock alignment and normalized
   events. This is small application-specific logic, not a replacement for an
   established music-information-retrieval library. The current implementation
   lane is `tolglow.integrations.analysis`; verify its tests/status in the ledger.
3. **Reuse Meyda's MIT feature extractors when browser-side analysis is needed.**
   Its `extract` API works independently of capture, and the tested package
   produces correct elementary RMS/energy/centroid values. Use it in an owned
   Worker or carefully bounded AudioWorklet pipeline; its stock WebAudio wrapper
   still uses `ScriptProcessorNode`. It does not supply a musical beat model.
4. **Use native aubio as the first optional inferred-beat provider to evaluate.**
   It has causal hop-based onset/tempo APIs, timestamps, delay and confidence.
   Keep it outside the render thread and qualify it on captured music first.
   The existing aubiojs WASM package is feasible to execute, but its API mismatch
   and failed synthetic callback-count gate mean it is **not qualified as the
   default clock provider**. Native aubio itself was not run in this research.
5. **Keep BeatNet as an optional learned downbeat/meter experiment**, with a
   pinned Python sidecar and explicit CPU/latency/model checks. Use Essentia or
   librosa/Sonic Annotator for richer offline analysis, corpus evaluation and
   prepared-track metadata. Do not insert their offline beat algorithms into a
   realtime frame loop because a library advertises “streaming.”

This selects working near-term boundaries without blocking basic kick/energy
control on installing a native or Python ML stack. Optional providers all
produce the same event data; changing a provider must not rewrite effects,
controllers or re-frame views. This document adds no runtime dependencies.

## Candidate comparison

| Candidate and checked source | Actual online/offline behavior | Integration and reuse decision | License facts to preserve |
| --- | --- | --- | --- |
| [aubio][aubio-src], C | `new_aubio_tempo`, `aubio_tempo_do`, `get_last[_s/_ms]`, `get_bpm`, `get_confidence`, delay accessors; equivalent onset API processes hops | First native inferred-beat candidate; C ABI via an explicit native adapter or separate process. No JVM binding/native package was validated here | GPL-3.0-or-later; retain library/source notices and assess the actual distribution boundary |
| [aubiojs][aubiojs-src], npm **0.2.1** | Emscripten/WASM Tempo and Onset execute in Node; per-hop input. Public Tempo wrapper lacks native exact timestamp/delay accessors | Browser/Node experiment, not a qualified default. Correct wrapper declarations and add timestamp API before precise phase alignment | Wrapper repository says MIT, but embedded aubio is GPL-3.0-or-later; MIT wrapper metadata does not relicense the compiled payload |
| [Meyda][meyda-src], npm **5.6.3** | `extract(features, signal, previousSignal)` provides frame descriptors; `createMeydaAnalyzer` uses WebAudio `ScriptProcessorNode` in inspected source | Good permissive feature reuse, especially browser side. Own capture/window/hop lifecycle; do not use its wrapper as evidence of AudioWorklet readiness. No beat/downbeat inference | MIT, as declared in package/repository metadata |
| [Essentia][essentia-src] / [essentia.js][essentiajs-src] | Large C++/WASM descriptor/model collection. Some frame algorithms are online; inspected `RhythmExtractor2013` and `BeatTrackerMultiFeature` emit at end-of-stream | Strong offline preparation/analysis and optional specialized feature provider; select algorithms individually. Wasm does not make an EOF algorithm causal | Library AGPL-3.0, proprietary licensing offered; pretrained models generally CC-BY-NC-SA 4.0 or proprietary. Check individual algorithms/dependencies/models |
| [madmom][madmom-src] | `RNNBeatProcessor(online=True)` and online DBN beat processing explicitly exist; ordinary offline models/processors differ | Python research/evaluation candidate. Preserve recurrent/decoder state across blocks; resetting per block is not online tracking. Not installed here | Source BSD-style; supplied model/data files CC-BY-NC-SA 4.0 unless otherwise stated |
| [BeatNet][beatnet-src], PyPI **1.1.3** | Explicit `stream`, `realtime`, `online` PF modes and offline DBN mode; outputs beat time and beat position, with downbeat/meter inference | Preferred learned downbeat experiment if needed; heavier Python/PyTorch/madmom/PortAudio boundary. Inspected source uses 22,050 Hz, 20 ms hop and 64 ms window; those are settings, not complete latency measurements | Repository declares CC-BY-4.0; audit packaged weights and transitive dependencies separately. Do not assume madmom's separately licensed models are required/cleared merely because its code is imported |
| [TarsosDSP][tarsos-src] | Java `ComplexOnsetDetector` processes blocks, but `BeatRootOnsetEventHandler` explicitly says beat tracking is **not realtime**: collect onsets, then call `trackBeats` | Easy JVM onset/filter integration if GPL packaging is chosen; not the default live beat estimator | GPL-3.0; bundled algorithms have attribution notices |
| [librosa][librosa-src] | `stream()` yields file blocks; its own documentation says `beat_track` needs the whole signal and is inappropriate for streamed data | Offline oracle/features and reproducible corpus tooling. `stream()` is not microphone capture or a causal beat tracker | ISC for library; optional dependencies and datasets retain their own licenses |
| [Sonic Annotator][sonic-src] | Batch audio-file feature extraction host for Vamp plugins | Useful offline batch comparison/export, not live show clock. Plugin capability/licensing depends on selected plugin | Host GPL-2.0-or-later; Vamp SDK BSD, plugins separately licensed |

A GPL/AGPL license is not inherently a ban on commercial use; its conditions
matter to the chosen packaging and distribution. Essentia's vendor page offers
commercial licensing and describes model restrictions separately. Do not convert
its AGPL library license into a fictional “noncommercial-only” license, or
ignore the actual noncommercial terms on model files. Afterglow currently has
an EPL-2.0 license; including that license's template exhibit alone does not
activate a GPL secondary-license option. This research therefore does not
silently copy or bundle copyleft libraries into the current application. A
separate process is an integration option, not a blanket legal exception.

## Source checks that change the decision

**Essentia “streaming” is a scheduler interface, not an assurance of immediate
results.** In the inspected C++ source, both
[`RhythmExtractor2013::process`][essentia-rhythm-code] and
[`BeatTrackerMultiFeature::process`][essentia-beat-code] return `PASS` while
`!shouldStop()`. Final tick results are produced after stream completion. The
algorithm reference also requires 44,100 Hz input for RhythmExtractor2013.
Repeatedly running it on sliding complete windows introduces context, repeated
work and unstable/revised estimates; that is a separate algorithm with measured
latency, not a free conversion to online tracking.

**TarsosDSP's beat helper is explicitly offline.** Its
[class documentation][tarsos-beat] states that all onsets are detected first and
beat estimation occurs in a post-processing step. The Java ecosystem is a good
fit for the host, but this particular beat helper does not meet live-clock needs.

**librosa makes the same distinction.** The checked 0.11.0
[`stream` documentation in source][librosa-stream] warns that `beat_track`
requires the whole signal. It also requires `center=False` for blockwise
frame algorithms to avoid introducing artificial padding in the middle of audio.

**Online neural modes require their intended model and persistent state.**
madmom exposes an online model choice; BeatNet separates PF causal inference
from its offline DBN usage. Feeding independent files/chunks into a fresh model
instance loses recurrent and tempo state. A sidecar must retain state per stream,
reset on discontinuity and report dropped samples and warmup confidence.

## Executed package feasibility checks

No microphone, physical audio routing, external service or music corpus was used.
Published npm tarballs were downloaded into temporary directories and loaded
directly; no install hooks ran and repository manifests were unchanged.

### Meyda: elementary feature reuse passed

The bundled **5.6.3** web build was loaded in Node **24.22.3**. With a rectangular
window, 1,024 float samples at 48 kHz and a 0.5-amplitude sine exactly at FFT bin
16 (750 Hz), three independent assertions passed:

- RMS `0.3535533878`, expected `sqrt(0.125)` within `1e-6`.
- Energy `127.999997998`, expected `128` within `1e-4`.
- Spectral centroid `16.000020916`, expected bin `16` within `1e-3`.

The [retained result](benchmarks/2026-11-21-meyda-smoke.json) also includes
flatness and rolloff. Note the centroid unit: this API returns FFT bins for this
extractor; convert with `sample-rate / buffer-size` before exposing Hz. Fix the
window and normalization contract before comparing implementations. This test
validates elementary extraction only, not browser capture, music classification
or performance under a lighting/render workload.

### aubiojs: runtime viable, accuracy gate failed

The **0.2.1** package's type declaration says `new Onset(window, hop, rate)`.
The actual compiled constructor rejects that call and requires
`new Onset("default", window, hop, rate)`. `Tempo(window, hop, rate)` and both
`do(Float32Array)` methods executed; both objects exposed `delete()` and were
explicitly freed. This concrete mismatch argues for adapter tests against the
published binary, not type declarations alone.

The probe generated 60 seconds of 120 BPM decaying 90 Hz + 1,900 Hz synthetic
clicks, sampled at 48 kHz with window 2,048 and hop 512. Across 5,625 blocks it
reported final BPM **121.6867**, **16 beat callbacks** and **76 onset callbacks**
for 120 source clicks. It failed a deliberately modest gate requiring more than
30 beat callbacks, more than 30 onsets and BPM within 5 of 120. The
[retained failed result](benchmarks/2026-11-21-aubiojs-smoke.json) is not relabeled
as passing merely because a plausible BPM was returned. It does not prove
native aubio fails, nor measure performance on normal music; it proves this
package/configuration is not yet a qualified live clock for our adapter.

The combined Tempo+Onset calls had approximately 0.2335 ms median and 0.4606 ms
p95 in this one Node run, with a **27.03 ms maximum**. Input generation, capture,
queueing, JVM transfer and alignment were outside those intervals. No deadline
or iPad result follows from this micro-measurement. The raw final confidence
was **2.648**, so it is not a calibrated `[0,1]` probability. Preserve provider
confidence semantics and calibrate before blending clock hypotheses.

The native C API provides exact last-event timestamps and delay accessors that
the current JS Tempo wrapper omits. Precise integration should expose those APIs
or explicitly verify the fractional return-value semantics, not assign the
callback arrival time as the detected beat time.

## Musical time and alignment contract

Beat/bar/phrase units are primary in authoring, auto policy and UI. A half-beat
attack, eight-bar breakdown hold or two-phrase progression should remain musical
when tempo changes. Use the shared musical-time helpers and explicit transport
snapshot; milliseconds and sample positions remain boundary units for audio,
network and device timestamps.

A quarter-note beat is not always the notated denominator beat. Record meter
numerator/denominator and the chosen beat convention. A phrase needs an explicit
length and anchor; do not silently assume every track is 4/4 or every phrase is
16 bars. Known transport bar/phrase metadata takes precedence. Inferred downbeat
and phrase are separate hypotheses with confidence and override controls.

A provider-neutral observation should carry at least this conceptual data
(the implemented analyzer API may use a smaller initial subset):

```clojure
{:stream-id :main-audio
 :sequence 421
 :sample-rate 48000
 :sample-position 2160000
 :observed-at-ms 45000.0
 :received-at-ms 45012.4
 :provider :kick-band
 :kind :onset
 :features {:low-band-energy 0.18 :rms 0.22}
 :confidence {:raw 3.7 :scale :threshold-ratio}
 :clock {:epoch 7 :beat 92.0 :bpm 123.0 :meter [4 4]}
 :alignment {:beat 91.99 :error-beats -0.01}}
```

Observed sample time, arrival time and publication time are different. Calibrate
capture/output latency and map sample counters to the host monotonic clock;
wall-clock synchronization alone is insufficient. At 48 kHz, a 512-sample hop is
about 10.67 ms, but detector windowing, lookahead, warmup, buffering, IPC and
render quantization all add latency. Timestamp compensation can describe the
past accurately; it cannot make a late physical light flash occur in the past.
Prediction should target a future musical boundary with explicit uncertainty.

When external transport is absent, a tracker may propose tempo and phase with
bounded slew, confidence/holdover, octave ambiguity handling and reset rules.
Do not phase-jump on every kick: syncopated kicks, missing kicks and non-four-on-
the-floor music violate that assumption. Accept a clock proposal through the
same control boundary as other transport changes; the audio callback never
mutates the show directly.

## Energy, breakdown and mood semantics

RMS, low-band energy, spectral flux/centroid, transient density and crest factor
are useful measurable descriptors. Normalize over explicit rolling horizons,
keep absolute level and relative change separate, and expose enough evidence
to tune thresholds. A low-band burst is a **kick-like candidate**, not a source-
separated drum classification; bass notes and room resonance can cause it.

Use beat/bar-scale smoothing and hysteresis to identify candidate breakdowns:
sustained reduction of energy/transient density relative to the preceding
musical context, separate entry/exit thresholds, minimum duration and a silence
state. Emit an event on transition rather than retriggering once per audio block.
Different genres, sparse sections, quiet mastering and deliberate silence need
configuration. A breakdown transition is a policy hint that a human or agent
may use, not a universally correct section label.

“Energetic,” “dark,” “tense” or “happy” combine perceptual and cultural judgments.
Brightness/tempo/energy heuristics can drive a named control axis, but should not
be presented as verified emotion recognition. Optional trained models need
model/version/license metadata, analysis context, confidence and domain
validation. Phrase boundaries likewise need structural evidence such as novelty,
repetition and arrangement changes; counting bars provides a useful planned
phrase grid, not detected musical structure.

## Runtime and validation plan

Capture/sample IO belongs in an owned shell with explicit start/stop and bounded
queues. The analyzer returns next state and observations from explicit input.
Callbacks enqueue normalized commands or feature updates; no hardware IO or
user callback runs inside `swap!`. Rendering consumes the latest committed
feature snapshot and never waits for audio or an ML model. Backpressure drops
or coalesces observations explicitly and records lost samples; it must not grow
an unbounded backlog of stale “beats.”

Each provider owns one stream/session, sample-rate negotiation, reset on seek or
sample discontinuity, cleanup and model warmup. Browser analysis must not depend
on a visible tab staying active to keep the physical show running. Keep audio
capture/processing independent from re-frame render subscriptions, and publish
UI feature meters at a bounded display cadence.

Before promoting a provider, test silence/DC/clipping, shifted block boundaries,
48/44.1/22.05 kHz, varying hop/window, tempo changes, half/double tempo, dropped
blocks, restart and source clock drift. Maintain a licensed small music corpus
with manually checked onset/beat/downbeat labels, multiple genres and breakdown
annotations; report precision/recall and alignment error separately. Evaluate
long-run CPU/allocation, p95/p99 latency, queue pressure and thermal behavior on
the actual JVM host and iPad Safari. A feature extractor test or synthetic BPM
match is not enough. Keep the failed aubiojs fixture as an adapter evaluation
case rather than tuning it away without explanation.

## Verified upstream revisions and references

GitHub branch heads below were fetched on the research date; package smokes used
published versions separately, so a current repository commit is not claimed to
be the source revision of an older npm binary.

| Project | Inspected revision/package |
| --- | --- |
| aubio | `ad5cf975aed08cc4562dd008cf9f83b12b82ffb8` |
| aubiojs | `d59b2a9d0773bba3893dc0b88895458b7982e8d9`; smoke npm `0.2.1` |
| Meyda | `ecf256616d43292c82cbf96e60a91b19ca10eb64`; smoke npm `5.6.3` |
| Essentia | `df27f949d7827a43b4afc216fd5c788454b0f8a4` |
| essentia.js | `819c00e8f034dc709e3d330ed8ac9ed37dfdb767` |
| madmom | `27f032e8947204902c675e5e341a3faf5dc86dae` |
| BeatNet | `81cedd4beeb7235262db80969a0c9ce9a48a0ed4`; PyPI metadata `1.1.3` |
| TarsosDSP | `41476b268b8b34664d3e43997fb05c95cec29e3d` |
| librosa | `087a8607dd86e98a9032a62b357eb9afd23a14c5`; stream contract also inspected at tag `0.11.0` |
| Sonic Annotator | `7465f2b7838964f42103ac254709532e9b6624e6` |

Additional license/model references: [Essentia licensing][essentia-license],
[Essentia model catalogue][essentia-models], [madmom license][madmom-license],
[BeatNet license][beatnet-license], [aubiojs npm artifact metadata][aubiojs-npm]
and [Meyda npm artifact metadata][meyda-npm]. No library beyond the two npm
smokes was installed or benchmarked during this research.

[aubio-src]: https://github.com/aubio/aubio/blob/ad5cf975aed08cc4562dd008cf9f83b12b82ffb8/src/tempo/tempo.h
[aubiojs-src]: https://github.com/qiuxiang/aubiojs/tree/d59b2a9d0773bba3893dc0b88895458b7982e8d9
[meyda-src]: https://github.com/meyda/meyda/blob/ecf256616d43292c82cbf96e60a91b19ca10eb64/src/main.ts
[essentia-src]: https://github.com/MTG/essentia/tree/df27f949d7827a43b4afc216fd5c788454b0f8a4
[essentiajs-src]: https://github.com/MTG/essentia.js/tree/819c00e8f034dc709e3d330ed8ac9ed37dfdb767
[madmom-src]: https://github.com/CPJKU/madmom/blob/27f032e8947204902c675e5e341a3faf5dc86dae/madmom/features/beats.py
[beatnet-src]: https://github.com/mjhydri/BeatNet/blob/81cedd4beeb7235262db80969a0c9ce9a48a0ed4/src/BeatNet/BeatNet.py
[tarsos-src]: https://github.com/JorenSix/TarsosDSP/tree/41476b268b8b34664d3e43997fb05c95cec29e3d
[librosa-src]: https://github.com/librosa/librosa/tree/087a8607dd86e98a9032a62b357eb9afd23a14c5
[sonic-src]: https://github.com/sonic-visualiser/sonic-annotator/blob/7465f2b7838964f42103ac254709532e9b6624e6/README.md
[essentia-rhythm-code]: https://github.com/MTG/essentia/blob/df27f949d7827a43b4afc216fd5c788454b0f8a4/src/algorithms/rhythm/rhythmextractor2013.cpp#L107
[essentia-beat-code]: https://github.com/MTG/essentia/blob/df27f949d7827a43b4afc216fd5c788454b0f8a4/src/algorithms/rhythm/beattrackermultifeature.cpp#L203
[tarsos-beat]: https://github.com/JorenSix/TarsosDSP/blob/41476b268b8b34664d3e43997fb05c95cec29e3d/core/src/main/java/be/tarsos/dsp/beatroot/BeatRootOnsetEventHandler.java#L32
[librosa-stream]: https://github.com/librosa/librosa/blob/0.11.0/librosa/core/audio.py#L290
[essentia-license]: https://essentia.upf.edu/licensing_information.html
[essentia-models]: https://essentia.upf.edu/models.html
[madmom-license]: https://github.com/CPJKU/madmom/blob/27f032e8947204902c675e5e341a3faf5dc86dae/LICENSE
[beatnet-license]: https://github.com/mjhydri/BeatNet/blob/81cedd4beeb7235262db80969a0c9ce9a48a0ed4/LICENSE
[aubiojs-npm]: https://registry.npmjs.org/aubiojs/0.2.1
[meyda-npm]: https://registry.npmjs.org/meyda/5.6.3
