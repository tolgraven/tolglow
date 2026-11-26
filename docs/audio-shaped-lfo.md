# Audio-shaped LFOs

An audio-shaped LFO takes the shape of one band of the captured input, once per
musical window, and plays it as a slow modulation cycle that morphs toward each
new shape ([issue #676](https://github.com/tolgraven/tolglow/issues/676)). A
synth through a Live effects rack, a bass line or a guitar phrase supplies the
shape; the LFO scans it over beats or bars, thousands of times slower than the
audio. Each LFO is a registered waveform, so it works wherever a waveform does:
a parameter LFO on a running cue, a linked knob on that LFO's period or
endpoints, the waveform picker and glyphs, selectors and effects that lower a
waveform.

[DSP](../src/tolglow/audio/shape.clj) ·
[Owner state and waveform](../src/tolglow/audio_lfo.clj) ·
[Passages](../src/tolglow/audio/fixtures.clj) ·
[Studio panel](../src/studio/tolglow/studio/audio_lfo.cljs) ·
[Offline report](../dev/tolglow/qa/audio_lfo.clj) ·
[Benchmark](benchmarks/2026-11-26-audio-lfo.edn)

## How a shape is made

The capture worker that already decodes each PCM block for the analyzer also
runs one extractor per configured LFO. An extractor is immutable data; each
block returns a new one.

1. **Band.** A fourth-order band-pass: two Butterworth high-passes at the low
   edge and two low-passes at the high edge (Linkwitz–Riley, −6 dB at each edge,
   24 dB/octave outside). Bands are `:sub` 20–60 Hz, `:bass` 60–250 Hz,
   `:low-mid` 250–800 Hz, `:mid` 0.8–2.5 kHz, `:high` 2.5–8 kHz, `:full` (no
   filter) or an explicit `[low high]` in Hz.
2. **Window.** Every sample is placed on the transport's beat grid from the
   capture clock (beat, BPM, meter, bar and phrase origins), the same mapping the
   captured waveform's beat markers use. A window is `{:beats n}`, `{:bars n}`
   or `{:phrases n}` from the bar (or phrase) origin. Only a whole window is
   shaped: one that opens part way through, or that a stop, a seek, invalid
   timing or a capture discontinuity interrupts, is dropped.
3. **Shape.** With `:extract :envelope` the window is divided into `:points`
   equal beat spans and each point is the band's RMS there, divided by the
   loudest point, so the cycle is the window's rhythm and dynamics. With
   `:extract :cycle` the last 21–340 ms of the band at the window's end goes to
   YIN; up to eight consecutive pitch periods are averaged, rotated so the
   fundamental starts at zero phase rising, and stretched to 0..1. Rotation
   keeps successive shapes of the same tone aligned, so a morph between them
   does not wander in phase.
4. **Silence and confidence.** A window whose band RMS is below `:silence-db`
   (−50 dBFS by default) is `:silent`. A cycle whose YIN confidence is below
   `:min-confidence` (0.6), or that finds no period, is `:low-confidence`.

The worker hands the newest extraction per LFO to the show through a mailbox
that keeps only the latest per LFO, tagged with the capture run and the
extractor spec it was measured under. The owner ignores an extraction for an
LFO that no longer exists, for an older spec (a band changed since) or for a
sequence it already committed.

## How it morphs

On each committed shape the owner:

- smooths it into the target: `target = smoothing × previous + (1 − smoothing) × new`
  (`:smoothing` 0..0.95, default 0);
- starts a morph **from the shape showing at that moment** to the new target,
  over `:morph` (default one bar) on `:curve` `:linear`, `:smoothstep` (default)
  or `:sine`. A shape that arrives mid-morph continues from the blend, so the
  table never jumps;
- republishes the LFO's waveform with the new target.

Silence follows `:on-silence`: `:hold` (default) keeps the target, `:release`
morphs to `:rest-shape` (`:low` 0, `:mid` 0.5 or `:sine`). Low confidence always
holds. The state shows why: `:ok`, `:silent`, `:low-confidence` or `:waiting`.

Morph progress is measured in transport beats from the beat the shape was
committed. A stopped transport holds its beat, so the morph and every LFO bound
to it stand still and resume exactly where they were, with no catch-up; the
extractor drops the window the stop interrupted and shapes the next whole one.
A seek through the show clock's rebase keeps morph progress. A transport that
jumps back without a rebase shows the target.

## The registered waveform

Each LFO publishes `:audio/<id>` with `:live? true`, `:category :audio` and a
`:source` naming the LFO, band and mode. Its template is the target table as a
sum of triangular hats over `fract(phase)` (periodic linear interpolation, at
most 63 points), so glyphs, previews, selectors, GLSL and lowered effects all
see the latest target. `signals/publish-live` replaces a live waveform's history
with its newest version, so a shape every bar does not grow the registry or the
saved show.

A parameter LFO bound to the waveform samples the **morphing** table at the
frame's transport beat instead of the compiled target. A lowered effect keeps
the copy of the target it was built with. A saved binding keeps working after
the waveform's version moves on. Removing an LFO leaves its waveform as a frozen
snapshot of the last target, so bindings keep playing it.

## From the REPL

```clojure
(require '[tolglow.show :as show] '[tolglow.audio-lfo :as audio-lfo])

;; Create or change an LFO: the same owner command Studio sends.
@(show/submit! runtime :command
   {:op :audio-lfo/configure :lfo-id :bass :name "Bass"
    :config {:band :bass :extract :envelope :window {:bars 1}
             :morph {:bars 2} :curve :smoothstep :smoothing 0.25 :on-silence :hold}})

;; Drive a running cue's level from it, scanning one cycle per bar.
@(show/submit! runtime :modulation-bind
   {:instance-id "0:3" :param :level
    :binding {:type :lfo :waveform :audio/bass :period {:bars 1} :from 0.1 :to 1.0
              :phase-reference :transport}})

;; Read what Studio shows: status, last window, input, from/to/current shapes.
(audio-lfo/lfo-view (:model @(:state runtime)) :bass)

@(show/submit! runtime :command {:op :audio-lfo/remove :lfo-id :bass})
```

Configuration keys: `:band`, `:extract`, `:window`, `:points` (8–63, default
32), `:silence-db`, `:min-confidence`, `:morph`, `:curve`, `:smoothing`,
`:on-silence`, `:rest-shape`. An unknown key or invalid value is refused with
`:control/error :invalid-audio-lfo` and the reason; the LFO keeps its previous
configuration. Changing the band, mode, window, points or thresholds waits for
the next whole window under the new spec.

The scan period is the binding's `:period`; a linked knob can drive it, and the
`:from`/`:to` endpoints, like any other LFO ([Linked knobs](linked-knobs.md)).

Offline, the same extractor, admission and morph run over any mono PCM:

```clojure
(require '[tolglow.audio.fixtures :as fixtures])
(def run (audio-lfo/replay (fixtures/passage :delay-synth {:bpm 84 :bars 8})
                           {:bpm 84 :config {:extract :cycle :band :full} :scan 1.0}))
(map (juxt :status :latency-ms :table-jump) (:commits run))
```

## In Studio

**Connections → Audio LFOs** lists each LFO from the shared `:audio-lfos`
projection. A card shows the band, status, level, confidence, pitch, how late
the last shape was committed after its window ended, and the shape count. The
**Source band** chart draws the last window the worker measured: two periods of
the band-isolated waveform for a cycle, or the band level across the window
over its beat grid for an envelope. The **LFO cycle** chart draws the previous
targets, the shape the morph started from, the target and the current morph as
the owner samples it, with a progress bar. **Band**, **Shape from**,
**Window**, **Morph over**, **Smoothing**, **In silence** and **Apply** send
`:audio-lfo/configure`; **Remove** and **Add audio LFO** send the other owner
commands. The waveform appears in every LFO picker with its glyph.

`lein run -m tolglow.qa.audio-lfo-studio <port> <control-dir>` runs a
preview-only show with two LFOs on a paced synthetic input and the Breathe
cue's level bound to one of them; `node dev/studio/audio-lfo-browser-qa.mjs`
checks the panel against `/api/state`, applies a change, mutes the input to
show the hold policy and writes desktop and phone stills.

## Measured

`lein run -m tolglow.qa.audio-lfo <dir>` replays three deterministic passages
at 84 and 172 BPM, eight bars each, 48 kHz in 480-frame blocks with a 60 fps
owner. The envelope runs use the bass band scanned once per bar; the cycle runs
use the full range scanned once per beat, because the 60–250 Hz band strips a
bass synth's harmonics and leaves a near-sine cycle. The retained report is
[2026-11-26-audio-lfo.edn](benchmarks/2026-11-26-audio-lfo.edn).

| Passage | Envelope | Cycle |
| --- | --- | --- |
| Dry 110 Hz sine | 7 of 7 windows `:ok` at both tempos | 7 of 7 `:ok`, 110 Hz, confidence 1.0 |
| Delayed synth (saw and square bars, 3/8-beat echo, reverb) | 7 of 7 `:ok`; the shape is the bar's four attacks and their echoes | 7 of 7 `:ok`; the LFO morphs between the saw and square cycles |
| Plucked phrase, then a silent bar | `ok silent ok silent ok silent ok`; hold keeps the last shape through each silent bar | the same pattern; three ringing notes give a jagged chord period |

- **Discontinuities.** The largest table change on any commit frame was 0 in
  every run: each morph starts from the shape that was showing. The LFO's
  largest frame-to-frame step on commit frames stays within its ordinary
  motion; scanning a sharp envelope once per bar moves fast by itself.
- **Attacks, decay and silence.** An envelope shape puts each attack on the
  same beat point and decays toward the next; scanned per bar it reproduces the
  phrase's dynamics as light. A cycle shape is taken at the end of the window,
  so it follows the sustain or tail there, not the attack. Silence holds or
  releases by policy. A stop holds everything.
- **Latency.** A shape is committed 4.7–19.1 ms after its window's bar line in
  the replay: up to one capture block (10 ms) plus up to one owner frame
  (16.7 ms). It is one window late by construction: the shape of bar *n* starts
  morphing at the start of bar *n + 1*.
- **Cost.** Extraction took a median 16–24 µs and a p99 27–143 µs per 10 ms
  block per LFO. The slowest single block in a run ranged from 0.06 to 11 ms;
  the 11 ms block was one outlier on a machine running a dozen other JVMs.
  Sampling the morphed table took a median 2.7–3.5 µs per frame. The benchmark
  excludes device capture and decode, the analyzer's other features, owner
  scheduling, IPC and rendering.

The tests cover the band filter's pass and stop bands, cycle alignment at any
input phase, beat-locked envelopes at 70, 120 and 180 BPM, silence and noise,
stop and resume without catch-up, block-size independence, morph continuity,
smoothing, both silence policies, stale and duplicate extractions, one registry
version after thirty shapes, parameter LFO sampling while stopped and on resume,
a linked knob on the scan period, persistence and rebase, the two-tempo replay,
and the real owner and capture worker on injected PCM.

## Not verified and open

- No physical interface, Live set or Max for Live device was used. The passages
  are synthetic; recorded guitar, synth and stem material through a real input
  remains to be compared ([#681](https://github.com/tolgraven/tolglow/issues/681)).
- One analyzer input feeds every LFO. Per-stem routing is
  [#675](https://github.com/tolgraven/tolglow/issues/675).
- Band, window and morph time are Studio and REPL controls; linked knobs reach
  the scan period and endpoints but not the LFO's own configuration.
- Autopilot signals that name a registered waveform, and effects that lower one,
  use the latest target, not the per-frame morph.
- Cycle mode needs a monophonic, steady band at the window's end; chords and
  noisy material report low confidence or a jagged period. It also needs
  seamless input: an early version of the QA source that re-read the passage
  at each block's transport beat put a seam in every block, and every cycle
  window reported low confidence while envelopes were unaffected.
- Ship/defer: the slice is ready to try on real material; the decision on
  defaults per instrument waits for the recordings above.
