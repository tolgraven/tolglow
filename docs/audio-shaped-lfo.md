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

The work is split by thread. The capture thread, which already decodes each
PCM block for the analyzer, does only what must follow the input: it filters
each distinct band once into a ring of recent samples shared by every LFO on
that band, accumulates each LFO's window, and when a window closes hands an
immutable job (the window's bins, or a fresh copy of the band's samples up to
the edge) to a separate shape worker thread. The worker does the window-end
analysis and publishes the result. Its pending work is one job per LFO and the
newest wins, so a slow analysis can delay a shape but never queue without
bound or hold up capture. A show can have at most 8 audio LFOs; a ninth is
refused with the reason.

1. **Band.** A band-pass of two Linkwitz–Riley edges: two Butterworth
   high-passes at the low edge and two low-passes at the high edge (fourth
   order per edge, −6 dB at each edge, 24 dB/octave outside). It is not a
   crossover: adjacent bands do not sum flat. Bands are `:sub` 20–60 Hz, `:bass` 60–250 Hz,
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
   `:extract :cycle` the band's samples at the window's end (21–340 ms, by the
   band's lowest frequency), stopping a **tail guard** before the bar line
   (below), go to a bounded YIN: only the last 2 × max-lag
   samples, after decimating band-limited audio by up to 16 so a low band costs
   no more than a high one (`:sub` searches 150 lags at 3 kHz instead of 2400 at
   48 kHz; `:full`, being unfiltered, is not decimated). The period is then
   refined at the full rate, and up to eight consecutive periods before the
   guard are averaged,
   rotated so the fundamental starts at zero phase rising, and stretched to
   0..1. Rotation
   keeps successive shapes of the same tone aligned, so a morph between them
   does not wander in phase.
4. **Silence and confidence.** A window whose band RMS is below `:silence-db`
   (−50 dBFS by default) is `:silent`. A cycle's confidence is 1 − YIN
   aperiodicity at the chosen period (the first dip below 0.2, else the deepest
   in range). Below `:min-confidence` (0.8), or with no period, it is
   `:low-confidence`. On the test material white noise scores 0.09–0.39, three
   ringing plucked notes 0.28–0.84 by band, and a steady synth or sine
   0.88–1.0.

**Tail guard** ([#912](https://github.com/tolgraven/tolglow/issues/912)). A
cycle's analysed tail ends 1/32 beat before the bar line by default: 26.8 ms
at 70 BPM, 15.6 ms at 120 and 10.4 ms at 180, never under 5 ms or over 60 ms.
`:tail-guard {:beats x}` (x above 0, at most 1/8) sets it per LFO. Audio that
leads the transport by up to about the guard keeps the next bar's first attack
out of the analysis. Each band's ring holds YIN's 2 × max-lag × factor plus the
longest guard (8192 samples for `:full` and 16384 for `:sub` at 48 kHz, as
before; `:bass`, `:low-mid` and `:mid` grow to 8192, 4096 and 4096). Every
extraction reports `:analysed-until-beat`, the transport beat its analysis
reads up to, and a cycle its `:tail-guard` `{:beats :ms}`; an envelope reads
its whole window and has no guard.

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
;; Where the last cycle was analysed up to, and the guard before its bar line.
(select-keys (:last (audio-lfo/lfo-view (:model @(:state runtime)) :bass)) [:analysed-until-beat :tail-guard])

@(show/submit! runtime :command {:op :audio-lfo/remove :lfo-id :bass})
```

Configuration keys: `:band`, `:extract`, `:window`, `:points` (8–63, default
32), `:silence-db`, `:min-confidence`, `:tail-guard` (optional, `nil` returns
to the default), `:morph`, `:curve`, `:smoothing`,
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
the last shape was committed after its window ended, **Analysed to** (the
last extraction's `:analysed-until-beat` and guard, for example "beat 7.969 ·
guard 15.6 ms") and the shape count. The audio input card shows the capture's
**Clock drift** (below). The
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

## Timing tolerance

Capture timestamps, the transport and the audio can disagree in two ways
([#912](https://github.com/tolgraven/tolglow/issues/912)), and both are now
guarded.

- **A transport that runs early or late against the audio** (a beat follower,
  Link jitter, output and input latency). The tail guard above absorbs audio
  that leads its bar line by up to about the guard; audio that trails it moves
  the analysed tail earlier into the bar's last note.
- **A device sample clock that drifts against the host clock.** Capture stamps
  block N at origin + cursor / sample-rate. A JavaSound line declares
  `:sample-clock :device`, and capture corrects its timestamps with a
  delay-locked loop (`tolglow.audio.drift`): the floor of
  `received-at-ms − end` per second of audio, a line fitted through the last
  16 floors, anchored by the latency floor of the first four with the drift
  taken out. The correction moves at most a quarter sample per 480-frame
  block (521 ppm at 48 kHz), so consecutive blocks stay contiguous for the
  analyzer, the waveform and the extractor (each treats a gap over half a
  sample as a discontinuity). `[:metrics :clock-drift]` in the capture status
  reports `:state`, `:ppm` (negative when the device is slow),
  `:correction-ms` and `:target-ms`; beyond 250 ppm (`:out-of-bound`), or with
  the correction more than 2 ms behind its target (`:behind`), timing is
  invalid, so no window is shaped and no beat is aligned, until it recovers.
  A recorded file and an injected source default to `:sample-clock :host`.

**Offset sweep.** `tolglow.qa.audio-lfo/offset-sweep` runs every passage in
cycle mode (full range, eight bars) at 70, 120 and 180 BPM with the transport
offset 30, 25, 20, 10 and 5 ms early, on time, and 1, 2, 5, 8 and 10 ms late,
through the offline replay and through the real capture and shape workers on
simulated time. The two paths agree to the window (the workers also shape the
last bar, which the replay's frame loop stops short of).

| Passage | Result |
| --- | --- |
| Dry 110 Hz sine | every window `:ok` at every tempo and offset, confidence 1.0, 110.0 Hz |
| Delayed synth (bar ends on 82.5 Hz) | every window `:ok` at 70 and 120 BPM (confidence ≥ 0.96, 82.33–82.62 Hz) and at 180 BPM from 25 ms early to 10 ms late (≥ 0.88) |
| Plucked phrase | played bars 0.80–0.83 at 70 BPM; at 120 and 180 BPM one played bar in four scores 0.78–0.80 at most offsets, as on master with the clock on time |

Below the bar, listed explicitly:

- **Delayed synth, 180 BPM, transport 30 ms early (audio 30 ms late):** 5 of 7
  windows `:low-confidence` (0.75–0.79). The tail then ends 40.4 ms before the
  bar line, on the attack of the 3/8-beat echo of the previous note, which
  starts 41.7 ms before the bar line at 180 BPM. Without a guard the same
  happens with the audio 40 ms late; the guard moves the limit earlier by its
  own length. 25 ms early passes (0.89).
- **Plucked phrase:** its played bar ends on three ringing notes, so YIN
  reports their common period (36.7 Hz) or one of them (55 or 110 Hz), at
  0.78–0.83 confidence whatever the offset. Its rest bars hold the previous
  bar's damped ring or the next pluck once the transport is offset, and report
  low confidence rather than silence. Neither is guard-related.

With the clock on time, the existing fixtures do not regress against master
30b5748a: every window keeps its status; the delayed synth's
confidence moves −0.013 to +0.031 and its pitch −0.28 to +0.29 Hz, with the
largest error from 82.5 Hz now 0.21 Hz (0.24 on master); the dry sine is
unchanged; the plucked phrase's chord windows move up to ±0.08 in confidence
and between the chord's period and one of its notes. Envelope extractions are
unchanged.

**Drift runs.** `tolglow.qa.audio-lfo/drift-run` plays the delayed synth at 120
BPM through real capture for ten minutes of simulated audio at 8 kHz, with
4 ms delivery latency plus up to 3 ms of seeded jitter, and a cycle LFO:

| Device clock | Tracking | Result |
| --- | --- | --- |
| 100 ppm slow | `:device` | measured −97 ppm, correction 60.08 ms, newest blocks within 0.37 ms of their real capture times after 20 s (0.63 ms before), 299 of 299 windows `:ok` |
| 100 ppm fast | `:device` | measured +104 ppm, correction −59.93 ms, within 0.36 ms, 299 of 299 `:ok` |
| 100 ppm slow | `:host` (uncorrected) | 60 ms behind after ten minutes; 204 of 299 windows `:low-confidence` once the drift passes the guard |
| 100 ppm fast | `:host` (uncorrected) | stops with `:capture-clock-skew` after 42 s |
| 1000 ppm slow | `:device` | `:out-of-bound` at −1013 ppm, timing invalid, no window shaped after the first two |

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
| Plucked phrase, then a silent bar | `ok silent ok silent ok silent ok`; hold keeps the last shape through each silent bar | the same pattern at 84 BPM; at 172 BPM two of the four played bars score below 0.8 with three notes ringing and hold the previous shape |

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
  (16.7 ms), plus the shape worker's analysis live (below). It is one window late by construction: the shape of bar *n* starts
  morphing at the start of bar *n + 1*.
- **Cost on the capture thread.** One LFO per run: a median 12–17 µs, p99
  21–43 µs and maximum 63–280 µs per 10 ms block, including the closing block
  that hands off the window. The **six-LFO budget run** plays the delayed synth
  in real time through six cycle-mode LFOs on one bar grid (`:sub`, `:bass`
  twice, `:low-mid`, `:mid`, `:full`; five bands) with the real shape worker, at
  70 and 180 BPM: a median 204 µs, p99 657–674 µs and maximum 1.93–1.97 ms per
  block (the closing blocks: median 678–717 µs), against 10 ms blocks and an
  80 ms capture buffer.
- **Cost on the shape worker.** Finishing one window took 14 µs (envelope) to
  2.3 ms (full-range cycle). In the six-LFO run every closed window was
  published (4 per LFO at each tempo) at most 5.8 ms after its closing block,
  against 1.3–3.4 s windows.
- **Sampling** the morphed table took a median 0.2–2.5 µs per frame. The
  benchmark excludes device capture and decode, the analyzer's other features,
  owner scheduling, IPC and rendering, and ran on a machine shared with other
  JVMs. Before this split, cycle-mode YIN ran over the whole ring on the
  capture thread and a closing block took up to 51 ms for one `:full` LFO.

The tests cover the band filter's pass and stop bands, cycle alignment at any
input phase, beat-locked envelopes at 70, 120 and 180 BPM, silence and noise,
stop and resume without catch-up, block-size independence, morph continuity,
smoothing, both silence policies, stale and duplicate extractions, one registry
version after thirty shapes, parameter LFO sampling while stopped and on resume,
a linked knob on the scan period, persistence and rebase, the two-tempo replay,
the bounded and decimated YIN, refined periods, confidence against noise,
shared band fronts and handed-off copies, the shape worker's newest-wins
hand-off, drain and cancel, the eight-LFO cap, and the real owner, capture
worker and shape worker on injected PCM.

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
- Cycle mode's YIN reads the last 2 × max-lag samples before the tail guard
  (67 ms on the full band at 48 kHz); refining the period and averaging the
  shape read up to eight periods before it. Before the guard, input 1.5–2 ms
  ahead of the transport put the next bar's first note in the tail (the QA
  source did this until it reported its own start time,
  [#878](https://github.com/tolgraven/tolglow/issues/878)); the guard and the
  drift correction above now cover a clock up to about the guard late and a
  device clock up to 250 ppm off. A real interface and a beat follower have
  not been qualified: their latency, jitter and drift are synthetic here.
- Ship/defer: the slice is ready to try on real material; the decision on
  defaults per instrument waits for the recordings above.
