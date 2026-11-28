# Signal tracks

A signal track keeps the values an audio feature, a stem, a sensor or a
controller delivered while a performance ran, so the performance can be
replayed without its live inputs: on another rig, at another tempo, or as
part of the decision log's show replay. Issue
[#885](https://github.com/tolgraven/tolglow/issues/885), specified in
[cross-rig translation](research/native-agent/cross-rig.md#what-has-to-be-recorded).

[Recording, store and package](../src/tolglow/signal_tracks.clj) ·
[Writer and REPL operations](../src/tolglow/signal_tracks/recorder.clj) ·
[Rehearsal](rehearsal.md) · [Stems](stems.md) · [Live routing](live-routing.md)

## What a recording holds

A recording folds committed show models, in commit order. It reads only what
the frame already resolved and never samples a waveform, a sensor or a random
source again. Three kinds of source make tracks:

| Source | Track ID | Taken from | Extra column |
| --- | --- | --- | --- |
| Routing ports: audio features, stem levels and onset gates, MIDI, automation and control observations | the port's source ID, e.g. `stem/bass/level`, `audio/rms` | `[:routing :ports]`, one row when a port's capture time changed | none |
| Audio events the frame consumed, such as stem onsets | `event/<emitter>/<type>`, e.g. `event/stem/kick/onset` | `[:variables :audio :events]`, one row per event | the event's own beat |
| Parameter writes from human controller surfaces (Push, X-Touch, MPE, MIDI, OSC, Studio, sensors, REPL) | `control/<adapter>/<cue>/<param>@<session>` | accepted `:param/set` commands in the control history | the instance written |

Each track keeps its ID, kind, unit, calibrated range and address (stem,
signal, adapter, cue, parameter), then rows of:

```clojure
[beat time-ms value confidence & extra]
```

`beat` is the transport beat of the frame that resolved the value, `time-ms`
the capture time on the show clock. An event track also keeps the template of
its events (everything but time, beat, confidence and value), so a replay
rebuilds the same event.

The recording keeps the tempo segments it was made under,
`{:beat :time-ms :bpm :playing?}`, one per tempo, play state or position
change. `signal-tracks/beat->time-ms` turns a beat back into show time; a
beat after a stop maps from where play resumed.

Bounds, as a take's parameter moves have them: a track keeps at most
`:max-rows` rows (default 4096, up to 65,536); later rows are counted under
`:dropped` and the track is marked `:truncated? true`. A recording keeps at
most 256 tracks (`:sources-dropped` counts rows of the sources past that) and
1,024 tempo segments. Any dropped frame, row or source marks the recording
`:truncated? true`.

`signal-tracks/finish` gives a bundle, `{:recording r :tracks {hash track}}`.
The recording names each track by the SHA-256 of its canonical EDN, and the
tracks sit beside it under those hashes. `validate-bundle` checks a restored
bundle against its hashes.

A recording is attached to a `:rehearsal-take`, a `:run-take` or a
`:command-recording` (`:attached-to`). This first slice records rehearsal
takes; run record takes and command recordings take the same recorder.

## Writing off the frame thread

`tolglow.signal-tracks.recorder/attach!` installs a show frame observer. Under
the owner lock it only offers the committed model, an immutable value, to a
bounded queue (default 1,024 frames) and never waits. One owned thread folds
the queue. A frame the full queue refuses is counted under
`:frames-dropped`, and the recording is marked truncated: a stalled writer
loses rows, never frame time. `finish!` detaches the observer, folds what is
queued and returns the bundle.

## Transport stop

A stopped transport holds the beat, so nothing recorded while stopped
advances in beats. Stems are held while stopped (their onsets launch nothing
and their levels stop feeding routes), so a stop adds no stem row at all: the
same take with and without a two-second stop has the same rows at the same
beats, and only the capture times after the stop carry the hold. A replay
delivers rows by beat, so it delivers nothing new while stopped and, once
play resumes, the rows of the next frame only.

## Replay a rehearsal take from its tracks

```clojure
(require '[tolglow.rehearsal :as rehearsal]
         '[tolglow.rehearsal.passage :as passage]
         '[tolglow.signal-tracks :as signal-tracks])

(def take (rehearsal/run-take (passage/concert 670 :b) {:record-signals? true}))
(def bundle (:signal-tracks take))
(keys (get-in bundle [:recording :tracks]))
;; => ("control/push-3/chorus-wash/level@push-3-rehearsal" ... "stem/bass/level" ...)

(def replay (rehearsal/run-take (passage/concert 670 :b) {:signal-tracks bundle}))
(= (map :dmx-hash (:frames take)) (map :dmx-hash (:frames replay)))
;; => true

(signal-tracks/summary (signal-tracks/track bundle "stem/bass/level") {:grid {:beats 1}})
;; => [{:beat 0.0 :level 0.6 :peak 0.85 :onset? true} ...]
```

With `:signal-tracks`, the passage's stem onsets, stem levels and Push
parameter writes are left out (`rehearsal/signal-input-passage`); section
lines, the agent, Push launches and ends, stops and stem routes stay. Each
row is delivered for the frame whose beat it was recorded at: events through
the capture callbacks' event queue, port rows as one capture feature block,
controller rows as the same `:param/set` command with the recorded source and
instance. `:signal-replay` in the result lists what was delivered.

## On a running show

```clojure
(require '[tolglow.signal-tracks.recorder :as tracks])

(def recorder (tracks/attach! runtime {:id "set-2026-11-28"
                                       :attached-to {:kind :run-take :take-id "run-12"}}))
;; ... perform ...
(def bundle (tracks/finish! recorder))
@(tracks/store! runtime bundle)        ; message type :signal-tracks, op :put
(tracks/recordings runtime)
(tracks/summary runtime "set-2026-11-28" "stem/bass/level" {:grid {:beats 1}})
@(tracks/remove! runtime "set-2026-11-28")
```

## Backups and sharing

Stored bundles live in the show model under
`[:extensions :tolglow.signal-tracks/store]`, at most 64 recordings and 1,024
tracks, each track kept once by hash. A show save or checkpoint (a full
backup) keeps them, and a restore validates every hash. A sharing export is
separate and explicit: `signal-tracks/export-package` carries a recording's
description and only the tracks named in `:tracks`, listing the omitted ones.
Each audio-derived track it carries is listed under `:privacy` with
`:identifies-music? true`, since stem and feature tracks can identify the
piece of music they were recorded from.

## Verification

`tolglow.signal-tracks-test`, real show owner, captured output:

- A concert take (stems routed to cues and a parameter, two Push parameter
  grabs) writes six tracks. Replaying it with the tracks as its only signal
  input reproduces every frame's output digest and frame record; without the
  tracks it does not.
- A counting stem source is read once per frame with recording on and off,
  the frame's waveform sampler runs as often, and the track holds each
  resolved value once, in order.
- A take with a two-second stop has the same rows at the same beats as
  without it; its replay matches the take frame for frame and never advances
  more than one frame's beats.
- A track bounded to 8 rows keeps the first 8, counts 12 dropped and is
  truncated.
- With the writer stalled and a 4-frame queue, 40 frames commit; refused
  frames are counted and mark the recording truncated.
- A stored bundle survives a show save and restore; a sharing package holds
  no track unless selected.

## Not yet

- Only rehearsal takes attach a recording. Run record takes (#683) and the
  command and frame recording of show replay (#856) need the same `attach!`
  wired to their lifetimes.
- Replay rebuilds stem levels, audio features and stem onset events. MIDI,
  automation and sensor ports are recorded but no harness feeds them back
  yet, and Push encoder gesture streams (`:gesture/sample`) are not tracked;
  `:param/set` writes are.
- Rows recorded while stopped replay on the first stopped frame at that beat;
  their capture times are kept but not used to pace them.
- Translating a track to another tempo or rig (#886) and the per-grid summary's
  use there are later work.
- Studio has no view of stored tracks (see
  [REPL and Studio parity](repl-studio-parity.md#record-signal-tracks)).
