# Live signal and captured audio projections

`tolglow.studio.waveforms` provides read-only data for the shared Studio/Push
widgets. Its two sources are deliberately explicit: a registered signal sampled
from current effect parameters, and measured incoming PCM. Neither executes an
effect, advances a controller, consumes randomness or reads a clock.

## Registered signal previews

`POST /api/waveforms/preview` accepts a live request:

```clojure
{:instance-id "0:1" :parameter :waveform
 :options {:samples 129 :cycles 1 :window 1/1024}}
```

The host captures one committed snapshot and resolves the instance's captured
definition/schema, ordered waveform choices and pinned versions. Parameter
bindings such as pulse duty `{:op :param :key :width}` use that instance's current
parameters. The same committed modulation overrides and response middleware are
applied in the same order as effect evaluation, using the captured private clock
for paused instances. A client cannot substitute live values through
`:context-params`. The optional client `:input-key` is only an invalidation hint.

Draft requests instead provide explicit `:selection`, `:context-params` and
`:options`; the previous `{:id :sine :options ...}` form remains available.
The actual numeric IR is lowered and compiled once per requested channel.
Two to 2,048 phase samples produce `:value`, `:slope`, `:angular-velocity` and
`:jagginess` rows with their units, derivative window and cycle range. The same
response includes `:preview-samples` for the compact widget and an `:identity`
containing show epoch, instance/definition/version, waveform revision and input
digests. A preview describes the selected registered signal; it is not a claim
to graph every downstream spatial fixture expression.

The optional display cache retains at most 64 projections keyed by exact
immutable definitions, effective parameter values, registry and sample options.
The public digest is never used as the cache's correctness key. Changing only
the frame counter does not recompile an unchanged shape. Invalid expressions or
stale instances produce a visible unavailable/stale result rather than an
invented trace. Show definitions and runtime state remain untouched.

## Captured PCM waveform

The existing JavaSound capture worker decodes each signed-16 PCM block once.
That same mono array feeds analysis and a bounded streaming summary. Stereo is
averaged exactly as in the existing analyzer; opposite-phase channels can cancel.
No raw sample array is retained in a model, browser snapshot or saved show file.

Default history is 256 columns of 20 ms, about 5.12 seconds. A column contains
measured minimum, maximum, RMS, low/mid/high band RMS and sample/time bounds.
Continuous 200 Hz and 2 kHz one-pole crossovers split the signal for the
Rekordbox/CDJ-inspired color rendering. These are frequency-energy channels,
not inferred mood or instrument labels. The frontend owns the palette.

Incomplete columns remain explicitly partial; their data carries into the next
block. Source/format changes, known lost frames, suspected overflow or a time
discontinuity reset history and filters and increment generation. Missing time
is never filled with synthetic sound. Invalid timing suppresses beat markers.
Valid markers derive from the supplied authoritative transport anchor, BPM and
meter, including fractional quarter-note bar lengths such as 7/8. Markers retain
source/epoch provenance; there is no free-running invented beat detector here.

`GET /api/audio` exposes `:waveform` after the first captured block. The inert
`audio/waveform` getter returns an empty `:available? false` shape before capture:

```clojure
{:bins [{:min -0.6 :max 0.7 :rms 0.4
         :bands {:low 0.3 :mid 0.2 :high 0.1}}]
 :duration-ms 20 :sample-rate 48000 :channels 2 :mix :mono-average
 :source-id "owned-capture-run" :generation 1 :sample-end 960
 :start-time-ms 0 :end-time-ms 20 :timing-valid? true :available? true
 :beat-markers [{:offset-ms 0 :beat 0 :bar? true :source :link :epoch 2}]}
```

Stopping capture preserves the last measured history, while the audio status
reports that capture has stopped. Starting a new capture clears it before
opening the source. Device acquisition remains an explicit existing operation.

## Push capture worker

Push views now carry show epoch, frame number, instance ID and captured definition
version. The frame counter allows audio/modulation refresh even when base knobs
have not changed. The existing display queue coalesces to one pending view;
its capture worker calls an optional `:project!` immediately before browser
capture. When supplied, it re-projects the last immutable view at the bounded
display cadence even without a new model view, so audio keeps moving while the
lighting evaluator is busy. Transports without a projector remain event-driven.
Projection never runs on the effect/render or MIDI caller thread.

`project-display` adds actual base/effective values, generated waveform samples,
and top-level `:audio-waveform`. Audio columns are reduced to at most 128 for the
existing bounded IPC message, retaining extrema and sample-weighted RMS.
`:display-mode` follows `[:settings :controllers :push-display :mode]`, with
`:parameters` as default and `:audio` explicit. Both views consume the same data.
No display component opens an input, polls the show independently, or owns a
second lighting clock. Physical Push/iPad evidence remains a separate gate.

## Verification

The focused waveform suite passed **11 tests / 86 assertions**, including a
sample-by-sample comparison with the real effect compiler, authoritative live
inputs, modulation and response/private-clock behavior, bounded exact cache,
measured frequency dominance, partial blocks/gaps, fractional-bar markers,
actual injected PCM capture, stereo cancellation, real Push view projection
on the display worker and a real Ring/owner parameter-edit workflow. Captured
clock bindings invalidate the preview cache without advancing effects. The
post-layout gate ran from `src/tolglow/`; stopped external clocks suppress beat
markers and delayed Push capture refreshes base/effective values coherently.
The earlier combined waveform/audio/display run passed
**27 tests / 193 assertions**, before the compact-audio, Ring and clock additions.
These counts overlap; they must not be added as independent coverage.

The final waveform/display gate passed **17 tests / 118 assertions**, including
the additional cadence regression: a single submitted Push view receives new
captured audio on a later display cycle without another model-view submission.
Existing capture failure, cleanup, coalescing and real process-protocol tests
also passed after the worker change.

Browser mounting, actual capture screenshots and the complete host/device
connection workflow are coordinated separately. Injected PCM and headless
display transport tests establish software behavior, not physical audio or
controller verification.

The retained [summary benchmark](benchmarks/2026-11-21-audio-waveform.edn)
measured 1,000 blocks after 200 warmups: 48 kHz, 480 samples/block and a 256-column
history. Summary plus immutable view median/p99 were **0.080/0.284 ms**, maximum
**1.045 ms**, on Clojure 1.12.7 and Java 21.0.13. The
[harness](benchmarks/audio-waveform-bench.clj) excludes device capture, decode,
the existing analyzer, scheduling, JSON/HTTP, Push IPC and browser rendering.
This stage result is not a complete audio or lighting deadline guarantee.
