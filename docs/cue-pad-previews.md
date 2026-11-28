# Cue pad previews

On the Perform page, visible cue cells show an isolated sample of their effect.
The same observation feeds the Push 2 cue pads. A cue can remain inactive while
its color, dimmer or material moves with the show clock. Pressing a pad keeps
its existing launch and release behavior. The physical adapter scales the
sample to 25% for an inactive cue, 100% for an active cue and 45% for a cue in
release. The Web grid keeps the active and releasing styles and category tag.

By default the sampler chooses one patched fixture targeted by the effect. It
honors cue and launch target selections. It uses the head’s actual position,
emitter mix, transport meter, authored and saved parameters, frozen
show variables, and the effect's captured definitions. For a launched instance
it uses its parameter overrides, private clock, opacity and release state. HDR
previews use the committed emitter gain and white-mix settings on that head. An intensity-only effect
uses its authored swatch as the color to dim. Movement-only and control cues
retain their authored swatches. This preview represents a single cue in
isolation. The room preview continues to show the final mixed fixture output.

A cue can choose its representative fixture or supply a portable visualization
expression using the effect's existing parameter schema.

```clojure
{:id :wash-preview
 :effect-id :wash
 :pad-preview {:fixture-id :head-4
               :color {:op :const :value [0.85 0.09 0.15]}
               :dimmer {:op :param :key :level}}}
```

Set `:pad-preview false` for a static pad. A missing target or a definition
without a colour or dimmer output keeps its authored color. A definition that
does not compile for its preview head, an instance whose parameters do not
resolve, or a prediction that throws (for example an output outside [0,1]
without HDR) is a fault: the pad keeps its authored color and Studio says which
cue failed and why, below the grid (see [Faults](#faults)).

## Breathing pads

On the web grid, a pad whose cue has a running instance breathes once per beat
over its preview colour, like a playing clip on a Push session view. The
breath peaks on the downbeat, the same phase convention as Afterglow's
`snapshot-beat-phase`. A releasing pad blinks bright for the first 40% of each
beat and dim for the rest, as Afterglow's Push grid blinks an ending cue. A
queued pad flashes twice per beat, and a pad held in **Hold** mode keeps a
brighter floor. The preview sample and the light layer are separate, so the
preview animation keeps playing underneath. With `prefers-reduced-motion` the
pads show a steady running indicator instead.

Each light layer is a CSS opacity animation lasting one beat, or half a beat
for the queued flash. No JavaScript runs per frame and there are no per-pad
timers. One clock per page aligns the animations to the transport. Each new
projected transport sample (`:beat`, `:bpm`, `:playing?`) gives the local time
of the last downbeat, and the clock holds every pad animation's start time on
the document timeline there. Stream and frame latency only ever delay a
sample, so the clock keeps the least-delayed sample of the last second and
moves the start time only when that estimate is more than 20 ms away. Samples
that were delayed longer, and start times a whole beat apart, change nothing.
A new tempo, a new transport generation (a seek or restart) or a jump of more
than 100 ms is followed with the next sample. A sample whose server
`:time-ms` shows it arrived more than 100 ms later than the promptest one in
that second, because the page's main thread or the stream stalled, changes
nothing; if every sample stays that late for a second, the clock follows the
new delay. The clock reads the animations
when a sample arrives or a pad animation starts. It writes `data-breath` and
`--beat-duration` (rounded to 0.1 ms) on `#cue-grid` only when they change.
Pads show the steady indicator when nothing on the page animates, the
transport is paused or has no tempo, two seconds pass without a sample, the
page is hidden or motion is reduced. One timer per sample decides that the
stream is silent. The next sample, visibility change or reduced-motion change
restarts the pads. The phase functions and the clock live in
[`pad_breath.cljs`](../src/studio/tolglow/studio/pad_breath.cljs).

From the REPL, the same inputs are `(:transport (show/snapshot runtime))` and
each instance's `:phase`. The physical Push adapter scales the preview sample
by state, as described above, and does not breathe yet.

## Timing and ownership

A worker samples the present and predicted color a quarter beat apart, bounded
to 60–250 ms at the baseline rate, then interpolates RGB on each presentation
update. Tempo, transport jumps, instance lifecycle and definition replacement
invalidate predictions. Sampled external inputs refresh with the next
keyframe.

Keyframe windows chain: while playing, when a window runs out the next one
starts where it ended and reuses its last prediction, so a cue costs one
prediction per window rather than two. A chained window resyncs to the
transport when its extrapolated beat drifts by 0.02 beat.

All readers of one committed frame (the Studio grids and the Push) share a
budget of 32 predictions (`(cue-pads/manager {:samples-per-frame n})`). The
first reader of a frame brings every leased cue up to date in one order; later
readers of that frame only read colours, so no reader starves because it came
second. The order is: cues with no keyframes or a changed identity, then
expired cues, then due fault retries, each the longest waiting first.

A cue's identity is the show epoch, the HDR mode and, for a running cue, its
instance's ID, phase, pause and start and release times. When identity changes
and the cue misses the budget it shows its authored colour until its turn,
never an old preview. Tempo, meter and playing are timing: when only they
change, or a window simply runs out, a cue that misses the budget holds the
colour it shows until its turn. That is normally the next frame. If deferrals
or drops continue for 16 frames, the sampler slows its keyframes one level
(doubling the interval), and it speeds up again only after 32 calm frames,
when the demand would fit the budget at the faster level. Two full pages at
180 BPM settle one level down; at 120 BPM they stay at the base rate.

Stopping the transport pauses every pad at the colour it shows, with no
prediction, as the show pauses its effects. A cue first shown while stopped
costs one prediction and has a single keyframe, so nothing animates or runs
ahead while stopped. Playing again resumes from the stopped beat: each pad
holds its colour until it is refreshed.

Prediction reads the instance's effective parameters from the frame's shared
parameter sample
([waveform projections](waveform-projections.md#shared-parameter-sample)), so
it never samples a waveform source, advances a controller or runs a response
chain a second time. A page can stagger while its MIDI palette acknowledgements arrive;
the next batch begins after the last updated pad. While that batch drains,
the physical session reuses its grid projection for the same committed
snapshot, page and origin. A new snapshot or viewport immediately replaces it. The tested transport uses a
captured Push 2 protocol stream. Physical Push 3 User Mode does not define
palette writes in the current adapter.

Each focused web view subscribes to the intersecting rows with a Transit query
`[:visualization/cue-pads page-id [left bottom] rows]`. Changing page, hiding
the window, navigating away or scrolling the grid out of view disposes the
remote reaction. A page that no Studio view or Push shows has no lease, so it
has no plan, no keyframes and no predictions; moving a lease to another page
drops the old page's entries on that frame. The query result is
`{:colors {cue-id rgb}}`, with `:faults` when visible cues failed. The physical adapter obtains a separate lease for its eight
rows and relinquishes it for pad pickers, suspension and disconnect. Visible
cues shared by the two surfaces also share their compiled plan and keyframes.
The last reader releases those cached entries. Up to 64 distinct rectangles
and 512 distinct cue IDs may be observed concurrently. The periodic full and
path-diff messages use Studio's ordinary acknowledged push stream.

The show thread publishes its last measured frame cost to the visual governor,
and the sampler adds its own pressure level as described above; the slower of
the two applies.
With visible work, two successive frames above 85% of the frame budget double
the visual keyframe interval, up to sixteen times the base interval. Cheap
presentation interpolation continues. Persistent pressure at this floor
releases the live-effect adaptive scheduler. Thirty-two successive frames
below 65% restore one level. Histories can use `:visualization-clock
:sample-period-ms`, which ranges from 100 to 1,600 ms. This policy operates on
presentation workers and never writes to DMX or launches an effect.

## Faults

While the Perform cue grid is shown, Studio shows a notice under it instead of
an unexplained color when:

- the page's query fails, or its lease is refused because 64 rectangles or 512
  cues are already observed ("Pad previews unavailable: …");
- a visible cue's plan, parameters or prediction fails (its name, the stage and
  the message, a few by name and then a count). A failed cue retries after one
  keyframe interval, then backs off, doubling up to two seconds. Retries wait
  behind healthy cues, a retry that misses the budget keeps its fault, and the
  fault clears when a prediction succeeds;
- a response-sample lease predicate threw. These are the messages of the
  records `show/response-sample-faults` returns, truncated to 240 characters,
  and whether each repeated. The REPL record's count ticks every frame and its
  ex-data need not be portable, so both stay with the REPL; a steady failure
  does not change the Studio projection.

From the REPL, the same state:

```clojure
(require '[tolglow.visualization.cue-pads :as cue-pads])
(cue-pads/diagnostics (:cue-pad-previews runtime))
;; => {:level 0 :clients 128 :leases 2 :demand #{...} :samples-per-frame 32
;;     :frame {:spent 24 :deferred 0 :dropped 0}
;;     :totals {:predictions 20464 :deferred 0 :dropped 0}
;;     :faults {:merlot {:stage :plan :message "Invalid effect expression"}}}
(show/response-sample-faults runtime)
```

## Verification

Run `lein test tolglow.visualization.cue-pads-test` for shared-cache lifecycle,
interpolation, isolation, failure fallback, pressure policy and captured MIDI
assertions. Its paged tests count the compiled effect steps the sampler runs
on 8 pages × 64 cues with frame times as data: per frame, per cue and window,
for shown and hidden pages, and for faults.

`lein run -m tolglow.bench-cue-pads OUTPUT [--warmup N --samples N --rounds N
--baseline FILE]` measures the owner tick and the pad sampling pass per frame
on that show with a Studio grid and a Push grid on two pages. `--baseline`
takes a copy of an earlier `cue_pads.clj` with its ns renamed and alternates it
with the current one in the same JVM. Measured on 2026-11-28 (Java 25, M4 Pro
shared with other jobs at a load average of 15–31, three alternating rounds
of 800 frames after 200 warmup, 16-term graphs), against the master
implementation:

| per frame | master | chained, one budget per frame |
|---|---|---|
| pad predictions, max / mean | 256 / 48.6 | 32 / 25.0–25.6 |
| pad sampling CPU, p95 / max | 11.9–14.3 / 23–31 ms | 3.8–5.3 / 6.2–9.1 ms |
| pad sampling CPU, mean | 2.52–2.84 ms | 2.07–2.83 ms |
| pad sampling allocation, mean / p95 | 8.22 / 40.9 MB | 4.29–4.38 / 5.49 MB |
| both pad passes on a frame with no prediction | 546 KB | 229 KB |
| owner tick allocation, mean | 384 KB | 384 KB |
| pads with a colour, min | 128 of 128 | 128 of 128 |

The owner's code is the same in both arms, and its CPU medians overlapped
(1.20–1.57 ms beside the master sampler, 1.43–1.64 ms beside this one). The
mean sampling CPU overlaps under this load; the spikes are what fell. Retained
heap, the Studio stream's other projections, Transit encoding and browser
drawing are excluded. To exercise actual Studio subscriptions from Chromium, build
`npm run studio:build`, start `lein run -m tolglow.qa.cue-pad-preview PORT_FILE
STOP_FILE`, then run `node dev/studio/cue-pad-preview-qa.mjs
http://127.0.0.1:PORT OUTPUT_DIRECTORY`. Create the stop file to release the QA
server. The browser check records an inactive moving page, switches pages, checks
the fault notice for the Faults page's overdriven cue and verifies the
outgoing unsubscribe when navigating away. A third argument to the QA server
binds that port instead of an ephemeral one.

To replay the Push 2 session, run
`lein run -m tolglow.qa.push-pad-feedback /tmp/push-pad-feedback.edn`.
The report records each outgoing palette write and counts the acknowledgements
and grid projections. A four-pass comparison with 64 independently phased
idle cues produced 3,598 identical palette writes in each measured 80-frame
window. Reusing the projection reduced calls from 566 to 80 and average
device-session CPU from 7.43–8.75 ms to 4.65–4.74 ms per committed frame.
The emulator acknowledges each batch immediately. Hardware latency and USB
scheduling need measurements on a connected Push 2.
