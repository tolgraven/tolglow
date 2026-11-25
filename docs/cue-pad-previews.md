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

Set `:pad-preview false` for a static pad. A missing target, unsupported
definition or invalid expression falls back to its authored color.

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
timers. One clock per page aligns the animations to the transport. On each
new projected transport sample (`:beat`, `:bpm`, `:playing?`) or transport
generation, it sets every pad animation's start time on the document timeline
to the local time of the last downbeat. A tempo change or seek therefore takes
effect with the next sample. Animations already within 6 ms are left alone, so
stream jitter does not nudge them. The clock writes `data-breath` and
`--beat-duration` on `#cue-grid` only when they change. Pads show the steady
indicator when nothing on the page animates, the transport is paused or has no
tempo, two seconds pass without a sample, the page is hidden or motion is
reduced. The next sample, visibility change or reduced-motion change restarts
them. The phase functions and the clock live in
[`pad_breath.cljs`](../src/studio/tolglow/studio/pad_breath.cljs).

From the REPL, the same inputs are `(:transport (show/snapshot runtime))` and
each instance's `:phase`. The physical Push adapter scales the preview sample
by state, as described above, and does not breathe yet.

## Timing and ownership

A worker samples the present and predicted color a quarter beat apart, bounded
to 60–250 ms at the baseline rate, then interpolates RGB on each presentation
update. Tempo, transport jumps, instance lifecycle and definition replacement
invalidate predictions. Sampled external inputs refresh with the next
keyframe. A page can stagger while its MIDI palette acknowledgements arrive;
the next batch begins after the last updated pad. While that batch drains,
the physical session reuses its grid projection for the same committed
snapshot, page and origin. A new snapshot or viewport immediately replaces it. The tested transport uses a
captured Push 2 protocol stream. Physical Push 3 User Mode does not define
palette writes in the current adapter.

Each focused web view subscribes to the intersecting rows with a Transit query
`[:visualization/cue-pads page-id [left bottom] rows]`. Changing page, hiding
the window, navigating away or scrolling the grid out of view disposes the
remote reaction. The physical adapter obtains a separate lease for its eight
rows and relinquishes it for pad pickers, suspension and disconnect. Visible
cues shared by the two surfaces also share their compiled plan and keyframes.
The last reader releases those cached entries. Up to 64 distinct rectangles
and 512 distinct cue IDs may be observed concurrently. The periodic full and
path-diff messages use Studio's ordinary acknowledged push stream.

The show thread publishes its last measured frame cost to the visual governor.
With visible work, two successive frames above 85% of the frame budget double
the visual keyframe interval, up to sixteen times the base interval. Cheap
presentation interpolation continues. Persistent pressure at this floor
releases the live-effect adaptive scheduler. Thirty-two successive frames
below 65% restore one level. Histories can use `:visualization-clock
:sample-period-ms`, which ranges from 100 to 1,600 ms. This policy operates on
presentation workers and never writes to DMX or launches an effect.

## Verification

Run `lein test tolglow.visualization.cue-pads-test` for shared-cache lifecycle,
interpolation, isolation, failure fallback, pressure policy and captured MIDI
assertions. To exercise actual Studio subscriptions from Chromium, build
`npm run studio:build`, start `lein run -m tolglow.qa.cue-pad-preview PORT_FILE
STOP_FILE`, then run `node dev/studio/cue-pad-preview-qa.mjs
http://127.0.0.1:PORT OUTPUT_DIRECTORY`. Create the stop file to release the QA
server. The browser check records an inactive moving page, switches pages and
verifies the outgoing unsubscribe when navigating away.

To replay the Push 2 session, run
`lein run -m tolglow.qa.push-pad-feedback /tmp/push-pad-feedback.edn`.
The report records each outgoing palette write and counts the acknowledgements
and grid projections. A four-pass comparison with 64 independently phased
idle cues produced 3,598 identical palette writes in each measured 80-frame
window. Reusing the projection reduced calls from 566 to 80 and average
device-session CPU from 7.43–8.75 ms to 4.65–4.74 ms per committed frame.
The emulator acknowledges each batch immediately. Hardware latency and USB
scheduling need measurements on a connected Push 2.
