# Clock domains, loops and seeks

`tolglow.clock/rebase` handles an explicit change of musical clock origin while
preserving progress through running local effects. A Live loop, seek, or switch
of clock source must not strand a one-shot, rewind an attack, consume an enormous
random backlog, or freeze a sequence behind its monotonic-beat guard.

The helper is pure. It transforms the model's known musical anchors and returns
a new model; it does not restart effects, launch/release cues, read a clock,
consume RNG, send output, or mutate the input. The host detects a source change,
source-epoch change or backward beat, applies the helper once, then performs its
normal composition/modulation/engine tick.

```clojure
(require '[tolglow.clock :as clock])

(clock/rebase show
  {:beat 1.0 :time-ms 42000.0 :bpm 128.0 :playing? true
   :meter {:beats-per-bar 4 :bars-per-phrase 4}
   :source :ableton :source-epoch 3})
```

Only `:beat` is required. The old coordinate defaults to
`[:transport :beat]`; `:from-beat` can explicitly identify an earlier captured
origin when a boundary has already obtained another snapshot. The shift is:

```text
offset = new external beat - old external beat
new local anchor = old local anchor + offset
```

For an effect started at beat 8 and currently at beat 9, seeking to beat 1 moves
its start to beat 0. Its elapsed progress stays one beat. Seeking to beat 0 moves
that historical start to -1; negative historical anchors are valid and necessary.

## Three meanings of time

| Domain | Meaning after a musical seek |
| --- | --- |
| Absolute musical position | Follows the newly captured transport beat. Explicit absolute beat/bar/phrase oscillators intentionally move to the new position. |
| Local musical progress | Start/release/window anchors shift with the origin. Elapsed oscillators, envelopes, one-shots, sequence slots, modulation glides and scheduled musical intervals preserve their progress. |
| Host milliseconds | Remain monotonic and keep their existing origin. Millisecond envelopes, deadlines, observation age and pause offsets do not jump with the song position. |

The compiler's default `:beat`/`:bar`/`:phrase` clock is elapsed. The explicit
`:clock :absolute` variant reads global transport position. Rebasing changes
their inputs according to these separate meanings; it does not rewrite their
expression definitions or reinterpret declared durations.

A supplied `:time-ms` is a host capture timestamp, not the song's elapsed
milliseconds. It may advance transport metadata but must not precede the model
or existing transport wall time. The normal engine tick advances model
`:time-ms`. BPM must remain within the shared `[1,1000]` domain; meter and play
state are validated before the new model is returned. Optional source/source-epoch
metadata is retained in transport for host discontinuity detection.

## Covered state

The transformation visits explicit owned fields, not arbitrary recursively
matching keys:

| Subsystem | Musical fields shifted |
| --- | --- |
| Engine instances | `:started-at-beat`, `:release-at-beat`; `:started-at` in normalized duration/release timers only when their `:duration :unit` is `:beats`. |
| Engine bookkeeping | `:last-frame-beat`, preserving the relationship to the last successful frame. |
| Instance system controls | `:system :last-clock :beat`, `:paused-clock :beat`, and beat-based `:gate :timer` anchors. |
| Response envelopes | State `:started-at-beat` and `:released-at-beat`. |
| Beat sequences | State start/last beats, start/last positions, and current step start/end beats. |
| Auto planner | Beat interval timer anchor and `:next-at-beat`. |
| Parameter modulation | Anchor/next beat, interpolation boundaries, finite human-hold deadlines and last beat, through `modulation/rebase-clock`. |
| Signal routing | The `:routing :state` clock/observation/envelope musical anchors, through `integrations.patch/rebase-clock`; queued `:pending` observations and learned `:ports` clock beats shift by the same offset. |

Paused effect clock offsets are elapsed quantities, so `:clock-offset-ms` and
`:clock-offset-beats` are preserved. Private system clocks share the global
musical origin and therefore shift. Opaque `:last-input-state` remains untouched.
Millisecond sequence state and its pause-aware logical clock are also unchanged.

The helper retains instance IDs, show epoch, registry and captured definitions,
canonical parameters, arbitrary effect state, seeds, sequence shuffle bags,
visit counts, active tokens, command receipts and controller priority. Duration
lengths and captured meters do not change. Advancing the rebased model, rather
than rebasing itself, performs any next due transition.

Signal routing receives the **same shared offset**, even if its last sampled
clock lagged transport. Current routing clock may move to the new capture while
historical observations acquire negative beat anchors. Their millisecond
timestamps and physical `:observed-at-ms` metadata retain their original age.
The routing boundary's existing current-clock constraints continue to apply.

## Host integration and recovery

Normal increasing beat samples use ordinary advancement. Calling `rebase` on
every frame would freeze local musical progress by moving its anchors every time.
The host records selected source and source epoch in model transport so switching
sources or handling a forward seek can be distinguished from normal progression.
This is wired into `studio/source-clock-model`: it rebases the original model
before merging a discontinuous captured clock, once per source/epoch/backwards
transition. A selected external source that has not supplied a clock pauses local
musical progress until its first capture arrives.

A backward beat with the same source is also a discontinuity. A forward seek
needs the source to change its epoch or explicitly signal that event; a large
number alone cannot tell whether time genuinely elapsed. The host should invoke
rebasing before composition, modulation, routing and engine advancement so none
sees a mixture of old anchors and new global beat.

An ordinary delayed frame still follows each subsystem's bounded catch-up policy.
For example, modulation preserves every missed random sample within its explicit
budget and reports structured failure above it. A known source seek instead uses
rebasing and preserves the RNG position, without pretending thousands of samples
ran during a transport jump.

The helper returns no partial model when validation fails. Host publication
remains an owned transaction; it must not commit only some shifted subsystems.
This is a musical-origin operation, not a reset of the host wall clock. Restarting
a show and importing a saved show retain their separate epoch/identity policies.

## Saved state and custom effects

The normal schema-2 freeze/read/thaw path accepts negative historical beat
coordinates. Restoring a show remaps active identities and retains local timing;
rebasing afterward moves that saved musical origin to the chosen external source.
The modulation restore helper preserves RNG, schedule and holds through ID
remapping; clock rebasing preserves them through the musical-origin change.

Custom effect state is opaque. A map named `:beat` inside an effect's `:state`, a
waveform definition, a historical event or registry metadata is not automatically
a clock anchor. Effects using the supplied elapsed clock need no extra handling.
A custom effect or future subsystem that owns absolute musical deadlines must
declare its own rebasing contract and be included at this explicit boundary.
There is no speculative recursive rewrite of user data.

## Verification

`tolglow.clock-test` exercises real engine/compiler one-shots, attack/release
continuity, explicit absolute versus elapsed oscillators, response envelopes,
beat and millisecond sequence executions, auto-planner deadlines, routing
envelopes, private pause clocks, and an actual show-file freeze/read/thaw with
modulation. Assertions check retained IDs/state/RNG, exact musical boundaries,
unchanged wall-time anchors and unchanged visible values where continuity is
required. No physical lighting or external Live session is involved.

Source-change detection and adapter timestamp mapping have separate host and
transport tests. Passing this helper's tests establishes the modeled domain
transformation; it does not establish every external device's clock quality.
The [routing host tests](routing-host.md) exercise actual MIDI song-position
source switches and repeated seeks alongside live one-shots and routing attacks.
