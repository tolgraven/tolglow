# Studio composition bridge

`tolglow.composition` keeps the existing pure host API:

```clojure
(save model definition)
(start model composition-id time-ms)
(advance model time-ms)
(stop model composition-id time-ms)
```

The host owns time and updates `model[:transport]` before calling these functions.
Beat sequences read `:beat`, `:bpm`, `:playing?`, and `:meter` directly. Never reconstruct
beat position by multiplying elapsed time by the current tempo: that loses tempo
history. The host should integrate each elapsed interval using its preceding
transport settings, then apply new transport commands for the next interval.

New sequence slots default to one beat. Musical definitions use duration maps:

```clojure
{:id :phrase :mode :sequence :loop :ping-pong
 :slots [{:cue-id :red :duration {:beats 1/2}}
         {:cue-id :blue :duration {:bars 2}}
         {:cue-id :spot :duration {:phrases 1}}]}
```

EDN ratios and JSON decimals express fractional beats. The launch captures meter
(`{:beats-per-bar 4 :bars-per-phrase 4}` by default), so later meter changes do not
reinterpret an active sequence. BPM changes alter remaining-time estimates but
never move a beat boundary. Every slot in a sequence uses one clock domain.
Explicit physical-time sequences use milliseconds:

```clojure
{:id :pulse :mode :sequence :loop :loop
 :slots [{:cue-id :red :duration {:ms 500}}
         {:cue-id :blue :duration {:ms 500}}]}
```

The direct `:duration-ms` and `:duration-beats` fields remain supported. An explicit
`:timebase :ms` uses a 1000ms slot default. Mixing duration representations within
one slot or clock domains within one sequence is rejected. Save validates known cue/effect references, unique
slot IDs, finite positive durations, parameter overrides, and launch options.
Macro slots do not gain an implicit duration or playback clock.

Both sequence timebases freeze while transport is paused. Millisecond runs retain
an explicit accumulated logical time, excluding intervals observed as paused;
beat runs follow the host's frozen beat position. Resuming does not replay elapsed
wall-clock steps. A sequence started while paused waits to launch its first slot.
Macros remain explicit immediate actions, independent of transport playback.

Each running composition in `:composition-runs` has `:playback` with `:kind`,
`:timebase`, `:playing?`, `:started?`, `:slot-count`, `:index`, `:slot-id`,
`:cue-id`, `:phase`, `:remaining-ms`, `:remaining-beats`, and `:done?` as applicable.
Milliseconds remaining for beat sequences are an estimate using the current BPM;
they do not control scheduling. The host may expose this projection in the web
snapshot. Completed once-only sequences are removed from the running map.

Launch tokens map to exact runtime instance IDs. Stopping one composition releases
only those instances, allowing their configured release tails to complete. Human
held/persistent cues, even using the same cue ID, remain untouched. Repeated stops
are no-ops. Show epochs discard stale executions and tokens without sending end
commands into the new show. Saving an edited definition leaves the active run's
captured definition intact; restart explicitly to adopt it.

`tolglow.composition-test` covers normalization, validation, tempo changes,
both pause modes, paused starts, owned release, restart isolation, and editing
while running using the actual control model and effect runtime.

The same shared timing core (`afterglow.time`, exposed to authoring as
`tolglow.composition.time`) supports effect/cue `:duration`, auto `:interval`,
envelope `:attack`/`:decay`/`:release`, and tween keyframe `:at` maps. Envelope
positive stages share one clock domain; omitted stages have zero duration.
Response state captures `:started-at-beat`, `:timing-meter`, and optionally
`:released-at-beat`. It transforms the per-frame parameter copy, preserving
the underlying show values. No timing function reads a wall clock or starts IO.
