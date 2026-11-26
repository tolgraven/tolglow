# Drawn moving-head aim paths

World-space path publication uses the same pure tween compiler and cue lifecycle
as authored effects. `POST /api/aim-path` accepts `id`, optional `name`, `cue-id`
and `page-id`, `targets` (patched moving-head IDs), and 2–256 finite XYZ `points`
in metres. Every target must have pan and tilt channels. The API accepts the
normal `runtime-id` and `expected-epoch` admission guards.

Use `duration: {beats: 4}`, `{bars: 1}`, or `{phrases: 1}`; an explicit millisecond
duration is also supported. The default is one bar. `mode` is `once`, `loop` or
`ping-pong`; `curve` uses the shared tween easing names. `closed?` optionally
joins the final point back to the first. Keyframes are spaced by chord distance,
so a linear curve travels at constant speed along each straight segment.
This is a piecewise linear path, not an inferred spline.

Publication validates and compiles the effect, publishes its next version and
creates/updates its cue atomically. It never launches the cue. The acknowledgement
includes `effect-id`, `cue-id`, `version`, and `page-full?` when no empty cell was
available. An existing cue placement is preserved. Running uses the ordinary
`cue/launch` command; choose `launch: {mode: one-shot}` for a single iteration.
Loop and ping-pong paths remain active until ended. They affect aiming only;
emission continues to be controlled by other effects.

An existing path can be updated with `expected-version`. Stale edits and IDs
belonging to unrelated effects/cues reject without partial publication. The cue
uses `preserve-state` reload policy. Musical clocks and captured meter are owned
by the runtime, including pause, release and tempo changes. Editor points,
settings and effect/cue versions are ordinary registry data preserved in full
show files. Runtime path evaluation does not depend on an open browser.

The 3D editor is a separately tracked integration. Tests of this API alone do
not establish touch interaction, visual quality or physical aiming accuracy.

The [3D editor guide](rig-editor.md) covers fixture placement, direct path drawing,
explicit cue creation/launch and reopening paths from saved shows.

## Calibrated paired arrival

Publish an opt-in point-to-point move with `:sync-arrival? true` through
`POST /api/aim-path`, the same `:aim-path` operation from a running REPL, or
**Rig & space → Draw path → Arrive together using calibrated motor rates** in
Studio. Each selected fixture needs measured positive pan and tilt rates under
its patched `:calibration :motor-rate-deg-per-sec`, for example
`{:pan 90 :tilt 45}`. Supply two reachable world-space points; repeated points
collapse first. The move uses an open `:once` traversal with `:linear` easing.
An optional `:duration` is a lower bound in beats, bars, phrases or explicit
milliseconds (default `{:beats 0}`).

```clojure
(tolglow.show/submit! runtime :aim-path
                      {:id :paired-arrival :sync-arrival? true
                       :targets [:head-a :head-b]
                       :points [[-1 0 0] [1 0 0]]
                       :duration {:bars 2}})
```

Publication solves both endpoints with each fixture's mount and calibration.
The start pan wrap follows the head's last committed pose when the runtime
has one, and the destination wrap follows the start. The physical minimum is
the maximum of mechanical angular travel divided by calibrated rate across all
heads and both axes; a move in which no head travels is refused. Pan and tilt
interpolate linearly in their mechanical coordinates for one common interval:

- When a musical minimum, estimated through the show transport's tempo and
  meter, is at least the physical minimum, the keyframes stay musical. A
  launched move then holds its launch-tempo interval (see below).
- Otherwise the keyframes use the physical interval in milliseconds, the
  calibrated rates being a device boundary.

The acknowledgement and Studio report `:arrival-duration-ms` at the current
tempo. The effect's `:duration` is the solved interval, so timed and one-shot
slots work. Editor metadata keeps the requested minimum as `:duration`, the
solved interval as `:arrival-duration`, the physical `:minimum-travel-ms` and a
`:rig-fingerprint` of each head's position, rotation and calibration.
Republishing saved editor settings solves everything again.

Preposition the heads at the first-point pose, then publish or republish, then
launch the cue from the Studio cue grid or REPL with
`{:op :cue/launch :cue-id :paired-arrival}`. Studio's **Create + run** launches
it persistently. The cue traverses once and holds the endpoint until ended.
Running instances keep their launch version. A launch is refused, with the
committed state unchanged, when:

- `:stale-calibration`: a selected head's position, rotation or calibration
  no longer matches the fingerprint. Repatching addresses does not count.
- `:tempo-exceeds-motor-rate`: a musical arrival would now be faster than the
  calibrated rates at the current tempo.
- `:start-wrap-mismatch`: a head's committed pan is nearer another in-range
  wrap of the start pose than the published one, so frame 0 would command a
  full-turn jump. `tick!` binds the last committed frame for its owner cycle
  only, and launches resolve before the next frame is rendered. Outside an owner
  cycle there is no committed frame, so only this check is skipped.

The checks run in `tolglow.spatial/apply-command`, which every Tolglow cue or
effect launch reaches: Studio, REPL, Push and HTTP commands, quantized grid
launches when they fire, macro and sequence slots, and event-subscriber
actions. A refused slot is appended to `:composition-faults` and the run keeps
playing. A refused subscriber action becomes a handler fault. Lighting scenes
launch only the generated cues they own, which are never synchronized paths.

### Tempo changes during a musical move

A launched musical move holds the interval its arrival took at launch, so the
heads keep the pace the launch check admitted. When `apply-command` accepts the
launch it records the arrival in beats (through the instance's captured meter)
and in milliseconds (at that launch's transport tempo) as the instance state
`:tolglow.compiler/launch-tempo`, for example `{:beats 8 :ms 4000.0}` for two
bars at 120 BPM. While the transport beat matches that tempo the move samples
the beat clock exactly as before, byte for byte. Once the beat departs from it,
the move samples elapsed milliseconds over the held interval instead:

- A tempo rise neither speeds the heads up nor ends the move early. Raising
  120 BPM to 480 BPM halfway keeps the slow head at its 11.25°/s launch pace
  (calibrated 22.5°/s) and arrival at 4 s; before the hold it commanded
  45°/s and arrived at 2.5 s.
- A tempo fall does not stretch the move; it still arrives at 4 s.
- A meter change has no effect, because the meter is captured at launch.
- Stopping the transport no longer freezes the move: it continues in
  milliseconds and arrives on schedule. A stateless sample cannot freeze and
  later resume without a jump.

Limits: a timed or one-shot slot keeps its own musical duration, so after a
tempo rise the slot can end before the held move arrives. A replayed command
id launches nothing and keeps the original hold; a new launch holds its own
tempo. Millisecond arrivals and ordinary aim paths have no hold. Studio's
instance view does not show the hold; read it from the REPL at
`[:model :instances <id> :state :tolglow.compiler/launch-tempo]`.

The timing describes scheduled pan/tilt commands. Physical arrival depends on the heads'
acceleration, internal position control and DMX delivery, and requires a
hardware measurement. Fixture-specific speed channels need their own
calibration.
