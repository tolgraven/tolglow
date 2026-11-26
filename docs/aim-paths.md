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

## When a positioning cue ends

When the last effect writing a moving head's pan or tilt ends, the head holds
its last committed pose. It moves again only when another effect writes that
axis. A one-shot aim path, a released loop or a direct pan/tilt cue therefore
leaves the head where it stopped instead of commanding pan and tilt 0 on the
next frame. The reason is physical: a head sent back to 0 usually has to
travel further when the next cue takes it, so its real motion is less likely
to match what the show commands. Holding keeps it close to where the next
command expects it. Motion is never clamped or rate-limited for the hold.

The frame owner implements this in `tolglow.show.position-hold`. Each frame
it finds the patched heads whose last committed pan or tilt is above 0 and
that no contribution writes this frame through `:pan`, `:tilt`, `:aim` or
`:direction` at nonzero opacity. It places one bottom contribution block that
writes those normalized values, so Afterglow packs them through the normal
coarse/fine, inverted and cross-universe path, and rich and compact history
carry the held pose into the next aim's wrap choice. Each axis is held
separately. A writer at any nonzero opacity takes its axis over and composes
against the packed default as before, so a partial-opacity position cue fades
toward pan/tilt 0 exactly as on earlier releases; a running cue at opacity 0
writes nothing, so the head holds. Edge-tracking followers are positioned by
the renderer and are not held. A head that was never moved, and any show in
which position writers keep running, renders byte-identical packets.

Other channels keep their release behaviour: dimmer, colour and other
emitters return to their idle values when their cue ends, and blackout still
darkens emission without moving heads. Stopping the show still sends the idle
shutdown packet, in which pan and tilt are 0. A fixture removed from the rig
is no longer rendered and its slots are released as before. `:show/restart`
clears cues but does not command heads to move: they hold until a new cue
writes them. There is no separate home command yet; launch a cue that writes
the home pose. `tolglow.position-hold-test` checks all of this on captured
packets ([#690](https://github.com/tolgraven/tolglow/issues/690)).

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
  meter, is at least the physical minimum, the keyframes stay musical and
  follow the transport like other aim paths.
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
Running instances keep their launch version.

Tempo never refuses a launch. When the tempo has risen since publication so a
musical arrival would now be shorter than the physical minimum, the launch
stretches the move to that minimum and the heads still arrive together. The
launched instance runs a private copy of the published move with the same
musical unit and a longer span, for example `{:beats 5}` launched at 170 BPM
becomes `{:beats 5.67}`, which lasts the 2 s minimum at that tempo. The move stays
on the beat clock, so it pauses on transport stop and resumes from where it
was. A one-shot launch lives until the stretched arrival unless the cue or
launch sets its own duration. The published definition is unchanged, and a
show saved during the move restores the stretched instance. The
launch acknowledgement and the instance carry `:arrival-stretch`, for example
`{:musical-duration {:beats 5} :duration {:beats 5.67} :musical-ms 1764.7
:duration-ms 2000.0 :stretched-by-ms 235.3 :bpm 170}`. A launch that needs no
stretch runs the published move unchanged and has no `:arrival-stretch`.

A launch is refused, with the
committed state unchanged, when:

- `:stale-calibration`: a selected head's position, rotation or calibration
  no longer matches the fingerprint. Repatching addresses does not count.
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

A tempo change while the move is running is not rechecked: the move follows
the beat clock, and commanding a head faster than it can follow is a valid
effect, not an error. The timing
describes scheduled pan/tilt commands. Physical arrival depends on the heads'
acceleration, internal position control and DMX delivery, and requires a
hardware measurement. Fixture-specific speed channels need their own
calibration.
