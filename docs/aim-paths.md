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
`POST /api/aim-path` or the same `:aim-path` operation from a running REPL.
Each selected fixture needs measured positive pan and tilt rates under its
patched `:calibration :motor-rate-deg-per-sec`, for example
`{:pan 90 :tilt 45}`. Supply two reachable world-space points. The move uses
an open `:once` traversal with `:linear` easing. An optional `:duration {:ms n}`
sets a lower bound on the travel time.

```clojure
(tolglow.show/submit! runtime :aim-path
                      {:id :paired-arrival :sync-arrival? true
                       :targets [:head-a :head-b]
                       :points [[-1 0 0] [1 0 0]]})
```

Publication solves both endpoints with each fixture's mount and calibration.
It chooses the destination pan wrap relative to the starting pose and takes
the maximum of mechanical angular travel divided by calibrated rate across
all heads and both axes. Pan and tilt then interpolate linearly in their
mechanical coordinates for that common interval. The acknowledgement reports
`:arrival-duration-ms` and the effect keeps the endpoint and timing data in
ordinary saved show state.

Preposition the heads at the first-point pose before launching. Launch the
published cue normally from the Studio cue grid or REPL with
`{:op :cue/launch :cue-id :paired-arrival}`. The cue traverses once and holds the
endpoint until ended. Republish after changing a selected fixture's mount or
calibration. Running instances keep their launch version. The 3D drawing
editor handles musical world-space paths. Calibrated paired arrival is
authored through the host operation and launched from the same cue grid.

The timing describes scheduled pan/tilt commands. Physical arrival depends on
the heads' acceleration, internal position control and DMX delivery, and
requires a hardware measurement. Fixture-specific speed channels need their
own calibration.
