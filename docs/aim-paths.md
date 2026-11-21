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
