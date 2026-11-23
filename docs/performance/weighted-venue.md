# Weighted cue volumes on a dense venue

These runs launch ordinary grid cues on a rig of 10,000 or 20,000 RGBW pixels
and 64 differently mounted moving heads. Each sphere moves at every tick,
paints its own color and fades across a 1.5 m smoothstep band. The four-field
case measures real overlap. In beam mode a fixed pixel can illuminate a field
even if its lens is outside the sphere. The moving heads also change aim.
The 24 × 9 × 24 m room includes receiver walls, a floor and the stage.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m \
  tolglow.perf.weighted-venue 10000 3 5 all /tmp/weighted-10000.edn
```

Run again with `20000` for the larger rig. The positional arguments give pixel
count, warmup frames, measured frames, scope and an optional output file. The
available scopes are `position` for the indexed field calculation, `optics`
for the isolated optical calculation against committed light output,
`host-position`, `host-beam`, `host` for both host cases and `all` for the
position calculation and both host cases. The continuous performance workflow
retains both dense reports whenever the paired checkouts include volume cues.

Each host case boots the show, spawns the cues and changes their positions
through the command queue. The scalar reference renders rich fixture values
and captures direct ArtDMX. The typed candidate publishes independently with a
25 ms physical clock. Every reference tick checks the entire model, intents,
all universe bytes, and compact previews for 16 pixels and all moving heads.
An analytic full-rig sphere scan checks projected memberships. Beam-only
memberships are counted separately for fixed pixels and movers. A fresh
candidate then replays the timed series against the captured packets, intents
and previews. Scalar work stays outside the timed series.

The geometry assay compares every signed-distance weight to an independent
smoothstep sphere calculation within `1e-10`. Geometry times and allocations
are for the calling thread. Host allocations cover the coordinator and warmed
workers. The measured series excludes network output, a real device,
independent clock wakeup jitter and the browser.
