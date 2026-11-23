# Musical effect keyframes

In the Studio effect builder, select **Sample at a fixed interval**, enter an interval and choose its unit. Use a fraction of a beat for short motion, or bar and phrase intervals for longer sweeps. The easing picker includes a two-handle Bézier editor, a damped spring and stepped or smooth curves. Continuous effects can also declare the same settings in the registry.

```clojure
{:id :beat-wash
 :version 1
 :name "Beat wash"
 :params {:travel {:type :number :bounds [0.0 1.0] :default 0.0}}
 :keyframes {:interval {:beats 1/2}
             :curve {:type :cubic-bezier :points [0.32 -0.1 0.65 1.1]}
             :progress {:mode :replace :param :travel}}
 :outputs {:color [0.8 0.12 0.04]
           :dimmer {:op :fract :args [{:op :multiply :args [0.25 {:op :beat}]}]}}}
```

Choose **Limit automatic slowdown** in the builder or an active cue's keyframe inspector to enter the longest permitted automatic sample interval. Leave fixed sampling off to run the effect at the full show rate whenever the governor has sufficient headroom. For example, a wash with `:keyframes {:max-interval {:beats 1/8}}` permits a two-frame interval at 120 BPM and 40 Hz. A tempo change to 240 BPM restores direct evaluation on the frame that receives the command. Other effects in the same compound cue keep their individual cadence.

A fixed interval and an automatic maximum can be enabled independently. The fixed interval always applies, even if deliberately longer than the automatic limit. The automatic maximum governs only additional slowdown. Both controls accept fractions of beats, bars or phrases, as well as explicit milliseconds.

The effect computes its endpoints using the actual and future instance clocks. A live parameter, bass energy from an audio-return mapping or a show variable can then move between those endpoints at every show frame. Set the progress source to elapsed time for regular tweens, replace it for scrubbing in either direction, or offset it to nudge the existing motion. The effect can start its next segment from an intermediate value when the source never reached one.

Expand the active cue's keyframe inspector and select a head and attribute to compare origin, target and displayed writes. The corresponding reactive query is `[:runtime/keyframe instance-id :head-1 :dimmer]`. Subscribing with only the instance ID returns the timing and progress. Full lighting fields stay on the show host.

A Transit show save includes the active endpoints and current progress. This also works for typed batch effects. Loading assigns fresh instance IDs and reconnects their captured windows. A discrete effect such as a shutter program runs each live frame. A failed future prediction leaves real-frame evaluation active for the rest of that segment.

See [Afterglow's scheduling and native output contract](https://github.com/tolgraven/afterglow/blob/9387c0ae211397734a761ac98c4894cbfeeb83e4/doc/architecture/effect-keyframes.md).

## Automatic sampling of timed edges

The cost-ranked scheduler leaves clock-driven floor and fractional wraps and
stepped output tweens at source rate. Sampling on opposite sides of a wrap can
miss a short dimmer pulse entirely or produce a flash on a dark frame. An
independent floor of a fixed fixture coordinate leaves a smooth time-varying
term eligible. This conservative check applies to portable output expressions.
The timing of opaque external generators cannot be inferred from this syntax.
Explicitly authored `:keyframes` remain available for deliberately stepped or
interpolated looks.

The host inspects each captured live definition revision as it enters the
active set and reuses that result across instances. A stable active/version set
does no further expression traversal. Removed and hot-reloaded revisions leave
the cache with their final instance. Real worker costs are still measured and
reported for protected effects.

### Reproduce the transient and recovery test

```sh
lein run -m tolglow.perf.transient-scheduling target/quality/transients.edn
```

The replay records 320 frames of real flash and wash DMX from the normal show
tick. It injects identical measurements for worker costs and frame lateness in
each test lane. Frames 17 through 112 report a 32 ms tick against a 25 ms budget.
The cost-only comparison changes flash bytes on 80 frames. The protected lane
matches the full-rate reference on all 320 frames. The smooth wash reaches a
stride of eight frames and returns to one during recovery. Only two definitions
are inspected over the entire run. This test isolates scheduling decisions. The
large-rig benchmark measures actual throughput and output error separately.


The release browser replay edits the live cue on an 834 px touch viewport,
publishes a cap-only revision, and reopens the running inspector. It checks
the stored beat fraction and that the fixed-interval field stays disabled.

```sh
npm run studio:build
lein run -m tolglow.qa.studio-browser keyframe-ceilings target/keyframe-ceilings
```
