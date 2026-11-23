# Musical effect keyframes

In the Studio effect builder, enable keyframes, enter an interval and choose its unit. Use a fraction of a beat for short motion, or bar and phrase intervals for longer sweeps. The easing picker includes a two-handle Bézier editor, a damped spring and stepped or smooth curves. Continuous effects can also declare the same settings in the registry.

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

The effect computes its endpoints using the actual and future instance clocks. A live parameter, bass energy from an audio-return mapping or a show variable can then move between those endpoints at every show frame. Set the progress source to elapsed time for regular tweens, replace it for scrubbing in either direction, or offset it to nudge the existing motion. The effect can start its next segment from an intermediate value when the source never reached one.

Expand the active cue's keyframe inspector and select a head and attribute to compare origin, target and displayed writes. The corresponding reactive query is `[:runtime/keyframe instance-id :head-1 :dimmer]`. Subscribing with only the instance ID returns the timing and progress. Full lighting fields stay on the show host.

A Transit show save includes the active endpoints and current progress. This also works for typed batch effects. Loading assigns fresh instance IDs and reconnects their captured windows. A discrete effect such as a shutter program runs each live frame. A failed future prediction leaves real-frame evaluation active for the rest of that segment.

See [Afterglow's scheduling and native output contract](https://github.com/tolgraven/afterglow/blob/40ff676cfa686685dc7e955934db92b379238a63/doc/architecture/effect-keyframes.md).
