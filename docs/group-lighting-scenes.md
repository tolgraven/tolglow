# Change a group to a measured light level

In Studio’s group scene control, choose a patch group or make a named selection,
set the visible-output target and apply it. **Vary** asks for a different look at
the same target. **Release** removes the group’s owned instance. Committed output
and target are shown separately. A target of 72% means 72% of that group’s
modeled full visible output, weighted by each fixture’s calibration or rated
emission. The quantity and uncertainty are described in [Lighting capacity](lighting-capacity.md).

For “something different at 72% on the washes”, chat can call
`set_lighting_scene` with the following arguments. The Ring route
`POST /api/lighting-scene` and headless show owner accept the same action data.

```clojure
(show/submit! runtime :lighting-scene {:op :vary :group-id :wash :target 0.72})
(get-in (show/snapshot runtime) [:lighting-scenes :groups])
```

A first request can use an existing group in the declarative fixture patch or
provide explicit fixture IDs. Valid fog and utility targets are ignored in a
mixed group. Unknown IDs, an all-utility selection and overlap with another
controlled group reject the entire request. Independently control disjoint
groups. Manual cues can overlap any group and keep their usual priority.

```clojure
(show/submit! runtime :lighting-scene
  {:op :apply :group-id :front :fixture-ids [:wash-left :wash-right]
   :target 0.45 :effect-ids [:ember :orchid] :params {:level 0.85}
   :allow-generated? false})
(show/submit! runtime :lighting-scene {:op :release :group-id :front})
```

Tolglow borrows compatible emission programs from the registry, intersects the
original effect and output target lists with the requested group, and launches
a group-owned low-priority cue. Movement-only effects, utility and show-control
cues are excluded. Vary cycles permitted sources and selects eligible parameter
values using their normal recommended ranges and randomization policy.
Explicit parameter values win. Setting `:effect-ids []` selects generated
emission only. When a saturated source cannot reach the requested output,
`:allow-generated? true` permits a generated fill using the actual emitters
available on those fixtures. Its tint varies with the generation. At full
capacity the fixture's full emitter mix leaves no headroom for tint changes.

Each committed frame supplies a new per-group measurement. The regulator
changes the opacity of its own instance by at most 0.2 per frame and settles
within 1.5 percentage points. Operator master, blackout, pause and manual cue
priority remain in effect. Slow or changing cues may keep a group seeking.
A fully lit source under a low master or a restricted color may show **Limited**
or **Unlit**. If a foreground manual look keeps the group above target even with
the owned scene at zero, the group reports **Over target**. Release only stops
the group’s owned instance. A registered subscriber can create a fresh look on
its next matching event. A show backup preserves the target, source and
owned instance, with instance identity rebound on restore.

## Drive groups from music or code

To follow a normalized show variable continuously, supply its path relative to
the model’s `:variables`. The existing audio analyzer provides a smoothed RMS
envelope in that map. A fixed `:target` in a later request disconnects the
previous variable binding.

```clojure
(show/submit! runtime :lighting-scene
  {:op :apply :group-id :wash :target-var [:audio :features :energy-envelope]
   :target 0.15})
```

To select a new look each time a track gets loud, register this subscriber
before starting the show’s audio capture. The analyzer’s actual emitted
`:audio/energy` event carries the RMS envelope at `[:data :energy-envelope]`.
A crossing above 0.20 requests a different look at 72%. A sustained loud
passage keeps the same look. A subsequent measurement at or below 0.08 rearms
the subscriber for the next rise. No group must be running beforehand.

```clojure
(show/submit! runtime :event-subscriber
  {:op :save
   :subscriber
   {:id :wash-on-energy-rise
    :match {:type :audio/energy}
    :actions [{:op :lighting/scene
               :gate {:path [:data :energy-envelope] :above 0.20 :below 0.08}
               :scene {:op :vary :group-id :wash :target 0.72}}]}})
```

Any registered event can supply the target itself using
`:event-path [:data :intensity]` on the action. In both cases the emitter,
measurement, gate and scene run through the same show owner as Studio.

## Steady-frame metering cost

Group feedback reads the committed bytes of its selected fixtures. A cached
reading remains valid only while the patch, group membership and all affected
universe bytes match. A changed fine byte forces a new measurement. Groups on
unrelated universes can retain their last reading. A steady target preserves the
model and instance identity. The controller still observes changed target
variables and operator settings on each frame.

Reproduce the software comparison from the project root.

```sh
lein run -m tolglow.bench-group-feedback 2048 60
```

Two warmed preview owners ran the same generated look on 2,048 RGB heads at a
72% target. One owner kept the group feedback enabled. The other continued the
same owned cue at its settled opacity. Sampling order alternated and the final
DMX universes had to match byte-for-byte. On the shared JDK 21 host, the
difference between the two median owner-thread CPU readings fell from 0.39 ms
to 0.10 ms after reusing unchanged measurements and publishing the observed show
variables. The corresponding median allocation difference fell from 8,192 to
2,952 bytes per frame. With reuse, wall p50/p95 were 4.06/26.10 ms with feedback
and 3.77/14.64 ms with the same look
alone. The benchmark includes the complete software tick, runs unpaced, and
reports CPU/allocation for the owner thread. It does not measure physical
output or universal 40 Hz delivery. A first scene still compiles the generated
effect and prepares its calibrated meter.

## Use measured light as a show signal

The accepted frame publishes `{:actual 0.713 :target 0.72 :status :at-target}`
under `[:variables :lighting/groups group-id]`. With rig metering enabled,
`[:variables :lighting/rig]` holds the rig’s actual fraction, target, follow mode
and status. These are ordinary show variables. Use
`[:lighting/groups :wash :actual]` as another group’s `:target-var` to mirror
the wash group’s measured energy. Releasing the wash removes its measurement.
Disabling the rig meter removes its variable.

Group state changes emit `:lighting/group-seeking`, `:lighting/group-at-target`,
`:lighting/group-limited` or `:lighting/group-released`. Limited events include
the specific status, such as `:over-target`. Rig changes emit
`:lighting/rig-level` when the rounded percentage, mode or status changes.
Continuous values remain available in the variable at frame precision. The
rig emits `:lighting/rig-unavailable` when metering stops.

This subscriber starts a look in another fixture group when the wash reaches
its own requested level. A steady wash at 72% triggers once.

```clojure
{:id :when-wash-settles
 :match {:type :lighting/group-at-target :emitter-id :wash}
 :actions [{:op :lighting/scene
            :scene {:op :vary :group-id :front :target 0.4}}]}
```
