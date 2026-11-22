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

## Ask Studio chat for a different look

With a show provider configured, ask **“Something different at 72% on the left movers. Tell me what they actually reach.”** The show agent can call `inspect_lighting` to discover the patched group ID and its light-emitting members, then call `set_lighting_scene` with the discovered ID, `"op": "vary"` and `"target": 0.72`. A second inspection reports the current generation, target, measured fraction, applied gain, numeric effect inputs and controller status. A group marked `seeking` is still settling. A group marked `limited`, `unlit` or `over-target` has an unmet target. Studio’s master, blackout and independently authored cues still apply.

Inspection returns the musical energy envelope and its measurement time, the estimated mixed-room fog level with its reference distance and last presentation time, and the last stationary camera observation with its epoch and sample time. A missing measurement is reported as null. The groups and eligible fixtures have separate page cursors. Filter by group ID to inspect the members of a large rig.

For a continuously modulated look, ask to bind the group’s light target to the music energy envelope. The tool accepts `"target-var": ["audio", "features", "energy-envelope"]`; built-in fog paths are `["fog/level"]` for the modeled room level and `["fog/observed-level"]` for the last camera reading. An existing user-defined show variable whose value is in 0–1 can also supply the target. A fixed target disconnects that binding. These tools operate on the current show.

A one-off variation and a repeating event are separate controls. For example, **“Whenever audio energy rises above 50% after dropping below 20%, vary the left movers to 72%.”** Chat can save or remove that group's `configure_energy_scene` binding. It uses the same `:audio/energy` subscriber and crossing action shown above. Inspection reports its thresholds and target. The operator's configured audio source supplies the measured energy.
