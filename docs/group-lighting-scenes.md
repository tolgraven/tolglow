# Balance a lighting scene

| When you want to… | Use |
| --- | --- |
| Match the brightness of a wash and a smaller spot | The **global brightness reference** |
| Keep only a few heads bright, even as cues overlap | The **lighting budget** and **active-source limit** |
| Choose between a steady wash and a restless sparkle | The **visual activity** score |
| Change the show when the music gets louder | The **audio energy** signal |

## Match unlike fixtures

In Studio, select a fixture group and choose its brightness reference. A source’s **local** brightness is its share of its own full-white output. A group’s local target uses its combined capacity, so an effect can light its members unevenly. With **global** brightness, the brightest full-white source in the entire patch sets the scale. Two fixtures asked for the same global level can therefore emit approximately the same amount of visible light.

For example, if a wash produces twice the full-white light of a spot, the wash at half and the spot at full can match. A higher common target takes the spot to its maximum. The source readback shows its local brightness, global brightness, highest achievable global brightness and whether it is limited. Measured full-white calibration gives the best comparison. Lamp wattage and emitter type supply estimates for unmeasured fixtures. [Fixture calibration](relative-fixture-brightness.md)

Choose **Apply** for a first look or **Vary** for a different effect. Tolglow chooses an eligible emission effect for the group, varies allowed parameters within their recommended ranges, and measures the resulting output. A steady group target controls a low-priority scene instance. **Release** stops that instance. You can use patch groups or explicit fixture IDs. Simultaneously regulated scene groups must be disjoint. Manual cues keep their usual priorities.

```clojure
(require '[tolglow.show :as show])

(show/submit! runtime :lighting-scene
  {:op :vary :group-id :front :brightness-reference :global :target 0.35})
```

Use `:effect-ids` to limit the registry candidates, `:params` for explicit parameter values and `:allow-generated? false` to prevent generated fill. An empty `:effect-ids []` requests generated emission only. Read the results in `(:lighting-scenes (show/snapshot runtime))`. A source blocked by its own maximum, a saturated color or the operator master can leave a target unmet.

## Limit simultaneous light

Apply a lighting budget when a composition calls for a few bright sources and the rest at a dim standby level. A budget is a ceiling. A sparse effect can use less. The allocator considers the combined frame, including manual launches and overlapping cues. Its wattage estimate uses rated emitter capacity and requested light output. Use the estimate to balance a look. Electrical circuit loading requires a power measurement.

```clojure
(show/submit! runtime :settings
  {:scope :lighting-budget
   :values {:enabled? true
            :global {:limit 0.55 :max-active 4 :standby-level 0.025
                     :strategy :proportional}
            :groups {:front {:limit 0.45 :max-active 2 :strategy :priority
                             :fixture-order [:lead-spot :side-wash]}}}})
```

In this example, up to four sources can run above standby, including up to two in the front group. Three cues can address six of nine heads and still share that one limit. Auto mode considers the combined demand when it chooses a cue and can retire its own earlier cues to make room. Manual launches also pass through the final cap.

- **Which sources stay bright?** The allocator admits them in fixture order. Supply `:fixture-order` to put particular heads first. Among admitted sources, `:proportional` shares the remaining watts and `:priority` spends those watts on the first heads in order.
- **What happens to the others?** The default `:standby-mode :shed` preserves the tint of sources reduced to standby and leaves unused heads dark. Choose `:standby-mode :all` for a low-priority standby look across the scope. Authored black, zero master and blackout still take effect. If the standby look alone would exceed the watt budget, its level falls too.
- **What did the show actually use?** Read `(get-in (show/variables runtime) [:lighting/budget :global])` for the active and shed counts, requested watts and delivered watts. The same fields are available under `[:lighting/budget :groups group-id]`. A powered lamp behind a closed shutter consumes estimated watts and leaves its visible-source slot free.

## Shape visual activity

Enable `:lighting-activity` to read scores from the final lighting output. A sustained wash, a fast strobe and a moving sparkle can have similar average brightness and very different scores. Coverage matters. A single sparkling head contributes less to the whole scene than a bank of heads flashing together. The readback includes brightness, coverage, flashing, movement and strobe components. Inspect `[:lighting/activity :scene]`, `[:lighting/activity :effects effect-id]` or `[:lighting/activity :groups group-id]` in `(show/variables runtime)`.

To soften the look, set response durations on the rig or a group. Attack smooths rises in emitted light. Release smooths falling levels, including the tail when a cue ends. Longer values also soften abrupt waveform edges in a running cue. After one duration, the output has moved about 63% toward a held input. The activity meter sees the smoothed result. A physical strobe channel continues to flash at its commanded rate. A longer attack delays how quickly the flashes reach full brightness. Change the shutter command to alter their rate.

```clojure
(show/submit! runtime :settings
  {:scope :output-response
   :values {:enabled? true
            :global {:attack {:beats 1/4} :release {:beats 1}}
            :groups {:front {:release {:bars 1}}}}})
```

Group stages override the corresponding global stage. In Studio, choose **Cue parameter** as the attack or release time source, then select the cue and its numeric parameter. For example, `{:beats {:cue-id :feel :param :attack :default 1/4}}` lets a device knob mapped to the running cue’s `:attack` parameter set attack in beats. The fallback applies while the cue is idle. Blackout and the operator’s zero master remain immediate.

Auto mode can use the visual score when choosing its next cue. For a restrained sequence, try an activity target near the low end. Auto ranks eligible cues by distance from that score and considers the shared output budget before launching. For an effect it has seen, it uses the observed peak for that effect revision. For a new declarative effect, it estimates activity from its outputs and targeted fixtures.

```clojure
(show/submit! runtime :auto
  {:enabled? true :interval {:bars 2} :count 1
   :activity-target 0.2 :scope {:cue-ids [:slow-pan :warm-wash :sparkle]}})
```

## Trigger a look from sound

`:audio/energy` names the normalized, smoothed RMS signal from the configured audio capture. The show variable uses that key, the patchbay source is `audio/energy`, and a subscriber to the `:audio/energy` event reads the same key in its event data. Silence measures zero and above-full-scale input saturates the control signal at one. The analyzer publishes the variable per measurement and throttles event delivery separately.

For continuous control, add `:target-var [:audio/energy]` to a lighting-scene request. For a new look on each loud passage, use a threshold with a lower rearm point. This example waits for audio energy to cross 0.4 and varies the front group once. A later reading at or below 0.15 rearms it.

```clojure
(show/submit! runtime :event-subscriber
  {:op :save
   :subscriber {:id :front-on-rise
                :match {:type :audio/energy}
                :actions [{:op :lighting/scene
                           :gate {:path [:data :audio/energy] :above 0.4 :below 0.15}
                           :scene {:op :vary :group-id :front :target 0.35}}]}})
```

A subscriber can also use the event value as the target with `:event-path [:data :audio/energy]`. To react to measured light, subscribe to `:lighting/group-at-target` with the group's `:emitter-id`. Steady output fires once on arrival. Other useful events include `:lighting/group-limited` and `:lighting/activity`. [Subscriber actions](event-subscribers.md)

## Ask Studio chat for a different look

Try **“Vary the front washes to a third of their brightness, then tell me what they reached.”** Chat discovers groups and checks measurements with `inspect_lighting`. It can choose a look with `set_lighting_scene`, limit simultaneous light with `set_lighting_budget`, or register a recurring audio-energy crossing with `configure_energy_scene`. **Vary** preserves a continuously bound target. Explicitly setting a new fixed target disconnects its variable binding.

For rig-wide utilization, read `:lighting/rig` in the show variables. The corresponding scene-group reading lives under `[:lighting/groups group-id]`. These readings and the activity score can be used from the REPL or mapped to another effect. [Rig capacity](lighting-capacity.md) · [Headless configuration](headless-modules.md) · [Group metering benchmark](performance/group-feedback.md)
