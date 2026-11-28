# Balance a lighting scene

| When you want to… | Use |
| --- | --- |
| Match the brightness of a wash and a smaller spot | The **global brightness reference** |
| Keep only a few heads bright, even as cues overlap | The **lighting budget** and **active-source limit** |
| Choose between a steady wash and a restless sparkle | The **visual activity** score |
| Change the show when the music gets louder | The **audio energy** signal |

## Let the dance floor set the wash

With the audience zones selected and armed in **Connections → Audience**, point a lighting group's target at the participant-weighted effort. One guest dancing at full effort and three at rest set the target to 0.25, even across different gateways. When everyone leaves, the crowd reading reaches zero. The group fades its own contribution to black. Other running lights can supply a house look.

```clojure
(require '[tolglow.show :as show])

(show/submit! runtime :lighting-scene
  {:op :apply :group-id :wash :effect-ids [] :target 0.0
   :target-var [:audience :levels :dance-effort]})
```

Use the name of a group in your patch. An empty effect list lets the scene generate its wash. Set `:brightness-reference :global` to match fixtures against the calibrated brightest source. Individual steps can trigger accents on top of this continuous level. [Enroll the audience gateways](nearby-audience.md)

## Match unlike fixtures

Select a group in Studio. **Local** brightness measures each source against its own full white. A local group target uses the group’s combined capacity, so an effect can light its members unevenly. **Global** brightness measures each source against the brightest full-white source in the entire patch.

If a wash produces twice the full-white light of a spot, the wash at half and the spot at full can match. Asking for more takes the spot to its ceiling. Studio shows both readings and how bright each source can get in its current color. Measure each fixture at full white for the best comparison. Unmeasured fixtures use wattage and emitter type. [Fixture calibration](relative-fixture-brightness.md)

Choose **Apply target**, **Something different** to vary the effect and its parameters, or **Release group** to stop the scene. Scene instances run at low priority. Each regulated group needs its own set of fixtures. Manual cues keep their usual priorities.

```clojure
(require '[tolglow.show :as show])

(show/submit! runtime :lighting-scene
  {:op :vary :group-id :front :brightness-reference :global :target 0.35})
```

Use `:fixture-ids` for explicit membership, `:effect-ids` to limit eligible effects and `:params` to set their inputs. An empty `:effect-ids []` requests generated light. Set `:allow-generated? false` to require a registered effect.

Read group results in `(:lighting-scenes (show/snapshot runtime))`. Studio lists up to 24 sources. Use `(show/source-brightness runtime [:wash :spot])` for any selected sources from one committed frame.

## Limit simultaneous light

The budget limits combined emitter drive across all cues. A sparse effect can use less. Estimated watts describe the look. Electrical circuit loading requires a power measurement.

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
- **What happens to the others?** `:standby-level` sets a fraction of emitter drive. The default `:standby-mode :shed` preserves the tint of sources reduced to standby and leaves unused heads dark. Choose `:standby-mode :all` for a low-priority standby look across the scope. Authored black, zero master and blackout still take effect. If the standby look alone would exceed the watt budget, its level falls too.
- **What did the show actually use?** Read `(get-in (show/variables runtime) [:lighting/budget :global])` for the active and shed counts, requested watts and delivered watts. The same fields are available under `[:lighting/budget :groups group-id]`. A powered lamp behind a closed shutter consumes estimated watts and leaves its visible-source slot free.

To operate the limit from a cue pad or device mapping, set its value to
`{:cue-id :stage-energy :param :cap :default 1.0}`. The allocator reads the
latest running instance of that cue and falls back to the supplied value on
release. The same show-control cue can carry the [energy autopilot](energy-autopilot.md)
brightness and activity targets.

## Shape visual activity

Enable `:lighting-activity` to score the final, visible output from zero to one. A faster fixture strobe raises the strobe component. A square dimmer LFO raises the flashing component, and rounding its edges reduces it. Moving-head speed contributes while the head is lit. A handful of sparkles can score highly on their own fixtures while contributing little to the whole room.

Read the score and its brightness, coverage, flashing, movement and strobe components under `[:lighting/activity :scene]` in `(show/variables runtime)`. Use `[:lighting/activity :effects effect-id]` or `[:lighting/activity :groups patch-group-id]` for a narrower view. The scene reads the combined output. Per-effect scores estimate how much each writer contributed when arithmetic blends share a channel.

Attack smooths rising light. Release smooths falling light and leaves a tail when a cue ends. After one duration the output has moved about 63% toward a held input. The activity meter sees the result. On a fixture with a mechanical strobe, the envelope controls the brightness of the flashes. The shutter command sets their rate.

```clojure
(show/submit! runtime :settings
  {:scope :output-response
   :values {:enabled? true
            :global {:attack {:beats 1/4} :release {:beats 1}}
            :groups {:front {:release {:bars 1}}}}})
```

Group stages override the corresponding global stage. In Studio, choose **Cue parameter** as the attack or release time source, then select the cue and its numeric parameter. For example, `{:beats {:cue-id :feel :param :attack :default 1/4}}` lets a device knob mapped to the running cue’s `:attack` parameter set attack in beats. The fallback applies while the cue is idle. Blackout and the operator’s zero master remain immediate.

In Auto, enable **Choose cues by visual activity** to favor quieter or busier looks. Auto ranks eligible cues by distance from the target and considers the output budget before launch. It uses the observed peak for a known effect revision and estimates new declarative effects from their outputs and targeted fixtures.

```clojure
(show/submit! runtime :auto
  {:enabled? true :interval {:bars 2} :count 1
   :activity-target 0.2 :scope {:cue-ids [:slow-pan :warm-wash :sparkle]}})
```

## Trigger a look from sound

`:audio/energy` names the normalized, smoothed RMS signal from the configured audio capture. The show variable uses that key, the patchbay source is `audio/energy`, and a subscriber to the `:audio/energy` event reads the same key in its event data. During silence the envelope decays toward zero. Its published value saturates at one. The analyzer publishes the variable per measurement and throttles event delivery separately.

For continuous control, add `:target-var [:audio/energy]` to a scene request. To vary a look once per loud passage, give the subscriber a higher trigger and a lower rearm threshold.

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

Try **“Vary the front washes to a third of their brightness, then tell me what they reached.”** Chat discovers groups and checks measurements with `inspect_lighting`. It can choose a look with `set_lighting_scene`, limit simultaneous light with `set_lighting_budget`, or register a recurring audio-energy crossing with `configure_energy_scene`. **Something different** preserves a continuously bound target. Explicitly setting a new fixed target disconnects its variable binding.

### What Chat may change

Chat may tighten the lighting budget but not loosen it past your setting. It
can lower a limit, an active-source count or a standby level, add a group rule,
switch budgets on, or change the strategy and fixture order. It cannot raise a
limit or count above yours, remove one of your rules, switch budgets off or add
standby light. After lowering a limit, Chat may raise it again up to your
value. Your setting is whatever you or the REPL last applied. A refused change
leaves the budget unchanged and returns
`{:status :rejected :details {:code :agent-budget-loosening :loosened [{:path [:global :limit] :ceiling 0.8 :proposed 0.9}]}}`,
which Chat reads in its tool result.

Studio's **Output budget** panel and REPL changes are not restricted. Studio
always counts as the web operator surface, so a request cannot claim to be an
agent, or anything else, to change which rule applies.

```clojure
@(show/submit! runtime :settings {:scope :lighting-budget :values {:enabled? true :global {:limit 0.8}}})
;; An agent run through the same tool boundary Chat uses:
(show/dispatch-tool! runtime "set_lighting_budget" {:limit 0.9} {:conversation-id "try" :run-id "try"})
;; => {:status :rejected ... :details {:code :agent-budget-loosening ...}}
(show/dispatch-tool! runtime "set_lighting_budget" {:limit 0.5} {:conversation-id "try" :run-id "try"})
;; => {:status :accepted ...}
```

The rule is `tolglow.lighting-budget.agent-policy/policy`: `:tighten-only` by
default, or `:unrestricted` to admit agent changes like yours. Change the
default there, or for one running process with
`(alter-var-root #'tolglow.lighting-budget.agent-policy/policy (constantly :unrestricted))`.

With `:lighting-capacity` enabled, read rig-wide utilization at `[:lighting/rig :actual]` in the show variables. The corresponding scene-group reading lives under `[:lighting/groups group-id]`. These readings and the activity score can be used from the REPL or mapped to another effect. [Rig capacity](lighting-capacity.md) · [Headless configuration](headless-modules.md) · [Performance measurements](performance/lighting-controls.md)
