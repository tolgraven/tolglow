# Using the same show from the REPL and Studio

When a performer can do something to a show, they should be able to find and do it from both the running REPL and Studio. A cue pad and a Clojure form can feel quite different while addressing the same cue, parameter or instance. This is a standing review agreement under R94. The examples below cover part of the show. [Request register](request-coverage.md) · [GitHub Issues](https://github.com/tolgraven/tolglow/issues)

## Try the round trip with Ember

Open **Perform** in a preview show with the **Night studies** demo page. In the header, **Open Clojure REPL** opens a trusted session with this show's handle bound to `runtime`. In an external JVM REPL, use the handle returned when that same application started. [Studio controls](studio-ui.md) · [REPL access and session bindings](web-repl.md)

Run the following forms individually. An accepted launch returns the ID of the running instance. The **Ember** card in Perform belongs to that instance. The unfired pad in the cue grid represents its definition.

```clojure
(require '[tolglow.show :as show])

(def launched
  @(show/submit! runtime {:op :cue/launch :cue-id :ember
                          :launch {:mode :persistent}}))

(select-keys launched [:status :instance-id])
```

On the active card, move **Level**. Read the result in the REPL, then send a value back and watch the same control update.

```clojure
(defn active-ember []
  (some #(when (= (:id %) (:instance-id launched)) %)
        (:instances (show/snapshot runtime))))

(get-in (active-ember) [:params :level])

@(show/submit! runtime {:op :param/set :instance-id (:instance-id launched)
                        :param :level :value 0.35})
```

Use **Release** on the card or submit the form below. The release envelope can keep the instance visible while it fades. To start again from Studio, select **Toggle** and tap the idle **Ember** pad. Its new running instance has a new ID. [Demo definitions](../src/tolglow/demo.clj) · [Studio command handling](../src/studio/tolglow/studio/events.cljs) · [Runtime snapshots and command submission](../src/tolglow/show.clj)

```clojure
@(show/submit! runtime {:op :instance/end :instance-id (:instance-id launched)})
```

## Migrate a running cue's state

A cue whose `:reload-policy` is `:migrate` transforms its private state when a
newer effect revision is published. Studio and the REPL publish through the
same `:definition` operation and read the same instance projection. The
running card's effect inspector header shows `v2 · migrate`. From the
REPL, publish a revision with a portable `:state-migration`
([declaration](effect-recipes.md#migrate-running-effect-state)):

```clojure
@(show/submit! runtime {:op :cue/launch :cue-id :ember
                        :launch {:mode :persistent :reload-policy :migrate}})

(let [{:keys [registry]} (:model (show/export-show runtime))
      effect (get-in registry [:effects (get-in registry [:cues :ember :effect-id])])]
  @(show/submit! runtime :definition
     {:kind :effects
      :definition (-> effect (update :version inc)
                      (assoc :state-migration {:from {(:version effect) [{:op :default :path [:phase] :value 0}]}}))}))
```

The `:launch` option opts this one instance in; a cue's own `:reload-policy`
sets the default for its launches. The snapshot shows `:effect-version` and
`:reload-policy` for the instance. A failing migration leaves the instance on its last good
revision with a `:reload-fault` whose `:stage` is `:migrate`. Studio lists it
in that inspector's diagnostics as `migrate · <message>`, with each cause's
location, such as `{:path ["state-migration" "from" 2 0 "path"]}`.
The REPL reads the identical map from the snapshot or the durable subscription:

```clojure
(require '[afterglow.reactive :as reactive])
@(reactive/subscribe (show/reactions runtime) [:runtime/effect-faults])
```

A rejected revision does not disturb modulation. A controller owned by the cue,
or one that modulates it, keeps its seed and schedule, and a bound waveform
keeps its value. Both surfaces read the same controller projection, so its
`:ticks` and `:next-beat` continue across the rejected publication:

```clojure
(get-in (show/snapshot runtime) [:modulation :controllers])
```

Gaps: Studio has no control that sets a cue's reload policy; author it in cue
EDN or the REPL. The diagnostics summary does not print `:from-version`, which
is present in the projected fault data. The automated check compares the REPL
subscription with Studio's Ring `/api/state` projection
([test](../test/tolglow/effects/state_migration_test.clj)); the rendered
inspector has not had a browser review for this policy.

## View recent cues

In **Perform**, the **Recent cues** tab puts the latest successful launch in
the top-left pad. The page holds up to 64 distinct cues and updates when a cue
launches. Launch a cue from the REPL with the same command shown above and
compare the tab with the committed page in the snapshot.

```clojure
(:recent-page (show/snapshot runtime))
```

## Match strip pixels to a fixture

Publish a fixture type with `:bounds-m` using **Fixture Library → Advanced definition · EDN**. Its patched extent appears in the rig snapshot in both Studio and the REPL. The REPL's `strip-segment-selector` produces the fixture IDs that either client can submit with a cue launch. Studio's picker still requires a resolved selection. [Strip extent example](spatial-show.md#match-a-strip-to-a-fixtures-extent)

## Select and reorder running cues

In **Perform → In the mix**, open **Detail** on a running card to select that
launch for editing. If the same cue is running more than once, the cards
show their distinct launch IDs. **Reorder** opens a separate mode. Choose
**Move** on one running card and **Place after** on another. The cards show
the committed compositing order.

From the REPL, read the first two running launches in the current order and
submit the same operation. Watch **In the mix** update in Studio.

```clojure
(let [{:keys [active-instance-order instances revision epoch runtime-id]} (show/snapshot runtime)
      running-ids (into #{} (map :id) (filter #(= :running (:phase %)) instances))
      [first-launch second-launch] (filter running-ids active-instance-order)]
  (when second-launch
    @(show/submit! runtime {:op :instance/move-after :instance-id first-launch
                            :after-instance-id second-launch :expected-revision revision
                            :epoch epoch :runtime-id runtime-id})))
```

### Set a running layer's priority

In **Reorder**, choose **Move** on a running card, enter a **Layer priority**
and select **Set priority**. Lower priorities compose first. If the selected
layer joins an existing priority, it composes after the peers already there.
An active native layer moves all of its current members together. Each launch
keeps its ID, parameters and timing. The cards display committed priorities and
order, including edits made through the REPL or OSC.

```clojure
(let [{:keys [revision epoch runtime-id]} (show/snapshot runtime)]
  @(show/submit! runtime {:op :instance/set-priority :instance-id (:instance-id launched)
                          :priority 4.5 :expected-revision revision
                          :epoch epoch :runtime-id runtime-id}))

;; Choose the initial priority at launch from a calculated or supplied value.
(let [priority 4.5]
  @(show/submit! runtime {:op :cue/launch :cue-id :ember
                          :launch {:mode :persistent :priority priority}}))
```

An OSC session can map a numeric input to a known active launch and another
input to the initial priority of a new launch. Each path takes one finite numeric
argument. Create the session with `show/connect-device!` and include these
entries in its `:mappings`. The live path addresses the specific launch selected
when configuring it.

```clojure
{"/ember/priority" {:op :instance/set-priority :instance-id (:instance-id launched)}
 "/ember/launch" {:op :cue/launch :cue-id :ember :priority-arg? true}}
```

### Modulate a running layer's priority

Layer priority is a typed waveform destination, `:layer/priority`: an integer
tier from −1000 to 1000 in the `:priority` category. In **Perform → In the mix**,
open a card's **Detail** and select **Source** on its **Layer priority** row, or
open the **Source** editor of an LFO already on that launch and choose
**Layer priority** under **Destination**. Set the **From** and **To** tiers. The
**Hysteresis band (tiers)**, **Minimum dwell** and **Dwell unit** (beats, bars
or phrases) fields control how far past a tier boundary the waveform must go,
and how long a tier is kept, before the layer changes order. A dwell authored in
bars or phrases at the REPL keeps that unit in the editor. **Use base value**
holds the committed priority and **Detach LFO** removes the source.

The **In the mix** cards appear in the order the latest frame composed them,
from `:effective-instance-order`. A card whose waveform tier differs from its
committed priority shows both, for example **Priority 1 · set 0**, in its header
and on the **Layer priority** row. **Reorder** captures and **Place after**
targets still use the committed order (`:active-instance-order`), because those
commands change committed state.

A manual priority change takes precedence over the waveform. **Set priority**,
**Place after**, `:instance/set-priority` and `:instance/move-after` on a launch
with a bound layer priority waveform also hold that source, recorded with
`:reason :manual-priority`. The row shows **Held after a manual priority
change**, and the projected source has `:held? true :hold-reason
:manual-priority`. **Resume LFO** or `:modulation-resume` with
`:params [:layer/priority]` returns the layer to the waveform. This matches how a
parameter edit holds a waveform on that parameter.

The REPL uses the same validated `:modulation-bind`, `:modulation-hold`,
`:modulation-resume` and `:modulation-unbind` operations. Each frame the show
owner samples the waveform once, applies the hysteresis and orders that frame
by the resulting tier. Ties keep launch order. The committed priority and
`:active-instance-order` do not change. The snapshot's
`:effective-instance-order` lists the order used for the latest frame, and the
launch's `[:parameter-sources :layer/priority]` reports the committed priority
as `:base-value` and the frame tier as `:effective-value`.

```clojure
(show/submit! runtime :modulation-bind
  {:instance-id (:instance-id launched) :param :layer/priority
   :binding {:type :lfo :waveform :triangle :period {:beats 4}
             :phase-reference :transport :from -1 :to 1
             :hysteresis {:band 1/4 :dwell {:beats 1/2}}}})

(let [{:keys [effective-instance-order instances]} (show/snapshot runtime)]
  [effective-instance-order
   (some #(when (= (:instance-id launched) (:id %))
            (get-in % [:parameter-sources :layer/priority :effective-value]))
         instances)])
```

Studio's destination list and binding fields come from the snapshot's
`:layer-priority-destination`, so the REPL and Studio share one schema. Members
of a compound native layer share one priority. A priority waveform on a member
is rejected; use **Set priority** or `:instance/set-priority` for the whole
layer. The remaining gap is a Push or OSC control surface for the hysteresis
fields; both can still bind the waveform through the shared command.

## Solo or mute running effects

In **Perform → In the mix**, use **Solo** or **Mute** on a running card.
**Muted** and **Excluded by Solo** show why the owner has gated a card's
contribution. The action buttons show which controls are selected. A mixed
button means some of the selected category or layer members have that flag.

Use **Add once** before an action or hold Shift while clicking to use additive mode. The
**Defaults** control sets the normal mode for Solo and Mute independently.
Choose a category or **All running** to act on that group. Expand **Layers**
when a native composition is playing to act on a particular layer.

Read the committed selection in the REPL, then submit a change and watch the
open cards update. The same selector supports categories and native layer IDs.

```clojure
(let [{:keys [effect-selection]} (show/snapshot runtime)]
  (when-let [instance-id (first (:included-instance-ids effect-selection))]
    @(show/submit! runtime {:op :effect-selection/toggle :control :mute
                            :selector {:instance-ids [instance-id]}
                            :additive? true})))
```

See [Effect Solo and Mute](effect-selection.md) for the shared policy and
examples for configuring the default modes.

## Suspend automatic section patterns

When the show starts with an enabled section pattern policy, open **Perform**
and find **Section patterns** above the live controls. On a narrow screen, first
select **Live**. **Suspend** and **Resume** send the shared show settings
operation. **Override active** means the show has committed the suspension.
**Override off** allows automatic patterns when the policy's timing and input
conditions are met.

After clicking **Suspend**, read the committed setting in the running REPL.
Clear it there and verify that the open Studio view returns to **Override off**.
Use `true` in the same command to suspend patterns from the REPL.

```clojure
(require '[tolglow.show :as show])

(get-in (show/snapshot runtime) [:settings :section-patterns :override?] false)

@(show/submit! runtime :settings
               {:scope :section-patterns :values {:override? false}})
```

## Choose the track feel

In **Perform**, open **Track feel** above **In the mix**. On a narrow screen,
first select **Live**. Pick a personality by its icon, mix a second one in with
**Blend with** and **Share**, draw new shapes with **Reshuffle shapes**, or
return to the authored looks with **Clear feel**. The closed row always shows
the committed selection.

Read that selection in the running REPL, then change it there and watch the
open panel follow. Both sides submit the `:personality` operation to the show
owner. IDs are the library's keyword or text IDs, weights are relative shares
and the seed is an integer.

```clojure
(require '[tolglow.show :as show])

(get-in (show/snapshot runtime) [:personalities :active])

@(show/submit! runtime :personality {:op :select :id :zigzag-shaky :seed 5})
@(show/submit! runtime :personality
               {:op :blend :mix [{:id :zigzag-shaky :weight 0.65} {:id :still-water :weight 0.35}]})
@(show/submit! runtime :personality {:op :select :id :no-such-feel})
```

The last form is refused with `"Unknown personality"` and `{:id :no-such-feel}`.
Studio shows the same message under the panel when its own request is refused,
and the committed selection stays on both sides. Studio's **Reshuffle shapes**
chooses a random seed, while the REPL names one. Studio blends the heaviest
current part with one other personality; the REPL can mix up to four.

Reviewed at `0ecbe763` and rechecked at `23485c2c` (same Studio sources, master
merged) with the optimized build in headless Chrome 155 through
the application's own REPL session: a Studio selection was read from the REPL,
a REPL selection updated the open panel, and the refusal matched on both
sides. [Operator guide](personalities.md) ·
[Browser check](../dev/studio/personality-picker-qa.mjs)

Gaps, tracked in [issue #647](https://github.com/tolgraven/tolglow/issues/647):
Studio cannot author or remove a personality or import a sharing package, and
its sculpt tool does not offer the personality input on a volume cue. Use the
`:put`, `:remove` and `:import` operations and the `:spatial-cue` spawn request
from the REPL. Close them when an operator can do each from Studio and read
the same committed library and cue definition in the REPL.

## Reconnect a local track reference

Supply the local mpv socket and track catalog in the host's `:section-reference`
configuration. The catalog uses canonical absolute track paths and the
configured profile's identity and duration.

```clojure
;; Add this to the show options alongside the enabled :section-patterns policy.
{:section-reference {:profile profile
                     :provenance {:source-id :mpv :session-id (random-uuid) :clock-id :show-monotonic}
                     :mpv {:socket-path "/home/operator/.local/run/mpv.sock"
                           :catalog {track (select-keys profile [:reference :duration-ms])}}}}
```

After player reconnect, show restart or restore, open **Connections → Track
reference** and choose **Reconnect mpv**. The status distinguishes unavailable,
acquiring, aligned, ambiguous and stale. A fresh session waits for matching
observations from the configured player. **Patterns suspended** reports the current manual
override, which reconnect preserves.

```clojure
(require '[tolglow.show :as show])

(show/section-reference-status runtime)
(let [{:keys [runtime-id epoch]} (show/section-reference-status runtime)]
  (show/reconnect-section-reference! runtime {:runtime-id runtime-id :expected-epoch epoch}))
```

The REPL and Studio receive the fresh reference session ID or a refusal from
the same show operation. For manually owned mpv connections, use the existing
[REPL adapter flow](../src/tolglow/integrations/README.md#observe-a-known-local-recording-in-mpv).
## Add a cue to a saved macro

In **Perform**, open a cue cell’s actions and choose **Add to macro**. Select a saved macro and press **Add cue**. Studio confirms the accepted save and the new slot appears in **Compose**. If the reply is lost, **Retry append** resends the same operation. If the cell changed before the save reached the show owner, reopen actions on the cue you want.

From the running REPL, use the IDs from `show/snapshot`, preserving keyword and string IDs as shown. Replace `:opening-look` with the ID of your saved macro.

```clojure
(require '[tolglow.show :as show])

(def append-request
  (let [{:keys [runtime-id epoch revision]} (show/snapshot runtime)]
    {:runtime-id runtime-id :expected-epoch epoch :base-revision revision
     :macro-id :opening-look :cue-id :ember :command-id (str (random-uuid))}))

@(show/submit! runtime :composition-append append-request)
(some #(when (= (:macro-id append-request) (:id %)) %) (:compositions (show/snapshot runtime)))
```

To retry an uncertain reply, resubmit `append-request` unchanged. If it has expired, inspect the committed slots before starting another append. The shared operation saves cue membership and leaves running instances in place.

## Audition a scene in the Room

Stage several cue and effect changes as one Room draft, watch them in the
Room, then apply them as one show transaction or discard them. Live output
keeps sending the committed show while the draft is staged. Stage, apply and
discard are ordinary owner commands, so the REPL, Studio and any adapter
receive the same acknowledgements and refusals.
[Draft model and transaction](../src/tolglow/show/scene_draft.clj) · [Issue #531](https://github.com/tolgraven/tolglow/issues/531)

```clojure
(require '[tolglow.show :as show])

;; Each stage captures the current show; a revision keeps the draft ID.
@(show/submit! runtime {:op :scene-draft/stage :draft-id "chorus"
                        :operations [{:op :cue/launch :cue-id :indigo}
                                     {:op :effect/launch :effect-id :sweep}]})

(show/scene-draft runtime)   ; draft, status, rendered/skipped frames, scheduled apply, last outcome

;; Apply now, or on the next bar of the show transport. One command ID,
;; one revision and one history entry cover every staged operation.
@(show/submit! runtime {:op :scene-draft/apply :draft-id "chorus"
                        :expected-draft-revision 0 :grid {:bars 1}})

@(show/submit! runtime {:op :scene-draft/discard :draft-id "chorus"})
```

In Studio, a staged draft shows **Draft audition** above **The Room** with its
ID, revision and number of staged operations. Only the Room renders the
draft's private frames, published in the snapshot as `:draft-output` with the
draft ID and revision. `:output` and every other preview stay live. **Apply**, **Apply on next bar** and **Discard** send the same commands
with the displayed revision. A stale revision, changed rig or registry, or a
retired instance is refused and reported in both places. The banner names a
stale draft's changed dependency and keeps a missed scheduled apply with its
reason, which `show/scene-draft` returns as `:stale` and `:last-outcome`.

The audition renders through the live frame function with its own clock,
instance state, keyframes, modulation and response history, on the CPU with
its own compiler and caches. It never sends packets, plans fog, polls display
leases, raises events or invokes callbacks. Input stages that belong to the
live show (sensors, subscribers, automatic policy, audio returns and patch
routing) do not run in the audition. Each frame is queued on an audition
worker after the live frame is published and its owner time measured, so it
never holds the owner lock or delays a live slot. A frame is skipped and
counted while the previous one is still rendering or when the live frame used
more than half its period. The Room can therefore lag the live frame by a
frame or more.

A quantized apply fires where grid cue launches fire, after the cycle's queued
input, so an earlier Discard or instance end is applied first. A clock source
change or a backward beat jump reports the apply as missed. The audition itself
follows the jump: its private musical anchors take the same rebase as the live
show's. While an apply is scheduled, Discard is the only way to cancel it.

The draft is a snapshot of the show at staging. Only the transport, master,
blackout and the active track feel follow the live show afterwards, since a
draft cannot stage a feel and an apply runs under the live one. The audition
never changes the feel. A later live launch, parameter edit
or settings change is not in the Room until you stage again. If the rig, any
cue or effect definition, or the material library changes after staging, the
draft shows `:stale` with the code an apply would be refused with
(`:stale-patch`, `:stale-registry`, `:stale-prefabs`) and stops rendering.
Stage it again to audition the changed show. A restart or a restore drops the
draft and its scheduled apply.

The audition has no beat-clock schedule of its own. A running master ramp on
the live show reaches the Room as its current frame value, so the Room fades
with the live show and lands on the committed level without a step. Parameter,
opacity and variable ramps edit state the snapshot does not follow: the Room
keeps the committed value from staging, also while such a ramp runs. Scheduled
events and subscriber steps fire only on the live show. The audition never
advances, commits, holds or cancels a ramp, and `show/schedule` reads the same
with or without a draft.

A retried apply returns its first receipt. An apply that reuses the command ID
of anything else is refused with `:command-id-conflict`.

To repeat the browser check on a preview-only show, build Studio with
`npm run studio:build`, then run `lein run -m tolglow.qa.scene-audition
target/scene-audition`. It stages a draft through `/api/command`, compares the
Room canvas with the live values in `/api/state`, applies on the next bar from
the banner, then makes a second draft stale with a live cue edit and discards
it. Set `CHROMIUM_PATH`, and `CHROMIUM_ARGS` to choose the GPU backend.

Open gaps, owned by [issue #531](https://github.com/tolgraven/tolglow/issues/531):

- Studio cannot compose a draft yet. Stage it from the REPL or another adapter;
  Studio shows, applies and discards it. Acceptance: compose and revise a draft
  from the cue grid, then inspect the same revision with `show/scene-draft`.
- A staged operation must name a cue by the ID type the registry holds. A live
  `:cue/launch` also finds a text-ID cue addressed by keyword, as JSON adapters
  send it; inside a draft that launch is refused with `:unknown-cue`.
  Acceptance: the same launch stages and applies from `/api/command`.
- The Push simulator and rig views still show the live frame. Acceptance: the
  Push Room shows the draft frames and a draft indicator.
- There is no structural undo for launches and definitions anywhere in the
  show. An applied draft is one history entry, so such an undo can treat it as
  one unit. Acceptance: one undo restores the show as it was before applying.
- Room screenshots on hardware and a captured boundary change remain to be
  recorded.

## Schedule events and ramps on the beat clock

Both surfaces read and change one projection: `:schedule` in the show
snapshot. The shared commands are `:schedule/post`, `:schedule/cancel`,
`:schedule/move`, and direct actions carrying `:at`, `:in` or `:grid`, such as
`:param/ramp`. Positions and durations are zero-based beats, bars and phrases
on the transport. [Beat-clock scheduling](scheduler.md)

From the REPL, ramp Ember's level and launch a cue two bars ahead. Then read
the projection that Studio shows:

```clojure
(def planned
  @(show/post-events! runtime
     [{:id "ember-swell" :label "Ember swell" :op :param/ramp :cue-id :ember
       :param :level :to 0.9 :over {:bars 4} :curve :ease-in-out}
      {:id "ice-in" :label "Ice in" :op :cue/launch :cue-id :ice :in {:bars 2}}]))

(:event-ids planned)
(show/schedule runtime :compact)
(:ramps (show/schedule runtime))
```

In **Perform**, the cue panel's **Scheduled** list shows **Ice in · beat …**
with **Cancel**, and the running **Ember swell** ramp with its target, end beat,
progress bar and **Stop**. Studio reads `[:show :schedule]` from the same
snapshot. Select **Stop** on the ramp. The level holds where the ramp left it.
Read the outcome back in the REPL:

```clojure
(last (:recent (show/schedule runtime)))
;; => {:event-id "ember-swell" :status :cancelled :reason :operator-cancel ...}
```

Studio's **Stop** and **Cancel** submit `{:op :schedule/cancel :event-ids [id]}`.
Studio and the REPL are operator surfaces, so they can cancel or move any
event. An agent can only change what it posted. Studio draws a ramp's progress
bar from the projection's `:start` and `:end` and the transport beat. Its knob
keeps showing the committed value until the ramp commits, because the ramp is a
frame overlay.
The REPL equivalent is `(show/cancel-events! runtime {:event-ids ["ice-in"]})`.
While a ramp runs, moving the **Level** control on Ember's active card cancels
it. `:recent` then records `:reason :human-override`, and the control keeps the
value you set.

A refusal looks the same from both sides. A batch with one invalid event, such
as an unknown curve, returns `:status :rejected :code :invalid-schedule` with
`Event <index>: …` and schedules nothing.

**On the timeline.** The **Timeline** panel in Perform draws the same schedule
around now: pending events to the right of the centre line, fired ones to the
left, ramps as bars with their easing curve. Read what it draws with
`(show/timeline runtime {:bars 8})`. Select an event and choose **Cancel
event**, or a running ramp and **Stop ramp**. Both submit the same
`:schedule/cancel` by event ID, and the outcome appears in
`(:recent (show/schedule runtime))` and as a cancelled marker on the timeline.
An event cancelled from the REPL moves on the open timeline in the same way.
[Musical timeline](timeline.md)

**Tracked gaps.** Studio lists and cancels scheduled work, but you cannot author
an event, ramp or reschedule there. Use the REPL, an agent or a reactive
action. Authoring and dragging on the timeline is not built. Push does not
display the schedule yet. Close these gaps when an operator can post and move an
event from Studio and see it in the REPL projection, and when Push shows the
same upcoming list.

## Play the showcase timeline

The [showcase show](showcase-show.md) plays its recorded timeline through the
same scheduler projection. Start it from the config
(`lein run --config tolglow.showcase/config` autoplays and loops) or from the
REPL with `(showcase/play! runtime {:loop? true})`. Studio's cue panel lists
the timeline's upcoming events, labelled **Launch …**, **End …** or with the
ramp's parameter, and shows each running ramp with **Stop**. Cancelling one
there records `:operator-cancel` in `(:outcomes (showcase/status player))`.

**Tracked gap.** Studio cannot start, stop or loop the timeline, and its
waveform binds and spatial-volume launches are direct commands that never
appear in the Scheduled list. Use `showcase/play!` and `showcase/stop!` from
the REPL. Close this gap when the Studio timeline in
[issue #646](https://github.com/tolgraven/tolglow/issues/646) can load and
play a timeline document.

## Turn one knob across several cues

Both surfaces read one projection, `:knobs` in the show snapshot, and send the
same owner commands: `:knob/create`, `:knob/link`, `:knob/unlink`,
`:knob/set`, `:knob/nudge`, `:knob/rename` and `:knob/remove`. Each link in the
projection carries its `:resolved` value with `:text`, `:unit` and, for beat
ratios, the exact `:ratio`, plus the destination's `:current` value, `:status`
and `:shared-with`. [Linked knobs](linked-knobs.md)

From the REPL, link a knob to two running cues and turn it:

```clojure
(require '[tolglow.knobs :as knobs])
@(show/submit! runtime :command
   {:op :knob/create :knob-id :look :name "Look"
    :links [{:id "sat" :destination {:kind :param :cue-id :ember :param :level} :range [0.2 1.0]}
            {:id "rate" :destination {:kind :lfo :cue-id :ice :param :level :field :period}
             :range [2 1/2] :quantize {:ratios :preferred}}]})
(:links @(show/submit! runtime :command {:op :knob/set :knob-id :look :value 3/7}))
(map (juxt :id (comp :text :resolved)) (:links (knobs/knob-view (:model @(:state runtime)) :look)))
;; => (["sat" "0.543"] ["rate" "1 beats"])
```

A refusal reads the same from both sides: linking a missing parameter returns
`:status :rejected :code :invalid-knob` with the reason, and the knob keeps its
previous links. A destination that disappears later stays in the projection as
`:status :dangling` with a `:reason`.

In Studio, **Perform → Live → Knobs** shows the same knob: the slider, and per
link the label, resolved text and status. Moving the slider sends `:knob/set`;
**Add destination**, the range fields, **Direction**, **Curve** and **Remove**
send `:knob/link`, `:knob/unlink` and `:knob/remove`. After the REPL turns the
knob, the open slider and values follow from the projection. A refused link
shows its reason in the panel. [Studio walkthrough](linked-knobs.md#using-it-from-studio)

**Tracked gaps.** The Studio picker links numeric destinations only. Colour,
enum and boolean links are created from the REPL. Push needs an Afterglow
adapter change before an encoder can be bound to a knob; the Tolglow command and
display data are ready ([Push](linked-knobs.md#push),
[issue #666](https://github.com/tolgraven/tolglow/issues/666)). Close this gap
when Studio can set non-numeric endpoints and a Push encoder turns the same knob
and shows the same resolved values.

## Generate a knob and keep it

Both surfaces read `:knob-candidates` in the show snapshot and send the same
owner commands: `:knob-candidate/generate`, `/reroll`, `/audition`, `/link`,
`/unlink`, `/keep` and `/discard`. The projection carries the candidate's seed,
character, score, explanation, recipe and links resolved at the swept value,
or the reason too few parameters were eligible, plus the Room audition's value
and draft revision. Agents use the same commands through the `knob_candidate`
tool. [Generated knobs](generated-knobs.md)

From the REPL, generate one, sweep it in the Room, reroll and keep it:

```clojure
@(show/submit! runtime :command {:op :knob-candidate/generate :seed 11})
@(show/submit! runtime :command {:op :knob-candidate/audition :value 1.0})
@(show/submit! runtime :command {:op :knob-candidate/reroll})
@(show/submit! runtime :command {:op :knob-candidate/keep :name "Lift"})
;; Later, or after a reload: the same links from the kept knob's recipe.
@(show/submit! runtime :command {:op :knob-candidate/generate :from-knob "generated-1"})
```

In Studio, **Perform → Live → Generate a knob** has **Character**, **Seed**,
cue chips and **Generate**, a **Sweep in the Room** slider, the links with
their explanations, **Reroll**, **Drop**, **Keep as knob**, **Discard** and
**Repeat a kept knob**. The Room banner reads **Knob candidate** while one is
auditioned. A REPL generation or audition moves the open panel and slider. A
refusal reads the same from both sides: auditioning with no candidate returns
`:status :rejected :code :no-candidate`, and applying the candidate's Room
draft as a scene returns `:code :preview-draft`.

**Tracked gaps.** A Push encoder cannot sweep a candidate yet (it reuses the
linked-knob Push binding, which waits on an Afterglow adapter change). Enum,
boolean and vector destinations are not generated. Close this gap when a Push
encoder sweeps the same candidate in the Room
([issue #667](https://github.com/tolgraven/tolglow/issues/667)).

## Reviewing a show feature

- **Find it from either side.** Name the view, control label and public REPL entry point. Include the shared command, input units, identifiers and scope so an operator who knows one surface can recognize the other.
- **Try both directions.** Edit through Studio and inspect committed show data from the REPL. Edit through the REPL with Studio open and check that its view updates. For a read-only feature, compare the two readings for the same show and instance.
- **Check a refusal.** Use a stale instance or an invalid input for the changed operation. Inspect the REPL acknowledgement and Studio's failure feedback. Verify that the last committed state is still observable. A local slider draft can move before a server acknowledgement arrives.
- **Name the boundary.** Explain a gesture or display affordance that belongs to one surface and identify the underlying show behavior. For example, Studio handles a cancelled finger press by releasing the instance that press launched. At the REPL the operator directly addresses that instance with `:instance/end`. Private rendering helpers are outside this operator workflow.
- **Track a missing path.** Give the gap a domain owner, GitHub issue and an observable acceptance check. Label a gap open until both paths have been exercised. Update the operator guide when the feature lands.

## A tracked gap

The running REPL can discover and restore the current show's working, named, and prior-session checkpoints through `show/recovery-status` and `show/load-latest-show!`. `show/discard-prior-recovery!` dismisses the prior-session checkpoint. Studio's checkpoint controls show the source and modification time and report the owner's restore or discard result. [Operator examples](show-files.md#working-recovery-and-manual-checkpoints) · [Studio checkpoint controls](https://github.com/tolgraven/tolglow/pull/563)

The tethered Push saved-show browser previews the latest named and working recovery checkpoints. With explicit checkpoint and Confirm buttons, the device confirms the file shown on the display through the shared import owner, shows progress or refusal, and reopens in a fresh session after a committed restore. Verification uses virtual MIDI and captured Push 2 and Push 3 displays. Browsing registered effects, cues, and waveforms and applying their actions remain tracked in the catalog issue. [Push controls](push-display-packaging.md#browse-saved-shows) · [Saved-show browser](https://github.com/tolgraven/tolglow/pull/581) · [Issue #552](https://github.com/tolgraven/tolglow/issues/552)

At revision `0681a694`, Studio's effect keyframe editor offers **Sample at a fixed interval** and **Limit automatic slowdown** for an individual effect. The runtime also accepts a show-wide scheduler setting and exposes timing diagnostics. In a preview show, this REPL command disables automatic keyframes for the show. The public status function reads the current diagnostic state when one exists.

```clojure
(require '[tolglow.effects.cadence :as cadence])

@(show/submit! runtime :settings
               {:scope :adaptive-keyframes :values {:enabled? false}})
(cadence/status runtime)
```

The global toggle and ranked timing view were missing from Studio at that revision. The **Perceptual scheduling and interpolation** owner tracks this in [issue #307](https://github.com/tolgraven/tolglow/issues/307). Close the gap when an operator can find and change the global policy in Studio, inspect the same committed setting in the REPL and compare diagnostics for the same show. [Keyframe editor](../src/studio/tolglow/studio/keyframe_editor.cljs) · [Scheduler behavior tests](../test/tolglow/predictive_keyframes_test.clj) · [GitHub Issues](https://github.com/tolgraven/tolglow/issues)

Studio and Push read one committed response sample instead of evaluating the response again. The owner retains that sample only while a reader holds a lease. An open Push display, a connected Studio client and an explicit REPL lease all count. A REPL can take a lease and read faults from lease predicates that threw:

```clojure
(def release (show/lease-response-samples! runtime))
(show/response-sample-faults runtime)
(release)
```

Studio does not display these lease faults yet. Close the gap when Studio shows the same fault records as the REPL. [Issue #533](https://github.com/tolgraven/tolglow/issues/533) · [PR #624](https://github.com/tolgraven/tolglow/pull/624)

Calibrated paired aim arrival uses one host operation from both surfaces. In Studio, **Rig & space → Draw path → Arrive together using calibrated motor rates** lists, reopens, publishes and converts synchronized paths. Studio always sends `:sync-arrival?` explicitly and shows the solved arrival time or the refusal. The REPL submits the same `:aim-path` operation and reads `:arrival-duration-ms` from the acknowledgement. Launch refusals (`:stale-calibration`, `:start-wrap-mismatch`) come back from the shared launch command on both surfaces. A launch at a tempo too fast for the motors is stretched rather than refused on both surfaces. The shared launch acknowledgement and the running instance carry `:arrival-stretch`; the REPL reads it from the acknowledgement, but Studio's cue grid does not display it yet. [Aim paths](aim-paths.md#calibrated-paired-arrival) · [Issue #481](https://github.com/tolgraven/tolglow/issues/481)

```clojure
@(show/submit! runtime :aim-path {:id :paired :sync-arrival? true :targets [:head-a :head-b]
                                  :points [[-1 0 0] [1 0 0]] :duration {:bars 2}})
```

The remaining gaps are tracked in issue #481. Studio has no editor for `:calibration :motor-rate-deg-per-sec`, so rates are set through the rig data or the REPL. Studio also cannot display the physical `:minimum-travel-ms` or the fingerprint behind a stale-calibration refusal. Close these gaps when an operator can enter rates in the fixture inspector and see why a launch was refused without the REPL.
