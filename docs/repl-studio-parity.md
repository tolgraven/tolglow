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
snapshot. Select **Stop** on the ramp and read the outcome back in the REPL:

```clojure
(last (:recent (show/schedule runtime)))
;; => {:event-id "ember-swell" :status :cancelled :reason :operator-cancel ...}
```

Studio's **Stop** and **Cancel** submit `{:op :schedule/cancel :event-ids [id]}`.
The REPL equivalent is `(show/cancel-events! runtime {:event-ids ["ice-in"]})`.
While a ramp runs, moving the **Level** control on Ember's active card cancels
it. `:recent` then records `:reason :human-override`, and the control keeps the
value you set.

A refusal looks the same from both sides. A batch with one invalid event, such
as an unknown curve, returns `:status :rejected :code :invalid-schedule` with
`Event <index>: …` and schedules nothing.

**Tracked gaps.** Studio lists and cancels scheduled work, but you cannot author
an event, ramp or reschedule there. Use the REPL, an agent or a reactive
action. The Studio timeline in
[issue #646](https://github.com/tolgraven/tolglow/issues/646) adds authoring and
dragging on the same projection. Push does not display the schedule yet. Close
these gaps when an operator can post and move an event from Studio and see it in
the REPL projection, and when Push shows the same upcoming list.

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
