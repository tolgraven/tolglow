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

## Reviewing a show feature

- **Find it from either side.** Name the view, control label and public REPL entry point. Include the shared command, input units, identifiers and scope so an operator who knows one surface can recognize the other.
- **Try both directions.** Edit through Studio and inspect committed show data from the REPL. Edit through the REPL with Studio open and check that its view updates. For a read-only feature, compare the two readings for the same show and instance.
- **Check a refusal.** Use a stale instance or an invalid input for the changed operation. Inspect the REPL acknowledgement and Studio's failure feedback. Verify that the last committed state is still observable. A local slider draft can move before a server acknowledgement arrives.
- **Name the boundary.** Explain a gesture or display affordance that belongs to one surface and identify the underlying show behavior. For example, Studio handles a cancelled finger press by releasing the instance that press launched. At the REPL the operator directly addresses that instance with `:instance/end`. Private rendering helpers are outside this operator workflow.
- **Track a missing path.** Give the gap a domain owner, GitHub issue and an observable acceptance check. Label a gap open until both paths have been exercised. Update the operator guide when the feature lands.

## A tracked gap

The running REPL can discover and restore the current show's working and named checkpoints through `show/recovery-status` and `show/load-latest-show!`. The show owner handles both imports. Studio and Push still need controls that distinguish the two checkpoints by source and modification time, then show the owner's restore receipt. [Operator examples](show-files.md#working-recovery-and-manual-checkpoints) · [Issue #551](https://github.com/tolgraven/tolglow/issues/551)

At revision `0681a694`, Studio's effect keyframe editor offers **Sample at a fixed interval** and **Limit automatic slowdown** for an individual effect. The runtime also accepts a show-wide scheduler setting and exposes timing diagnostics. In a preview show, this REPL command disables automatic keyframes for the show. The public status function reads the current diagnostic state when one exists.

```clojure
(require '[tolglow.effects.cadence :as cadence])

@(show/submit! runtime :settings
               {:scope :adaptive-keyframes :values {:enabled? false}})
(cadence/status runtime)
```

The global toggle and ranked timing view were missing from Studio at that revision. The **Perceptual scheduling and interpolation** owner tracks this in [issue #307](https://github.com/tolgraven/tolglow/issues/307). Close the gap when an operator can find and change the global policy in Studio, inspect the same committed setting in the REPL and compare diagnostics for the same show. [Keyframe editor](../src/studio/tolglow/studio/keyframe_editor.cljs) · [Scheduler behavior tests](../test/tolglow/predictive_keyframes_test.clj) · [GitHub Issues](https://github.com/tolgraven/tolglow/issues)
