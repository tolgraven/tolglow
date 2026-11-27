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

A modulation period outside 1/1024 to 2^40 beats is refused when it is
published, started or bound. The REPL acknowledgement and Studio's request
error carry the same message, which names the field. A cue owner whose bar
period leaves the range only in its captured meter runs without its controller.
Both surfaces then read the same `:modulation/refused` diagnostic, which the
Studio modulation panel lists:

```clojure
(get-in (show/snapshot runtime) [:modulation :diagnostics])
```

A controller that skips a backlog beyond its catch-up budget reports
`:modulation/rebased` in the same projection.

Gap: the Studio interval field checks only that the value is positive. The
server's request error is the only range feedback Studio gives. The panel's
rendering of the refusal has not had a browser review.

Gaps: Studio has no control that sets a cue's reload policy; author it in cue
EDN or the REPL. The diagnostics summary does not print `:from-version`, which
is present in the projected fault data. The automated check compares the REPL
subscription with Studio's Ring `/api/state` projection
([test](../test/tolglow/effects/state_migration_test.clj)); the rendered
inspector has not had a browser review for this policy.

## Read a definition as Studio shows it

The **Definitions** editor and its Neovim buffer show a registry entry as
pretty-printed EDN. The host and Studio share one printer,
[`tolglow.studio.pretty`](../src/shared/tolglow/studio/pretty.cljc), so the REPL
prints the same text from the same data:

```clojure
(require '[tolglow.studio.pretty :as pretty])
(println (pretty/edn (get-in (:model (show/export-show runtime)) [:registry :effects :ember])))
```

A collection stays on one line when it fits 100 columns. Otherwise every map
entry gets its own line; a value that does not fit after its key starts the next
line, indented two columns past the key, so deep expressions drift right slowly
(only very deep ones, such as the showcase `:confetti` expression, still pass the
width). A vector of scalars fills its lines, except a keyword-argument vector
(`[:key value …]` with at least one value that is not a keyword), which puts each
pair on its own line; a vector of keywords only, such as a target list, fills its
lines. `:id`, `:version` and `:name` come
first; small maps keep their authored order and larger maps sort their keys. The text reads back to the same data and has no trailing newline,
so the Neovim echo of a loaded definition matches it exactly. The layout is a
view only: publication reads any EDN layout, and the registry stores data.
ClojureScript prints whole floats such as `1.0` as `1`, so text that Studio
generates (**Generate EDN**) can differ from the host's text in those numbers.
Studio's read-only data views (debug event data, sensor UDP status, fault
locations, RDM readback and job results) use the same printer, and tool-call
JSON in the Assistant is indented by two spaces. The Develop REPL console
pretty-prints a data result or error data with the same printer, inside the
REPL's existing bounds (64 items per collection, 10 levels, 8192 characters per
result). Printing stops at the character bound, and a map or set longer than
64 items shows the first 64 that `pr` would show, sorted, so a huge result costs
no more than the text shown. Strings and other values print as `pr` prints them.

## Sample a gradient parameter

An effect output can read a `:gradient` parameter with
`{:op :gradient :key :palette :args [position]}` (#689). It is ordinary
definition data. The REPL builds it with `fx/gradient`, and Studio's
**Definitions** editor accepts the same EDN. Both publish through the shared
`:definition` operation. Both surfaces get the same refusal for
`{:op :param :key :palette}`, with a message that names the `:gradient` form.
[Gradient samples](effect-recipes.md#sample-a-gradient-parameter)

```clojure
(require '[tolglow.effects :as fx])
@(show/submit! runtime :definition
   {:kind :effects
    :definition (fx/effect :user/ramp
                  {:params {:palette (fx/parameter [{:position 0.0 :color [1.0 0.0 0.0]}
                                                    {:position 1.0 :color [0.0 0.0 1.0]}])}}
                  (fx/color (fx/gradient :palette (fx/position :x))))}})
```

A running instance's gradient changes through `:param/set` or a `:param/ramp`
with a vector of stops. The running card's parameter row draws the committed
stops as one ramp, and the rig output shows the sampled colours on the next
frame. A value with more stops than the schema's `:max-stops` is resampled for
output. Both surfaces read the reduction from the same instance projection: the
REPL reads it as below, and the card's row says "Outputs sample 8 evenly spaced
stops of 10".

```clojure
(:gradient-reductions (first (:instances (show/snapshot runtime))))
;; => {:palette {:from 10 :to 8}}
```

Gap: Studio and Push have no stop editor. Studio displays a gradient value but
cannot change its stops, so stops are edited in the REPL, in definition EDN or
through the scheduler. The Push multi-pad stop editor is tracked in
[issue #529](https://github.com/tolgraven/tolglow/issues/529).

### Sample a gradient in a block input

A block stage input of colour type can hold the same sample (#775), for
example a colour gel whose transmission follows the gradient along x. The common
data is the stage input in the definition's `:blocks` map:

```clojure
{:blocks {[:outputs :color]
          {:stages [{:block :optics/gel
                     :inputs {:transmission (fx/gradient :palette (fx/position :x) {:extend :mirror})}}]}}}
```

In Studio, **Library → Assign blocks** offers **Gradient sample** for a colour
input when the definition has a gradient parameter. It picks the gradient,
draws its stops with the parameter row's ramp, picks the extension and edits
the position as a scalar input of its own: a constant, a fixture position axis,
a scalar parameter, a clock, a musical-period oscillator or a scalar private
varying whose stages run an oscillator block. **Write stages to EDN** writes the
form above, and loading a definition that contains it shows the same choices.
A parameter or varying input lists what the compiler accepts there: a scalar
input reads numbers, and a colour input reads colours and numbers (the IR
broadcasts a number). A private varying takes its base's type, parameter reads
included, so only a scalar varying is offered as a gradient position. A gradient
is never offered as a plain `{:op :param}` input, and **Block binding** no
longer lists a gradient. Publishing goes through the same `:definition`
operation as the REPL, with the same refusals and messages.

From the REPL, read what Studio published and the colours it renders:

```clojure
(get-in (some #(when (= :tide-gel (:id %)) %) (:effects (show/snapshot runtime)))
        [:blocks [:outputs :color] :stages 0 :inputs :transmission])
;; => {:op :gradient, :key :palette, :args [{:op :position, :axis :x}], :extend :mirror}

(update-vals (get-in (show/snapshot runtime) [:output :values]) :emitted-color)
```

A REPL publication of the same definition shows in Studio's registry list.
Loading it into the EDN editor and opening **Assign blocks** shows the sample
for editing. After `npm run studio:build`,
`CHROMIUM_PATH=… lein with-profile +dev run -m tolglow.qa.block-builder-gradient target/block-builder-gradient-qa`
authors, publishes, reloads and revises the sample in headless Chromium, and
reads each published definition and the rendered pixel colours from the show
in between. Gap: Studio cannot edit the stops themselves, as above.

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

### Select and reorder from a Push 3 pad grid

A Push 3 in User Mode can open the same two modes on its pads
([issue #545](https://github.com/tolgraven/tolglow/issues/545)). Bind captured,
otherwise unbound buttons under `:cue-grid` when connecting. The grid shows
one pad per running launch in the committed order, top-left first, in the
launch's pad colour. Select commits that surface's selection,
`:control-surface/layout {:selected [:instance id]}`, and never launches or
ends anything. There is one selection: the Push encoders and display always
edit the committed launch. When the Push moves its own target, for example
by launching from a pad with the grid closed, that launch becomes the
committed selection. Stopping a cue from a pad leaves the selection where it
was. Pressing the selected pad in Select points the encoders
back at it. A selected launch that ends reads as no selection, and a restore
moves the selection to the restored launch. Reorder
takes a first pad, which blinks, and a second pad as the target. It sends
the same `:instance/move-after` with `:expected-revision` as **Place after**.
A press on a pad whose launch moved since the last refresh does nothing.
Page Up and Page Down move the surface window, so 70 launches take two pages.
Code lives in `tolglow.surface.cue-grid`, which holds the modes and the
`:active-cues` content, and `tolglow.surface.push-3`, which holds the pad and
button addresses and the LED bytes.

```clojure
(show/connect-device! runtime :push-3
  {:session-id "push-a" :user-mode? true :expression-mode :poly-aftertouch
   :input-index 0 :output-index 0 :layout captured-layout
   :cue-grid {:buttons {:select :cue-grid/select :reorder :cue-grid/reorder
                        :up :cue-grid/page-up :down :cue-grid/page-down}}})
(let [grid (some #(when (= "push-a" (:id %)) %) (:logical (:control-surfaces (show/snapshot runtime))))]
  [(:window grid) (:count grid) (get-in grid [:layout :selected])])
;; => [{:x 0, :y 0} 70 [:instance "0:12"]]
@(show/surface-layout! runtime "push-a" {:selected [:instance "0:14"]})   ; the Push encoders follow
@(show/bank-surface! runtime "push-a" :down)                               ; launches 65–70
```

In Studio, **Perform → Group mixer → Surfaces** shows the grid's session box as
"Cues 65–70 of 70", with page buttons and the selected launch. Each **In the
mix** card on the current page has a row·pad badge such as `push-a 1·3`. The
selected launch shows **Selected on push-a**. With **Follow selection** on,
**Detail** on a card pages the grid to that launch and selects it there. Two
Pushes linked with `{:mode :extend :direction :down}` page as one 128-pad grid.
`lein with-profile +dev run -m tolglow.qa.push-cue-grid-browser` checks this
round trip with a virtual Push 3 in headless Chrome.

**Tracked gaps.** The pad LED bytes use the Push 2 palette and animation
channels. Push 3 pad addresses come from a capture. Neither has been checked
on a physical Push 3 yet; the #545 hardware checklist is still open. Studio
selection opens **Detail** but the Push selection only adds a badge in Studio.
Because the selection is shared, **Follow selection** in Studio also moves the
Push encoders to the card a Studio user opens, even while someone else is
playing the Push. A Push launch likewise changes the selection Studio shows.
There is no per-operator selection yet. `:cue-variation-pads?` is refused
together with `:cue-grid`, because both own the pads. Push 2 keeps Afterglow's
own Select and Reorder grids, which use a private page rather than the
surface window.
Pushes extended to the right need `:columns 16`. A Studio grid cannot mirror
the Push yet (#706 slice 5), and the Push has no shuffle button.

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

Use **Add once** before an action or hold Shift while clicking to use additive
mode for that action. Use **Exclusive once** or hold Alt/Option to make that
action replace the selection, even while the default is additive. The
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

To solo one running effect exclusively while the Solo default is additive,
send `:mode :exclusive`. The acknowledgement's `:selected-instance-ids` holds
only that instance, and the open Studio cards show every other card as
**Excluded by Solo** after the same commit. Clicking **Exclusive once** and then
**Solo** in Studio sends the same command; read it back with the snapshot above.

```clojure
(let [{:keys [effect-selection]} (show/snapshot runtime)]
  (when-let [instance-id (first (:included-instance-ids effect-selection))]
    (select-keys @(show/submit! runtime {:op :effect-selection/toggle :control :solo
                                         :selector {:instance-ids [instance-id]}
                                         :mode :exclusive})
                 [:status :mode :selected-instance-ids])))
```

See [Effect Solo and Mute](effect-selection.md) for the shared policy and
examples for configuring the default modes.

The X-Touch's CONTROL inverts the configured mode for one press, as the
operator's Live script does. It sends the same single toggle with the inverted
`:mode` ([X-Touch](xtouch.md#mixer-layout-tolglow-706-slice-2)), so Studio,
the REPL and the unit share one atomic command (#749).

After `npm run studio:build`, `lein with-profile +dev run -m
tolglow.qa.effect-selection-browser 18890 target/effect-selection-qa` checks both directions against the production
Studio build in headless Chrome: Studio shows a REPL exclusive Solo, and
`show/snapshot` shows the result of **Exclusive once**, Alt-click and a plain
click. Remaining gap: these gestures have not been tried on an iPad or other
real touchscreen.

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

A Push encoder bound to the knob (Push 3 `:encoder-bindings` `{:target :knob}`,
or the Push 2 `:knob-page`) sends the same `:knob/nudge`, and its display shows
the knob's value and resolved link texts from the same projection
([Push](linked-knobs.md#push)).

**Tracked gaps.** The Studio picker links numeric destinations only. Colour,
enum and boolean links are created from the REPL. Push binding is proven on
virtual Push 2/3 replays, not on hardware, and the native Push 2 display does
not draw knobs yet ([issue #666](https://github.com/tolgraven/tolglow/issues/666)).
Close this gap when Studio can set non-numeric endpoints.

## Save and recall knob looks

Studio, Push and the REPL read `:knob-presets` in the show snapshot and send
`:knob-preset/capture`, `:knob-preset/recall` and `:knob-preset/clear` with a
`:slot` from 0 to 7. [Knob looks](linked-knobs.md#knob-looks)

```clojure
(show/submit! runtime :command {:op :knob-preset/capture :slot 0})
(:links @(show/submit! runtime :command {:op :knob-preset/recall :slot 0}))
(map :state (:slots (:knob-presets (show/snapshot runtime))))
;; => (:selected :empty :empty :empty :empty :empty :empty :empty)
```

In Studio, **Perform → Live → Knobs → Knob looks** lists the same eight slots.
**Save**, **Recall**, **Overwrite** and **Clear** send those commands with the
shown slot as `:expected`. A REPL save or recall changes the strip on the next
snapshot, and a Studio recall moves the knobs the REPL reads. Saving into a
saved slot without `:overwrite? true` returns `:code :knob-preset-occupied` and
Studio shows the reason. On Push, the top pad row of the knob-look overlay
sends the same commands.

**Tracked gaps.** Slots have numbers, not names. Physical Push 2 and Push 3
evidence is pending in [issue #547](https://github.com/tolgraven/tolglow/issues/547).

## Link control surfaces and move their session box

Both surfaces read one projection, `:control-surfaces` in the show snapshot, and
send the same owner commands: `:control-surface/link`, `:control-surface/unlink`
and `:control-surface/bank`. Devices send `:control-surface/attach` and
`:control-surface/detach` when they connect and disconnect, and their bank
buttons send `:control-surface/bank` too. `:logical` lists every link and every
unlinked surface with its `:window`, `:span`, content `:size`, last window
(`:limit`) and, per member, the cells it shows. The follow setting lives at
`[:settings :controllers :surfaces :follow-selection?]`.
[X-Touch surfaces and links](xtouch.md#surfaces-links-and-the-session-box)

From the REPL, attach two units, extend the main unit with the extender to its
right, page the window and read it back:

```clojure
(show/connect-device! runtime :x-touch {:session-id "xt-main" :input-index 0 :output-index 6
                                         :effect-group-view :manual})
(show/connect-device! runtime :x-touch {:session-id "xt-ext" :input-index 5 :output-index 11
                                         :extender? true})
@(show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"])          ; or {:mode :mirror}, {:mode :follow :offset {:x 1}}
@(show/bank-surface! runtime "desk" :next)                           ; :first :last :previous ...
@(show/bank-surface! runtime "desk" {:reveal [:group [:manual :mix]]})
(map (juxt :id :mode :window) (:logical (:control-surfaces (show/snapshot runtime))))
;; => (["desk" :extend {:x 16, :y 0}])
@(show/submit! runtime :settings {:scope :controllers :values {:surfaces {:follow-selection? true}}})
```

In Studio, **Perform → Group mixer → Surfaces** shows a session box per link or
unlinked surface: its name and mode, "Strips 17–20 of 20", and ⏮ ‹ › ⏭ bank
buttons. **Link** joins two unlinked surfaces (extended to the right or left,
mirrored, or followed one page to the right) and **Unlink** dissolves a link. Group rows on a surface are outlined
and badged with the surface and strip (`xt-main 3`); **In the mix** outlines and
badges the cards whose cue strip is on a surface. With **Follow selection** on,
focusing a group's volume, opening a group, or opening a card moves each surface
that holds that strip to the page that shows it. A bank button on the hardware,
a REPL move and a Studio move all update the open session boxes and both units.
A refused move (an unknown surface, a cell that is not on it) shows its reason;
a follow move for a cell a surface does not hold is ignored quietly.

Each logical surface also carries a committed layout state (#706 slice 2):
the encoder mode, its page, the selected strip and flip, under `:layout` in
each `:logical` entry. The X-Touch PAN, SEND, PLUG-IN, USER, EQ/INST, SELECT
and FLIP buttons, `show/surface-layout!` and Studio's encoder mode menu in the
session box all commit `:control-surface/layout`:

```clojure
@(show/surface-layout! runtime "desk" {:encoder-mode :plug-in :selected [:cue :ember]})
(:layout (first (:logical (:control-surfaces (show/snapshot runtime)))))
;; => {:encoder-mode :plug-in, :page 0, :selected [:cue :ember], :flip? false}
```

The session box's menu shows the mode that a button press on the unit
selected, and its tooltip names the page, the selected strip and flip.

**Tracked gaps.** Studio has no form for up/down links, follow offsets other
than one page, or a member's non-default viewport; the REPL accepts all three.
Studio shows the selected strip only in the menu's tooltip and cannot select a
strip or page the encoders itself (the REPL and the unit can). A Studio page component is not yet a surface of its
own (#706 slice 5). Close these gaps when Studio can place a member anywhere and
a Studio grid can mirror or follow a Push page.

## Shape an LFO from a band of the input

Both surfaces read `:audio-lfos` in the show snapshot and send the same owner
commands, `:audio-lfo/configure` and `:audio-lfo/remove`. The LFO's waveform,
`:audio/<id>`, is in the same picker catalog as every other waveform, and a
parameter LFO bound to it is the ordinary `:modulation-bind`.
[Audio-shaped LFOs](audio-shaped-lfo.md)

```clojure
@(show/submit! runtime :command {:op :audio-lfo/configure :lfo-id :bass :name "Bass"
                                 :config {:band :bass :extract :envelope :morph {:bars 2}}})
(select-keys (tolglow.audio-lfo/lfo-view (:model @(:state runtime)) :bass)
             [:status :band-label :progress :last])
```

In Studio, **Connections → Audio LFOs** shows the same LFO: status, level,
confidence, pitch and lateness, the source band's last window and the morph
from the previous shape to the target. **Apply**, **Remove** and **Add audio
LFO** send the commands above; an invalid setting from either side is refused
with the reason and the previous configuration stays. A REPL configure
appears in the open panel on the next snapshot.

**Tracked gaps.** Linked knobs reach a bound LFO's period and endpoints, not
the audio LFO's band or morph time. Physical input and recorded material are
open on [issue #676](https://github.com/tolgraven/tolglow/issues/676).

## Route instrument stems to cues

Both surfaces read the capture status (`:features :stems`, `:config :stems`)
and change ordinary show data: an event subscriber matching
`{:type :audio/onset :emitter-id :stem/<id>}` for onsets, and a signal patch
route from `stem/<id>/level` for levels. `tolglow.audio.stem-routes` builds and
reads both for the REPL and Studio. [Instrument stems](stems.md)

```clojure
(require '[tolglow.stems :as stems])
@(:receipt (stems/route-onset! runtime :kick :flash))
@(:enabled (stems/route-level! runtime :guitar :breathe :level))
(stems/view runtime)
```

In Studio, **Connections → Audio input → Instrument stems** shows a card per
stem with its level, onset lamp, onset and bleed counts, gate and routes.
**Launch on onset** and **Drive with level** save the same subscriber and
patch route; a route added from the REPL appears on the card, and a route
added on the card appears in `(stems/view runtime)` and in the patch bay. An
unknown cue or a parameter outside its bounds is refused by the subscriber or
patch save with its reason, and the previous routing stays.

**Tracked gaps.** Studio cannot choose a recording for the `:file` input or
edit a stem's detector settings beyond its preset (it keeps any set from the REPL); the REPL can. Linked knobs
are not yet a patch destination, so a stem level reaches a knob's targets only
by routing each target. Physical interfaces and latency stay open on
[issue #675](https://github.com/tolgraven/tolglow/issues/675).

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

Destinations an existing knob drives are left out of a fresh candidate and of
every reroll on both sides. Only the REPL and the agent tool can opt in with
`:include-knob-driven? true`. A restart drops the candidate everywhere, and an
audition whose cues were all released is refused with `:no-running-cues`.

**Tracked gaps.** A Push encoder cannot sweep a candidate yet (it reuses the
linked-knob Push binding, which waits on an Afterglow adapter change). Enum,
boolean and vector destinations are not generated. Close this gap when a Push
encoder sweeps the same candidate in the Room
([issue #667](https://github.com/tolgraven/tolglow/issues/667)). Studio has no
`:include-knob-driven?` control and does not show a link's `:shared-with`, and
its Room view does not yet compare `:frame-revision` with `:draft-revision`, so
it can show the previous value's frame for a frame or two during a sweep
([issue #737](https://github.com/tolgraven/tolglow/issues/737)).

## Take or dismiss the checkpoint from before the update

A show folder opened in the Mac app keeps its own recovery, named by its
`.tolglow-folder-id`. The working and prior checkpoints that desktop shows
autosaved to the shared directory before that change are frozen once, at the
first Mac folder claim, and offered to every Mac show folder as `:legacy` and
`:legacy-prior`; they are never restored at startup. Both surfaces read
`show/recovery-status` and call the same functions; Studio reaches them through
`/api/show/checkpoints`, `/api/show/checkpoints/restore` with `kind legacy` or
`legacy-prior`, and `/api/show/checkpoints/dismiss-legacy`. [Show files](show-files.md#the-checkpoint-from-before-per-folder-recovery) · [Issue #742](https://github.com/tolgraven/tolglow/issues/742)

```clojure
(select-keys (show/recovery-status runtime) [:checkpoints :folder])
@(show/load-latest-show! runtime :legacy)   ; restore it into this folder
(show/dismiss-legacy-recovery! runtime)     ; or stop offering both to every folder
```

In Studio, **Restore show** lists **Checkpoint from before the update** and
**Previous session from before the update** with their saved times, **Restore**
and **Dismiss**, each behind a confirmation, and its **Show folder** section
explains the identity file. A note above the view says when one is waiting,
with the saved times, and, on a folder's first open, what `.tolglow-folder-id`
is; a closed note stays closed after a reload. After a REPL restore or
dismissal the waiting note in an open Studio goes away with the next state
frame. After a Studio restore, `show/recovery-status` no longer lists them in
any folder. A refusal reads the same: once taken, the REPL restore throws
naming the folder that took them, a second dismissal returns false, and
Studio's **Dismiss** shows "The checkpoint from before the update is no longer
offered". While one window is restoring, the other's restore or dismissal is
refused with `:legacy-busy` on both surfaces.

Studio's own REPL session (**Open Clojure REPL**, `/api/repl/eval`) binds
`runtime` to the same show map that now carries its recovery owner; before
#742 that map lacked it, so `show/recovery-status` returned nil there.
Checked by `test/tolglow/desktop_test.clj` through the real desktop entry point,
Studio's routes and the REPL session, by Studio's event tests, and by
`lein run -m tolglow.qa.show-folder` in headless Chrome. No foreground Mac
window has shown the note yet.

## Let the show wander with a section plan

Both surfaces send the same `:auto` request (`POST /api/auto` from Studio,
`show/submit! runtime :auto …` at the REPL, `configure_auto` for agents) and
read the same `:auto` projection in the snapshot: `:enabled?`, `:config`, the
current `:section` and the last decision's `:reasons`. [Auto show](auto-show.md)

From the REPL, start the R1o example's plan, read the section and the last
decision, then hand one look to yourself:

```clojure
(require '[tolglow.automation.red-show :as red-show])
@(show/submit! runtime :auto (red-show/auto-config 217))
(select-keys (:auto (show/snapshot runtime)) [:section :reasons])
;; Any edit of an auto look hands it over; auto never ends it again.
@(show/submit! runtime :command {:op :param/set :instance-id "<auto instance>" :param :level :value 0.4})
```

In Studio, **Perform → Tools → Let it wander** shows **Keep the section plan
(red, purple, quiet, strobe)** for a plan started at the REPL and a status line
such as **Section quiet · Moving in darkness**. Ticking **Vary marked
parameters within their ranges** and pressing **Start auto** sends
`:randomize {:params :marked}` with `:keep-plan? true`; the REPL then reads the
same four sections with the palette kept. A REPL stop turns the header to
**Auto mode · off** and the plan stays on offer. A refusal reads the same from
both sides: a count of 70 returns `:status :rejected` with `Invalid auto config:
Count must be an integer from 1 to 64 at [:count]`, Studio shows that message,
and the committed config is unchanged. Checked in headless Chrome by
`tolglow.qa.auto-show-browser`.

**On a musical line (#767).** The same request with `:at` waits on the beat
scheduler and applies on its line before auto decides there. The common data
is the `:auto/configure` event in `show/schedule` and the snapshot's
`[:auto :pending]` (event ID, label, due beat, enabled?, source) and
`[:auto :section-end-beat]`:

```clojure
@(show/submit! runtime :auto (assoc (red-show/auto-config 218) :at :next-phrase))
(get-in (show/snapshot runtime) [:auto :pending])
@(show/submit! runtime :command {:op :schedule/cancel :event-ids ["<event-id>"]})
```

In Studio, **Start or stop** (Now, Next bar, Next phrase, Next section line)
makes **Start auto** and **Stop auto** send the same `:at`; the pending change
appears under the buttons and on the timeline with an **auto** icon, and its
**Cancel** sends `:schedule/cancel`. A REPL post appears in the panel's list; a
Studio post appears in the REPL's `[:auto :pending]`; a cancel from either side
empties both; an immediate stop from either side withdraws it. A refused `:at`
(a position a bar or more behind, no section playing; Studio's **Next section
line** chosen while none plays is sent and refused the same way) returns
`:status :rejected` with its message to both. A position less than a bar
behind applies at once with `:late? true :late-beats` in the receipt. Agents send `configure_auto` with `at` and read it back with
`inspect_schedule`. Checked by `tolglow.automation.configure-at-test` (REPL and
agent paths through the real owner), Studio's event tests and
`tolglow.qa.auto-show-browser` in headless Chrome.

**Tracked gaps.** Studio cannot author or edit a section plan, palette, ranges,
category filters or a lit limit; it keeps the ones the REPL saved. The REPL
reads which instances were handed over only from the reasons of the decision
that noticed it. Push has no auto-mode control. Close these when a performer can
build a plan in Studio and start or stop auto mode from Push, both reading the
same projection ([issue #217](https://github.com/tolgraven/tolglow/issues/217)).

## Rehearse a passage with the agent

A rehearsal uses no command of its own: the recorded agent sends
`configure_auto` (with `at` the section line) and `schedule_events` through `show/dispatch-tool!`, stems
enter through `show/audio-callbacks` and are routed with `tolglow.stems`, and
Push gestures are ordinary commands with a Push 3 source. Whatever reads those
reads the rehearsal. [Rehearsal](rehearsal.md)

From the REPL, review both modes, or play one in real time on a running show:

```clojure
(require '[tolglow.rehearsal :as rehearsal] '[tolglow.rehearsal.passage :as passage])
(println (:report (rehearsal/club 670)))
(println (:report (rehearsal/concert 670)))
;; On a show started with (rehearsal/room-model p), Studio open:
(rehearsal/play! runtime (passage/concert 670 :b))
```

In Studio, during `play!` (or `tolglow.qa.rehearsal room <port> …`) the show
is an ordinary running show: the Room renders its committed frames, **Perform →
Tools → Let it wander** reads the agent's mood as the running auto config and
section, and the schedule lists the posted anchors and the chorus build. A
Studio action meanwhile (Stop auto, a cue launch) goes through the same owner;
the agent's next section line reconfigures auto again. A refusal reads the
same: an anchor posted after its bar comes back from `schedule_events` with the
scheduler's "has already passed" message in the agent log. Checked so far
through Studio's own projection only: `tolglow.qa.rehearsal room 18871 club 670`
sampled `/api/state` every 4 s and read the auto section (drive, lift, none in
the breakdown and quiet) and the running looks while the agent's four changes
were accepted 0–0.1 beats after their lines. The Studio page was not opened in
a browser for this.

**Tracked gaps.** Studio cannot start a rehearsal, choose a passage or take,
or read the review and its flags; the review is REPL and file output. Push
cannot start one. Close these when an operator can start a Room rehearsal and
read its review in Studio, from the same `tolglow.rehearsal` operations
([issue #670](https://github.com/tolgraven/tolglow/issues/670)).

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

Each committed frame has one shared parameter sample, held beside the committed state. The Studio stream, the Push parameter display, waveform previews, cue pad prediction and the REPL resolve an instance's effective parameters from it at most once per frame, whether or not a lease retained the owner's response inputs. A Push display leases those inputs only while it shows parameters or audio, not scene, block-trace or pad-overlay views. The REPL reads the same frame:

```clojure
(show/parameter-sample runtime)
;; => {:identity {:show-id :main :epoch 0 :frame-number 6 :time-ms 375 :transport-generation 0}
;;     :instances [{:instance-id "0:1" :definition-version 1 :params {:level 0.5625 ...}
;;                  :response-values [{:id :gain :value 0.75 ...}]}]}
```

Studio shows the same values as `:parameter-sources` effective values on each instance, identified by the stream's `:epoch`, `:frame-number` and `:transport-generation`. Studio has no view of the whole-frame sample itself. [Waveform projections](waveform-projections.md#shared-parameter-sample) · [Issue #533](https://github.com/tolgraven/tolglow/issues/533)

Calibrated paired aim arrival uses one host operation from both surfaces. In Studio, **Rig & space → Draw path → Arrive together using calibrated motor rates** lists, reopens, publishes and converts synchronized paths. Studio always sends `:sync-arrival?` explicitly and shows the solved arrival time or the refusal. The REPL submits the same `:aim-path` operation and reads `:arrival-duration-ms` from the acknowledgement. Launch refusals (`:stale-calibration`, `:start-wrap-mismatch`) come back from the shared launch command on both surfaces. A launch at a tempo too fast for the motors is stretched rather than refused on both surfaces. The shared launch acknowledgement and the running instance carry `:arrival-stretch`; the REPL reads it from the acknowledgement, but Studio's cue grid does not display it yet. [Aim paths](aim-paths.md#calibrated-paired-arrival) · [Issue #481](https://github.com/tolgraven/tolglow/issues/481)

```clojure
@(show/submit! runtime :aim-path {:id :paired :sync-arrival? true :targets [:head-a :head-b]
                                  :points [[-1 0 0] [1 0 0]] :duration {:bars 2}})
```

The remaining gaps are tracked in issue #481. Studio has no editor for `:calibration :motor-rate-deg-per-sec`, so rates are set through the rig data or the REPL. Studio also cannot display the physical `:minimum-travel-ms` or the fingerprint behind a stale-calibration refusal. Close these gaps when an operator can enter rates in the fixture inspector and see why a launch was refused without the REPL.

Retrospective Capture has a shared operation. The runtime owns the capture buffer. `show/capture-phrase!` at the REPL and **Connections → Capture** in Studio both call `show/capture-action!` with `:capture`. The REPL receives the phrase; Studio receives its summary and a MIDI clip download of the same capture (`:smf`). `(show/capture-action! rt :status {})` reads the buffer and the last capture that both surfaces show. Two gaps remain. Writing the capture into Live from Studio (`live-connection/capture!` over the configured bridge) is REPL-only, and Push has no Capture button: the uniform surface layer lives with the Push controllers, outside this change. Close them when Studio and Push both send the last capture to Live and the REPL can read the resulting clip identities. [Capture into Live](live-routing.md#capture-the-last-phrase-into-live) · [Issue #671](https://github.com/tolgraven/tolglow/issues/671) · [Issue #680](https://github.com/tolgraven/tolglow/issues/680)

Rig editor placement and aim calibration use the shared rig and fixture operations from both surfaces. In Studio, **Rig & space → Place fixture** builds the draft instance with `tolglow.rig.placement/place` and **Save rig** sends it through `POST /api/rig`. The REPL builds the same marked entry with the same function, or writes `{:placement? true :fixture-type-id … :id … :universe … :address …}` itself, and calls `(show/submit! runtime :rig rig)`. On both surfaces the host instantiates marked entries from the registry, refuses one without documented movement travel, and leaves every unmarked fixture as submitted. **Calibrate aim** calls `POST /api/fixtures/aim-check` and `aim-fit`, which are `(show/fixture-action! runtime :aim-check …)` and `:aim-fit` at the REPL; **Send test aim** is the ordinary `:surface-aim` operation. A REPL rig edit redraws the open editor through the state stream. Remaining gaps: the fit proposes mounting rotation only, not travel ranges or reversal, and Studio has no pan/tilt jog control, so observations take the committed output or typed DMX values. [Rig editor](rig-editor.md#place-fixtures-from-the-library) · [Issue #226](https://github.com/tolgraven/tolglow/issues/226)

## See what ran as Live runs

The runtime keeps a run record (#683). It has one channel per launched cue,
one take per run, a lane for each overlapping run and the effect-group path.
Both surfaces read the same projection, `show/live-runs-action!` with
`:status`. In Studio, open **Connections → Live runs** and choose **Show what
ran**. It lists each launched cue with its group, lanes and takes. From the
REPL:

```clojure
(show/live-runs runtime)          ; the projection Studio shows
(show/live-runs-record runtime)   ; exact beats, launch values and moves
```

Mirroring the record into Live needs a companion connection. With the Live
bridge connected, Studio's **Sync to Live** and the REPL both use the bridge's
own connection, so the mirror does not take the companion from the bridge (a
connection with its own session loses it at the bridge's next poll):

```clojure
(show/sync-live-runs! runtime)   ; through the running Live bridge (show/live-bridge-connection)
```

After a sync, **Show what ran** marks each take `in Live` with its clip and
lists the problems the read-back found, such as a run track outside its Group
Track. The automated check compares the Ring projection with the REPL's
projection, before and after a sync
([test](../test/tolglow/integrations/live_runs_test.clj)).

Run-clip playback reaches the show through the Live bridge. The REPL can feed
the same event to `show/live-run-clip!`. The panel and
`/api/live-runs?limit=n` list the newest 256 takes by default.

Gaps:
- The panel refreshes on request, not from the state stream.
- Push has no run view.
- The panel was checked through server-rendered markup and event tests and
  in headless Chrome against Ring. It was not checked on an iPad.

[Run tracks in Live](live-routing.md#mirror-what-ran-as-run-tracks-clips-and-group-tracks) · [Issue #683](https://github.com/tolgraven/tolglow/issues/683)

## Edit an effect's parameters from Live

Each run channel can hold a **Tolglow parameters** Max for Live device
(#625). Its slots carry the effect's parameters with Studio's labels, units,
ranges, decimals and control shapes, because both read one control
projection (`tolglow.authoring.parameter-controls`). The command and data
both surfaces share:

- the projection: `show/live-parameter-devices` and
  `GET /api/live-parameter-devices` (Studio: **Connections → Live runs →
  Show parameter devices**). It lists each channel's slots, Push-style banks,
  unsupported parameters, where the devices are on the run tracks and the
  ownership counters;
- the sync: `show/sync-live-runs!` and `POST /api/live-runs/sync` (Studio:
  **Sync to Live**). It configures each device and writes a take's launch
  values and moves into its run clip as slot envelopes;
- Live's values: `show/live-parameter-device-value!`, which the bridge calls
  for every `parameter-device` callback. They become ordinary `:param/set`
  commands with origin `:live-playback` (automation) or `:live-input` (a
  performer's turn).

```clojure
(show/live-parameter-devices runtime)    ; the panel's data
(show/sync-live-runs! runtime)           ; configure devices, write clips, watch the tracks
(show/live-parameter-device-value! runtime
  {:type :parameter-device-value :track-id "track:3" :slot 5 :value 0.75
   :automation-state 1 :watch-session "repl" :serial 1})
```

Feedback in each direction: a Level move from Studio or Push reaches the Live
device once (`set-parameter-values`) and holds Live automation on that slot;
Live's echo of it is dropped. A turn of the dial in Live moves Studio's
control and is kept as a take move. Automation playback moves Studio's
control without making a take. The automated checks drive one runtime from
Push, Studio and simulated Live values and compare the Ring projection with
the REPL's ([test](../test/tolglow/integrations/live_devices_test.clj),
[browser check](../dev/studio/live-parameter-devices-browser-qa.mjs)).

The same panel mounts a shared parameter frame of the first running channel
(`show/parameter-frames-action!`, `/api/parameter-frames`). A Live device or
the Push display asking for the same instance, parameter, window and style at
a size within 10% gets the same render.

Gaps:
- Push does not mount shared frames yet; its display draws its own.
- The Live side (device creation, parameter attributes at run time,
  automation recording) has not run in a licensed Live; see the checklist on
  the pull request.
- Studio has no per-slot editor in this panel; edit values on the instance
  card as usual.
- Checked through event tests, server-rendered markup and headless Chrome,
  not on an iPad.

[Live routing](live-routing.md#edit-an-effects-parameters-from-live) · [Issue #625](https://github.com/tolgraven/tolglow/issues/625)

## Play a cue page from the launcher pads

The cue launcher (#228) has one runtime operation, `show/live-launcher-action!`,
for both surfaces. In Studio, open **Connections → Cue launcher**:
- **Show pads** reads `:status`: the selected page as Push's 64 Pads grid,
  each pad with its note, cue and colour, a summary of the layout, the Live
  setup and the last pad clip, setup problems and stale-pad warnings.
- **Set up in Live** runs `:setup` and **Write pad clip** runs `:write-clip`,
  through the running Live bridge.

**Connections → Ableton Live → Pad layout** sets the bridge's `:pad-layout`,
saved with **Save configuration** like the rest of the bridge. From the REPL:

```clojure
(show/live-launcher runtime :pads)                     ; GET /api/live-launcher?page-id=pads
(show/setup-live-launcher! runtime {:page-id :pads})   ; POST /api/live-launcher/setup
(show/write-live-launcher-clip! runtime {:page-id :pads})  ; POST /api/live-launcher/write-clip
```

Both sides use the Live bridge's own companion connection
(`show/live-bridge-connection`), so neither takes the companion from the
other. Refusals match: before the bridge connects, setup reports
`not-connected` on both; an unknown page gives Studio 404 and the REPL an
exception naming the page. A setup from either side shows on the other
([test](../test/tolglow/integrations/live_launcher_test.clj)).

Pad hits themselves use the shared commands: a pad launches its cue with
`:cue/launch` from the Live bridge, as a Studio pad or the REPL would.

Gaps:
- The panel refreshes on request, not from the state stream.
- Push has no launcher setup control; playing the pads on Push goes through
  Live, which is the point of the layout.
- The panel was checked through server-rendered markup, event tests and
  headless Chrome against Ring, not on an iPad.

[Live routing](live-routing.md#play-a-cue-page-from-one-drum-rack-track) · [Issue #228](https://github.com/tolgraven/tolglow/issues/228)

Video strip mapping uses one message from both surfaces. In Studio, **Rig & space → Video strips** sends `:video-mapping` edits through `POST /api/video-mapping`. The REPL sends the same edits with `show/video-action! runtime :edit`, using the geometry helpers in `tolglow.video.mapping`. Both read source status and sampled strip colours from the snapshot's `:video` key. A REPL edit redraws the open editor through the state stream. Studio has no control for a video cue's `:placement` parameters beyond the ordinary parameter controls on its card. [Video on LED strips](video-strips.md) · [Issue #672](https://github.com/tolgraven/tolglow/issues/672)

Shader cues use the definition command and the video actions from both surfaces. In Studio, **Rig & space → Shader cues** launches and stops through the ordinary cue commands. It auditions GLSL through `POST /api/video/shader/audition` and saves the next version through `POST /api/video/shader/save`. The REPL calls `show/video-action! runtime :audition` and `:save-glsl` with the same `{:effect-id :glsl}` payload, and `:shaders` for the producer status Studio shows. Both see compile logs and the last-working fallback in that status. The projector page has no REPL counterpart beyond `:frame`, which returns the latched frame it draws. Studio has no editor for a shader's uniforms, size, rate or viewport. Those are edited as definition data in the library or the REPL. [Shader cues](shader-cues.md) · [Issue #673](https://github.com/tolgraven/tolglow/issues/673)
Video surfaces use the same message. **Rig & space → Video surfaces** sends `{:op :put-surface}`, `{:op :delete-surface}` and `{:op :surface-cue}` through `POST /api/video-mapping`; the REPL sends them with `show/video-action! runtime :edit`. Both read the heads in contact and each running surface's evaluated pose from the snapshot's `:video :surfaces`, and a REPL edit redraws the editor and the Room. A spawned surface cue's crop, position, rotation, scale, wave and contact parameters use the ordinary parameter controls on its card. Remaining gap: the Room has no drag handles for a surface; its pose is edited with numbers. [Video on 3D surfaces](video-surfaces.md) · [Issue #674](https://github.com/tolgraven/tolglow/issues/674)
