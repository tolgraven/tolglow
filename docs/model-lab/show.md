# Compact show forms for the REPL and local models

`tolglow.agents.forms/submit!` submits ordinary authoring and performance messages to the active show's owner. It returns the ordinary receipt promise. `request` shows the `[type payload]` pair before submission. Neither function adds a network endpoint.

```clojure
(require '[tolglow.agents.forms :as forms]
         '[tolglow.effects :as fx])

;; runtime is your running Tolglow show. Read each receipt before dependent writes.
@(forms/submit! runtime [:registry/publish :effects :tour/breath
                         {:template :breathing-wash :color [0.55 0.08 0.18]
                          :wave {:shape :triangle :period {:bars 2}}}])
@(forms/submit! runtime [:cue/put :tour/lull :tour/breath {:params {:level 0.45}}])
@(forms/submit! runtime [:cue/launch :tour/lull {:mode :one-shot :duration {:beats 3/2}}])
@(forms/submit! runtime [:param/set [:instance/of :tour/lull] :level [:local :peak]] {:peak 0.6})
```

Write normal Clojure functions in a trusted REPL. A function or function Var supplied as a value takes the captured immutable show model and returns a value. `[:local k]` looks up a value or resolver in the caller's explicit bindings. Named Clojure functions such as `fx/triangle` build the existing effect IR. The JVM invokes no text supplied in EDN, performs no symbol lookup and exposes no runtime handles to a resolver. The examples below use ordinary lexical scope.

```clojure
(let [period {:bars 2}
      level (fn [model] (* 0.75 (:master model)))]
  @(forms/submit! runtime [:registry/publish :effects :tour/focus
                          {:params {:level [:local :level]}
                           :outputs {:dimmer (fx/param :level)
                                     :focus (fx/triangle {:period period})}}]
                  {:level level}))
```

`:instance/of` chooses the latest running instance of the given cue by launch order. Use a concrete instance ID to address a releasing instance or an earlier concurrent launch. Publication choices `:effect`, `:wash`, `:breathing-wash` and `:aim-sweep` call the corresponding constructors in `tolglow.effects`. The existing registry validates defaults and options. Generated commands capture the current show epoch. Publication and cue replacement also carry the observed definition version.

| Form | Meaning |
| --- | --- |
| `[:registry/publish :effects id options]` | Register or update a compiled effect. |
| `[:cue/put cue-id effect-id options]` | Define a cue for the registry effect or an explicitly pinned effect version. Options are optional. |
| `[:cue/launch cue-id options]`, `[:effect/launch effect-id options]` | Launch with ordinary mode, targets, priority, params and musical duration. Options are optional. |
| `[:param/set instance param value]` | Edit a running instance. |
| `[:instance/opacity-set instance value]`, `[:instance/gain-set instance value]`, `[:instance/envelope-set instance envelope]` | Adjust an instance's output or response. |
| `[:instance/pause instance]`, `[:instance/resume instance]`, `[:instance/end instance]`, `[:instance/remove instance]`, `[:instance/opacity-toggle instance]` | Use normal instance lifecycle and system controls. |
| `[:cue/save-params cue-id instance]`, `[:cue/clear-saved-params cue-id]` | Store or clear the launch preset on a cue. |
| `[:group/apply selector action]` | Apply an instance action to the selected group. |
| `[:master/set value]`, `[:transport/set changes]`, `[:page/put id page]` | Change show settings or register a page with its cue cells. |

## Build examples

```sh
lein test tolglow.agents.forms-test
lein run -m clojure.main dev/model-lab/show/export.clj target/model-lab-show
```

The export starts isolated, unscheduled preview shows. It sends each compact form through the real show queue, advances the owner, checks the receipt and closes the runtime. Group scenarios also verify the selected member count. The trusted Clojure examples compile normally as part of the exporter; its templates produce the model's textual answer and the executable check from the same expression. Q/A records contain the applicable registry or meter context and derive their answers using the registry, demo show and Afterglow timing functions. Each output record keeps the scenario setup, source and family with the messages.

The `show/edn`, `show/clojure` and `show/qa` modes share family-level splits. Changing a fixture subset or a beat duration keeps a template in the same split. The resulting directories contain `train.jsonl`, `valid.jsonl`, `test.jsonl` and a manifest with realized counts and source digests. These examples use the local show API and demo rig as an owner-local corpus. Only the training split belongs in a training mix. Compare held-out responses on fresh preview shows, with physical output disabled.

The verified corpus contains 596 records across 29 scenario families. Through-water, gel and calibrated fog examples publish an effect, launch it and advance the preview owner. The dataset has 430 training, 80 validation and 86 test records. The generator verifies these scenarios independent of recorded studio sessions or model credits.
