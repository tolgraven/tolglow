(ns tolglow.integrations.agent
  "Bounded streaming tool loop. Provider text/tool proposals become validated
  tool data; a supplied dispatcher owns the shared show-control operations.
  Calling run! is an explicit I/O boundary, never a fixture rendering step."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [tolglow.integrations.providers :as providers])
  (:import [java.util UUID]))

(def ^:private id-schema {:type "string" :minLength 1 :maxLength 256})

(def ^:private page-offset {:type "integer" :minimum 0 :maximum 2147483647})

(def ^:private level-schema {:type "number" :minimum 0 :maximum 1})

(def ^:private musical-duration
  {:type "object" :description "One positive musical duration, for example {beats:0.5}."
   :properties (zipmap [:beats :bars :phrases] (repeat {:type "number" :minimum 0}))
   :additionalProperties false})

(def knob-tool
  "The `knob` tool (#667): propose, audition and reroll a generated knob mapping
  over every destination type, keep it, and edit kept knobs with the same
  :knob/* and :knob-candidate/* commands as the REPL and Studio. The show
  dispatcher translates it with `tolglow.knob-candidates/knob-tool-command`."
  {:name "knob"
   :description (str "Linked knobs: one 0–1 knob driving several typed destinations. "
                     "propose drafts a mapping across the running cues (optional seed, character energy|colour|motion, cue-ids, "
                     "size [min max] links 2–8, types to restrict destination types, include-knob-driven?, or from-knob to repeat a kept knob's recipe). "
                     "Destinations may be numbers, integers, colours, vectors, choices (enums, stepped through declared values) "
                     "and switches (booleans, switching at a knob threshold); stepped links smooth the knob with an envelope, so brief spikes do not change the step. "
                     "reroll tries the next seed with a different mapping. audition sweeps the draft in the Room at value 0–1 (live output unchanged). "
                     "keep turns the draft into a knob (optional knob-id, name, value); discard drops it. "
                     "link adds or replaces one link {id, destination {kind param|lfo|variable, cue-id, param, field|input}, range [from to], "
                     "direction up|down, curve, input [low high], quantize integer|{ratios preferred|free}|{step n}, "
                     "envelope {attack {beats n}, release {beats n}} (beats to cross the whole knob range)}: "
                     "on the draft, or on the kept knob named by knob-id. unlink drops a link by link-id the same way. "
                     "create, rename, set (value) and nudge (delta) act on the kept knob knob-id; set and nudge change live output. "
                     "remove, and unlink on a kept knob, work only on knobs this conversation created. "
                     "Read knobs and the draft, with resolved values and explanations, under knobs and knob-candidates in inspect_show.")
   :parameters {:type "object"
                :properties {:op {:type "string" :enum ["propose" "reroll" "audition" "keep" "discard" "link" "unlink"
                                                        "create" "rename" "remove" "set" "nudge"]}
                             :seed {:type "integer"} :character {:type "string" :enum ["energy" "colour" "motion"]}
                             :cue-ids {:type "array" :items id-schema :maxItems 64}
                             :size {:type "array" :items {:type "integer" :minimum 2 :maximum 8} :minItems 2 :maxItems 2}
                             :types {:type "array" :minItems 1 :maxItems 6
                                     :items {:type "string" :enum ["number" "integer" "colour" "vector" "choice" "switch"]}}
                             :include-knob-driven? {:type "boolean"}
                             :from-knob id-schema :knob-id id-schema
                             :name {:type "string" :minLength 1 :maxLength 128}
                             :value level-schema :delta {:type "number" :minimum -1 :maximum 1}
                             :link {:type "object"} :link-id id-schema}
                :required ["op"] :additionalProperties false}})

(def default-tools
  "Provider-neutral tool declarations for the studio's semantic dispatcher.
  Definitions/auto policy still undergo domain validation in that dispatcher."
  [{:name "inspect_show" :description "Read rig, registry, pages, active instances and auto policy."
    :parameters {:type "object" :properties {} :additionalProperties false}}
   {:name "publish_effect" :description "Validate and publish a declarative effect definition with stable id and increasing version."
    :parameters {:type "object" :properties {:definition {:type "object"}} :required ["definition"] :additionalProperties false}}
   {:name "put_cue" :description "Create or update a cue referencing a registered effect."
    :parameters {:type "object" :properties {:cue {:type "object"}} :required ["cue"] :additionalProperties false}}
   {:name "launch_cue" :description "Launch a cue persistently, held, once, or for a duration."
    :parameters {:type "object" :properties {:cue-id {:type "string"}
                                             :launch {:type "object"}}
                 :required ["cue-id"] :additionalProperties false}}
   {:name "stop_instance" :description "Release or force-stop an exact active instance, not a cue definition."
    :parameters {:type "object" :properties {:instance-id {:type "string"} :force? {:type "boolean"}}
                 :required ["instance-id"] :additionalProperties false}}
   {:name "set_parameter" :description "Set a bounded parameter on an exact active instance."
    :parameters {:type "object" :properties {:instance-id {:type "string"} :param {:type "string"} :value {}}
                 :required ["instance-id" "param" "value"] :additionalProperties false}}
   {:name "knob_candidate"
    :description (str "Generate, reroll, audition, edit, keep or discard one draft linked-knob mapping across the running cues. "
                      "generate takes an optional integer seed, character (energy, colour or motion), cue-ids to restrict the selection, "
                      "size as two link-count bounds from 2 to 8 in either order, include-knob-driven? true to also offer destinations existing knobs drive "
                      "(left out by default), or from-knob to repeat a kept knob's saved recipe. reroll tries the next seed with a different mapping. "
                      "audition stages the candidate in the Room at knob value 0–1; the live show is unchanged. "
                      "link replaces one candidate link by id and unlink drops one. keep creates an ordinary knob (optional knob-id, name, value); "
                      "discard drops the draft. Read the candidate, its explanation, or why too few parameters were eligible under knob-candidates in inspect_show.")
    :parameters {:type "object"
                 :properties {:op {:type "string" :enum ["generate" "reroll" "audition" "link" "unlink" "keep" "discard"]}
                              :seed {:type "integer"} :character {:type "string" :enum ["energy" "colour" "motion"]}
                              :cue-ids {:type "array" :items id-schema :maxItems 64}
                              :size {:type "array" :items {:type "integer" :minimum 2 :maximum 8} :minItems 2 :maxItems 2}
                              :include-knob-driven? {:type "boolean"}
                              :from-knob id-schema :value level-schema :knob-id id-schema
                              :name {:type "string" :minLength 1 :maxLength 128}
                              :link {:type "object"} :link-id id-schema}
                 :required ["op"] :additionalProperties false}}
   knob-tool
   {:name "inspect_lighting" :description "Discover patched and controlled lighting groups, eligible emitters, actual and target light levels, and current audio energy and fog measurements. Inspect before changing a group and again to check the measured outcome. Each list is paged independently. Optional group-id filters its members."
    :parameters {:type "object" :properties {:group-id id-schema :offset page-offset :fixture-offset page-offset
                                             :limit {:type "integer" :minimum 1 :maximum 64}}
                 :additionalProperties false}}
   {:name "set_lighting_scene"
    :description "Create, vary or release a group's owned look. Supply fixture-ids to create a custom group. Optional target-var binds a normalized show variable by path, e.g. [audio/energy], [fog/level] or [fog/observed-level]."
    :parameters {:type "object" :properties {:op {:type "string" :enum ["apply" "vary" "release"]}
                 :group-id id-schema :target {:type "number" :minimum 0 :maximum 1}
                 :brightness-reference {:type "string" :enum ["global" "local"]}
                 :fixture-ids {:type "array" :items id-schema :minItems 1}
                 :effect-ids {:type "array" :items id-schema}
                 :target-var {:type "array" :items id-schema :minItems 1 :maxItems 16}
                 :params {:type "object"} :allow-generated? {:type "boolean"}}
                 :required ["op" "group-id"] :additionalProperties false}}
   {:name "set_lighting_budget"
    :description "Limit combined emission across active cues. Omit group-id for the rig. limit is the fraction of full weighted output; max-active limits concurrent sources above standby. Proportional shares the available output and priority preserves preferred fixtures first."
    :parameters {:type "object"
                 :properties {:group-id id-schema :enabled? {:type "boolean"}
                              :limit {:type "number" :minimum 0 :maximum 1}
                              :max-active {:type "integer" :minimum 0}
                              :standby-level {:type "number" :minimum 0 :maximum 1}
                              :standby-mode {:type "string" :enum ["shed" "all"]}
                              :strategy {:type "string" :enum ["proportional" "priority"]}
                              :fixture-order {:type "array" :items id-schema}}
                 :additionalProperties false}}
   {:name "configure_energy_autopilot"
    :description "Steer average fixture brightness and visual activity. Edit only the controls you supply. enabled? switches the manual latch. A running mapped enable cue can also keep the conductor active. Use group-id for a patched group, page-id for the emitters on a cue page, or scope=global for the rig. interval sets decision timing, window sets averaging time, and duration sets one-shot cue length. audio-targets follows current audio energy for the named axes. To restore a fixed level, supply that axis in targets."
    :parameters {:type "object"
                 :properties {:enabled? {:type "boolean"} :group-id id-schema :page-id id-schema
                              :scope {:type "string" :enum ["global"]}
                              :targets {:type "object" :properties {:brightness level-schema :activity level-schema}
                                        :additionalProperties false}
                              :audio-targets {:type "array" :items {:type "string" :enum ["brightness" "activity"]}
                                              :minItems 1 :maxItems 2}
                              :interval musical-duration :window musical-duration :duration musical-duration
                              :strategy {:type "string" :enum ["gradual" "instant"]}
                              :preference {:type "string" :enum ["parameters" "cues" "generate" "author"]
                                           :description "generate builds a local one-shot. Choose author only when the performer requests ongoing provider-authored effects."}
                              :rotate? {:type "boolean"}}
                 :additionalProperties false}}
   {:name "configure_energy_scene"
    :description "Save or remove a persistent audio-energy crossing for a group. On rising above 'above', vary its look to target; another rise requires energy to fall back to 'below'. For save supply all three normalized numbers. Saving the binding does not start audio capture."
    :parameters {:type "object" :properties {:op {:type "string" :enum ["save" "remove"]} :group-id id-schema
                                             :below {:type "number" :minimum 0 :maximum 1}
                                             :above {:type "number" :minimum 0 :maximum 1}
                                             :target {:type "number" :minimum 0 :maximum 1}}
                 :required ["op" "group-id"] :additionalProperties false}}
   {:name "configure_auto"
    :description (str "Configure automatic show selection policy using validated cue ids, timing and constraints. "
                      "Without at it applies now. With at it waits on the beat clock and applies on that line, before auto decides there, "
                      "so a mood change lands on the downbeat: at {bar|beat|phrase} (zero-based transport position; one that passed less than a bar ago applies now, marked late; a bar or more is rejected), "
                      "or \"next-bar\", \"next-phrase\" or \"next-section\" (the end of the auto section now playing). "
                      "A pending change is listed by inspect_schedule and withdrawn with cancel_scheduled (in any later turn of this conversation).")
    :parameters {:type "object"
                 :properties {:configuration {:type "object"}
                              :at {:description "Zero-based position object {bar, beat, phrase} or one of next-bar, next-phrase, next-section."}}
                 :required ["configuration"] :additionalProperties false}}
   {:name "schedule_events"
    :description (str "Post up to 64 show events ahead of time on the beat clock in one atomic batch; one invalid event rejects all. "
                      "Each event is an action {op, ...} with optional id, label and one timing key: at {bar|beat|phrase} (zero-based transport position), "
                      "in {beats|bars|phrases} from now, or grid {bars:1} for the next boundary. Ops: cue/launch {cue-id, launch}, instance/end {instance-id}, "
                      "param/set {instance-id|cue-id, param, value}, param/ramp {instance-id|cue-id, param, to, over {beats:8}, curve} "
                      "(target opacity|variable instead of param; a cue-id addresses every running instance of that cue), variable/set {variable, value}, macro/run {macro-id}. "
                      "Master and blackout stay with the human operator and cannot be scheduled. A position already passed is rejected. "
                      "Curves: linear, ease-in, ease-out, ease-in-out, sine, smoothstep, step. A ramp starts from the current value; the show animates it.")
    :parameters {:type "object" :properties {:events {:type "array" :minItems 1 :maxItems 64 :items {:type "object"}}}
                 :required ["events"] :additionalProperties false}}
   {:name "cancel_scheduled"
    :description "Cancel pending scheduled events and running ramps by event-ids, label, destination {target, instance-id, param}, or mine?=true for everything this run posted. A cancelled ramp holds its current value."
    :parameters {:type "object" :properties {:event-ids {:type "array" :items id-schema} :label {:type "string"}
                                             :destination {:type "object"} :mine? {:type "boolean"}}
                 :additionalProperties false}}
   {:name "inspect_schedule"
    :description "Compact read-back of the beat clock: current beat/bar, upcoming events [due-beat id op label], active ramps [id destination progress value to end-beat] and recent outcomes [id status reason]."
    :parameters {:type "object" :properties {} :additionalProperties false}}])

(defn- field [m k] (if (contains? m k) (get m k) (get m (name k))))

(defn- key-name [k] (if (keyword? k) (name k) (str k)))

(defn- portable? [x]
  (cond (or (nil? x) (string? x) (keyword? x) (boolean? x)) true
        (number? x) (Double/isFinite (double x))
        (vector? x) (every? portable? x)
        (map? x) (every? (fn [[k v]] (and (or (string? k) (keyword? k)) (portable? v))) x)
        :else false))

(defn validate-arguments
  "Validate the supported JSON Schema subset: type, properties, required,
  additionalProperties=false, items, enum, minimum/maximum, string and array
  lengths. Return path/message errors. Domain/compiler validation remains the
  dispatcher's job. Unsupported schema constructs fail closed."
  ([schema value] (validate-arguments schema value []))
  ([schema value path]
   (let [type (field schema :type)
         allowed-keys #{"type" "description" "title" "properties" "required" "additionalProperties"
                        "items" "enum" "minimum" "maximum" "minLength" "maxLength" "minItems" "maxItems"}
         unsupported (remove allowed-keys (map key-name (keys schema)))
         type-valid? (case type
                       nil true "object" (map? value) "array" (vector? value) "string" (string? value)
                       "number" (and (number? value) (Double/isFinite (double value)))
                       "integer" (integer? value) "boolean" (instance? Boolean value) "null" (nil? value) false)
         error (fn [message] {:path path :message message})]
     (vec
      (concat
       (when-not (portable? value) [(error "Tool arguments must be finite JSON data")])
       (when (seq unsupported) [(error (str "Unsupported tool schema keys: " (str/join ", " unsupported)))])
       (when (and (some? (field schema :additionalProperties))
                  (not (instance? Boolean (field schema :additionalProperties))))
         [(error "Only boolean additionalProperties is supported")])
       (when-not type-valid? [(error (str "Expected JSON type " type))])
       (when (and (field schema :enum) (not (some #(= value %) (field schema :enum)))) [(error "Value is outside enum")])
       (when (and type-valid? (map? value))
         (let [properties (or (field schema :properties) {})
               by-name (into {} (map (fn [[k v]] [(key-name k) v])) properties)
               value-keys (set (map key-name (keys value)))]
           (concat
            (for [required (field schema :required) :when (not (value-keys (key-name required)))]
              {:path (conj path required) :message "Required tool argument is missing"})
            (mapcat (fn [[k v]]
                      (if-let [property (get by-name (key-name k))]
                        (validate-arguments property v (conj path k))
                        (when (false? (field schema :additionalProperties))
                          [{:path (conj path k) :message "Unknown tool argument"}]))) value))))
       (when (and type-valid? (vector? value))
         (concat
          (when-let [items (field schema :items)]
            (mapcat (fn [[i v]] (validate-arguments items v (conj path i))) (map-indexed vector value)))
          (when (and (field schema :minItems) (< (count value) (field schema :minItems))) [(error "Too few items")])
          (when (and (field schema :maxItems) (> (count value) (field schema :maxItems))) [(error "Too many items")])) )
       (when (and type-valid? (string? value))
         (concat (when (and (field schema :minLength) (< (count value) (field schema :minLength))) [(error "String is too short")])
                 (when (and (field schema :maxLength) (> (count value) (field schema :maxLength))) [(error "String is too long")])))
       (when (and type-valid? (number? value))
         (concat (when (and (field schema :minimum) (< value (field schema :minimum))) [(error "Number is below minimum")])
                 (when (and (field schema :maximum) (> value (field schema :maximum))) [(error "Number is above maximum")]))))))))

(defn- require-running! [cancelled?]
  (when @cancelled? (throw (ex-info "Agent run cancelled" {:agent/error :cancelled}))))

(defn- emit-event! [emit! cancelled? event]
  (try (emit! event)
       (catch Exception exception
         (reset! cancelled? true)
         (throw (ex-info "Agent event consumer disconnected" {:agent/error :cancelled} exception)))))

(defn- provider-round! [config credential messages tools transport emit! cancelled? options round]
  (let [req (providers/request (assoc config :stream? true :tools tools) messages credential)
        stream (atom (providers/new-stream (:provider config)))
        text (StringBuilder.)
        errors (atom [])
        consume! (fn [events]
                   (doseq [event events]
                     (when (= :text/delta (:type event)) (.append text ^String (:text event)))
                     (when (= :error (:type event)) (swap! errors conj (:error event)))
                     (emit-event! emit! cancelled? (assoc event :round round))))
        response (transport req
                            (fn [chunk]
                              (require-running! cancelled?)
                              (let [parsed (providers/stream-feed @stream chunk)]
                                (reset! stream (:state parsed))
                                (consume! (remove #(= :done (:type %)) (:events parsed)))))
                            (assoc (select-keys options [:timeout-ms :connect-timeout-ms :max-response-chars])
                                   :cancelled? cancelled?))]
    (when-not (and (integer? (:status response)) (<= 200 (:status response) 299))
      (throw (ex-info "Provider returned an unsuccessful HTTP status"
                      {:agent/error :provider-http :status (:status response)})))
    (let [finished (providers/finish-stream @stream)]
      (reset! stream (:state finished))
      (consume! (:events finished)))
    (when (seq @errors) (throw (ex-info "Provider stream failed" {:agent/error :provider-stream :errors @errors})))
    {:text (str text) :tools (providers/completed-tools @stream)}))

(defn- execute-tool! [call definitions executed dispatch! emit! cancelled? metadata]
  (require-running! cancelled?)
  (let [{:keys [id name input error]} call
        declaration (get definitions name)
        problems (cond error [{:path [] :message "Provider tool arguments are invalid JSON"}]
                       (not (and (string? id) (not (str/blank? id)))) [{:path [:id] :message "Tool call needs an ID"}]
                       (nil? declaration) [{:path [:name] :message "Tool is not declared for this session"}]
                       :else (validate-arguments (:parameters declaration) input))
        previous (get executed id)
        same-call? (and previous (= name (:name previous)) (= input (:input previous)))
        result (cond
                 (seq problems) {:ok? false :error :invalid-tool-call :errors problems}
                 same-call? (:result previous)
                 previous {:ok? false :error :tool-id-conflict}
                 :else
                 (do
                   (emit-event! emit! cancelled? {:type :tool/start :id id :name name :arguments input})
                   (try
                     (dispatch! name input (assoc metadata :tool-call-id id
                                                  :command-id (str (:run-id metadata) "/" id)))
                     (catch Exception exception
                       {:ok? false :error :tool-failed :message (.getMessage exception)}))))]
    (require-running! cancelled?)
    (when-not (portable? result)
      (throw (ex-info "Tool dispatcher must return portable JSON result data" {:agent/error :invalid-tool-result :tool name})))
    (emit-event! emit! cancelled? {:type :tool/result :id id :name name :result result :replayed? (boolean same-call?)})
    {:executed (if (or previous (seq problems)) executed (assoc executed id {:name name :input input :result result}))
     :message {:role :tool :tool-call-id (or id (str "invalid-" (:round metadata))) :name name :content result}}))

(defn run!
  "Run a bounded provider/tool conversation. Tools are generic declarations.
  dispatch! [tool-name args metadata] executes validated semantic operations;
  emit! receives streamed text and tool/result events. options: :transport
  (defaults to providers/http-transport!), :cancelled? atom, :run-id,
  :conversation-id (passed to dispatch! in metadata), :max-rounds
  (8), :max-tool-calls (32), and HTTP limits. No operation runs at namespace load.
  Same tool IDs/arguments within a run replay their prior result, never execute
  twice. Changed arguments with a reused ID are rejected. Model calls remain
  off the render loop and root dispatch retains final domain authorization."
  ([config credential messages tools dispatch! emit!]
   (run! config credential messages tools dispatch! emit! {}))
  ([config credential messages tools dispatch! emit! options]
   (let [cancelled? (or (:cancelled? options) (atom false))
         run-id (or (:run-id options) (str (UUID/randomUUID)))
         max-rounds (get options :max-rounds 8) max-tools (get options :max-tool-calls 32)
         transport (get options :transport providers/http-transport!)
         definitions (into {} (map (juxt :name identity)) tools)
         progress (atom {:messages messages :rounds 0 :tool-calls 0})]
     (when-not (and (every? #(and (integer? %) (pos? %)) [max-rounds max-tools])
                    (instance? clojure.lang.IAtom cancelled?) (ifn? dispatch!) (ifn? emit!)
                    (= (count definitions) (count tools))
                    (every? #(and (string? (:name %)) (map? (:parameters %))) tools))
       (throw (ex-info "Invalid agent limits, tools, dispatcher, or cancellation token" {})))
     (try
       (emit-event! emit! cancelled? {:type :agent/start :run-id run-id :provider (:provider config) :model (:model config)})
       (loop [conversation messages round 0 tool-count 0 executed {}]
         (require-running! cancelled?)
         (when (>= round max-rounds)
           (throw (ex-info "Agent round limit reached" {:agent/error :round-limit})))
         (let [response (provider-round! config credential conversation tools transport emit! cancelled? options round)
               calls (:tools response)
               _ (when (some #(or (:error %) (not (and (string? (:id %)) (not (str/blank? (:id %)))))) calls)
                   (throw (ex-info "Provider returned invalid tool JSON or identity" {:agent/error :invalid-tool-call})))
               assistant {:role :assistant :content (:text response)
                          :tool-calls (mapv (fn [call] {:id (:id call) :name (:name call) :arguments (:input call)}) calls)}
               conversation (conj (vec conversation) assistant)]
           (reset! progress {:messages conversation :rounds (inc round) :tool-calls tool-count})
           (if (empty? calls)
             (let [result {:status :completed :run-id run-id :text (:text response)
                           :messages conversation :rounds (inc round) :tool-calls tool-count}]
               (emit-event! emit! cancelled? {:type :agent/done :run-id run-id :status :completed})
               result)
             (do
               (when (> (+ tool-count (count calls)) max-tools)
                 (throw (ex-info "Agent tool-call limit reached" {:agent/error :tool-limit})))
               (let [processed (reduce (fn [{:keys [executed messages]} call]
                                         (let [result (execute-tool! call definitions executed dispatch! emit! cancelled?
                                                                     (cond-> {:run-id run-id :round round}
                                                                       (:conversation-id options)
                                                                       (assoc :conversation-id (:conversation-id options))))]
                                           {:executed (:executed result) :messages (conj messages (:message result))}))
                                       {:executed executed :messages []} calls)]
                 (reset! progress {:messages (into conversation (:messages processed))
                                   :rounds (inc round) :tool-calls (+ tool-count (count calls))})
                 (recur (:messages @progress) (inc round)
                        (+ tool-count (count calls)) (:executed processed)))))))
       (catch Exception exception
         (let [data (ex-data exception)
               status (if (or @cancelled? (= :cancelled (:provider/error data))) :cancelled :error)
               result (merge @progress {:status status :run-id run-id :error (or (:agent/error data) (:provider/error data) :agent-failed)
                                        :message (.getMessage exception)})]
           (when-not @cancelled? (emit-event! emit! cancelled? (assoc result :type :agent/error)))
           result))))))
