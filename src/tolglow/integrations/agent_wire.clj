(ns tolglow.integrations.agent-wire
  "The normative wire form a model emits (#863). The schemas live in
  resources/agent/wire/ and are loaded from there: the on-clock proposer reply,
  the off-clock author, configure, move and inspect strict tools, and the
  cross-rig translation choice. Every property is required, every object is
  closed and only keywords both Claude structured outputs and
  tolglow.integrations.agent/validate-arguments accept appear in them.

  A reply passes in order: enum values lowercased (providers do not guarantee
  enum casing), validate-arguments against the wire schema, the owner's bounds
  (counts and ranges the wire cannot carry), then lowering of each proposal's
  events to the schedule_events vocabulary. The show owner remains the
  authority on identifiers and on the basis revision. Nothing here opens a
  connection; request builders return data for an injected transport."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.integrations.agent :as agent]
            [tolglow.integrations.providers :as providers])
  (:import [java.util Locale]))

;; Schemas ------------------------------------------------------------------

(defn read-ordered-json
  "Parse JSON text with string keys, keeping each object's declared key order
  (as array maps), so a schema is sent with the order it was written in.
  Structured outputs emit properties in schema order."
  [text]
  (let [n (atom 0)
        tagged (json/read-str text :key-fn (fn [k] [(swap! n inc) k]))
        order (fn order [x]
                (cond (map? x) (apply array-map (mapcat (fn [[[_ k] v]] [k (order v)]) (sort-by ffirst x)))
                      (vector? x) (mapv order x)
                      :else x))]
    (order tagged)))

(def schema-ids
  "Every shipped wire schema."
  [:proposal :author :configure :move :inspect :translation-choice])

(defn schema-resource [id] (str "agent/wire/" (name id) ".wire.schema.json"))

(defn example-resource [id] (str "agent/wire/examples/" (name id) ".wire.json"))

(defn resource-text [path]
  (if-let [url (io/resource path)]
    (slurp url)
    (throw (ex-info "Missing agent wire resource" {:path path}))))

(def ^:private loaded
  (delay (into {} (map (fn [id] [id (read-ordered-json (resource-text (schema-resource id)))])) schema-ids)))

(defn schema
  "The wire schema for id, with string keys in declared order."
  [id]
  (or (get @loaded id) (throw (ex-info "Unknown agent wire schema" {:id id}))))

(defn example
  "A shipped example reply for id, parsed as a provider reply is (keyword keys)."
  [id]
  (json/read-str (resource-text (example-resource id)) :key-fn keyword))

;; Static limits ----------------------------------------------------------------

(def claude-limits
  "Claude structured outputs' explicit limits per request, across every strict
  schema in it (output_config.format and each strict tool): 20 strict tools,
  24 optional parameters (each property not in its object's required list) and
  16 parameters with union types (anyOf or a type array, so nullable fields
  count). Source:
  https://platform.claude.com/docs/en/build-with-claude/structured-outputs#schema-complexity-limits,
  read 2026-11-28 (recorded in docs/research/native-agent/model.md). Claude also
  has an internal compiled-grammar size limit and a 180 s compile timeout,
  which cannot be checked offline: one live compile per provider of the final
  schemas is a recorded manual check at #862 bench time."
  {:strict-tools 20 :optional-parameters 24 :union-parameters 16})

(def owner-bound-keywords
  "Keywords validate-arguments accepts but Claude structured outputs reject
  (numeric and length bounds, array bounds beyond minItems 0 or 1). The wire
  carries none; the owner applies them after the wire pass (owner-bounds)."
  #{"minimum" "maximum" "minLength" "maxLength" "minItems" "maxItems"})

(def wire-keywords
  "Keywords a wire schema node may use: validate-arguments' set without bounds."
  (apply disj agent/schema-keywords owner-bound-keywords))

(def request-shapes
  "Each request a backend sends. :output-format sends one schema as the reply
  format; :strict-tools declares each schema as a strict tool with tool_choice
  auto. :every-strict-tool is the upper bound of all six declared together."
  {:proposer {:mode :output-format :schemas [:proposal]}
   :off-clock {:mode :strict-tools :schemas [:author :configure :move :inspect]}
   :translation {:mode :output-format :schemas [:translation-choice]}
   :every-strict-tool {:mode :strict-tools :schemas schema-ids}})

(defn- lowercase-enum-value? [v]
  (and (string? v) (= v (.toLowerCase ^String v Locale/ROOT)) (not (re-find #"\s" v))))

(def ^:private empty-stats
  {:nodes 0 :objects 0 :properties 0 :enum-values 0 :optional 0 :unions 0 :max-depth 0 :violations []})

(defn- combine [a b]
  (-> (merge-with + (dissoc a :max-depth :violations) (dissoc b :max-depth :violations))
      (assoc :max-depth (max (:max-depth a) (:max-depth b))
             :violations (into (:violations a) (:violations b)))))

(defn- node-stats [node path depth]
  (if-not (map? node)
    (assoc empty-stats :violations [{:path path :problem :not-a-schema}])
    (let [type (get node "type")
          container? (contains? #{"object" "array"} type)
          props (get node "properties")
          required (get node "required")
          union? (or (vector? type) (some #(contains? node %) ["anyOf" "oneOf"]))
          problems (concat
                    (for [k (sort (map str (keys node))) :when (not (wire-keywords k))]
                      {:path path :problem (if (owner-bound-keywords k) :owner-bound :unsupported-keyword) :keyword k})
                    (when-not (string? type) [{:path path :problem :type-not-single :type type}])
                    (when (= "object" type)
                      (concat
                       (when-not (false? (get node "additionalProperties")) [{:path path :problem :open-object}])
                       (when-not (map? props) [{:path path :problem :no-properties}])
                       (for [r required :when (not (contains? props r))]
                         {:path path :problem :required-unknown :property r})))
                    (when (and (= "array" type) (not (map? (get node "items"))))
                      [{:path path :problem :untyped-items}])
                    (for [v (get node "enum") :when (not (lowercase-enum-value? v))]
                      {:path path :problem :enum-casing :value v}))
          own {:nodes 1 :objects (if (= "object" type) 1 0) :properties (count props)
               :enum-values (count (get node "enum"))
               :optional (if (map? props) (count (remove (set required) (keys props))) 0)
               :unions (if union? 1 0)
               :max-depth (if container? depth 0)
               :violations (vec problems)}
          child-depth (if container? (inc depth) depth)
          children (concat (when (map? props) (for [[k child] props] [child (conj path "properties" k)]))
                           (when (map? (get node "items")) [[(get node "items") (conj path "items")]])
                           (for [k ["anyOf" "oneOf" "allOf"] [i branch] (map-indexed vector (get node k))]
                             [branch (conj path k i)]))]
      (reduce combine own (map (fn [[child p]] (node-stats child p child-depth)) children)))))

(defn schema-stats
  "Static counts for one schema: nodes, objects, properties, enum values,
  optional and union-typed parameters, maximum container depth (the root
  object is 1) and every keyword, type, closure or enum-casing violation."
  [schema]
  (node-stats schema [] 1))

(defn check-schemas
  "Count named schemas sent together in one request of mode (:output-format or
  :strict-tools) against claude-limits. Strict tools count only in
  :strict-tools mode; optional and union parameters count across all schemas.
  Objects, properties, enum values and depth are reported as grammar-size
  proxies. :ok? is false when a limit is exceeded or any node breaks the wire
  keyword subset, is open, lacks a single type or has a non-lowercase enum."
  [mode named-schemas]
  (let [per (into (sorted-map) (map (fn [[id s]] [id (schema-stats s)])) named-schemas)
        total (reduce combine empty-stats (vals per))
        tool-names (map name (keys named-schemas))
        counts {:strict-tools (if (= mode :strict-tools) (count named-schemas) 0)
                :optional-parameters (:optional total)
                :union-parameters (:unions total)}
        exceeded (vec (for [[k limit] (sort claude-limits) :when (> (get counts k) limit)]
                        {:limit k :count (get counts k) :max limit}))
        violations (vec (concat
                         (for [[id s] per v (:violations s)] (assoc v :schema id))
                         (for [n tool-names :when (not (re-matches #"[a-zA-Z0-9_-]{1,64}" n))]
                           {:schema n :problem :tool-name})))]
    (merge counts
           (select-keys total [:objects :properties :enum-values :max-depth])
           {:mode mode
            :per-schema (into (sorted-map) (map (fn [[id s]] [id (dissoc s :violations)])) per)
            :exceeded exceeded :violations violations
            :ok? (and (empty? exceeded) (empty? violations))})))

(defn check-request
  "check-schemas for one of request-shapes with the shipped schemas."
  [shape]
  (let [{:keys [mode schemas]} (or (get request-shapes shape)
                                   (throw (ex-info "Unknown agent wire request shape" {:shape shape})))]
    (check-schemas mode (into (array-map) (map (fn [id] [id (schema id)])) schemas))))

(defn report
  "check-request for every request shape. A REPL view of the static count."
  []
  (into (sorted-map) (map (fn [shape] [shape (check-request shape)])) (keys request-shapes)))

(def ^:private checked (delay (report)))

(defn- require-within-limits! [shape]
  (let [result (get @checked shape)]
    (when-not (:ok? result)
      (throw (ex-info "Agent wire schemas exceed the structured-output limits or keyword subset"
                      (select-keys result [:exceeded :violations]))))))

;; Requests ------------------------------------------------------------------------

(defn- output-name [id] (str "tolglow_" (str/replace (name id) "-" "_")))

(defn strict-tool
  "A provider-neutral strict tool declaration for a wire schema."
  [id]
  (let [s (schema id)]
    {:name (name id) :description (get s "description") :parameters s :strict true}))

(def ^:private sampling-keys [:temperature :top-p :top-k :tool-choice])

(def reply-token-floor
  "The smallest :max-tokens a reply request accepts, by schema. A six-event
  wire proposal is about 1,140 output tokens (docs/research/native-agent/model.md),
  so three alternatives are about 3,400; a translation choice is a few hundred.
  Thinking counts toward max_tokens on current Claude models, so callers add
  headroom above the floor (proposer-max-tokens)."
  {:proposal 3400 :translation-choice 400})

(def proposer-max-tokens
  "A proposer reply's suggested :max-tokens: three six-event alternatives
  (about 3,400 tokens) plus thinking headroom."
  8192)

(defn- base-config [config]
  (when-let [present (seq (filter #(contains? config %) sampling-keys))]
    (throw (ex-info "Wire requests send no sampling parameters and no forced tool choice"
                    {:keys (vec present)})))
  (assoc config :stream? (get config :stream? false)))

(defn reply-request
  "Request one structured reply in wire schema id (:proposal or
  :translation-choice): Anthropic output_config.format, or a strict
  response_format for OpenAI-compatible providers and OpenRouter. config is a
  providers/request config without sampling parameters or tools and with an
  explicit :max-tokens of at least reply-token-floor; several alternatives
  come in one reply. An optional :effort sets Anthropic output_config.effort
  where the model accepts that level (providers/effort-levels). The request is
  not streamed unless :stream? is true; read-reply reads either form."
  [config id messages credential]
  (require-within-limits! (if (= id :proposal) :proposer :translation))
  (when (contains? config :tools)
    (throw (ex-info "A reply request carries only its reply schema; declare tools with tools-request" {})))
  (let [floor (get reply-token-floor id)
        max-tokens (:max-tokens config)]
    (when-not (and (integer? max-tokens) (>= max-tokens floor))
      (throw (ex-info (str "A " (name id) " reply request needs an explicit :max-tokens of at least " floor
                           " (thinking counts toward it; proposer-max-tokens is " proposer-max-tokens ")")
                      {:max-tokens max-tokens :floor floor}))))
  (providers/request (assoc (base-config config) :output-schema {:name (output-name id) :schema (schema id)})
                     messages credential))

(defn tools-request
  "Request with the off-clock wire schemas declared as strict tools and the
  default (auto) tool choice. OpenAI-compatible providers also get
  parallel_tool_calls false, since strict schemas are not guaranteed for
  parallel calls there. Tool calls still pass accept before dispatch."
  [config messages credential]
  (require-within-limits! :off-clock)
  (providers/request (assoc (base-config config)
                            :tools (mapv strict-tool (:schemas (:off-clock request-shapes)))
                            :parallel-tool-calls false)
                     messages credential))

;; Replies -------------------------------------------------------------------------

(defn- key-name [k] (if (keyword? k) (name k) (str k)))

(defn lower-enums
  "Lowercase every string the schema constrains with an enum, wherever the
  reply follows the schema's shape. Other strings, and keys the schema does
  not declare, are left for validation to judge."
  [schema value]
  (let [props (get schema "properties")
        items (get schema "items")]
    (cond
      (and (get schema "enum") (string? value)) (.toLowerCase ^String value Locale/ROOT)
      (and (map? value) (map? props))
      (reduce-kv (fn [m k v] (assoc m k (if-let [p (get props (key-name k))] (lower-enums p v) v))) {} value)
      (and (vector? value) (map? items)) (mapv #(lower-enums items %) value)
      :else value)))

(defn parse-reply
  "Decode reply text as JSON data with keyword keys, or return path errors."
  [text]
  (try {:ok? true :value (json/read-str text :key-fn keyword)}
       (catch Exception _ {:ok? false :code :not-json :errors [{:path [] :message "Reply is not one JSON document"}]})))

(defn- streamed-stop
  "{:stop :category :refusal} from a streamed reply's events: the last finish
  reason for the first choice (Anthropic message_delta with its refusal
  category, OpenAI finish_reason) and any OpenAI refusal text."
  [events]
  (let [first-choice? #(contains? #{nil 0} (:choice %))
        finished (last (filter #(and (= :choice/finished (:type %)) (first-choice? %)) events))
        refusal (->> events (filter #(and (= :refusal/delta (:type %)) (first-choice? %))) (map :text) (apply str))]
    {:stop (:reason finished) :category (get-in finished [:stop-details :category]) :refusal (not-empty refusal)}))

(defn read-reply
  "Turn a providers/invoke! result for a reply request into reply data, from a
  non-streamed :response or a streamed reply's :events. A reply cut off at
  max_tokens (OpenAI finish_reason length) is :truncated, a safety refusal
  (Anthropic stop_reason refusal, OpenAI content_filter or a message refusal)
  is :refused and any other stop is :unexpected-stop, each before parsing:
  such output need not match the schema. Thinking blocks are not reply text.
  Returns {:ok? true :value} or {:ok? false :code :errors}."
  [{:keys [ok? text response events error]}]
  (let [anthropic? (contains? response :stop_reason)
        choice (get-in response [:choices 0])
        streamed (when-not response (streamed-stop events))
        stop (cond streamed (:stop streamed) anthropic? (:stop_reason response) :else (:finish_reason choice))
        category (if streamed (:category streamed) (get-in response [:stop_details :category]))
        refusal (if streamed (:refusal streamed) (get-in choice [:message :refusal]))
        refused (fn [detail] {:ok? false :code :refused
                              :errors [{:path [] :message (str "The provider refused the request" (when detail (str ": " detail)))}]})]
    (cond
      (not ok?) {:ok? false :code :provider-error :errors [{:path [] :message "The provider call failed"}]
                 :error (or error (:error (first (filter #(= :error (:type %)) events))))}
      (#{"max_tokens" "length"} stop)
      {:ok? false :code :truncated :errors [{:path [] :message "The reply stopped at max_tokens before it was complete"}]}
      (= "refusal" stop) (refused category)
      (or (= "content_filter" stop) (string? refusal)) (refused refusal)
      (not (#{"end_turn" "stop"} stop))
      {:ok? false :code :unexpected-stop :errors [{:path [] :message (str "The reply ended with stop reason " (pr-str stop))}]}
      :else (parse-reply text))))

(defn accept
  "Lower enum casing, then validate against wire schema id with
  validate-arguments. Returns {:ok? :value :errors}."
  [id reply]
  (let [s (schema id)
        value (lower-enums s reply)
        errors (agent/validate-arguments s value)]
    {:ok? (empty? errors) :value value :errors errors}))

;; Owner bounds -----------------------------------------------------------------

(def ^:private position-fields ["phrase" "bar" "beat"])

(def owner-bounds
  "Counts and ranges the wire cannot carry, by schema: each entry is a path of
  property names (:items for an array's items) and validate-arguments bounds.
  The owner checks them after the wire pass, which is where the schema stops
  and validation starts."
  {:proposal
   (into [[["proposals"] {"minItems" 1 "maxItems" 3}]
          [["proposals" :items "confidence"] {"minimum" 0 "maximum" 1}]
          [["proposals" :items "note"] {"maxLength" 240}]
          [["proposals" :items "basis_rev"] {"minimum" 0}]
          [["proposals" :items "groups" :items "intent"] {"maxItems" 4}]
          [["proposals" :items "groups" :items "until"] {"maxItems" 1}]
          [["proposals" :items "groups" :items "guard"] {"maxItems" 4}]
          [["proposals" :items "events"] {"maxItems" 64}]
          [["proposals" :items "events" :items "select" "ids"] {"maxItems" 1024}]
          [["proposals" :items "events" :items "rgb"] {"maxItems" 3}]
          [["proposals" :items "events" :items "params" :items "rgb"] {"maxItems" 3}]
          [["proposals" :items "events" :items "over" "n"] {"minimum" 0}]
          [["proposals" :items "events" :items "launch_length" "n"] {"minimum" 0}]]
         (concat
          (for [f position-fields] [["proposals" :items "events" :items "timing" f] {"minimum" 0}])
          (for [f position-fields] [["proposals" :items "groups" :items "until" :items f] {"minimum" 0}])))
   :translation-choice
   [[["confidence"] {"minimum" 0 "maximum" 1}]
    [["alternatives"] {"maxItems" 2}]
    [["alternatives" :items "confidence"] {"minimum" 0 "maximum" 1}]]
   :move
   (vec (for [f position-fields] [["move" :items "timing" f] {"minimum" 0}]))
   :inspect
   [[["inspect" :items "offset"] {"minimum" 0}]
    [["inspect" :items "limit"] {"minimum" 1 "maximum" 64}]]})

(defn- schema-path [path]
  (vec (mapcat #(if (= :items %) ["items"] ["properties" %]) path)))

(defn bounded-schema
  "Wire schema id with owner-bounds added: the owner's validation schema."
  [id]
  (reduce (fn [s [path bounds]]
            (let [p (schema-path path)]
              (when-not (map? (get-in s p))
                (throw (ex-info "Owner bound names no wire schema node" {:id id :path path})))
              (update-in s p merge bounds)))
          (schema id) (get owner-bounds id)))

(defn check-bounds
  "Path errors for a wire-valid value that breaks the owner's bounds."
  [id value]
  (agent/validate-arguments (bounded-schema id) value))

;; Lowering ----------------------------------------------------------------------

(def ^:private units {"phrase" :phrases "bar" :bars "beat" :beats})

(defn- nonzero-units [{:keys [phrase bar beat]}]
  (into (array-map) (for [[k v] [["phrase" phrase] ["bar" bar] ["beat" beat]] :when (not (zero? v))] [(units k) v])))

(defn- lower-timing
  "[timing-map errors] for a wire timing. at sums its fields as a zero-based
  position; in and grid take one nonzero unit (a duration has exactly one
  unit); now posts without timing (the next owner cycle)."
  [{:keys [kind phrase bar beat] :as timing} path]
  (case kind
    "at" [{:at (or (not-empty (into (array-map) (for [[k v] [[:phrase phrase] [:bar bar] [:beat beat]] :when (not (zero? v))] [k v])))
                   {:beat 0})} []]
    ("in" "grid") (let [d (nonzero-units timing)]
                    (cond (> (count d) 1)
                          [nil [{:path path :message (str kind " takes one nonzero unit of phrase, bar or beat")}]]
                          (and (= "grid" kind) (empty? d))
                          [nil [{:path path :message "grid needs a positive length"}]]
                          :else [{(keyword kind) (or (not-empty d) {:beats 0})} []]))
    "now" [{} []]))

(defn- slot
  "[value errors] for a value_kind slot: the owner reads only that slot."
  [{:keys [value_kind num rgb flag choice]} path]
  (case value_kind
    "num" [num []]
    "rgb" (if (= 3 (count rgb)) [rgb []] [nil [{:path (conj path :rgb) :message "An rgb value has three numbers"}]])
    "flag" [flag []]
    "choice" [choice []]
    "none" [nil [{:path (conj path :value_kind) :message "This op needs a value; value_kind is none"}]]))

(defn- musical [{:keys [unit n]}] (when (not= "none" unit) {(keyword unit) n}))

(def ^:private selector-ops
  #{"cue/launch" "effect/launch" "instance/end" "instance/kill" "instance/opacity"})

(defn- lower-event
  "Lower one wire event to the schedule_events vocabulary. Returns
  {:event domain :errors [...]}; identifiers stay strings for the owner."
  [group-ids path {:keys [id group op timing select target name over curve launch_mode launch_length params]
                   :as event}]
  (let [[timing-map timing-errors] (lower-timing timing (conj path :timing))
        {:keys [ids categories pick]} select
        ;; The owner reads a present :ids, even an empty one, as "only these":
        ;; a categories-only selection omits it or it would match nothing.
        selector (cond-> {:pick pick} (seq ids) (assoc :ids ids) (seq categories) (assoc :categories categories))
        needs-select? (or (selector-ops op) (and (#{"param/set" "param/ramp"} op) (#{"param" "opacity"} target)))
        [value value-errors] (if (#{"param/set" "param/ramp" "instance/opacity" "variable/set"} op)
                               (slot event path) [nil []])
        rows (for [[i {row-name :name kind :kind :as row}] (map-indexed vector params)]
               (let [[v errs] (slot (assoc row :value_kind kind) (conj path :params i))]
                 [[(keyword row-name) v] errs]))
        destination (case target
                      "param" {:target "param" :param name}
                      "opacity" {:target "opacity"}
                      "variable" {:target "variable" :variable name}
                      nil)
        errors (concat
                timing-errors value-errors (mapcat second rows)
                (when-not (or (= "" group) (group-ids group))
                  [{:path (conj path :group) :message "Names no group header in this proposal"}])
                (when (and needs-select? (empty? ids) (empty? categories))
                  [{:path (conj path :select) :message "This op needs select ids or categories"}])
                (when (and (#{"macro/run" "macro/stop"} op) (not= 1 (count ids)))
                  [{:path (conj path :select :ids) :message "A macro op names exactly one macro ID"}])
                (when (and (#{"param/set" "param/ramp"} op) (nil? destination))
                  [{:path (conj path :target) :message "param/set and param/ramp need target param, opacity or variable"}])
                (when (and (or (= "variable/set" op)
                               (and (#{"param/set" "param/ramp"} op) (#{"param" "variable"} target)))
                           (str/blank? name))
                  [{:path (conj path :name) :message "This destination needs a name"}])
                (when (and (= "param/ramp" op) (nil? (musical over)))
                  [{:path (conj path :over) :message "A ramp needs a musical length"}]))
        domain (when (empty? errors)
                 (merge
                  {:op op}
                  ;; A blank ID lets the owner number the event within its command.
                  (when-not (str/blank? id) {:id id})
                  (when-not (= "" group) {:label group})
                  timing-map
                  (case op
                    ("cue/launch" "effect/launch")
                    (let [launch (cond-> {}
                                   (not= "none" launch_mode) (assoc :mode launch_mode)
                                   (musical launch_length) (assoc :duration (musical launch_length))
                                   (seq rows) (assoc :params (into {} (map first) rows)))]
                      (cond-> {:select selector} (seq launch) (assoc :launch launch)))
                    ("instance/end" "instance/kill") {:select selector}
                    "instance/opacity" {:select selector :value value}
                    "param/set" (cond-> (assoc destination :value value) needs-select? (assoc :select selector))
                    "param/ramp" (cond-> (assoc destination :to value :over (musical over) :curve curve)
                                   needs-select? (assoc :select selector))
                    "variable/set" {:variable name :value value}
                    ("macro/run" "macro/stop") {:macro-id (first ids)})))]
    {:event domain :errors (vec errors)}))

(defn- lower-group [{:keys [group intent rank optional on_failure check until guard]}]
  {:group group :intent intent :rank rank :optional? optional :on-failure (keyword on_failure)
   :check (keyword check) :until (first until)
   :guard (mapv (fn [g] (-> (select-keys g [:kind :subject :op :num :text :flag :for_bars])
                            (update :kind keyword) (update :op keyword))) guard)})

(defn lower-proposal
  "Lower one wire proposal (at index i of the reply) to its domain parts:
  schedule_events events, group headers and cancellations as data. Group
  guards, expiry and cancellation by intent are carried as data for the
  proposal contract (#826); only events reach the scheduler today. The basis
  is the host's, not the reply's echo (admit-proposals checks the echo).
  Errors carry the wire path."
  [i {:keys [id confidence note cancel groups events]}]
  (let [path [:proposals i]
        group-ids (set (map :group groups))
        duplicate-groups (for [[g n] (frequencies (map :group groups)) :when (> n 1)] g)
        lowered (map-indexed (fn [j e] (lower-event group-ids (conj path :events j) e)) events)
        errors (vec (concat
                     (for [g duplicate-groups] {:path (conj path :groups) :message (str "Group " (pr-str g) " is declared twice")})
                     (mapcat :errors lowered)))]
    {:id id :confidence confidence :note note
     :cancel (mapv #(select-keys % [:ids :group :intent]) cancel)
     :groups (mapv lower-group groups)
     :events (mapv :event lowered)
     :errors errors}))

;; Basis -------------------------------------------------------------------------

(defn- source-key [source] (select-keys source [:adapter :session-id]))

(defn- own [source entries]
  (filter #(= (source-key source) (source-key (:source %))) entries))

(defn- plan-entry
  "What a basis keeps of one pending event: its ID, op, anchor and the action
  it was posted with (timing spec, target, value). The due beat is kept only for
  an absolute event; a relative one moves with every clock rebase, including a
  Link phase nudge, without its plan changing."
  [entry]
  (cond-> (dissoc entry :due-beat :step :steps :late-beats :posted-beat :source :category :origin)
    (= :absolute (:anchor entry)) (assoc :due-beat (:due-beat entry))))

(defn pending-plan
  "source's own plan in one committed model, read through the shared schedule
  projection: :upcoming holds a plan-entry per pending event and :ramps the
  event IDs of its running ramps, both in event ID order."
  [model source]
  (let [view (subscribers/schedule-view model)]
    {:upcoming (->> (own source (:upcoming view)) (map plan-entry) (sort-by (comp str :event-id)) vec)
     :ramps (->> (own source (:ramps view)) (map :event-id) (sort-by str) vec)}))

(defn plan-changes
  "How source's plan in model departs from a recorded pending-plan, ignoring
  the plan simply proceeding. An event that fired (or a ramp that started or
  completed) is the plan proceeding; a relative event whose due beat moved
  with the clock is unchanged. A change is a recorded event removed without
  firing (cancelled, missed, rejected or out of the recent window), an own
  event that was not in the record, or a recorded event whose absolute due
  beat or posted action changed. Returns [{:event-id :change}] with :change
  :removed, :added or :changed, in event ID order."
  [recorded model source]
  (let [view (subscribers/schedule-view model)
        now (into {} (map (juxt :event-id identity)) (map plan-entry (own source (:upcoming view))))
        running (set (map :event-id (own source (:ramps view))))
        proceeded (into running (comp (filter #(#{:fired :completed} (:status %))) (map :event-id))
                        (own source (:recent view)))
        known (into (set (map :event-id (:upcoming recorded))) (:ramps recorded))]
    (->> (concat
          (for [{:keys [event-id] :as entry} (:upcoming recorded)]
            (cond (contains? now event-id) (when (not= entry (get now event-id)) {:event-id event-id :change :changed})
                  (proceeded event-id) nil
                  :else {:event-id event-id :change :removed}))
          (for [event-id (:ramps recorded) :when (not (proceeded event-id))]
            {:event-id event-id :change :removed})
          (for [event-id (concat (keys now) running) :when (not (known event-id))]
            {:event-id event-id :change :added}))
         (remove nil?) distinct (sort-by (comp str :event-id)) vec)))

(defn request-basis
  "The basis the host records for one proposer request, from the committed
  model the observation was frozen from: the observation token sent to the
  model, that model's revision, the show epoch, the auto session, the source
  the request is for and that source's own pending plan (pending-plan;
  orchestration.md, \"basis\")."
  [model source obs]
  (let [source (source-key source)]
    (when-not (and (keyword? (:adapter source)) (some? (:session-id source)))
      (throw (ex-info "A request basis needs a source with :adapter and :session-id" {:source source})))
    {:obs obs :revision (:revision model) :epoch (:epoch model)
     :auto-session (get-in model [:auto :state :owner-id])
     :source source
     :pending (pending-plan model source)}))

(defn request-record
  "What the host keeps for a request it sent, keyed by request-id: the basis
  (with the source it was taken for) is read from here, never from the model's
  reply. An optional :deadline-beat is the transport beat by which the answer
  must be arbitrated; it holds while the transport is stopped, since the beat
  does. The bounded, expiring store of records belongs to the orchestrator
  (#826)."
  ([request-id basis] (request-record request-id basis {}))
  ([request-id basis {:keys [deadline-beat]}]
   (when-not (and (string? request-id) (not (str/blank? request-id)) (map? basis) (map? (:source basis)))
     (throw (ex-info "A request record needs a request ID and a basis taken for a source" {})))
   (when-not (or (nil? deadline-beat)
                 (and (number? deadline-beat) (Double/isFinite (double deadline-beat)) (not (neg? deadline-beat))))
     (throw (ex-info "A request deadline is a nonnegative transport beat" {:deadline-beat deadline-beat})))
   (cond-> {:request-id request-id :basis basis}
     deadline-beat (assoc :deadline-beat (double deadline-beat)))))

(defn check-basis
  "Errors when an answer to record can no longer be used against model: the
  transport reached the record's :deadline-beat (:late), or the show moved past
  the basis in a way that matters to the answer (:stale-basis): another epoch,
  another auto session, or a change to the source's own plan (plan-changes).
  The plan proceeding, unrelated commands that only bump the show revision and
  clock rebases of relative events do not make a basis stale (orchestration.md);
  the arbiter guards the window between arbitration and commit with
  post-proposal!'s expected revision."
  [record model]
  (let [{:keys [source obs] :as basis} (:basis record)
        id (:request-id record)
        now (request-basis model source obs)
        beat (double (get-in model [:transport :beat] 0))
        deadline (:deadline-beat record)]
    (vec (concat
          (when (and deadline (>= beat deadline))
            [{:path [:deadline-beat] :code :late
              :message (str "Request " id " was due by beat " deadline "; the transport is at beat " beat)}])
          (for [k [:epoch :auto-session] :when (not= (get basis k) (get now k))]
            {:path [:basis k] :code :stale-basis
             :message (str "The " (name k) " changed since request " id)})
          (for [{:keys [event-id change]} (plan-changes (:pending basis) model source)]
            {:path [:basis :pending] :code :stale-basis :event-id event-id :change change
             :message (str "Own event " (pr-str event-id) " was " (name change) " since request " id)})))))

(defn- id-errors [proposals]
  (concat
   (for [[i {:keys [id]}] (map-indexed vector proposals)
         :when (not (re-matches #"[A-Za-z0-9_.:-]{1,64}" id))]
     {:path [:proposals i :id] :message "A proposal ID is 1 to 64 letters, digits, '.', ':', '_' or '-'"})
   (let [seen (frequencies (map :id proposals))]
     (for [[i {:keys [id]}] (map-indexed vector proposals) :when (> (seen id) 1)]
       {:path [:proposals i :id] :message (str "Proposal ID " (pr-str id) " is used twice in this reply")}))))

(defn- basis-errors [record proposals]
  (let [{:keys [obs revision]} (:basis record)]
    (concat
     (for [[i p] (map-indexed vector proposals) :when (not= obs (:basis_obs p))]
       {:path [:proposals i :basis_obs] :code :basis-mismatch
        :message (str "Request " (:request-id record) " sent observation " (pr-str obs))})
     (for [[i p] (map-indexed vector proposals) :when (not= revision (:basis_rev p))]
       {:path [:proposals i :basis_rev] :code :basis-mismatch
        :message (str "Request " (:request-id record) " sent revision " revision)}))))

(defn admit-proposals
  "The whole wire pass for a parsed proposer reply to the request in record
  (request-record): enum lowercasing, validate-arguments, owner bounds, the
  reply's basis echo against the host's record, then lowering. Every
  alternative shares the request's basis; exclusivity among them is the
  arbiter's (one alternative per source per window). Returns {:ok? true
  :proposals [...]} with the host's basis on each, or {:ok? false :stage
  (:wire :bounds :basis :lowering) :errors}."
  [record reply]
  (let [{:keys [ok? value errors]} (accept :proposal reply)]
    (if-not ok?
      {:ok? false :stage :wire :errors errors}
      (let [bound-errors (check-bounds :proposal value)
            echo-errors (when (empty? bound-errors) (vec (basis-errors record (:proposals value))))]
        (cond
          (seq bound-errors) {:ok? false :stage :bounds :errors bound-errors}
          (seq echo-errors) {:ok? false :stage :basis :errors echo-errors}
          :else
          (let [proposals (vec (map-indexed lower-proposal (:proposals value)))
                lowering-errors (vec (concat (id-errors (:proposals value)) (mapcat :errors proposals)))]
            (if (seq lowering-errors)
              {:ok? false :stage :lowering :errors lowering-errors}
              {:ok? true
               :proposals (mapv #(-> % (dissoc :errors) (assoc :basis (:basis record) :request-id (:request-id record)))
                                proposals)})))))))

(defn- owner-errors [i ack]
  (let [index (get-in ack [:details :index])]
    (cond
      (integer? index) [{:path [:proposals i :events index] :message (:message ack)}]
      :else [{:path [:proposals i] :message (:message ack)}])))

(defn post-proposal!
  "Post lowered proposal i's events as one atomic schedule_events batch through
  dispatch! [tool arguments metadata] (show/dispatch-tool! on a runtime).
  expected-revision is the revision of the snapshot the arbiter checked the
  basis against (check-basis); the owner refuses the batch as :stale-revision
  if any command committed after it. The owner also refuses unknown IDs and
  passed positions; a refusal gains :errors with the wire path it names. A
  reply's :command-id names each alternative's command as
  <command-id>/<proposal id>. A proposal without events posts nothing."
  [dispatch! i proposal expected-revision metadata]
  (when-not (and (integer? expected-revision) (not (neg? expected-revision)))
    (throw (ex-info "post-proposal! needs the arbitrated snapshot's revision" {:expected-revision expected-revision})))
  (if (empty? (:events proposal))
    {:status :accepted :event-ids []}
    (let [ack (dispatch! "schedule_events" {:events (:events proposal)}
                         (cond-> (assoc metadata :expected-revision expected-revision)
                           (:command-id metadata) (update :command-id str "/" (:id proposal))))]
      (cond-> ack (= :rejected (:status ack)) (assoc :errors (owner-errors i ack))))))
