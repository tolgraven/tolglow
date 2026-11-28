(ns tolglow.agent-goals
  "Operator goals for a show-control agent (#857): what the night, a set, a
  section or a scene should feel like, in the operator's words, with optional
  palette keyframes and an energy curve on musical positions.

  Goals are show data saved under the show's :extensions, so show saves and
  full backups keep them. One validated message, `:agent-goal`, puts, edits,
  reorders and removes them for the REPL and Studio alike:

    @(show/submit! runtime :agent-goal
       {:op :put :goal {:scope :set :text \"Ember reds drifting to violet\"
                        :palette [{:at {:phrase 0} :colors [[0.9 0.1 0.02]]}
                                  {:at {:phrase 12} :colors [[0.35 0.02 0.3]]}]}})

  Every saved goal is active; at most eight are saved. Text is data: nothing
  here reads it, and a goal holds no capability, quota or command. An agent
  session cannot write goals. Positions are zero-based transport positions
  ({:beat :bar :phrase}, added together, as the scheduler's :at), never
  milliseconds. See docs/repl-studio-parity.md#tell-the-agent-what-the-night-should-feel-like."
  (:require [clojure.string :as str]
            [afterglow.time :as time]))

(def state-key
  "Saved under the show's :extensions, so show saves and full backups keep it."
  ::goals)

(def state-path [:extensions state-key])

(def max-active 8)

(def max-text
  "Characters (Unicode code points) in a goal's text."
  600)

(def scopes [:night :set :section :scene])

(def operations #{:put :edit :reorder :remove})

(def ^:private max-palette 16)

(def ^:private max-colors 8)

(def ^:private max-energy 32)

(def ^:private max-author 64)

(def ^:private goal-keys #{:id :scope :text :from :until :palette :energy :order :by})

(def ^:private optional-keys #{:from :until :palette :energy})

(def ^:private stored-keys (conj goal-keys :revision))

(def ^:private id-pattern #"[A-Za-z0-9][A-Za-z0-9._:-]{0,63}")

(defn- refuse!
  ([reason message] (refuse! reason message {}))
  ([reason message details]
   (throw (ex-info message {::reason reason :details details}))))

(defn- text-length [^String s] (.codePointCount s 0 (.length s)))

(defn- wire-name [x] (cond (keyword? x) (name x) (string? x) x))

(defn- goal-id
  "Goal IDs are text, so JSON, EDN keyword and string forms agree."
  [id]
  (let [s (wire-name id)]
    (when (and s (re-matches id-pattern s)) s)))

(defn- scope-of [scope]
  (let [k (some-> scope wire-name keyword)]
    (when (some #{k} scopes) k)))

(defn- unit-fraction? [x] (and (time/finite-number? x) (<= 0 x 1)))

;; ---------------------------------------------------------------------------
;; Positions

(defn- position? [at]
  (and (map? at) (seq at) (every? #{:beat :bar :phrase} (keys at))
       (every? #(and (time/finite-number? %) (not (neg? %))) (vals at))))

(defn- millisecond-keys? [at]
  (and (map? at) (some #{:ms :time-ms :seconds :s :millis} (keys at))))

(defn position-beat
  "Beat of a zero-based {:beat :bar :phrase} position in meter."
  [at meter]
  (let [{:keys [beats-per-bar bars-per-phrase]} (time/normalize-meter meter)]
    (+ (:beat at 0) (* (:bar at 0) beats-per-bar) (* (:phrase at 0) beats-per-bar bars-per-phrase))))

(defn- check-position! [field at]
  (when (millisecond-keys? at)
    (refuse! :agent-goal/position-not-musical
             "Goal positions are musical: use :beat, :bar or :phrase, not milliseconds" {:field field :at at}))
  (when-not (position? at)
    (refuse! :agent-goal/invalid-position
             "A goal position needs nonnegative :beat, :bar and/or :phrase coordinates" {:field field :at at}))
  at)

(defn- check-increasing! [field points meter]
  (let [beats (mapv #(position-beat (:at %) meter) points)]
    (when-not (apply < beats)
      (refuse! :agent-goal/unordered-keyframes
               (str "Goal " (name field) " keyframes need strictly increasing positions") {:field field}))))

;; ---------------------------------------------------------------------------
;; Validation

(defn- check-text! [text]
  (when-not (and (string? text) (not (str/blank? text)))
    (refuse! :agent-goal/missing-text "A goal needs the operator's words as text"))
  (let [n (text-length text)]
    (when (> n max-text)
      (refuse! :agent-goal/text-too-long
               (str "A goal's text is at most " max-text " characters; this one has " n)
               {:length n :limit max-text})))
  text)

(defn- check-palette! [palette meter]
  (when-not (and (vector? palette) (<= 1 (count palette) max-palette))
    (refuse! :agent-goal/invalid-palette (str "A goal palette is 1 to " max-palette " keyframes")))
  (doseq [[i {:keys [at colors] :as keyframe}] (map-indexed vector palette)]
    (when-not (and (map? keyframe) (every? #{:at :colors} (keys keyframe)))
      (refuse! :agent-goal/invalid-palette "A palette keyframe has :at and :colors only" {:index i}))
    (check-position! :palette at)
    (when-not (and (vector? colors) (<= 1 (count colors) max-colors)
                   (every? #(and (vector? %) (= 3 (count %)) (every? unit-fraction? %)) colors))
      (refuse! :agent-goal/invalid-palette
               (str "A palette keyframe has 1 to " max-colors " linear RGB colours with components in 0…1")
               {:index i})))
  (check-increasing! :palette palette meter)
  palette)

(defn- check-energy! [energy meter]
  (when-not (and (vector? energy) (<= 1 (count energy) max-energy))
    (refuse! :agent-goal/invalid-energy (str "A goal energy curve is 1 to " max-energy " points")))
  (doseq [[i {:keys [at level] :as point}] (map-indexed vector energy)]
    (when-not (and (map? point) (every? #{:at :level} (keys point)))
      (refuse! :agent-goal/invalid-energy "An energy point has :at and :level only" {:index i}))
    (check-position! :energy at)
    (when-not (unit-fraction? level)
      (refuse! :agent-goal/invalid-energy "An energy level is in 0…1" {:index i :level level})))
  (check-increasing! :energy energy meter)
  energy)

(defn- check-range! [{:keys [from until]} meter]
  (some->> from (check-position! :from))
  (some->> until (check-position! :until))
  (when (and from until (not (< (position-beat from meter) (position-beat until meter))))
    (refuse! :agent-goal/empty-range "A goal's :from position must come before its :until position"
             {:from from :until until})))

(defn- check-author! [by]
  (when-not (and (string? by) (not (str/blank? by)) (<= (text-length by) max-author))
    (refuse! :agent-goal/invalid-author (str "A goal's author is text of at most " max-author " characters")))
  by)

(defn- normalize-goal
  "Check a complete goal (without order or revision) and return it in stored form."
  [goal meter]
  (when-not (map? goal) (refuse! :agent-goal/invalid-goal "A goal is a map"))
  (when-let [unknown (seq (remove goal-keys (keys goal)))]
    (refuse! :agent-goal/unknown-field (str "Unknown goal field " (pr-str (first unknown))) {:fields (vec unknown)}))
  (let [scope (scope-of (:scope goal))
        goal (into {} (remove (fn [[k v]] (and (optional-keys k) (nil? v)))) goal)]
    (when-not scope
      (refuse! :agent-goal/invalid-scope "A goal's scope is :night, :set, :section or :scene" {:scope (:scope goal)}))
    (check-text! (:text goal))
    (check-range! goal meter)
    (some-> (:palette goal) (check-palette! meter))
    (some-> (:energy goal) (check-energy! meter))
    (check-author! (:by goal))
    (-> (select-keys goal [:id :text :from :until :palette :energy :by])
        (assoc :scope scope))))

;; ---------------------------------------------------------------------------
;; State

(def ^:private empty-state {:goals {} :next 1})

(defn stored
  "The saved goal state: {:goals {id goal} :next n}. The same object while no
  goal changes, so a cache can key on it by identity."
  [model]
  (get-in model state-path empty-state))

(defn- in-order [goals] (sort-by (juxt :order :id) (vals goals)))

(defn- renumber
  "Orders 1…n in the given order."
  [ordered]
  (into {} (map-indexed (fn [i goal] [(:id goal) (assoc goal :order (inc i))])) ordered))

(defn- move [goals id order]
  (let [others (vec (remove #(= id (:id %)) (in-order goals)))
        index (dec (min order (inc (count others))))]
    (renumber (concat (subvec others 0 index) [(get goals id)] (subvec others index)))))

(defn valid-saved-state?
  "Portable structure for show validation: at most eight goals that each pass
  the command's checks, numbered 1…n in order."
  [{:keys [goals next] :as state} & [meter]]
  (boolean
   (try
     (and (map? state) (every? #{:goals :next} (keys state)) (map? goals) (pos-int? next)
          (<= (count goals) max-active)
          (= (set (range 1 (inc (count goals)))) (set (map :order (vals goals))))
          (every? (fn [[id goal]]
                    (and (map? goal) (every? stored-keys (keys goal))
                         (= id (goal-id id) (:id goal)) (pos-int? (:revision goal)) (keyword? (:scope goal))
                         (= (dissoc goal :order :revision) (normalize-goal (dissoc goal :order :revision) meter))))
                  goals))
     (catch clojure.lang.ExceptionInfo _ false))))

;; ---------------------------------------------------------------------------
;; The command

(defn- agent-source? [source] (and (map? source) (#{:agent "agent"} (:adapter source))))

(defn- default-author [source]
  (case (some-> (:adapter source) wire-name)
    ("web" "studio") "studio"
    nil "operator"
    (wire-name (:adapter source))))

(defn- existing! [goals id]
  (or (get goals id)
      (refuse! :agent-goal/unknown-goal (str "No goal " (pr-str id)) {:id id})))

(defn- check-expected! [command goal]
  (when (and (contains? command :expected-revision) (not= (:expected-revision command) (:revision goal)))
    (refuse! :agent-goal/stale "That goal changed since it was shown. Look again and retry"
             {:id (:id goal) :revision (:revision goal)})))

(defn- check-order! [order n]
  (when-not (and (integer? order) (<= 1 order n))
    (refuse! :agent-goal/invalid-order (str "A goal's order is 1 to " n) {:order order}))
  order)

(defn- next-id [{:keys [goals next]}]
  (loop [n next]
    (let [id (str "g-" n)]
      (if (contains? goals id) (recur (inc n)) [id (inc n)]))))

(defn- put [{:keys [goals] :as state} {:keys [goal source] :as command} meter]
  (when-not (map? goal) (refuse! :agent-goal/invalid-goal "Put needs a :goal map"))
  (let [requested (:id goal)
        id (when (some? requested)
             (or (goal-id requested)
                 (refuse! :agent-goal/invalid-id "A goal ID is 1 to 64 letters, digits, '.', '_', ':' or '-'"
                          {:id requested})))
        current (get goals id)
        _ (when current (check-expected! command current))
        _ (when (and (nil? current) (>= (count goals) max-active))
            (refuse! :agent-goal/too-many
                     (str "At most " max-active " goals are active. Remove one before adding another")
                     {:limit max-active}))
        [id next] (if id [id (:next state)] (next-id state))
        order (:order goal)
        n (cond-> (count goals) (nil? current) inc)
        goal (normalize-goal (merge {:by (or (:by current) (default-author source))}
                                    (into {} (remove (comp nil? val)) (dissoc goal :order)) {:id id}) meter)
        stored (assoc goal :order (or (:order current) n) :revision (inc (:revision current 0)))
        goals (cond-> (assoc goals id stored)
                (some? order) (move id (check-order! order n)))]
    [(assoc state :goals goals :next next) id]))

(defn- edit [{:keys [goals] :as state} {:keys [id changes] :as command} meter]
  (let [id (goal-id id)
        current (existing! goals id)]
    (check-expected! command current)
    (when-not (and (map? changes) (seq changes))
      (refuse! :agent-goal/invalid-goal "Edit needs a :changes map naming the fields to change"))
    (when (some #{:id :order} (keys changes))
      (refuse! :agent-goal/invalid-goal "Edit cannot change a goal's ID or order; use :reorder"))
    (let [goal (normalize-goal (merge (dissoc current :order :revision) changes) meter)]
      [(assoc-in state [:goals id] (assoc goal :order (:order current) :revision (inc (:revision current))))
       id])))

(defn- reorder [{:keys [goals] :as state} {:keys [id order] :as command}]
  (let [id (goal-id id)
        current (existing! goals id)]
    (check-expected! command current)
    [(assoc state :goals (move goals id (check-order! order (count goals)))) id]))

(defn- remove-goal [{:keys [goals] :as state} {:keys [id] :as command}]
  (let [id (goal-id id)
        current (existing! goals id)]
    (check-expected! command current)
    [(assoc state :goals (renumber (remove #(= id (:id %)) (in-order goals)))) id]))

(declare active)

(defn apply-command
  "Apply one :agent-goal message to model. Returns {:model :ack}; a refusal
  leaves the model unchanged and its ack names a :reason and a :message."
  [model {:keys [source] :as command}]
  (try
    (when (agent-source? source)
      (refuse! :agent-goal/operator-only "Goals are the operator's; an agent session cannot write them"))
    (let [op (some-> (:op command) wire-name keyword)
          meter (get-in model [:transport :meter])
          state (stored model)
          [next-state id] (case op
                            :put (put state command meter)
                            :edit (edit state command meter)
                            :reorder (reorder state command)
                            :remove (remove-goal state command)
                            (refuse! :agent-goal/unknown-operation
                                     "A goal command's :op is :put, :edit, :reorder or :remove" {:op (:op command)}))
          next-model (-> (if (seq (:goals next-state))
                           (assoc-in model state-path next-state)
                           (update model :extensions dissoc state-key))
                         (update :revision (fnil inc 0)))]
      {:model next-model
       :ack {:status :accepted :revision (:revision next-model) :op op :goal-id id
             :goals (mapv :id (active next-model))}})
    (catch clojure.lang.ExceptionInfo error
      (if-let [reason (::reason (ex-data error))]
        {:model model :ack {:status :rejected :reason reason :message (ex-message error)
                            :details (:details (ex-data error))}}
        (throw error)))))

;; ---------------------------------------------------------------------------
;; Projections

(defn active
  "Active goals in order, as stored maps. Pure; read by the agent
  observation and every surface."
  [model]
  (vec (in-order (:goals (stored model)))))

(defn projection
  "The :agent-goals entry of the show snapshot."
  [model]
  {:active (active model) :limit max-active :text-limit max-text :scopes scopes})

(defn- wire-position [at] (into {} (map (fn [[k v]] [(name k) v])) at))

(defn observation
  "Active goals as the `goals` section of `tolglow-agent-obs/1`: JSON-shaped
  maps with text keys, in order. Positions keep their musical units."
  [model]
  (mapv (fn [{:keys [id scope by order text from until palette energy]}]
          (cond-> {"id" id "scope" (name scope) "by" by "order" order "text" text}
            from (assoc "from" (wire-position from))
            until (assoc "until" (wire-position until))
            palette (assoc "palette" (mapv (fn [{:keys [at colors]}] {"at" (wire-position at) "colors" colors}) palette))
            energy (assoc "energy" (mapv (fn [{:keys [at level]}] {"at" (wire-position at) "level" level}) energy))))
        (active model)))
