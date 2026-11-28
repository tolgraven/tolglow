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
  here reads it, and a goal holds no capability, quota or command. Only human
  sources (the REPL and Studio) write goals, and the author (:by) comes from
  the source. Positions are zero-based transport positions ({:beat :bar
  :phrase}, added together, as the scheduler's :at), never milliseconds.
  Ordering that depends on the meter (a range, keyframe order, keyframes
  inside the range) is checked when a goal is written, never when a show is
  saved, so a later meter change cannot make the show unsavable.
  See docs/repl-studio-parity.md#tell-the-agent-what-the-night-should-feel-like."
  (:require [afterglow.time :as time]
            [tolglow.agent-goal-text :as goal-text]))

(def state-key
  "Saved under the show's :extensions, so show saves and full backups keep it."
  ::goals)

(def state-path [:extensions state-key])

(def max-active 8)

(def max-text
  "Characters (Unicode code points) in a goal's text."
  600)

(def max-author 64)

(def scopes [:night :set :section :scene])

(def operations #{:put :edit :reorder :remove})

(def ^:private max-palette 16)

(def ^:private max-colors 8)

(def ^:private max-energy 32)

(def ^:private goal-keys #{:id :scope :text :from :until :palette :energy :order :by})

(def ^:private optional-keys #{:from :until :palette :energy})

(def ^:private stored-keys (conj goal-keys :revision))

(def ^:private routing-keys
  "Envelope keys the owner path may carry beside a goal command."
  #{:op :source :command-id :epoch :show-id :expected-epoch :runtime-id})

(def ^:private command-keys
  {:put #{:goal :expected-revision}
   :edit #{:id :changes :expected-revision}
   :reorder #{:id :order :expected-revision}
   :remove #{:id :expected-revision}})

(def ^:private human-adapters
  "Sources that may write goals: the REPL and Studio's listener."
  #{:repl :web :studio})

(def ^:private id-pattern #"[A-Za-z0-9][A-Za-z0-9._:-]{0,63}")

(defn- refuse!
  ([reason message] (refuse! reason message {}))
  ([reason message details]
   (throw (ex-info message {::reason reason :details details}))))

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
;; Text

(defn- checked-text!
  "NFC text that passes the shared goal-text rules, or a refusal."
  [s limit missing-reason message field]
  (let [s (when (string? s) (goal-text/normalize s))]
    (when-let [[problem details] (goal-text/problem s limit)]
      (case problem
        :missing (refuse! missing-reason message {:field field})
        :invalid-character
        (refuse! :agent-goal/invalid-character
                 (str "A goal's " (name field) " cannot hold control, bidirectional, invisible or tag characters ("
                      (goal-text/code-point-text (:code-point details)) ")")
                 (assoc details :field field))
        :too-long
        (refuse! (if (= :text field) :agent-goal/text-too-long :agent-goal/invalid-author)
                 (str "A goal's " (name field) " is at most " limit " characters; this one has " (:length details))
                 (assoc details :field field))))
    s))

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
    ;; Doubles: coordinates may mix ratios, decimals and BigDecimals.
    (+ (double (:beat at 0)) (* (double (:bar at 0)) beats-per-bar) (* (double (:phrase at 0)) beats-per-bar bars-per-phrase))))

(defn- check-position! [field at]
  (when (millisecond-keys? at)
    (refuse! :agent-goal/position-not-musical
             "Goal positions are musical: use :beat, :bar or :phrase, not milliseconds" {:field field :at at}))
  (when-not (position? at)
    (refuse! :agent-goal/invalid-position
             "A goal position needs nonnegative :beat, :bar and/or :phrase coordinates" {:field field :at at}))
  at)

;; ---------------------------------------------------------------------------
;; Validation

(defn- check-palette! [palette]
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
  palette)

(defn- check-energy! [energy]
  (when-not (and (vector? energy) (<= 1 (count energy) max-energy))
    (refuse! :agent-goal/invalid-energy (str "A goal energy curve is 1 to " max-energy " points")))
  (doseq [[i {:keys [at level] :as point}] (map-indexed vector energy)]
    (when-not (and (map? point) (every? #{:at :level} (keys point)))
      (refuse! :agent-goal/invalid-energy "An energy point has :at and :level only" {:index i}))
    (check-position! :energy at)
    (when-not (unit-fraction? level)
      (refuse! :agent-goal/invalid-energy "An energy level is in 0…1" {:index i :level level})))
  energy)

(defn- check-ordering!
  "Checks that depend on the meter, made only when a goal is written."
  [{:keys [from until palette energy]} meter]
  (let [beat #(position-beat % meter)]
    (when (and from until (not (< (beat from) (beat until))))
      (refuse! :agent-goal/empty-range "A goal's :from position must come before its :until position"
               {:from from :until until}))
    (doseq [[field points] [[:palette palette] [:energy energy]] :when points]
      (let [beats (mapv (comp beat :at) points)]
        (when-not (apply < beats)
          (refuse! :agent-goal/unordered-keyframes
                   (str "Goal " (name field) " keyframes need strictly increasing positions") {:field field}))
        (when (or (and from (< (first beats) (beat from))) (and until (> (peek beats) (beat until))))
          (refuse! :agent-goal/keyframe-outside-range
                   (str "Goal " (name field) " keyframes lie within its :from–:until range") {:field field}))))))

(defn- normalize-goal
  "Check a complete goal (without order or revision) and return it in stored
  form. With a meter, also check the ordering that depends on it."
  [goal meter]
  (when-not (map? goal) (refuse! :agent-goal/invalid-goal "A goal is a map"))
  (when-let [unknown (seq (remove goal-keys (keys goal)))]
    (refuse! :agent-goal/unknown-field (str "Unknown goal field " (pr-str (first unknown))) {:fields (vec unknown)}))
  (let [scope (scope-of (:scope goal))
        goal (into {} (remove (fn [[k v]] (and (optional-keys k) (nil? v)))) goal)]
    (when-not scope
      (refuse! :agent-goal/invalid-scope "A goal's scope is :night, :set, :section or :scene" {:scope (:scope goal)}))
    (let [text (checked-text! (:text goal) max-text :agent-goal/missing-text "A goal needs the operator's words as text" :text)
          by (checked-text! (:by goal) max-author :agent-goal/invalid-author "A goal names its author" :by)]
      (some->> (:from goal) (check-position! :from))
      (some->> (:until goal) (check-position! :until))
      (some-> (:palette goal) check-palette!)
      (some-> (:energy goal) check-energy!)
      (when meter (check-ordering! goal meter))
      (-> (select-keys goal [:id :from :until :palette :energy])
          (assoc :scope scope :text text :by by)))))

;; ---------------------------------------------------------------------------
;; State

(def ^:private empty-state {:goals {} :next 1})

(defn stored
  "The saved goal state: {:goals {id goal} :next n}. The same object while no
  goal changes, so a cache can key on it by identity. :next survives an empty
  list, so goal IDs are never reused within a show."
  [model]
  (get-in model state-path empty-state))

(defn- in-order [goals] (sort-by (juxt :order :id) (vals goals)))

(defn- renumber
  "Orders 1…n in the given sequence."
  [ordered]
  (into {} (map-indexed (fn [i goal] [(:id goal) (assoc goal :order (inc i))])) ordered))

(defn- move [goals id order]
  (let [others (vec (remove #(= id (:id %)) (in-order goals)))
        index (dec (min order (inc (count others))))]
    (renumber (concat (subvec others 0 index) [(get goals id)] (subvec others index)))))

(defn valid-saved-state?
  "Portable structure for show validation: at most eight goals, each with
  checked fields and text, numbered 1…n in order. Meter-dependent ordering
  is the command's check, so a meter change never invalidates a save."
  [{:keys [goals next] :as state}]
  (boolean
   (try
     (and (map? state) (every? #{:goals :next} (keys state)) (map? goals) (pos-int? next)
          (<= (count goals) max-active)
          (= (set (range 1 (inc (count goals)))) (set (map :order (vals goals))))
          (every? (fn [[id goal]]
                    (and (map? goal) (every? stored-keys (keys goal))
                         (= id (goal-id id) (:id goal)) (pos-int? (:revision goal)) (keyword? (:scope goal))
                         (= (dissoc goal :order :revision) (normalize-goal (dissoc goal :order :revision) nil))))
                  goals))
     (catch clojure.lang.ExceptionInfo _ false))))

;; ---------------------------------------------------------------------------
;; The command

(defn- adapter-of [source] (some-> (:adapter source) wire-name keyword))

(defn- check-source!
  "Only a human source writes goals: the REPL (no source, or :repl) or
  Studio's listener (:web, which the listener assigns)."
  [source]
  (when (some? source)
    (when-not (and (map? source) (every? #{:adapter :session-id} (keys source))
                   (contains? human-adapters (adapter-of source)))
      (refuse! :agent-goal/operator-only
               "Goals are the operator's: only the REPL and Studio may write them" {:source source}))))

(defn- repl-source? [source] (or (nil? source) (= :repl (adapter-of source))))

(defn- author-of [source]
  (case (adapter-of source)
    (:web :studio) "studio"
    :repl "repl"
    "operator"))

(defn- check-command-keys! [op command]
  (when-let [unknown (seq (remove (into routing-keys (command-keys op)) (keys command)))]
    (refuse! :agent-goal/unknown-command-key
             (str "Unknown key " (pr-str (first unknown)) " in a goal " (name op) " command") {:keys (vec unknown)})))

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
  (when (and (some? (:by goal)) (not (repl-source? source)))
    (refuse! :agent-goal/author-from-source "A goal's author comes from where it was sent; only the REPL may name one"))
  (let [requested (:id goal)
        id (when (some? requested)
             (or (goal-id requested)
                 (refuse! :agent-goal/invalid-id "A goal ID is 1 to 64 letters, digits, '.', '_', ':' or '-'"
                          {:id requested})))
        current (get goals id)
        _ (when current
            (when-not (contains? command :expected-revision)
              (refuse! :agent-goal/exists
                       (str "Goal " id " exists. Edit it, or put with :expected-revision to replace it") {:id id}))
            (check-expected! command current))
        _ (when (and (nil? current) (>= (count goals) max-active))
            (refuse! :agent-goal/too-many
                     (str "At most " max-active " goals are active. Remove one before adding another")
                     {:limit max-active}))
        [id next] (if id [id (:next state)] (next-id state))
        order (:order goal)
        n (cond-> (count goals) (nil? current) inc)
        goal (normalize-goal (merge {:by (or (:by current) (author-of source))}
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
    (when (some #{:id :order :by} (keys changes))
      (refuse! :agent-goal/invalid-goal "Edit cannot change a goal's ID, order or author; use :reorder to move it"))
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
  leaves the model unchanged and its ack names a :reason and a :message. An
  accepted put or edit acks the goal's new :goal-revision."
  [model {:keys [source] :as command}]
  (try
    (check-source! source)
    (let [op (some-> (:op command) wire-name keyword)
          _ (when-not (operations op)
              (refuse! :agent-goal/unknown-operation
                       "A goal command's :op is :put, :edit, :reorder or :remove" {:op (:op command)}))
          _ (check-command-keys! op command)
          ;; The effective meter, defaults included: ordering is always checked here.
          meter (time/normalize-meter (get-in model [:transport :meter]))
          state (stored model)
          [next-state id] (case op
                            :put (put state command meter)
                            :edit (edit state command meter)
                            :reorder (reorder state command)
                            :remove (remove-goal state command))
          next-model (-> model (assoc-in state-path next-state) (update :revision (fnil inc 0)))]
      {:model next-model
       :ack (cond-> {:status :accepted :revision (:revision next-model) :op op :goal-id id
                     :goals (mapv :id (active next-model))}
              (get-in next-state [:goals id]) (assoc :goal-revision (get-in next-state [:goals id :revision])))})
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

(defn- quantize
  "x rounded to places decimals; whole values stay integers."
  [x places]
  (let [f (Math/pow 10 places)
        d (/ (Math/round (* (double x) f)) f)]
    (if (== d (Math/rint d)) (long d) d)))

(defn- wire-position [at] (into {} (map (fn [[k v]] [(name k) (quantize v 3)])) at))

(defn observation
  "Active goals as the `goals` section of `tolglow-agent-obs/1`: JSON-shaped
  maps with text keys, in order. Quantised as interface.md asks: position
  coordinates to three decimals, colours to two, energy levels to three."
  [model]
  (mapv (fn [{:keys [id scope by order text from until palette energy]}]
          (cond-> {"id" id "scope" (name scope) "by" by "order" order "text" text}
            from (assoc "from" (wire-position from))
            until (assoc "until" (wire-position until))
            palette (assoc "palette" (mapv (fn [{:keys [at colors]}]
                                             {"at" (wire-position at)
                                              "colors" (mapv (fn [c] (mapv #(quantize % 2) c)) colors)})
                                           palette))
            energy (assoc "energy" (mapv (fn [{:keys [at level]}] {"at" (wire-position at) "level" (quantize level 3)})
                                         energy))))
        (active model)))
