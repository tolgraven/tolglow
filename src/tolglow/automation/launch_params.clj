(ns tolglow.automation.launch-params
  "Seeded parameter choices for cues the auto planner launches (#217).

  `choose` reads one cue, its effect schemas and an explicit seed, and returns
  the launch parameters plus a reason for every parameter it leaves alone. It
  never mutates a definition or a running instance and reads no clock.

  Domains follow each schema: numbers and integers stay inside hard bounds and
  the narrowest authored range; musical periods (:unit :beats/:bars/:phrases or
  :semantic/type :beat-ratio) pick a legal power-of-two division; colors are
  chosen whole from a weighted palette, never componentwise when one is given;
  enums honor optional weights; booleans flip a fair coin. A parameter a
  performer saved on the cue, a schema that opts out, a seed parameter and a
  value the launch fixes explicitly are never randomized. Each parameter draws
  from its own substream keyed by (seed, cue, parameter), so adding or removing
  a parameter does not change any other choice."
  (:require [tolglow.automation.random :as random]
            [tolglow.knob-candidates.generate :as generate]
            [tolglow.registry :as registry]))

(def selections
  "Which parameters to vary. :none keeps the domains but varies nothing."
  #{:marked :all :none})

(def musical-units #{:beats :bars :phrases})

(def ^:private divisions
  "Legal musical divisions, in the parameter's own unit."
  [1/16 1/8 1/4 1/2 1 2 4 8 16 32 64])

(def ^:private config-keys #{:params :ranges :palette :max-change :categories :saved})

(defn- pair? [x]
  (and (vector? x) (= 2 (count x)) (every? registry/finite-number? x) (apply <= x)))

(defn- rgb? [x]
  (and (vector? x) (= 3 (count x)) (every? #(and (registry/finite-number? %) (<= 0 % 1)) x)))

(defn- weight? [w] (and (registry/finite-number? w) (not (neg? w))))

(defn normalize
  "Accept Studio's textual selection and return the config unchanged otherwise."
  [config]
  (cond-> config
    (#{"marked" "all" "none"} (:params config)) (update :params keyword)
    (= "randomize" (:saved config)) (assoc :saved :randomize)))

(defn validate
  "Path/message errors for a :randomize config, [] when valid."
  [config]
  (let [config (normalize config)]
    (cond
      (not (map? config)) [{:path [] :message "Randomize settings must be a map"}]
      :else
      (vec
       (concat
        (when-not (every? config-keys (keys config))
          [{:path [] :message "Unknown randomize setting"}])
        (let [params (:params config :marked)]
          (when-not (or (selections params)
                        (and (vector? params) (every? keyword? params) (= (count params) (count (distinct params)))))
            [{:path [:params] :message "Choose :marked, :all, :none or distinct parameter keys"}]))
        (when-not (and (map? (:ranges config {})) (every? keyword? (keys (:ranges config {})))
                       (every? pair? (vals (:ranges config {}))))
          [{:path [:ranges] :message "Ranges must be ordered finite pairs keyed by parameter"}])
        (when (contains? config :palette)
          (let [palette (:palette config)]
            (when-not (and (vector? palette) (seq palette) (<= (count palette) 32)
                           (every? #(and (map? %) (rgb? (:color %)) (weight? (:weight % 1))) palette)
                           (pos? (reduce + (map #(:weight % 1) palette))))
              [{:path [:palette] :message "Palette needs 1-32 linear RGB colors with nonnegative weights and a positive total"}])))
        (when (and (contains? config :max-change)
                   (not (and (registry/finite-number? (:max-change config)) (not (neg? (:max-change config))))))
          [{:path [:max-change] :message "Maximum change must be finite and nonnegative"}])
        (when (and (contains? config :categories)
                   (not (and (vector? (:categories config)) (every? keyword? (:categories config)))))
          [{:path [:categories] :message "Categories must be a vector of category keywords"}])
        (when-not (#{:keep :randomize} (:saved config :keep))
          [{:path [:saved] :message "Saved values are :keep (default) or :randomize"}]))))))

(defn cue-categories
  "Categories a cue belongs to: its effect's output-derived and declared
  categories, plus any the cue declares. A cue may belong to several."
  [effect cue]
  (into (set (registry/definition-categories effect))
        (concat (:categories cue) (when (:category cue) [(:category cue)]))))

(defn- musical? [schema]
  (or (musical-units (:unit schema)) (= :beat-ratio (:semantic/type schema))))

(defn- domain-range [schema config k]
  (let [authored (or (get-in config [:ranges k]) (get-in schema [:random :range]))
        bounds (or (:bounds schema) (when (= :color (:type schema)) [0 1]))
        recommended (:recommended schema)
        ranges (filter some? [authored recommended bounds])]
    (when (seq ranges)
      [(apply max (map first ranges)) (apply min (map second ranges))])))

(defn- marked? [schema config k]
  (or (true? (get-in schema [:random :enabled?]))
      (contains? (:ranges config {}) k)
      (and (= :color (:type schema)) (contains? config :palette))))

(defn- exclusion
  "Why parameter k is left alone, or nil when it is randomized."
  [k schema config cue launch-params]
  (let [selection (:params config :marked)
        role (generate/param-role k schema)]
    (cond
      (or (= :none selection) (and (vector? selection) (not (some #{k} selection)))) :not-selected
      (contains? launch-params k) :fixed-by-launch
      (and (not= :randomize (:saved config :keep)) (contains? (:saved-params cue) k)) :performer-saved
      (or (false? (get-in schema [:random :enabled?])) (true? (get-in schema [:random :locked?]))) :locked
      (= :seed role) :seed-parameter
      (not (#{:number :integer :boolean :enum :color :vector :range} (:type schema))) :unsupported-type
      (and (= :marked selection) (not (marked? schema config k))) :not-marked
      (and (#{:number :integer :vector :range} (:type schema)) (nil? (domain-range schema config k))) :no-domain
      :else nil)))

(defn- musical-choice [seed schema range current max-change]
  (let [[low high] range
        low (if max-change (max low (- current max-change)) low)
        high (if max-change (min high (+ current max-change)) high)
        legal (filterv #(and (<= low % high) (or (not= :integer (:type schema)) (integer? %))) divisions)]
    (when (empty? legal)
      (throw (ex-info "No legal musical division in range" {:range [low high]})))
    (let [[seed value] (random/choose seed legal)]
      ;; Every listed division is exact in binary floating point.
      [seed (if (= :integer (:type schema)) (long value) (double value))])))

(defn- choose-value [seed k schema config current]
  (let [max-change (or (:max-change config) (get-in schema [:random :max-change]))]
    (cond
      (and (= :color (:type schema)) (:palette config))
      (let [[seed entry] (random/choose-weighted seed (:palette config) #(:weight % 1))]
        [seed (:color entry)])

      (and (= :enum (:type schema)) (map? (get-in schema [:random :weights])))
      (let [weights (get-in schema [:random :weights])]
        (random/choose-weighted seed (:values schema) #(get weights % 0)))

      (and (#{:number :integer} (:type schema)) (musical? schema))
      (musical-choice seed schema (domain-range schema config k) current max-change)

      :else
      (let [range (domain-range schema config k)
            spec (cond-> (-> schema (assoc-in [:random :enabled?] true) (update :random dissoc :weights :locked?))
                   range (assoc-in [:random :range] range)
                   (some? max-change) (assoc-in [:random :max-change] max-change))]
        (random/randomize-value seed spec current)))))

(defn choose
  "Return {:params {k value} :excluded {k reason}} for one launch of cue.
  seed is the planner's decision seed; tick distinguishes successive decisions.
  launch-params are values the launch fixes, which stay as given."
  [effect cue config seed tick launch-params]
  (let [config (normalize config)
        schemas (:params effect {})
        current (registry/resolve-params schemas (merge (:params cue) (:saved-params cue) launch-params))
        categories (:categories config)]
    (if (and (seq categories) (not (some (cue-categories effect cue) categories)))
      {:params {} :excluded (into (sorted-map-by #(compare (str %1) (str %2)))
                                  (map (fn [k] [k :category])) (keys schemas))}
      (reduce
       (fn [result k]
         (let [schema (get schemas k)]
           (if-let [reason (exclusion k schema config cue launch-params)]
             (assoc-in result [:excluded k] reason)
             (try
               (let [[_ value] (choose-value (random/derive-seed seed tick (:id cue) k) k schema config (get current k))]
                 (if (registry/valid-value? schema value)
                   (assoc-in result [:params k] value)
                   (assoc-in result [:excluded k] :outside-schema)))
               (catch clojure.lang.ExceptionInfo _
                 (assoc-in result [:excluded k] :infeasible))))))
       {:params {} :excluded {}}
       (sort-by str (keys schemas))))))
