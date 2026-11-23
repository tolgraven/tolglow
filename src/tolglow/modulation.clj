(ns tolglow.modulation
  "Pure musical parameter modulation. One owner advances explicit controller
  RNG/schedules; immutable overlays are then shared by independent effect workers.
  Canonical instance parameters, IO and wall clocks are never changed here."
  (:require [afterglow.time :as time]
            [afterglow.control.params :as params]
            [tolglow.registry :as registry]
            [tolglow.automation.random :as random]
            [tolglow.composition.tween :as tween]))

(def ^:private numeric-types #{:number :integer :color :vector :range})
(def ^:private selectors {:instance-ids :id :effect-ids :effect-id :cue-ids :cue-id})
(def ^:private config-keys
  #{:id :seed :every :select :params :mode :curve :priority :ranges :distribution
    :max-change :exclude-owner? :max-catch-up})

(defn- id? [x] (or (keyword? x) (and (string? x) (not (empty? x)))))
(defn- controller-id? [x]
  (or (id? x) (and (vector? x) (= 2 (count x)) (= :cue (first x)) (string? (second x)))))
(defn- fail! [type message data] (throw (ex-info message (assoc data :type type))))
(defn- pair? [x]
  (and (vector? x) (= 2 (count x)) (every? time/finite-number? x) (apply <= x)))
(defn- keys? [x]
  (and (vector? x) (every? keyword? x) (= (count x) (count (distinct x)))))

(defn validate-config
  "Return path/message errors. :id is optional in an effect's :modulation map;
  start requires an ID and reconcile-cues supplies [:cue instance-id]."
  [config]
  (if-not (and (map? config) (registry/data? config))
    [{:path [] :message "Modulation config must be portable map data"}]
    (let [select (:select config {}) every (:every config {:beats 1})
          selected (:params config :all)]
      (vec (concat
            (when-not (every? config-keys (keys config))
              [{:path [] :message "Unknown modulation setting"}])
            (when (and (contains? config :id) (not (controller-id? (:id config))))
              [{:path [:id] :message "Controller needs a stable ID"}])
            (when-not (integer? (:seed config 0))
              [{:path [:seed] :message "Seed must be an integer"}])
            (when-not (and (map? every) (= 1 (count every))
                           (#{:beats :bars :phrases} (ffirst every))
                           (time/finite-number? (val (first every)))
                           (pos? (double (val (first every)))))
              [{:path [:every] :message "Period needs positive beats, bars or phrases"}])
            (when-not (and (map? select)
                           (every? (conj (set (keys selectors)) :categories) (keys select))
                           (every? #(and (vector? %) (every? id? %)) (vals select)))
              [{:path [:select] :message "Selectors need vectors of instance, effect, cue IDs or categories"}])
            (when-not (or (= :all selected) (keys? selected))
              [{:path [:params] :message "Parameters must be :all or distinct parameter keys"}])
            (when-not (#{:sample-hold :glide} (:mode config :sample-hold))
              [{:path [:mode] :message "Mode must be sample-hold or glide"}])
            (try (tween/curve (:curve config :linear) 0.5) []
                 (catch Exception _ [{:path [:curve] :message "Invalid interpolation curve"}]))
            (when-not (time/finite-number? (:priority config 0))
              [{:path [:priority] :message "Priority must be finite"}])
            (when-not (and (map? (:ranges config {}))
                           (every? keyword? (keys (:ranges config {})))
                           (every? pair? (vals (:ranges config {}))))
              [{:path [:ranges] :message "Parameter ranges must be ordered finite pairs"}])
            (when (and (contains? config :distribution)
                       (not (#{:uniform :triangular} (:distribution config))))
              [{:path [:distribution] :message "Distribution must be uniform or triangular"}])
            (when (and (contains? config :max-change)
                       (not (and (time/finite-number? (:max-change config))
                                 (not (neg? (:max-change config))))))
              [{:path [:max-change] :message "Maximum change must be finite and nonnegative"}])
            (when-not (boolean? (:exclude-owner? config true))
              [{:path [:exclude-owner?] :message "Owner exclusion must be boolean"}])
            (when-not (and (integer? (:max-catch-up config 1024))
                           (<= 1 (:max-catch-up config 1024) 100000))
              [{:path [:max-catch-up] :message "Catch-up budget must be an integer in [1,100000]"}]))))))

(defn- normalize-config [config]
  (let [errors (validate-config config)]
    (when (seq errors) (fail! ::invalid-config "Invalid modulation config" {:errors errors})))
  (merge {:seed 0 :every {:beats 1} :select {} :params :all :mode :sample-hold
          :curve :linear :priority 0 :ranges {} :exclude-owner? true :max-catch-up 1024} config))

(defn new-state []
  {:schema-version 1 :next-order 0 :controllers [] :holds {}
   :show-id nil :epoch nil :last-beat nil})

(defn- valid-state! [state]
  (when-not (and (map? state) (registry/data? state) (= 1 (:schema-version state))
                 (integer? (:next-order state)) (not (neg? (:next-order state)))
                 (vector? (:controllers state)) (map? (:holds state))
                 (or (nil? (:show-id state)) (id? (:show-id state)))
                 (or (nil? (:epoch state)) (and (integer? (:epoch state)) (not (neg? (:epoch state)))))
                 (or (nil? (:last-beat state)) (time/finite-number? (:last-beat state)))
                 (= (count (:controllers state)) (count (distinct (map :id (:controllers state))))))
    (fail! ::invalid-state "Invalid portable modulation state" {}))
  (doseq [controller (:controllers state)]
    (when-not (and (controller-id? (:id controller)) (map? (:config controller))
                   (= (:id controller) (get-in controller [:config :id]))
                   (empty? (validate-config (:config controller)))
                   (= (:config controller) (normalize-config (:config controller)))
                   (integer? (:seed controller)) (integer? (:order controller))
                   (not (neg? (:order controller))) (integer? (:ticks controller))
                   (not (neg? (:ticks controller)))
                   (every? time/finite-number? (map controller [:anchor-beat :next-beat :interval-beats]))
                   (pos? (double (:interval-beats controller))) (map? (:values controller)))
      (fail! ::invalid-state "Invalid controller schedule or RNG state" {:controller-id (:id controller)}))
    (doseq [[id cells] (:values controller)]
      (when-not (and (string? id) (map? cells))
        (fail! ::invalid-state "Invalid parameter interpolation cells" {:controller-id (:id controller)}))
      (doseq [[param cell] cells]
        (when-not (and (keyword? param) (map? cell) (contains? cell :from) (contains? cell :to)
                       (#{:sample-hold :glide} (:mode cell))
                       (every? time/finite-number? (map cell [:at-beat :until-beat]))
                       (< (:at-beat cell) (:until-beat cell)))
          (fail! ::invalid-state "Invalid parameter interpolation cell" {:controller-id (:id controller) :param param}))
        (tween/curve (:curve cell) 0.5))))
  (doseq [[id holds] (:holds state)]
    (when-not (and (string? id) (map? holds)) (fail! ::invalid-state "Invalid manual holds" {}))
    (doseq [[param hold] holds]
      (when-not (and (keyword? param) (map? hold) (contains? hold :until-beat)
                     (or (nil? (:until-beat hold)) (time/finite-number? (:until-beat hold))))
        (fail! ::invalid-state "Invalid manual parameter hold" {:instance-id id :param param}))))
  state)

(defn validate-state
  "Validate a saved controller state without advancing clocks or RNG."
  [state]
  (try (valid-state! state) []
       (catch Exception error [{:path [] :message (.getMessage error)}])))

(defn- clock [model frame]
  (let [clock (time/frame-clock (merge {:transport (:transport model)} frame))]
    (when-not (time/finite-number? (:beat clock))
      (fail! ::invalid-clock "Modulation needs an explicit finite beat" {}))
    (when (and (contains? frame :epoch) (not= (:epoch model) (:epoch frame)))
      (fail! ::stale-epoch "Modulation frame belongs to another epoch" {}))
    clock))

(defn- bind-epoch [state model]
  (let [state (valid-state! (or state (new-state)))]
    (cond
      (nil? (:epoch state)) (assoc state :show-id (:show-id model) :epoch (:epoch model))
      (and (= (:show-id model) (:show-id state)) (= (:epoch model) (:epoch state))) state
      :else (assoc (new-state) :show-id (:show-id model) :epoch (:epoch model)))))

(defn start
  "Start/replace one controller at an explicit beat, capturing its meter. A new
  activation gets a new precedence order. The first advance samples immediately
  at the anchor beat; :glide then moves from canonical values toward that sample."
  [state config frame]
  (valid-state! state)
  (let [config (normalize-config config)
        _ (when-not (controller-id? (:id config)) (fail! ::invalid-config "Start requires a controller ID" {}))
        clock (time/frame-clock frame) beat (:beat clock)
        _ (when-not (time/finite-number? beat) (fail! ::invalid-clock "Start requires a finite beat" {}))
        meter (time/normalize-meter (:meter clock))
        interval (time/duration->beats (:every config) meter)
        _ (when-not (and (time/finite-number? interval) (pos? (double interval)))
            (fail! ::invalid-config "Period is outside the representable beat domain" {}))
        controller {:id (:id config) :config config :order (:next-order state)
                    :seed (:seed config) :anchor-beat beat :next-beat beat :ticks 0
                    :interval-beats interval :timing-meter meter :values {}}]
    (-> state
        (update :next-order inc)
        (update :controllers #(conj (vec (remove (fn [c] (= (:id c) (:id config))) %)) controller)))))

(defn stop [state controller-id]
  (valid-state! state)
  (update state :controllers #(vec (remove (fn [c] (= controller-id (:id c))) %))))

(defn hold
  "Suspend overlays for selected instance parameters. nil :until-beat holds
  until release-hold; otherwise the hold expires at that exact beat. The host
  applies the human edit to canonical params, then records this pure hold.
  Old interpolation cells are removed so resuming cannot jump to stale values."
  [state {:keys [instance-id params until-beat]}]
  (valid-state! state)
  (when-not (and (string? instance-id) (keys? params)
                 (or (nil? until-beat) (time/finite-number? until-beat)))
    (fail! ::invalid-hold "Hold needs an instance ID, parameter keys and optional finite deadline" {}))
  (-> (reduce #(assoc-in %1 [:holds instance-id %2] {:until-beat until-beat}) state params)
      (update :controllers
              #(mapv (fn [controller] (update-in controller [:values instance-id]
                                               (fn [values] (apply dissoc (or values {}) params)))) %))))

(defn release-hold [state instance-id parameter-keys]
  (valid-state! state)
  (when-not (and (string? instance-id) (keys? parameter-keys))
    (fail! ::invalid-hold "Release needs an instance ID and parameter keys" {}))
  (update-in state [:holds instance-id] #(apply dissoc (or % {}) parameter-keys)))

(defn- held? [holds id param beat]
  (when-let [hold (get-in holds [id param])]
    (or (nil? (:until-beat hold)) (< beat (:until-beat hold)))))

(defn owner-definition [model instance]
  (let [current (get-in model [:registry :effects (:effect-id instance)])]
    (if (and (not= :pinned (:reload-policy instance :pinned)) current
             (not= (:version current) (get-in instance [:reload-fault :effect-version]))
             (> (:version current) (:effect-version instance))) current (:definition instance))))

(defn- owner-active? [instance definition clock]
  (and (= :running (:phase instance))
       (not= (:version definition) (get-in instance [:fault :effect-version]))
       (if-let [timer (:duration-timer instance)] (not (:done? (time/sample timer clock)))
         (if-let [duration (:duration-ms instance)]
           (< (:time-ms clock) (+ (:started-at-ms instance) duration)) true))))

(defn- prepared-owner [instance definition clock]
  ;; Match engine reload-before-expiry ordering, including revival during release.
  (if (and (= :restart (:reload-policy instance))
           (> (:version definition) (:effect-version instance)))
    (cond-> (assoc instance :phase :running :started-at-ms (:time-ms clock) :started-at-beat (:beat clock))
      (:duration instance) (assoc :duration-timer
                                  (time/start (:duration instance) (assoc clock :meter (:timing-meter instance)))))
    instance))

(defn reconcile-cues
  "Derive controllers from ordinary running effects with :modulation config.
  Controller IDs are [:cue instance-id]. End/release/removal and exact duration
  expiry stop them before effect evaluation. Owners are excluded by default.
  Unchanged config preserves RNG; changed config or :restart reload restarts at
  this frame's beat. First activation uses the owner's captured beat and meter."
  [state model frame]
  (let [state (bind-epoch state model) clock (clock model frame)
        owners (->> (:instances model) vals (sort-by (juxt :priority :order :id))
                    (keep (fn [instance]
                            (let [definition (owner-definition model instance)
                                  instance (prepared-owner instance definition clock)]
                              (when (and (:modulation definition) (owner-active? instance definition clock))
                                [instance definition])))))
        ids (set (map (comp :id first) owners))
        state (update state :controllers #(vec (remove (fn [c]
                                                         (and (:owner-instance-id c)
                                                              (not (ids (:owner-instance-id c))))) %)))]
    (reduce
      (fn [state [instance definition]]
        (let [id [:cue (:id instance)]
              config (normalize-config (merge {:priority (:priority instance)} (:modulation definition) {:id id}))
              old (some #(when (= id (:id %)) %) (:controllers state))
              version (:version definition)
              restart? (and old (not= version (:owner-version old)) (= :restart (:reload-policy instance)))
              changed? (and old (or (not= (:modulation definition) (:owner-config old))
                                   (not= (:priority config) (get-in old [:config :priority]))))]
          (if (and old (not restart?) (not changed?))
            (update state :controllers #(mapv (fn [c] (if (= id (:id c)) (assoc c :owner-version version) c)) %))
            (let [launch-clock (if old clock
                                  (assoc clock :beat (:started-at-beat instance (:beat clock))
                                               :meter (:timing-meter instance (:meter clock))))]
              (update (start state config launch-clock) :controllers
                      #(mapv (fn [c] (if (= id (:id c))
                                       (assoc c :owner-instance-id (:id instance) :owner-version version
                                                :owner-config (:modulation definition)) c)) %))))))
      state owners)))

(defn- categories [model instance]
  (into (set (registry/definition-categories (:definition instance)))
        (mapcat (fn [data] (concat (:categories data) (when (:category data) [(:category data)])))
                [instance (:definition instance) (get-in model [:registry :cues (:cue-id instance)])])))

(defn- matches? [model controller instance]
  (let [{:keys [select exclude-owner?]} (:config controller)]
    (and (not (and exclude-owner? (= (:owner-instance-id controller) (:id instance))))
         (every? (fn [[selector field]]
                   (or (not (contains? select selector))
                       (some #{(get instance field)} (get select selector)))) selectors)
         (or (not (contains? select :categories))
             (some (categories model instance) (:categories select))))))

(defn- targets [model controller]
  (vec (mapcat
         (fn [instance]
           (let [definition (owner-definition model instance)
                 schemas (:params definition {})
                 reloaded? (not= (:effect-version instance) (:version definition))
                 values (params/resolve-values schemas
                                               (if reloaded? (select-keys (:params instance {}) (keys schemas))
                                                   (:params instance {})))
                 instance (assoc instance :definition definition :params values)]
             (when (matches? model controller instance)
               (let [selected (get-in controller [:config :params])
                     selected (if (= :all selected) (sort-by str (keys schemas)) selected)]
                 (for [key selected :when (contains? schemas key)
                       :let [schema (get schemas key)
                             schema (if (and (nil? (:values schema)) (:options schema))
                                      (assoc schema :values (:options schema)) schema)]]
                   {:instance-id (:id instance) :param key :schema schema :current (get values key)})))))
         (sort-by :id (vals (:instances model))))))

(defn- constrain [schema value]
  (let [bounds (or (:bounds schema) (when (= :color (:type schema)) [0 1]))
        clamp (fn [x] (if bounds (max (first bounds) (min (second bounds) x)) x))
        result (cond
                 (= :integer (:type schema))
                 (let [rounded (Math/round (double value))]
                   (if bounds (long (max (Math/ceil (double (first bounds)))
                                         (min (Math/floor (double (second bounds))) rounded))) rounded))
                 (vector? value) (mapv clamp value)
                 :else (clamp value))]
    (if (= :range (:type schema)) (vec (sort result)) result)))

(defn interpolate-value [schema from to fraction]
  (if (numeric-types (:type schema))
    (constrain schema (tween/interpolate from to fraction))
    (if (< fraction 0.5) from to)))

(defn source-index [model diagnostics]
  (let [errors (into {} (for [{:keys [controller-id instance-id param message]} diagnostics
                              :when message] [[controller-id instance-id param] message]))]
    (reduce (fn [result controller]
              (reduce (fn [out {:keys [instance-id param]}]
                        (let [id (:id controller) held? (boolean (get-in model [:modulation :holds instance-id param]))
                              cell (get-in controller [:values instance-id param])
                              error (get errors [id instance-id param])]
                          (update-in out [instance-id param] (fnil conj [])
                                     (cond-> {:type :random :controller-id id :held? held?}
                                       (not (or held? cell)) (assoc :pending? true)
                                       error (assoc :error error)))))
                      result (targets model controller)))
            {} (sort-by (juxt #(get-in % [:config :priority]) :order) (get-in model [:modulation :controllers])))))

(defn- sample-cell [schema cell beat]
  (if (and (= :glide (:mode cell)) (numeric-types (:type schema)))
    (constrain schema (tween/interpolate (:from cell) (:to cell)
                                        (tween/curve (:curve cell)
                                                     (/ (- beat (:at-beat cell))
                                                        (- (:until-beat cell) (:at-beat cell))))))
    (:to cell)))

(defn- choice [seed config {:keys [schema current param]}]
  (let [schema (cond-> schema
                 (= :color (:type schema)) (assoc :bounds (or (:bounds schema) [0.0 1.0])))
        schema (update schema :random merge {:enabled? true}
                       (cond-> {}
                         (contains? (:ranges config) param) (assoc :range (get-in config [:ranges param]))
                         (:distribution config) (assoc :distribution (:distribution config))
                         (contains? config :max-change) (assoc :max-change (:max-change config))))]
    (random/randomize-value seed schema current)))

(defn- eligible-values [values targets holds beat]
  (reduce (fn [result {:keys [instance-id param schema]}]
            (let [cell (get-in values [instance-id param])]
              (if (and cell (not (held? holds instance-id param beat))
                       (params/valid-value? schema (:from cell)) (params/valid-value? schema (:to cell)))
                (assoc-in result [instance-id param] cell) result))) {} targets))

(defn- sample-boundary [controller targets holds at-beat]
  (let [until-beat (+ (:anchor-beat controller) (* (inc (:ticks controller)) (:interval-beats controller)))
        _ (when-not (and (time/finite-number? until-beat) (> until-beat at-beat))
            (fail! ::clock-precision "Period no longer advances this beat coordinate" {:controller-id (:id controller)}))
        config (:config controller)
        result
        (reduce
          (fn [{:keys [seed values] :as result} {:keys [instance-id param schema current] :as target}]
            (if (held? holds instance-id param at-beat) result
              (let [old (get-in (:values controller) [instance-id param])
                    current (if (and old (params/valid-value? schema (:from old)) (params/valid-value? schema (:to old)))
                              (sample-cell schema old at-beat) current)]
                (try
                  (let [[seed next-value] (choice seed config (assoc target :current current))]
                    (-> result (assoc :seed seed)
                        (assoc-in [:values instance-id param]
                                  {:from current :to next-value :at-beat at-beat :until-beat until-beat
                                   :mode (:mode config) :curve (:curve config)})))
                  (catch clojure.lang.ExceptionInfo error
                    (update result :diagnostics conj
                            {:type :modulation/skipped :controller-id (:id controller)
                             :instance-id instance-id :param param :message (.getMessage error)}))))))
          {:seed (:seed controller) :values {} :diagnostics []} targets)]
    {:controller (-> controller (assoc :seed (:seed result) :values (:values result) :next-beat until-beat)
                     (update :ticks inc))
     :diagnostics (:diagnostics result)}))

(defn- cell-overrides [values targets beat]
  (reduce (fn [result {:keys [instance-id param schema]}]
            (if-let [cell (get-in values [instance-id param])]
              (let [value (sample-cell schema cell beat)]
                (if (params/valid-value? schema value)
                  (assoc-in result [instance-id param] value)
                  (fail! ::invalid-value "Modulated value violates schema" {:param param})))
              result))
          {} targets))

(defn captured-overrides
  "Read saved controller cells at their captured beat without advancing the RNG or schedules."
  [model]
  (let [{:keys [last-beat holds controllers]} (:modulation model)]
    (reduce (fn [result controller]
              (let [selected (targets model controller)
                    cells (eligible-values (:values controller) selected holds last-beat)]
                (merge-with merge result (cell-overrides cells selected last-beat))))
            {} (sort-by (juxt #(get-in % [:config :priority]) :order) controllers))))

(defn- advance-controller [controller model holds beat]
  (let [targets (targets model controller)
        fraction (if (< beat (:next-beat controller)) -1.0
                     (double (/ (- beat (:next-beat controller)) (:interval-beats controller))))
        budget (get-in controller [:config :max-catch-up])
        _ (when (or (not (Double/isFinite fraction)) (>= fraction budget))
            (fail! ::catch-up-limit "Modulation catch-up exceeds its explicit work budget"
                   {:controller-id (:id controller) :max-catch-up budget
                    :next-beat (:next-beat controller) :current-beat beat
                    :minimum-samples (when (Double/isFinite fraction) (inc (bigint (Math/floor fraction))))
                    :recovery [:capture-clock-discontinuity-and-rebase :increase-explicit-budget]}))
        due (if (neg? fraction) 0 (inc (long (Math/floor fraction))))]
    (loop [remaining due controller controller diagnostics []]
      (if (zero? remaining)
        (let [values (eligible-values (:values controller) targets holds beat)
              overrides (cell-overrides values targets beat)]
          {:controller (assoc controller :values values) :overrides overrides
           :events (if (pos? due) [{:type :modulation/sampled :controller-id (:id controller)
                                    :samples due :ticks (:ticks controller) :next-beat (:next-beat controller)}] [])
           :diagnostics diagnostics})
        (let [result (sample-boundary controller targets holds (:next-beat controller))]
          (recur (dec remaining) (:controller result) (into diagnostics (:diagnostics result))))))))

(defn advance
  "Advance all controllers once per owner frame, then share returned overlays
  with workers. Intersect selectors; later priority/activation wins per parameter.
  Missing numeric domains produce diagnostics, not guessed ranges. Catch-up
  samples each missed boundary using the supplied model, with a fail-loud budget.
  Same-beat frames do not consume RNG. An actual show/epoch change clears state."
  [state model frame]
  (let [previous state state (reconcile-cues state model frame) clock (clock model frame) beat (:beat clock)
        _ (when (and (:last-beat state) (< beat (:last-beat state)))
            (fail! ::stale-beat "Modulation beat cannot move backwards within an epoch" {}))
        ordered (sort-by (juxt #(get-in % [:config :priority]) :order) (:controllers state))
        results (mapv #(advance-controller % model (:holds state) beat) ordered)
        by-id (into {} (map (juxt (comp :id :controller) :controller)) results)
        holds (into {} (keep (fn [[id parameters]]
                              (let [active (into {} (filter (fn [[param _]] (held? (:holds state) id param beat))) parameters)]
                                (when (and (contains? (:instances model) id) (seq active)) [id active])))) (:holds state))]
    {:state (assoc state :last-beat beat :holds holds
                   :controllers (mapv #(get by-id (:id %)) (:controllers state)))
     :overrides (reduce (fn [result item] (merge-with merge result (:overrides item))) {} results)
     :events (into (if (and (:epoch previous) (not= [(:show-id previous) (:epoch previous)]
                                                   [(:show-id model) (:epoch model)]))
                     [{:type :modulation/reset :epoch (:epoch model)}] []) (mapcat :events results))
     :diagnostics (vec (mapcat :diagnostics results))}))

(defn middleware
  "Return a pure context transform. Compose as
  (comp compiled-effect response-middleware (modulation/middleware overrides)).
  Only the frame's instance copy changes; never write overlays into model params."
  [overrides]
  (when-not (and (map? overrides) (registry/data? overrides))
    (fail! ::invalid-overrides "Parameter overlays must be portable maps" {}))
  (fn [context]
    (if-let [values (get overrides (get-in context [:instance :id]))]
      (let [schemas (get-in context [:instance :definition :params] {})
            errors (params/validate-values schemas values)]
        (when (seq errors) (fail! ::invalid-overrides "Overlay does not fit the captured effect" {:errors errors}))
        (update-in context [:instance :params] merge values)) context)))

(defn rebase-clock
  "Explicit musical clock discontinuity, e.g. external Live loop/seek/source epoch.
  Shift beat anchors, next samples, glide phases and finite hold deadlines by
  new :beat minus :last-beat; preserve RNG, values, ticks, meter and intervals.
  Optional :from-beat supplies the previous coordinate before the first frame.
  The owner must apply the same clock policy to engine/instance timers separately.
  Ordinary missed frames still use bounded deterministic catch-up in advance."
  [state new-clock]
  (valid-state! state)
  (let [beat (:beat (time/frame-clock new-clock))
        old-beat (or (:from-beat new-clock) (:last-beat state)
                     (when (and (empty? (:controllers state)) (empty? (:holds state))) beat))]
    (when-not (and (time/finite-number? beat) (time/finite-number? old-beat)
                   (time/finite-number? (- beat old-beat)))
      (fail! ::invalid-clock "Clock rebase needs a finite beat and prior last-beat or explicit from-beat" {}))
    (let [offset (- beat old-beat)
          shift (fn [value]
                  (let [shifted (+ value offset)]
                    (when-not (time/finite-number? shifted)
                      (fail! ::clock-precision "Clock rebase exceeds the finite beat domain" {}))
                    shifted))
          shift-cell (fn [cell] (-> cell (update :at-beat shift) (update :until-beat shift)))
          shifted (-> state
                      (assoc :last-beat beat)
                      (update :controllers
                              #(mapv (fn [controller]
                                       (-> controller (update :anchor-beat shift) (update :next-beat shift)
                                           (update :values
                                                   (fn [values]
                                                     (into {} (map (fn [[id parameters]]
                                                                     [id (into {} (map (fn [[param cell]] [param (shift-cell cell)])) parameters)])) values))))) %))
                      (update :holds
                              (fn [holds]
                                (into {} (map (fn [[id parameters]]
                                                [id (into {} (map (fn [[param hold]]
                                                                   [param (if (some? (:until-beat hold))
                                                                            (update hold :until-beat shift) hold)])) parameters)])) holds))))]
      (valid-state! shifted))))

(defn restore-state
  "Rebind a saved state to restored show/epoch and remapped active instance IDs.
  Unlike an actual restart this preserves exact RNG, beat anchors and next-beat.
  :instance-ids maps old IDs to new IDs; unmapped per-instance cells/holds/selectors
  are dropped and controllers whose cue owner is absent are removed."
  [state {:keys [show-id epoch instance-ids]}]
  (valid-state! state)
  (when-not (and (id? show-id) (integer? epoch) (not (neg? epoch)) (map? instance-ids)
                 (every? string? (keys instance-ids)) (every? string? (vals instance-ids))
                 (= (count instance-ids) (count (distinct (vals instance-ids)))))
    (fail! ::invalid-restore "Restore needs show/epoch and an injective instance ID map" {}))
  (let [remap-values (fn [values] (into {} (keep (fn [[id value]] (when-let [next-id (get instance-ids id)]
                                                                 [next-id value]))) values))]
    (assoc state :show-id show-id :epoch epoch :holds (remap-values (:holds state))
           :controllers
           (into []
                 (keep (fn [controller]
                         (let [owner (:owner-instance-id controller)
                               next-owner (get instance-ids owner)]
                           (when (or (nil? owner) next-owner)
                             (cond-> (update controller :values remap-values)
                               owner (assoc :owner-instance-id next-owner :id [:cue next-owner])
                               owner (assoc-in [:config :id] [:cue next-owner])
                               (contains? (get-in controller [:config :select]) :instance-ids)
                               (update-in [:config :select :instance-ids] #(into [] (keep instance-ids) %)))))))
                 (:controllers state)))))
