(ns tolglow.modulation
  "Pure musical parameter modulation. One owner advances explicit controller
  RNG/schedules; immutable overlays are then shared by independent effect workers.
  Canonical instance parameters, IO and wall clocks are never changed here."
  (:require [afterglow.control.instance-order :as instance-order]
            [afterglow.control.params :as params]
            [afterglow.control.system :as system]
            [afterglow.time :as time]
            [tolglow.registry :as registry]
            [tolglow.automation.random :as random]
            [tolglow.composition.tween :as tween])
  (:import [java.util Comparator]))

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

(def min-period-beats
  "Shortest controller period: 1/1024 beat, 0.33 ms at 180 BPM and 0.84 ms at
  70 BPM. That is shorter than any lighting frame, so every faster period would
  only spend the catch-up budget on samples no output can show."
  1/1024)

(def max-period-beats
  "Longest controller period: 2^40 beats, about 11,600 years at 180 BPM. The
  schedule adds a period to a beat coordinate. With an anchor and a period both
  at most 2^40 beats, the next boundary is below 2^41 beats. A double still
  resolves 2^-11 beat there, half the shortest period, so every period in range
  advances its schedule. A longer period could overflow the boundary or round
  it back onto the anchor; it cannot start or schedule."
  1099511627776)

(defn period-beats
  "Beats in one controller period under `meter` (the default 4/4 with four-bar
  phrases when omitted), or nil when `every` is not one finite positive
  :beats, :bars or :phrases value."
  ([every] (period-beats every time/default-meter))
  ([every meter]
   (when (and (map? every) (= 1 (count every)) (#{:beats :bars :phrases} (ffirst every))
              (time/finite-number? (val (first every))) (pos? (val (first every))))
     (try (time/duration->beats every meter) (catch clojure.lang.ExceptionInfo _ ##Inf)))))

(defn- period-in-range? [beats]
  (and (time/finite-number? beats) (<= min-period-beats beats max-period-beats)))

(defn- period-range-message [label every beats meter meter-name]
  (str label " " (pr-str every) " is "
       (if (time/finite-number? beats) (str (double beats) " beats") "beyond the finite beat range")
       (when-not (contains? every :beats)
         (str " in the " meter-name " meter (" (:beats-per-bar meter) " beats per bar, "
              (:bars-per-phrase meter) " bars per phrase)"))
       "; a period must be between 1/1024 and 2^40 (1099511627776) beats"))

(defn period-range-error
  "The message refusing a finite positive musical period outside
  [min-period-beats, max-period-beats] in `meter`, or nil. `label` names the
  field, such as \"Waveform period :period\", and `meter-name` the meter,
  such as \"default\"."
  ([label every] (period-range-error label every time/default-meter "default"))
  ([label every meter meter-name]
   (let [meter (time/normalize-meter meter)
         beats (period-beats every meter)]
     (when (and (some? beats) (not (period-in-range? beats)))
       (period-range-message label every beats meter meter-name)))))

(defn- period-errors
  "Errors for :every. A bounded check also refuses periods outside
  [min-period-beats, max-period-beats] under the default meter; start checks
  the captured meter again."
  [every bounded?]
  (let [unit (when (and (map? every) (= 1 (count every))) (#{:beats :bars :phrases} (ffirst every)))
        beats (period-beats every)]
    (cond
      (nil? unit)
      [{:path [:every] :message "Modulation period :every needs exactly one :beats, :bars or :phrases value"}]
      (nil? beats)
      [{:path [:every unit] :message (str "Modulation period :every " (pr-str every) " must be finite and positive")}]
      bounded?
      (when-let [message (period-range-error "Modulation period :every" every)]
        [{:path [:every unit] :message message}]))))

(defn- config-errors [config bounded?]
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
            (period-errors every bounded?)
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

(defn validate-config
  "Return path/message errors. :id is optional in an effect's :modulation map;
  start requires an ID and reconcile-cues supplies [:cue instance-id]. The
  period must be finite, positive and within [min-period-beats,
  max-period-beats] in the default meter; start checks its captured meter."
  [config]
  (config-errors config true))

(defn validate-saved-config
  "Return the errors of validate-config except the period range. Saved shows
  and running definitions use it, so a backup whose period an older owner
  accepted still loads; the frame refuses that controller alone."
  [config]
  (config-errors config false))

(defn- with-defaults [config]
  (merge {:seed 0 :every {:beats 1} :select {} :params :all :mode :sample-hold
          :curve :linear :priority 0 :ranges {} :exclude-owner? true :max-catch-up 1024} config))

(defn- normalize-config [config]
  (let [errors (validate-config config)]
    (when (seq errors)
      (fail! ::invalid-config (str "Invalid modulation config: " (:message (first errors))) {:errors errors})))
  (with-defaults config))

(defn new-state []
  {:schema-version 1 :next-order 0 :controllers [] :holds {}
   :show-id nil :epoch nil :last-beat nil})

(defn- with-refused
  "Store refusal records under :refused, keyed by controller ID, and leave the
  key out when there are none."
  [state refused]
  (if (seq refused) (assoc state :refused refused) (dissoc state :refused)))

(defn- hold-record? [hold]
  (and (map? hold) (contains? hold :until-beat)
       (or (nil? (:until-beat hold)) (time/finite-number? (:until-beat hold)))))

(defn- valid-state! [state]
  (when-not (and (map? state) (registry/data? state) (= 1 (:schema-version state))
                 (integer? (:next-order state)) (not (neg? (:next-order state)))
                 (vector? (:controllers state)) (map? (:holds state))
                 (or (nil? (:show-id state)) (id? (:show-id state)))
                 (or (nil? (:epoch state)) (and (integer? (:epoch state)) (not (neg? (:epoch state)))))
                 (or (nil? (:last-beat state)) (time/finite-number? (:last-beat state)))
                 (= (count (:controllers state)) (count (distinct (map :id (:controllers state)))))
                 (or (nil? (:refused state))
                     (and (map? (:refused state))
                          (every? (fn [[id record]]
                                    (and (controller-id? id) (map? record) (map? (:diagnostic record))
                                         (or (nil? (:owner-instance-id record)) (id? (:owner-instance-id record)))))
                                  (:refused state)))))
    (fail! ::invalid-state "Invalid portable modulation state" {}))
  (doseq [controller (:controllers state)]
    ;; Saved controllers keep loading with a period outside today's bounds;
    ;; the frame refuses such a controller alone if it cannot schedule.
    (when-not (and (controller-id? (:id controller)) (map? (:config controller))
                   (= (:id controller) (get-in controller [:config :id]))
                   (empty? (config-errors (:config controller) false))
                   (= (:config controller) (with-defaults (:config controller)))
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
      (when-not (and (keyword? param) (hold-record? hold))
        (fail! ::invalid-state "Invalid manual parameter hold" {:instance-id id :param param}))
      (when (contains? hold ::gesture-owner)
        (let [{::keys [gesture-owner previous-hold]} hold]
          (when-not (and (vector? gesture-owner) (= 3 (count gesture-owner)) (every? id? gesture-owner)
                         (nil? (:until-beat hold))
                         (or (nil? previous-hold)
                             (and (hold-record? previous-hold) (not (contains? previous-hold ::gesture-owner)))))
            (fail! ::invalid-state "Invalid gesture parameter hold" {:instance-id id :param param}))))))
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
        interval (period-beats (:every config) meter)
        _ (when-not (period-in-range? interval)
            (fail! ::invalid-period (period-range-message "Modulation period :every" (:every config) interval meter "captured")
                   {:controller-id (:id config) :path [:every (ffirst (:every config))]
                    :every (:every config) :meter meter}))
        controller {:id (:id config) :config config :order (:next-order state)
                    :seed (:seed config) :anchor-beat beat :next-beat beat :ticks 0
                    :interval-beats interval :timing-meter meter :values {}}]
    (-> state
        (update :next-order inc)
        (update :controllers #(conj (vec (remove (fn [c] (= (:id c) (:id config))) %)) controller))
        (with-refused (dissoc (:refused state) (:id config))))))

(defn stop
  "Stop a controller, or clear a refused global controller's record."
  [state controller-id]
  (valid-state! state)
  (-> state
      (update :controllers #(vec (remove (fn [c] (= controller-id (:id c))) %)))
      (with-refused (dissoc (:refused state) controller-id))))

(defn hold
  "Suspend overlays for selected instance parameters. nil :until-beat holds
  until release-hold; otherwise the hold expires at that exact beat. An optional
  keyword :reason records why the owner held it, such as a manual priority edit. The host
  applies the human edit to canonical params, then records this pure hold.
  Old interpolation cells are removed so resuming cannot jump to stale values."
  [state {:keys [instance-id params until-beat reason]}]
  (valid-state! state)
  (when-not (and (string? instance-id) (keys? params)
                 (or (nil? until-beat) (time/finite-number? until-beat))
                 (or (nil? reason) (keyword? reason)))
    (fail! ::invalid-hold "Hold needs an instance ID, parameter keys, optional finite deadline and keyword reason" {}))
  (-> (reduce #(assoc-in %1 [:holds instance-id %2] (cond-> {:until-beat until-beat} reason (assoc :reason reason)))
              state params)
      (update :controllers
              #(mapv (fn [controller] (update-in controller [:values instance-id]
                                               (fn [values] (apply dissoc (or values {}) params)))) %))))

(defn release-hold [state instance-id parameter-keys]
  (valid-state! state)
  (when-not (and (string? instance-id) (keys? parameter-keys))
    (fail! ::invalid-hold "Release needs an instance ID and parameter keys" {}))
  (update-in state [:holds instance-id] #(apply dissoc (or % {}) parameter-keys)))

(defn hold-gesture-param
  "Hold a sampled parameter until the gesture settles. A new explicit hold or
  resume replaces this entry through the regular hold operations."
  [state instance-id param owner]
  (let [path [:holds instance-id param]
        previous (get-in state path)]
    (if (= owner (::gesture-owner previous))
      state
      (-> (hold state {:instance-id instance-id :params [param]})
          (update-in path assoc ::gesture-owner owner ::previous-hold previous)))))

(defn- committed-hold [hold]
  (dissoc hold ::gesture-owner ::previous-hold))

(defn settle-gesture-hold
  "Settle only the hold the gesture still owns. Cancel restores the displaced
  deadline, if any. Controller cells and schedules keep their current state."
  [state instance-id param owner commit?]
  (let [path [:holds instance-id param]
        hold (get-in state path)]
    (if (= owner (::gesture-owner hold))
      (if commit?
        (update-in state path committed-hold)
        (if-let [previous (::previous-hold hold)]
          (assoc-in state path previous)
          (release-hold state instance-id [param])))
      state)))

(defn portable-holds
  "Project the effective holds without live gesture ownership."
  [holds]
  (reduce-kv (fn [projected instance-id parameters]
               (reduce-kv (fn [projected param hold]
                            (if (contains? hold ::gesture-owner)
                              (assoc-in projected [instance-id param] (committed-hold hold))
                              projected))
                          projected parameters))
             holds holds))

(defn- held? [holds id param beat]
  (when-let [hold (get-in holds [id param])]
    (or (nil? (:until-beat hold)) (< beat (:until-beat hold)))))

(defn last-good-view
  "Return `model` as modulation reads it when the engine rejects every pending
  hot reload: each instance keeps its captured definition. The owner samples
  this view beside the model, from the same prior state, to give the engine
  the inputs of a frame that turns a candidate down. It is never committed."
  [model]
  (assoc model ::last-good? true))

(defn with-frame-params
  "Return `model` as modulation reads it in a frame whose parameter ramps
  overlay `params` ({instance-id {param value}}) on the committed values and
  will commit `targets` ({instance-id {param to}}). The view only answers which
  revision each instance follows; it is never committed."
  ([model params] (with-frame-params model params nil))
  ([model params targets]
   (cond-> model
     (seq params) (assoc ::frame-params params)
     (seq targets) (assoc ::ramp-targets targets))))

(defn attempted-revision
  "The newer registry definition the engine attempts for this instance on the
  next frame, or nil. Paused and pinned instances defer hot reload."
  [model instance]
  (let [current (get-in model [:registry :effects (:effect-id instance)])]
    (when (and (not (get-in instance [:system :paused?]))
               (not= :pinned (:reload-policy instance :pinned)) current
               (not= (:version current) (get-in instance [:reload-fault :effect-version]))
               (> (:version current) (:effect-version instance)))
      current)))

(defn- accepts? [definition values]
  (try (params/resolve-values (:params definition {}) (select-keys values (keys (:params definition {}))))
       true
       (catch Exception _ false)))

(defn- ramp-values
  "The committed parameters, the frame's ramp values and the ramp targets of an
  instance: the three parameter sets a revision must accept during a ramp."
  [committed framed targets]
  [committed (merge committed framed) (merge committed targets)])

(defn ramp-preparation
  "The parameter overlay the owner gives the engine for an instance whose
  attempted revision refuses one of its ramped values, or nil to keep the
  frame's ramp values. The engine prepares a revision from the parameters it
  receives, so it refuses the revision at preparation when it receives the
  refused set: the committed values ({}, no overlay) or the ramp targets. The
  engine already refuses one that rejects the frame values. The ramp still
  reaches the captured revision through the parameter overrides."
  [model instance framed targets]
  (when-let [current (attempted-revision model instance)]
    (let [[committed frame target] (ramp-values (:params instance {}) framed targets)]
      (cond
        (not (accepts? current committed)) {}
        (not (accepts? current frame)) nil
        (not (accepts? current target)) (merge framed targets)))))

(defn reload-candidate
  "Return the newer registry definition the engine attempts for this instance
  on the next frame, or nil. Paused and pinned instances defer hot reload. A
  candidate whose schemas reject a retained parameter is none: the engine
  refuses it at preparation and keeps the captured definition. During a ramp
  the retained parameter must fit with its committed value, its frame value
  and the ramp's target (see `with-frame-params` and `ramp-preparation`)."
  [model instance]
  (when-not (::last-good? model)
    (when-let [current (attempted-revision model instance)]
      (let [id (:id instance)]
        (when (every? #(accepts? current %)
                      (ramp-values (:params instance {}) (get-in model [::frame-params id])
                                   (get-in model [::ramp-targets id])))
          current)))))

(defn reload-pending?
  "True when the next frame attempts a hot reload of some running instance."
  [model]
  (boolean (some #(reload-candidate model %) (vals (:instances model)))))

(defn without-reloads
  "Return `model` as the engine and modulation read it in a frame that defers
  the hot reloads of `instance-ids` until their programs are compiled (#774):
  those instances read as pinned, so they keep their captured definition for
  this frame. A frame view only; `restore-reloads` puts the policies back."
  [model instance-ids]
  (reduce (fn [model id]
            (if (contains? (:instances model) id) (assoc-in model [:instances id :reload-policy] :pinned) model))
          model instance-ids))

(defn restore-reloads
  "Give the instances of `result`, a model evaluated from a `without-reloads`
  view, the reload policies they have in `model`."
  [result model instance-ids]
  (reduce (fn [result id]
            (if (and (contains? (:instances result) id) (contains? (:instances model) id))
              (assoc-in result [:instances id :reload-policy] (get-in model [:instances id :reload-policy]))
              result))
          result instance-ids))

(defn owner-definition
  "Select the definition for the next frame: the pending reload candidate, or
  the captured definition."
  [model instance]
  (or (reload-candidate model instance) (:definition instance)))

(defn- owner-active? [instance definition global-clock]
  ;; Duration timers run on the instance's private clock, as in the engine, so
  ;; a held or paused owner keeps its controller for its whole duration.
  (let [clock (system/private-clock instance global-clock)]
    (and (= :running (:phase instance))
         (not= (:version definition) (get-in instance [:fault :effect-version]))
         (if-let [timer (:duration-timer instance)] (not (:done? (time/sample timer clock)))
           (if-let [duration (:duration-ms instance)]
             (< (:time-ms clock) (+ (:started-at-ms instance) duration)) true)))))

(defn- prepared-owner [instance definition global-clock]
  ;; Match engine reload-before-expiry ordering, including revival during
  ;; release. The engine restarts on the instance's private clock, which
  ;; owner-active? also samples. Paused instances get no reload candidate.
  (if (and (= :restart (:reload-policy instance))
           (> (:version definition) (:effect-version instance)))
    (let [clock (system/private-clock instance global-clock)]
      (cond-> (assoc instance :phase :running :started-at-ms (:time-ms clock) :started-at-beat (:beat clock))
        (:duration instance) (assoc :duration-timer
                                    (time/start (:duration instance) (assoc clock :meter (:timing-meter instance))))))
    instance))

(def ^:private refusable-start
  "Failures that refuse one owner's controller rather than the frame."
  #{::invalid-config ::invalid-period})

(defn- refused-controller [id instance error]
  (merge {:type :modulation/refused :controller-id id :owner-instance-id (:id instance)
          :cue-id (:cue-id instance) :effect-id (:effect-id instance) :message (ex-message error)}
         (when-let [path (:path (first (:errors (ex-data error))))] {:path path})
         (select-keys (ex-data error) [:path :every :meter])))

(defn- refusal-record
  "What an owner's refusal was judged on, with the diagnostic it reports each
  frame. The same owner version, config and priority stay refused without
  another attempt; a change starts the controller at that frame's beat."
  [instance version owner-config diagnostic]
  {:owner-instance-id (:id instance) :owner-version version :owner-config owner-config
   :priority (:priority instance) :diagnostic diagnostic})

(defn- same-refusal? [record instance version owner-config]
  (and record (= version (:owner-version record)) (= owner-config (:owner-config record))
       (= (:priority instance) (:priority record))))

(defn- reconcile [state model frame]
  (let [state (bind-epoch state model) clock (clock model frame)
        owners (->> (:instances model) vals (sort instance-order/authored-instance-comparator)
                    (keep (fn [instance]
                            (let [definition (owner-definition model instance)
                                  instance (prepared-owner instance definition clock)]
                              (when (and (:modulation definition) (owner-active? instance definition clock))
                                [instance definition])))))
        ids (set (map (comp :id first) owners))
        state (-> state
                  (update :controllers #(vec (remove (fn [c]
                                                       (and (:owner-instance-id c)
                                                            (not (ids (:owner-instance-id c))))) %)))
                  ;; Global refusals stay until that ID is started again or stopped.
                  (with-refused (into {} (filter (fn [[_ {owner :owner-instance-id}]] (or (nil? owner) (ids owner))))
                                      (:refused state))))]
    (reduce
      (fn [{:keys [state] :as result} [instance definition]]
        (let [id [:cue (:id instance)]
              old (some #(when (= id (:id %)) %) (:controllers state))
              refused (get-in state [:refused id])
              version (:version definition)
              owner-config (:modulation definition)]
          (if (same-refusal? refused instance version owner-config)
            ;; Judged already: report the same refusal without another attempt.
            (update result :diagnostics conj (:diagnostic refused))
            (try
              (let [config (normalize-config (merge {:priority (:priority instance)} owner-config {:id id}))
                    restart? (and old (not= version (:owner-version old)) (= :restart (:reload-policy instance)))
                    changed? (and old (or (not= owner-config (:owner-config old))
                                         (not= (:priority config) (get-in old [:config :priority]))))]
                (assoc result :state
                       (if (and old (not restart?) (not changed?))
                         (update state :controllers #(mapv (fn [c] (if (= id (:id c)) (assoc c :owner-version version) c)) %))
                         ;; A revision after a refusal starts like a reload, at
                         ;; this frame's beat and meter, not like a first launch.
                         (let [launch-clock (if (or old refused) clock
                                               (assoc clock :beat (:started-at-beat instance (:beat clock))
                                                            :meter (:timing-meter instance (:meter clock))))]
                           (-> (start state config launch-clock)
                               (update :controllers
                                       #(mapv (fn [c] (if (= id (:id c))
                                                        (assoc c :owner-instance-id (:id instance) :owner-version version
                                                                 :owner-config owner-config) c)) %))
                               (with-refused (dissoc (:refused state) id)))))))
              ;; Refuse this owner's controller alone; every other owner,
              ;; controller and effect keeps its frame.
              (catch clojure.lang.ExceptionInfo error
                (if (refusable-start (:type (ex-data error)))
                  (let [diagnostic (refused-controller id instance error)]
                    (-> result
                        (update-in [:state :controllers] #(vec (remove (fn [c] (= id (:id c))) %)))
                        (update :state #(with-refused % (assoc (:refused %) id (refusal-record instance version owner-config diagnostic))))
                        (update :diagnostics conj diagnostic)))
                  (throw error)))))))
      {:state state :diagnostics []} owners)))

(defn reconcile-cues
  "Derive controllers from ordinary running effects with :modulation config.
  Controller IDs are [:cue instance-id]. End/release/removal and exact duration
  expiry stop them before effect evaluation. Owners are excluded by default.
  Unchanged config preserves RNG; changed config or :restart reload restarts at
  this frame's beat. First activation uses the owner's captured beat and meter.
  An owner whose controller cannot start, such as a period outside the beat
  range in its captured meter, runs without one; advance reports it."
  [state model frame]
  (:state (reconcile state model frame)))

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

(def ^:private controller-comparator
  (reify Comparator
    (compare [_ left right]
      (let [priority (instance-order/compare-priority (:priority (:config left))
                                                      (:priority (:config right)))]
        (if (zero? priority) (compare (:order left) (:order right)) priority)))))

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
            {} (sort controller-comparator (get-in model [:modulation :controllers])))))

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
            (fail! ::clock-precision (str "Modulation period :every " (pr-str (get-in controller [:config :every]))
                                          " no longer advances its schedule at beat " at-beat)
                   {:controller-id (:id controller)}))
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
            {} (sort controller-comparator controllers))))

(defn- rebase-backlog
  "Skip the missed boundaries a controller cannot sample within its catch-up
  budget. Its schedule moves to the last boundary at or before `beat` and its
  tick count by the skipped boundaries, so it then samples that boundary once
  and keeps its own grid, as a stopped transport resumes without catch-up.
  Return [controller skipped], or nil when the backlog is not countable."
  [controller fraction beat]
  (when (< fraction 9.0E15)
    (let [{:keys [anchor-beat ticks interval-beats]} controller
          boundary #(+ anchor-beat (* (+ ticks %) interval-beats))
          skipped (long (Math/floor fraction))
          skipped (if (and (pos? skipped) (> (boundary skipped) beat)) (dec skipped) skipped)]
      [(assoc controller :ticks (+ ticks skipped) :next-beat (boundary skipped)) skipped])))

(defn- advance-controller [controller model holds beat]
  (let [targets (targets model controller)
        fraction (if (< beat (:next-beat controller)) -1.0
                     (double (/ (- beat (:next-beat controller)) (:interval-beats controller))))
        budget (get-in controller [:config :max-catch-up])
        over? (>= fraction budget)
        rebased (when (and over? (Double/isFinite fraction)) (rebase-backlog controller fraction beat))
        _ (when (and over? (nil? rebased))
            (fail! ::clock-precision (str "Modulation period :every " (pr-str (get-in controller [:config :every]))
                                          " cannot count its boundaries up to beat " beat)
                   {:controller-id (:id controller)}))
        [controller skipped] (or rebased [controller 0])
        due (cond (neg? fraction) 0 rebased 1 :else (inc (long (Math/floor fraction))))]
    (loop [remaining due controller controller diagnostics
           (if rebased
             [{:type :modulation/rebased :controller-id (:id controller) :skipped skipped
               :max-catch-up budget :next-beat (:next-beat controller) :current-beat beat
               :message (str "Skipped " skipped " missed modulation steps beyond the catch-up budget of "
                             budget "; the controller resumes at its next boundary")}]
             [])]
      (if (zero? remaining)
        (let [values (eligible-values (:values controller) targets holds beat)
              overrides (cell-overrides values targets beat)]
          {:controller (assoc controller :values values) :overrides overrides
           :events (if (pos? due) [{:type :modulation/sampled :controller-id (:id controller)
                                    :samples due :ticks (:ticks controller) :next-beat (:next-beat controller)}] [])
           :diagnostics diagnostics})
        (let [result (sample-boundary controller targets holds (:next-beat controller))]
          (recur (dec remaining) (:controller result) (into diagnostics (:diagnostics result))))))))

(defn- refused-in-frame
  "A controller refused alone, with the record that keeps an owner's
  controller refused until its revision, config or priority changes."
  [controller model message]
  (let [owner (:owner-instance-id controller)
        diagnostic (cond-> {:type :modulation/refused :controller-id (:id controller) :message message
                            :every (get-in controller [:config :every]) :interval-beats (:interval-beats controller)}
                     owner (assoc :owner-instance-id owner))]
    {:controller nil :overrides {} :events [] :diagnostics [diagnostic]
     :refusal (if owner
                (when-let [instance (get-in model [:instances owner])]
                  [(:id controller) (refusal-record instance (:owner-version controller)
                                                    (:owner-config controller) diagnostic)])
                [(:id controller) {:diagnostic diagnostic}])}))

(defn- safe-advance-controller
  "Advance one controller, or refuse it alone: a saved interval outside the
  period range, or a schedule that no longer advances the beat coordinate. A
  backlog beyond the catch-up budget is skipped, never a frame failure."
  [controller model holds beat]
  (if-not (period-in-range? (:interval-beats controller))
    (refused-in-frame controller model
                      (str "Saved modulation interval of " (double (:interval-beats controller))
                           " beats is outside 1/1024 to 2^40 (1099511627776) beats"))
    (try (advance-controller controller model holds beat)
         (catch clojure.lang.ExceptionInfo error
           (if (= ::clock-precision (:type (ex-data error)))
             (refused-in-frame controller model (ex-message error))
             (throw error))))))

(defn advance
  "Advance all controllers once per owner frame, then share returned overlays
  with workers. Intersect selectors; later priority/activation wins per parameter.
  Missing numeric domains produce diagnostics, not guessed ranges. Catch-up
  samples each missed boundary using the supplied model, up to the controller's
  budget; a longer backlog skips to the current boundary and reports it.
  A controller that cannot schedule is refused alone with a diagnostic.
  Same-beat frames do not consume RNG. An actual show/epoch change clears state."
  [state model frame]
  (let [previous state
        {state :state refused :diagnostics} (reconcile state model frame)
        clock (clock model frame) beat (:beat clock)
        _ (when (and (:last-beat state) (< beat (:last-beat state)))
            (fail! ::stale-beat "Modulation beat cannot move backwards within an epoch" {}))
        ordered (sort controller-comparator (:controllers state))
        results (mapv #(safe-advance-controller % model (:holds state) beat) ordered)
        by-id (into {} (keep (fn [{:keys [controller]}] (when controller [(:id controller) controller]))) results)
        holds (into {} (keep (fn [[id parameters]]
                              (let [active (into {} (filter (fn [[param _]] (held? (:holds state) id param beat))) parameters)]
                                (when (and (contains? (:instances model) id) (seq active)) [id active])))) (:holds state))]
    {:state (-> state
                (assoc :last-beat beat :holds holds
                       :controllers (into [] (keep #(get by-id (:id %))) (:controllers state)))
                (with-refused (into (or (:refused state) {}) (keep :refusal) results)))
     :overrides (reduce (fn [result item] (merge-with merge result (:overrides item))) {} results)
     :events (into (if (and (:epoch previous) (not= [(:show-id previous) (:epoch previous)]
                                                   [(:show-id model) (:epoch model)]))
                     [{:type :modulation/reset :epoch (:epoch model)}] []) (mapcat :events results))
     :diagnostics (-> refused
                      (into (keep (fn [[_ {:keys [owner-instance-id diagnostic]}]]
                                    (when-not owner-instance-id diagnostic)))
                            (:refused state))
                      (into (mapcat :diagnostics) results))}))

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

(defn candidate-overrides
  "Restrict overrides that do not follow a hot reload, such as ramp overlays,
  to the values a pending candidate's schemas accept. Instances without a
  candidate keep theirs, and the last-good revision reads the unrestricted map."
  [model overrides]
  (reduce-kv (fn [result id values]
               (if-let [candidate (some->> (get-in model [:instances id]) (reload-candidate model))]
                 (let [schemas (:params candidate {})
                       kept (into {} (filter (fn [[param value]]
                                               (and (contains? schemas param)
                                                    (params/valid-value? (get schemas param) value))))
                                  values)]
                   (if (seq kept) (assoc result id kept) (dissoc result id)))
                 result))
             (or overrides {}) (or overrides {})))

(def last-good-frame
  "Frame inputs that mark a step as evaluated on the engine's reload fallback."
  {::last-good? true})

(defn reload-middleware
  "Return a context transform for a frame that may reject a hot reload. The
  engine merges `last-good-frame` into the frame of every step it evaluates on
  its reload fallback; those steps read `last-good`, all others `overrides`."
  [overrides last-good]
  (let [candidate (middleware overrides)
        fallback (middleware last-good)]
    (fn [context]
      (if (get-in context [:frame ::last-good?]) (fallback context) (candidate context)))))

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
          shift-hold (fn [hold]
                       (cond-> hold
                         (some? (:until-beat hold)) (update :until-beat shift)
                         (some? (get-in hold [::previous-hold :until-beat]))
                         (update-in [::previous-hold :until-beat] shift)))
          shifted (-> state
                      (assoc :last-beat beat)
                      (update :controllers
                              #(mapv (fn [controller]
                                       (-> controller (update :anchor-beat shift) (update :next-beat shift)
                                           (update :values
                                                   (fn [values]
                                                     (into {} (map (fn [[id parameters]]
                                                                     [id (into {} (map (fn [[param cell]] [param (shift-cell cell)])) parameters)])) values))))) %))
                      (update :holds update-vals #(update-vals % shift-hold)))]
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
    (-> state
        (assoc :show-id show-id :epoch epoch :holds (remap-values (portable-holds (:holds state)))
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
                     (:controllers state)))
        (with-refused (into {} (keep (fn [[id record]]
                                       (if (nil? (:owner-instance-id record))
                                         [id record]
                                         (when-let [next-owner (get instance-ids (:owner-instance-id record))]
                                           [[:cue next-owner]
                                            (-> record (assoc :owner-instance-id next-owner)
                                                (update :diagnostic assoc :owner-instance-id next-owner
                                                        :controller-id [:cue next-owner]))]))))
                            (:refused state))))))
