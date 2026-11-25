(ns tolglow.integrations.section-patterns
  "Pure, opt-in pattern intents on an owner-supplied musical boundary."
  (:require [tolglow.automation.random :as random]
            [tolglow.integrations.reference-position :as position])
  (:import [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def ^:private defaults {:enabled? false :version 1 :show-seed 0 :on :beat
                         :patterns [] :min-confidence 0.5 :max-causal-age-ms 750.0})

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- confidence? [x] (and (finite? x) (<= 0 x 1)))
(defn- counter? [x] (and (integer? x) (<= 0 x)))
(defn- id? [x] (or (keyword? x) (and (string? x) (seq x)) (integer? x) (uuid? x)))

(defn- policy [input]
  (when-not (and (map? input) (every? (set (keys defaults)) (keys input)))
    (throw (ex-info "Invalid section pattern options" {})))
  (let [{:keys [enabled? version show-seed on patterns min-confidence max-causal-age-ms] :as policy}
        (merge defaults input)]
    (when-not (and (boolean? enabled?) (id? version) (integer? show-seed) (#{:beat :bar :phrase} on)
                   (vector? patterns) (every? id? patterns) (or (not enabled?) (seq patterns))
                   (confidence? min-confidence) (finite? max-causal-age-ms) (pos? max-causal-age-ms))
      (throw (ex-info "Invalid section pattern policy" {})))
    policy))

(defn- section-profile [{:keys [version reference sections duration-ms] :as profile} min-confidence]
  (when profile
    (when-not (and (= 1 version) (id? (:id reference)) (id? (:version reference))
                   (finite? duration-ms) (pos? duration-ms) (vector? sections) (seq sections)
                   (every? (fn [{:keys [id start-position-ms end-position-ms start-beat boundary-confidence]}]
                             (and (id? id) (every? finite? [start-position-ms end-position-ms start-beat])
                                  (<= 0 start-position-ms) (< start-position-ms end-position-ms)
                                  (confidence? boundary-confidence))) sections)
                   (= (count sections) (count (set (map :id sections))))
                   (== 0 (:start-position-ms (first sections)))
                   (== duration-ms (:end-position-ms (peek sections)))
                   (every? (fn [[a b]] (== (:end-position-ms a) (:start-position-ms b)))
                           (partition 2 1 sections)))
      (throw (ex-info "Expected a version 1 track profile with contiguous section positions" {})))
    (reduce (fn [result {:keys [id boundary-kind boundary-confidence features] :as section}]
              (if-let [reason (cond
                                (not= :confirmed-change boundary-kind) :unconfirmed-boundary
                                (< boundary-confidence min-confidence) :low-confidence
                                (not (map? (:signature features))) :missing-signature)]
                (update result :unqualified conj {:section-id id :reason reason})
                (update result :sections conj
                        (select-keys section [:id :family-id :start-position-ms :end-position-ms
                                              :start-beat :boundary-confidence]))))
            {:reference (select-keys reference [:id :version]) :sections [] :unqualified []} sections)))

(defn new-planner
  "Start a planner for a version 1 track profile, or nil for causal-only use.
  A policy enables output explicitly with :enabled? true and a nonempty
  vector of :patterns. Supply :show-seed, :version and :on :beat/:bar/:phrase
  to control reproducibility and the required owner boundary."
  [profile options]
  (let [policy (policy options)]
    {:policy policy :profile (section-profile profile (:min-confidence policy))
     :mode :stopped}))

(defn- clock-id [{:keys [source source-epoch session-id]}] [source source-epoch session-id])

(defn- reference-fence [projection]
  (select-keys projection [:provenance :occurrence :generation]))

(defn- current-projection [state reference-state now-ms]
  (when reference-state
    (let [projected (position/snapshot reference-state now-ms)
          previous (:reference-fence state)
          counters (juxt :occurrence :generation)]
      (when-not (and (= (:provenance previous) (:provenance projected))
                     (neg? (compare (counters projected) (counters previous))))
        projected))))

(defn- choose-pattern [{:keys [show-seed version patterns]} coordinate]
  (let [bytes (.getBytes (pr-str [version show-seed coordinate]) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") bytes)
        seed (.getInt (ByteBuffer/wrap digest))]
    {:decision-seed seed :pattern (second (random/choose seed patterns))}))

(defn- reference-forecast [state projected now-ms scope]
  (let [{:keys [reference position-ms rate lookahead-until-ms provenance occurrence]} projected
        fence (reference-fence projected)
        pending (:pending state)
        due-at (fn [section] (+ now-ms (/ (- (:start-position-ms section) position-ms) rate)))
        preserved (when (and (= :reference (:mode pending)) (= fence (:fence pending))
                             (= scope (:scope pending))
                             (< position-ms (get-in pending [:section :end-position-ms]))
                             (< (due-at (:section pending)) lookahead-until-ms))
                    (:section pending))
        section (or preserved
                    (some (fn [{:keys [start-position-ms] :as section}]
                            (when (and (<= position-ms start-position-ms)
                                       (> start-position-ms (get state :reference-through -1))
                                       (< (due-at section) lookahead-until-ms))
                              section)) (get-in state [:profile :sections])))]
    (when section
      (let [coordinate [(:id reference) (:version reference) (:id section)
                        (:start-position-ms section) (:start-beat section)]]
        (merge (choose-pattern (:policy state) coordinate)
               {:mode :reference :scope scope :fence fence :reference reference :section section
                :due-at-ms (max now-ms (due-at section))
                :ready? (>= position-ms (:start-position-ms section))
                :delivery-id [scope provenance occurrence reference (:id section)]})))))

(defn- recent-causal? [{:keys [time-ms]} now-ms max-age]
  (and (finite? time-ms) (<= 0 (- now-ms time-ms) max-age)))

(defn- causal-candidate [state fresh stream now-ms owner scope]
  (let [{:keys [min-confidence max-causal-age-ms] :as policy} (:policy state)
        pending (:pending state)
        [source epoch session] (clock-id owner)
        event (or (last (sort-by :sequence
                                (filter (fn [{:keys [type boundary-beat confidence clock-source clock-epoch
                                                     clock-session-id] :as event}]
                                          (and (= :audio/section-change type) (= 1 (:version event))
                                               (= [source epoch session] [clock-source clock-epoch clock-session-id])
                                               (finite? boundary-beat) (<= (:fallback-beat state) boundary-beat (:beat owner))
                                               (confidence? confidence) (>= confidence min-confidence)
                                               (recent-causal? event now-ms max-causal-age-ms))) fresh)))
                  (when (and (= :causal (:mode pending)) (= stream (:stream pending)) (= scope (:scope pending))
                             (recent-causal? (:observation pending) now-ms max-causal-age-ms))
                    (:observation pending)))]
    (when event
      (let [{:keys [boundary-beat confidence sequence]} event]
        (merge (choose-pattern policy [(:source-id stream) boundary-beat])
               {:mode :causal :scope scope :stream stream :ready? true
                :observation (select-keys event [:time-ms :boundary-beat :confidence :sequence])
                :section {:start-beat boundary-beat :boundary-confidence confidence}
                :delivery-id [scope stream sequence]})))))

(defn advance
  "Return {:state :mode :forecast :events :unqualified}. Supply monotonic
  :now-ms, optional :reference-state from reference-position, and :transport
  with :show-epoch, :source, :source-epoch, :beat, :playing? and the current
  owner tick's :boundaries set. Optional :session-id identifies the clock.
  Forecasts recheck a fresh lock and never supply their own owner boundary.
  Supply :reference-now-ms in the reference clock's domain if it differs
  from the show clock. Forecast deadlines then use the reference clock.

  For fallback, pass :causal-stream {:source-id :session-id} and new raw
  :causal-events from that capture run. A fallback boundary must have started
  after fallback began. :override? suppresses output and discards pending
  work. Callers must admit returned intents through the shared show owner."
  [state {:keys [now-ms reference-now-ms reference-state transport causal-stream causal-events override?]
          :or {causal-events []}}]
  (let [{:keys [show-epoch beat playing? boundaries]} transport
        scope {:show-epoch show-epoch :clock (clock-id transport)}
        stream (when causal-stream (select-keys causal-stream [:source-id :session-id]))]
    (when-not (and (finite? now-ms) (<= (get state :now-ms now-ms) now-ms)
                   (counter? show-epoch) (<= (get state :show-epoch show-epoch) show-epoch)
                   (id? (:source transport)) (counter? (:source-epoch transport))
                   (or (nil? (:session-id transport)) (id? (:session-id transport)))
                   (finite? beat) (boolean? playing?)
                   (or (nil? boundaries) (and (set? boundaries) (every? #{:beat :bar :phrase} boundaries)))
                   (or (nil? stream) (every? id? ((juxt :source-id :session-id) stream)))
                   (vector? causal-events) (<= (count causal-events) 256))
      (throw (ex-info "Section patterns require current owner time, transport and capture provenance" {})))
    (let [reference-now-ms (or reference-now-ms now-ms)
          projected (current-projection state reference-state reference-now-ms)
          reference-scope (when projected (select-keys projected [:provenance :occurrence]))
          state (cond-> state
                  (not= scope (:scope state)) (dissoc :pending :last-boundary :fallback-beat)
                  (or (not= show-epoch (:show-epoch state))
                      (and reference-scope (not= reference-scope (:reference-scope state))))
                  (dissoc :reference-through)
                  projected (assoc :reference-fence (reference-fence projected))
                  reference-scope (assoc :reference-scope reference-scope)
                  (not= stream (:causal-stream state)) (dissoc :causal-through))
          active? (and (:enabled? (:policy state)) playing? (not override?))
          locked? (and (= :locked (:status projected))
                       (= (:reference projected) (get-in state [:profile :reference])))
          mode (cond (not active?) :stopped locked? :reference stream :causal :else :unavailable)
          state (assoc state :fallback-beat (when (= :causal mode) (or (:fallback-beat state) beat)))
          fresh (when stream
                  (filter #(and (= (:source-id stream) (get-in % [:source :id]))
                                (counter? (:sequence %))
                                (> (:sequence %) (get state :causal-through 0))) causal-events))
          pending (case mode
                    :reference (reference-forecast state projected reference-now-ms scope)
                    :causal (causal-candidate state fresh stream now-ms transport scope)
                    nil)
          boundary? (and (contains? boundaries (get-in state [:policy :on]))
                         (or (nil? (:last-boundary state)) (> beat (:last-boundary state))))
          event (when (and boundary? (:ready? pending))
                  (-> pending
                      (dissoc :ready? :due-at-ms :observation)
                      (assoc :type :music/section-pattern :version 1 :show-beat beat :time-ms now-ms
                             :policy-version (get-in state [:policy :version]))))
          state (cond-> (assoc state :now-ms now-ms :show-epoch show-epoch :scope scope :mode mode
                                    :causal-stream stream :pending (when-not event pending))
                  (seq fresh) (assoc :causal-through (apply max (map :sequence fresh)))
                  boundary? (assoc :last-boundary beat)
                  (= :reference (:mode event)) (assoc :reference-through (get-in event [:section :start-position-ms])))]
      {:state state :mode mode :forecast (when (= :reference mode) (:pending state))
       :events (if event [event] []) :unqualified (get-in state [:profile :unqualified] [])})))
