(ns tolglow.composition.envelope
  "Pure ADSR/AR response sampled from an explicit musical or millisecond clock."
  (:require [tolglow.registry :as registry]
            [tolglow.composition.tween :as tween]
            [afterglow.time :as time]))

(defn normalize
  ([spec] (normalize spec nil))
  ([spec meter]
   (when-not (map? spec)
     (throw (ex-info "Envelope must be a map" {:errors [{:path [] :message "Envelope must be a map"}]})))
   (let [canonical? (some #(contains? spec %) [:attack :decay :release])
         supplied (into {} (for [stage [:attack :decay :release]
                                :let [millisecond-key (keyword (str (name stage) "-ms"))]
                                :when (or (contains? spec stage) (contains? spec millisecond-key))
                                :let [_ (when (and (contains? spec stage) (contains? spec millisecond-key))
                                          (throw (ex-info "Choose one duration for each envelope stage"
                                                          {:errors [{:path [stage] :message "Ambiguous stage duration"}]})))]]
                            [stage (try (time/normalize-duration
                                         (if (contains? spec stage) (get spec stage) {:ms (get spec millisecond-key)}) meter)
                                        (catch clojure.lang.ExceptionInfo cause
                                          (throw (ex-info "Invalid envelope stage"
                                                          {:errors (mapv #(assoc % :path
                                                                                (if (contains? spec stage)
                                                                                  (into [stage] (:path %)) [millisecond-key]))
                                                                         (:errors (ex-data cause)))} cause))))]))
         units (set (map :unit (filter #(pos? (:value %)) (vals supplied))))
         _ (when (> (count units) 1)
             (throw (ex-info "Envelope stages must use one clock domain"
                             {:errors [{:path [] :message "Do not mix positive musical and millisecond stages"}]})))
         unit (or (first units) (when canonical? (:unit (first (vals supplied)))) :ms)
         normalized (merge {:sustain 1.0 :curve :linear} (dissoc spec :attack-ms :decay-ms :release-ms)
                           {:timebase unit :attack-length (get-in supplied [:attack :value] 0)
                            :decay-length (get-in supplied [:decay :value] 0)
                            :release-length (get-in supplied [:release :value] 0)})]
     (when-not (and (registry/finite-number? (:sustain normalized)) (<= 0 (:sustain normalized) 1))
       (throw (ex-info "Invalid envelope" {:errors [{:path [:sustain] :message "Sustain must be between zero and one"}]})))
     (try (tween/curve (:curve normalized) 0.5)
          (catch clojure.lang.ExceptionInfo cause
            (throw (ex-info "Invalid envelope curve" {:errors [{:path [:curve] :message "Unknown easing curve"}]} cause))))
     normalized)))

(defn- gate-level [{:keys [attack-length decay-length sustain curve]} state start position]
  (let [elapsed (- position start)
        initial (:initial-level state 0.0)]
    (cond
      (neg? elapsed) initial
      (< elapsed attack-length) (tween/interpolate initial 1.0 (tween/curve curve (/ elapsed attack-length)))
      (< (- elapsed attack-length) decay-length)
      (tween/interpolate 1.0 sustain (tween/curve curve (/ (- elapsed attack-length) decay-length)))
      :else sustain)))

(defn initial-state
  "Capture both available coordinates and meter. Numeric input means
  milliseconds; musical envelopes need an explicit :beat clock."
  ([clock] (initial-state clock 0.0))
  ([clock level]
   (let [clock (time/frame-clock clock)]
     (when-not (and (or (registry/finite-number? (:time-ms clock)) (registry/finite-number? (:beat clock)))
                    (registry/finite-number? level) (<= 0 level 1))
       (throw (ex-info "Invalid envelope start" {:clock clock :level level})))
     (cond-> {:initial-level level}
       (contains? clock :time-ms) (assoc :started-at-ms (:time-ms clock))
       (contains? clock :beat) (assoc :started-at-beat (:beat clock) :timing-meter (time/normalize-meter (:meter clock)))))))

(defn compile-envelope
  "Return [state clock] -> {:value :done?}. Canonical :attack/:decay/:release
  duration maps use the captured :timing-meter and :started-at-beat/
  :released-at-beat. Explicit *-ms stages use the millisecond clock."
  [spec]
  (normalize spec)
  (fn [state clock]
    (let [{:keys [release-length curve timebase] :as normalized} (normalize spec (:timing-meter state))
          start-key (if (= :beats timebase) :started-at-beat :started-at-ms)
          release-key (if (= :beats timebase) :released-at-beat :released-at-ms)
          now (time/coordinate clock timebase)
          start (get state start-key)
          released (get state release-key)]
      (when-not (and (registry/finite-number? start)
                     (registry/finite-number? (:initial-level state 0.0)) (<= 0 (:initial-level state 0.0) 1)
                     (or (nil? released) (and (registry/finite-number? released) (>= released start)))
                     (or (not (contains? state :release-level))
                         (and (registry/finite-number? (:release-level state)) (<= 0 (:release-level state) 1))))
        (throw (ex-info "Envelope needs finite start/current/release coordinates"
                        {:state state :clock clock :timebase timebase})))
      (if (some? released)
        (if (< now released) {:value (gate-level normalized state start now) :done? false}
            (let [elapsed (- now released)
                  level (get state :release-level (gate-level normalized state start released))]
              {:value (if (>= elapsed release-length) 0.0
                          (* level (- 1 (tween/curve curve (/ elapsed release-length)))))
               :done? (>= elapsed release-length)}))
        {:value (gate-level normalized state start now) :done? false}))))

(defn sample [spec state clock] ((compile-envelope spec) state clock))

(defn release
  "Capture release from the current value; repeated release is idempotent."
  [spec state clock]
  (let [unit (:timebase (normalize spec (:timing-meter state)))
        key (if (= :beats unit) :released-at-beat :released-at-ms)]
    (if (contains? state key) state
        (assoc state key (time/coordinate clock unit) :release-level (:value (sample spec state clock))))))

(defn retrigger
  "Start a new gate, either :restart at zero or :continue at current value."
  [spec state clock mode]
  (case mode
    :restart (initial-state clock)
    :continue (initial-state clock (:value (sample spec state clock)))
    (throw (ex-info "Retrigger must be restart or continue" {:mode mode}))))
