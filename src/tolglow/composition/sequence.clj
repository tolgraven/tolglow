(ns tolglow.composition.sequence
  "Pure owned cue sequencing. Time is supplied, and intents are returned as
  data. The shell correlates launch tokens with accepted runtime instance IDs."
  (:require [tolglow.registry :as registry]
            [afterglow.time :as time]
            [tolglow.automation.random :as random]))

(defn- validate-normalized-sequence [{:keys [steps mode seed timebase] :or {mode :once seed 0 timebase :ms}}]
  (vec
   (concat
    (when-not (and (vector? steps) (seq steps))
      [{:path [:steps] :message "Sequence requires a nonempty step vector"}])
    (when-not (#{:once :loop :ping-pong :random :shuffle} mode)
      [{:path [:mode] :message "Unknown sequence mode"}])
    (when-not (integer? seed) [{:path [:seed] :message "Seed must be an integer"}])
    (when-not (#{:ms :beats} timebase)
      [{:path [:timebase] :message "Timebase must be ms or beats"}])
    (when (vector? steps)
      (concat
       (when-not (= (count steps) (count (distinct (map :id steps))))
         [{:path [:steps] :message "Slot IDs must be unique"}])
       (mapcat
        (fn [[i step]]
          (concat
           (when-not (and (map? step) (some? (:id step)) (some? (:cue-id step)))
             [{:path [:steps i] :message "Step needs stable slot and cue IDs"}])
           (let [key (if (= timebase :beats) :duration-beats :duration-ms)
                 other (if (= timebase :beats) :duration-ms :duration-beats)]
             (concat
              (when-not (and (registry/finite-number? (get step key)) (pos? (get step key)))
                [{:path [:steps i key] :message "Step duration must be positive and finite in the sequence timebase"}])
              (when (contains? step other)
                [{:path [:steps i other] :message "Do not mix beat and millisecond durations in one sequence"}])))
           (when-not (registry/data? step)
             [{:path [:steps i] :message "Step must contain portable data"}])) )
        (map-indexed vector steps)))))))

(defn normalize-spec
  "Resolve musical durations against a launch meter. New steps default to one
  beat; explicit :duration-ms/:duration-beats select their respective clocks."
  ([spec] (normalize-spec spec nil))
  ([spec meter]
   (when-not (and (map? spec) (vector? (:steps spec)) (every? map? (:steps spec)))
     (throw (ex-info "Sequence requires step maps in a vector"
                     {:errors [{:path [:steps] :message "Sequence requires step maps in a vector"}]})))
   (let [steps (:steps spec)
         durations (mapv (fn [i step]
                           (let [keys (filter #(contains? step %) [:duration :duration-ms :duration-beats])]
                             (when (> (count keys) 1)
                               (throw (ex-info "Ambiguous sequence duration"
                                               {:errors [{:path [:steps i] :message "Choose one duration representation"}]})))
                             (try (time/normalize-duration
                                   (cond (contains? step :duration) (:duration step)
                                         (contains? step :duration-ms) {:ms (:duration-ms step)}
                                         (contains? step :duration-beats) {:beats (:duration-beats step)}
                                         (= :ms (:timebase spec)) {:ms 1000}
                                         :else {:beats 1}) meter)
                                  (catch clojure.lang.ExceptionInfo cause
                                    (throw (ex-info "Invalid sequence duration"
                                                    {:errors (mapv #(assoc % :path
                                                                          (if (contains? step :duration)
                                                                            (into [:steps i :duration] (:path %))
                                                                            [:steps i (first keys)]))
                                                                   (:errors (ex-data cause)))} cause)))))) (range) steps)
         units (set (map :unit durations))
         unit (or (:timebase spec) (first units) :beats)]
     (when (or (> (count units) 1) (and (seq units) (not (units unit))))
       (throw (ex-info "Sequence needs one clock domain"
                       {:errors [{:path [:timebase] :message "Do not mix musical and millisecond durations"}]})))
     (assoc spec :timebase unit
            :steps (when (vector? steps)
                     (mapv (fn [step duration]
                             (assoc (dissoc step :duration :duration-ms :duration-beats)
                                    (if (= :beats unit) :duration-beats :duration-ms) (:value duration))) steps durations))))))

(defn validate-sequence [spec]
  (try (validate-normalized-sequence (normalize-spec spec))
       (catch clojure.lang.ExceptionInfo cause
         (or (:errors (ex-data cause)) [{:path [] :message (.getMessage cause)}]))))

(defn- checked [spec meter]
  (let [spec (normalize-spec spec meter)
        errors (validate-normalized-sequence spec)]
    (when (seq errors) (throw (ex-info "Invalid sequence" {:errors errors}))) spec))

(defn- clock [spec value]
  (let [clock (time/frame-clock value)
        beats? (= :beats (:timebase spec))]
    (when-not (and (map? clock) (registry/finite-number? (:time-ms clock))
                   (or (not beats?)
                       (and (registry/finite-number? (:beat clock))
                            (registry/finite-number? (:bpm clock)) (pos? (:bpm clock)))))
      (throw (ex-info "Sequence needs explicit time-ms; beat sequences also need finite beat and positive bpm"
                      {:clock value :timebase (:timebase spec :ms)})))
    (assoc clock :position (if beats? (:beat clock) (:time-ms clock)))))

(defn- position-keys [spec]
  (if (= :beats (:timebase spec)) [:step-start-beat :step-end-beat :duration-beats]
      [:step-start-ms :step-end-ms :duration-ms]))

(defn- set-step-window [spec state start]
  (let [[start-key end-key duration-key] (position-keys spec)
        end (+ start (get-in spec [:steps (:step-index state) duration-key]))]
    (when-not (registry/finite-number? end)
      (throw (ex-info "Sequence step boundary exceeds finite time" {:start start})))
    (assoc state start-key start end-key end)))

(defn- select-step [spec state first?]
  (let [n (count (:steps spec))
        mode (:mode spec :once)
        i (:step-index state)
        direction (:direction state 1)]
    (case mode
      :once (if first? (assoc state :step-index 0)
                (if (< (inc i) n) (update state :step-index inc) (assoc state :done? true)))
      :loop (assoc state :step-index (if first? 0 (mod (inc i) n)))
      :ping-pong (if (or first? (= 1 n)) (assoc state :step-index 0 :direction 1)
                    (let [direction (if (or (>= (+ i direction) n) (neg? (+ i direction)))
                                      (- direction) direction)]
                      (assoc state :step-index (+ i direction) :direction direction)))
      :random (let [[seed index] (random/choose (:seed state) (range n))]
                (assoc state :seed seed :step-index index))
      :shuffle (let [[seed bag] (if (seq (:remaining state)) [(:seed state) (:remaining state)]
                                   (random/shuffle-values (:seed state) (range n)))]
                 (assoc state :seed seed :step-index (first bag) :remaining (vec (rest bag)))))))

(defn initial-state
  "Create a sequence execution with an explicit unique owner ID. A definition
  may have many simultaneous executions; use a different owner for each one.
  Existing millisecond specs accept a numeric time or {:time-ms n}. Specs with
  :timebase :beats use :duration-beats slots and require an explicit clock map
  {:time-ms n :beat b :bpm tempo}. Beat position, not elapsed wall time, drives
  boundaries; bpm is used only for a UI deadline estimate."
  [spec time owner-id]
  (when-not (and (some? owner-id) (registry/data? owner-id))
    (throw (ex-info "Sequence needs an owner and finite start time" {})))
  (let [meter (time/normalize-meter (:meter (time/frame-clock time)))
        spec (checked spec meter)
        {:keys [time-ms beat bpm position]} (clock spec time)
        state (select-step spec {:owner-id owner-id :visit 0 :seed (:seed spec 0)
                                 :done? false :active-token nil :started-at-ms time-ms
                                 :last-time-ms time-ms :started-position position
                                 :last-position position :timebase (:timebase spec :ms) :timing-meter meter} true)
        state (cond-> state (= :beats (:timebase spec))
                (assoc :started-at-beat beat :last-beat beat :bpm bpm))]
    (set-step-window spec state position)))

(defn- launch-intent [spec state]
  (let [step (get-in spec [:steps (:step-index state)])]
    {:op :composition/launch :owner-id (:owner-id state) :member (:step-index state)
     :token [(:owner-id state) (:visit state)] :slot-id (:id step)
     :cue-id (:cue-id step)
     :launch (merge {:mode :persistent :params (:params step {})} (:launch step))}))

(defn advance
  "Advance to logical time and return {:state :intents :step :done?}.
  Missed steps are skipped, not burst-launched. At most :max-transitions (4096
  default) boundaries may be crossed in one call; larger seeks fail before a
  result is returned. Repeated time with returned state emits no duplicates.
  A shell maps composition release tokens to captured instance IDs. Beat specs
  accept {:time-ms :beat :bpm}, preserving phase across tempo changes and pause
  (constant beat). Backward beat seeks need a fresh owned execution."
  [spec state time]
  (let [spec (checked spec (:timing-meter state))
        {:keys [time-ms position beat bpm playing?]} (clock spec time)
        [start-key end-key _] (position-keys spec)
        limit (:max-transitions spec 4096)]
    (when-not (and (= (:timebase spec :ms) (:timebase state :ms))
                   (>= time-ms (:last-time-ms state))
                   (>= position (:last-position state (:last-time-ms state))))
      (throw (ex-info "Sequence time and beat must be monotonic with an unchanged timebase; seek with a fresh execution"
                      {:clock time :timebase (:timebase spec :ms)})))
    (when-not (and (integer? limit) (pos? limit))
      (throw (ex-info "Sequence transition budget must be positive" {})))
    (let [next-state
          (loop [current state transitions 0]
            (if (or (:done? current) (< position (get current end-key))) current
                (do
                  (when (>= transitions limit)
                    (throw (ex-info "Sequence advance exceeds transition budget"
                                    {:limit limit :time-ms time-ms})))
                  (let [start (get current end-key)
                        next (select-step spec (update current :visit inc) false)
                        next (if (:done? next) next
                                 (set-step-window spec next start))]
                    (recur next (inc transitions))))))
          ready? (>= position (:started-position state (:started-at-ms state)))
          token (when (and ready? (not (:done? next-state)))
                  [(:owner-id state) (:visit next-state)])
          old-token (:active-token state)
          changed? (not= token old-token)
          intents (cond-> []
                    (and changed? old-token)
                    (conj {:op :composition/release :owner-id (:owner-id state) :token old-token})
                    (and changed? token)
                    (conj (launch-intent spec next-state)))
          next-state (cond-> (assoc next-state :last-time-ms time-ms :last-position position :active-token token)
                       (= :beats (:timebase spec)) (assoc :last-beat beat :bpm bpm))
          remaining (when token (- (get next-state end-key) position))
          remaining-ms (when (and token (not (false? playing?)))
                         (if (= :beats (:timebase spec)) (* remaining (/ 60000.0 bpm)) remaining))]
      {:state next-state :intents intents :done? (:done? next-state)
       :step (when token (get-in spec [:steps (:step-index next-state)]))
       :phase (when token (/ (- position (get next-state start-key))
                              (- (get next-state end-key) (get next-state start-key))))
       :remaining-ms remaining-ms
       :remaining-beats (when (= :beats (:timebase spec)) remaining)})))

(defn stop
  "Release this execution's current token exactly once."
  [state]
  {:state (assoc state :done? true :active-token nil)
   :intents (if-let [token (:active-token state)]
              [{:op :composition/release :owner-id (:owner-id state) :token token}] [])
   :done? true})
