(ns tolglow.clock
  "Pure musical-clock rebasing at an explicit source/loop/seek boundary.
  Shift known local beat anchors, not durations or arbitrary user state. No
  clocks, subscriptions, devices, commands or random generators are owned here."
  (:require [afterglow.time :as time]
            [afterglow.control.sensors :as sensors]
            [tolglow.modulation :as modulation]
            [tolglow.integrations.patch :as patch]))

(defn- fail! [message data]
  (throw (ex-info message (assoc data :type ::invalid-rebase))))

(defn- shift-number [value offset path]
  (when-not (time/finite-number? value)
    (fail! "Beat anchor must be finite" {:path path}))
  (let [shifted (+ value offset)]
    (when-not (time/finite-number? shifted)
      (fail! "Rebased anchor exceeds the finite beat domain" {:path path}))
    shifted))

(defn- shift-fields [value fields offset path]
  (reduce (fn [result field]
            (if (some? (get result field))
              (update result field shift-number offset (conj path field)) result)) value fields))

(defn- shift-timer [timer offset path]
  (if (nil? timer) timer
    (case (get-in timer [:duration :unit])
      :beats (update timer :started-at shift-number offset (conj path :started-at))
      :ms timer
      (fail! "Timer needs an explicit musical or millisecond domain" {:path path}))))

(defn- shift-instance [instance offset]
  (let [path [:instances (:id instance)]]
    (cond-> (shift-fields instance [:started-at-beat :release-at-beat] offset path)
      (:duration-timer instance) (update :duration-timer shift-timer offset (conj path :duration-timer))
      (:release-timer instance) (update :release-timer shift-timer offset (conj path :release-timer))
      (:system instance)
      (update :system
              (fn [system]
                (cond-> system
                  (:last-clock system) (update :last-clock shift-fields [:beat] offset (conj path :system :last-clock))
                  (:paused-clock system) (update :paused-clock shift-fields [:beat] offset (conj path :system :paused-clock))
                  (get-in system [:gate :timer])
                  (update-in [:gate :timer] shift-timer offset (conj path :system :gate :timer))))))))

(defn- shift-run [id run offset]
  (if (= :beats (get-in run [:state :timebase] (get-in run [:spec :timebase])))
    (update run :state shift-fields
            [:started-at-beat :last-beat :started-position :last-position :step-start-beat :step-end-beat]
            offset [:composition-runs id :state])
    run))

(defn- shift-auto [auto offset]
  (if-let [state (:state auto)]
    (assoc auto :state
           (cond-> (shift-fields state [:next-at-beat] offset [:auto :state])
             (:interval-timer state)
             (update :interval-timer shift-timer offset [:auto :state :interval-timer]))) auto))

(defn rebase
  "Rebase known musical domains to a captured external clock, preserving local
  elapsed progress. new-clock requires finite :beat. Optional :from-beat overrides
  the old model transport coordinate. Call explicitly for source/source-epoch
  changes and seeks, never for every ordinary advancing transport sample.

  Shift active instance beat anchors and beat timers, frame beat bookkeeping,
  responses, beat sequence positions/windows, auto beat schedules, signal routing and modulation
  schedules/glides/holds. Private system clock anchors and beat gate timers shift;
  elapsed pause offsets do not. Millisecond anchors/timers, durations, IDs/tokens,
  canonical parameters, captured definitions and arbitrary effect state stay as
  they were. Absolute oscillators intentionally read the new transport beat.

  Supplied :time-ms/:bpm/:playing?/:meter/:source/:source-epoch become transport
  metadata only. Wall time must remain nondecreasing; model :time-ms advances
  through the normal engine tick. The function returns a new model or throws
  before returning; it never restarts instances or consumes random samples."
  [model new-clock]
  (when-not (and (map? model) (map? (:transport model)) (map? new-clock))
    (fail! "Clock rebase needs a model, transport and captured clock map" {}))
  (let [captured (time/frame-clock new-clock)
        beat (:beat captured)
        previous (get new-clock :from-beat (get-in model [:transport :beat]))]
    (when-not (and (time/finite-number? previous) (time/finite-number? beat)
                   (time/finite-number? (- beat previous)))
      (fail! "Rebase needs finite old/new beat coordinates" {}))
    (when (and (contains? captured :time-ms)
               (not (and (time/finite-number? (:time-ms captured))
                          (>= (:time-ms captured) (max 0 (:time-ms model 0)
                                                       (get-in model [:transport :time-ms] (:time-ms model 0)))))))
      (fail! "Musical seek must not move the host millisecond clock backwards" {:path [:time-ms]}))
    (when (and (contains? captured :bpm)
               (not (and (time/finite-number? (:bpm captured)) (<= 1 (:bpm captured) 1000))))
      (fail! "BPM must be finite and in [1,1000]" {:path [:bpm]}))
    (when (and (contains? captured :playing?) (not (boolean? (:playing? captured))))
      (fail! "Playing state must be boolean" {:path [:playing?]}))
    (let [offset (- beat previous)
          transport (cond-> (merge (select-keys captured [:time-ms :beat :bpm :playing?])
                                   (select-keys new-clock [:source :source-epoch]))
                      (contains? captured :meter) (assoc :meter (time/normalize-meter (:meter captured))))
          rebased
          (cond-> (-> model
                      (update :transport merge transport)
                      (shift-fields [:last-frame-beat] offset []))
            (map? (:instances model))
            (update :instances #(into {} (map (fn [[id instance]] [id (shift-instance instance offset)])) %))
            (some? (:responses model))
            (update :responses #(mapv (fn [response]
                                       (if (:state response)
                                         (update response :state shift-fields [:started-at-beat :released-at-beat]
                                                 offset [:responses (:id response) :state]) response)) %))
            (map? (:composition-runs model))
            (update :composition-runs #(into {} (map (fn [[id run]] [id (shift-run id run offset)])) %))
            (seq (get-in model [:event-subscribers :runs]))
            (update-in [:event-subscribers :runs] #(mapv (fn [run] (update run :due-beat + offset)) %))
            (:auto model) (update :auto shift-auto offset)
            (:modulation model)
            (update :modulation
                    (fn [state]
                      (let [old (or (:last-beat state) previous)]
                        (modulation/rebase-clock state {:from-beat old
                                                        :beat (shift-number old offset [:modulation :last-beat])}))))
            (get-in model [:routing :state])
            (update-in [:routing :state] patch/rebase-clock
                       (merge (get-in model [:routing :state :clock]) (:transport model)
                              transport {:from-beat previous}))
            (get-in model [:routing :pending])
            (update-in [:routing :pending]
                       #(mapv (fn [entry] (update entry :clock shift-fields [:beat] offset [:routing :pending :clock])) %))
            (get-in model [:routing :ports])
            (update-in [:routing :ports]
                       #(into {} (map (fn [[id entry]]
                                        [id (update entry :clock shift-fields [:beat] offset [:routing :ports id :clock])])) %)))]
      (cond-> rebased
        (:sensors rebased)
        (update-in [:sensors :sources]
                   #(into {} (map (fn [[id source]] [id (update source :state sensors/rebase-clock offset)])) %))))))
