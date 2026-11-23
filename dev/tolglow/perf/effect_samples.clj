(ns tolglow.perf.effect-samples
  "Read completed governor and keyframe observations for a physical replay.")

(defn- effect-row [generation windows id entry]
  (let [sampled? (= generation (:sampled-at entry))
        window (get windows id)
        outcome (cond
                  (:forecast-failed? window) :forecast-fallback
                  (and window sampled?) :endpoint
                  window :interpolated
                  sampled? :direct
                  :else :no-evaluation)]
    (cond-> {:effect-id (first (:version entry))
             :outcome outcome
             :evaluations (:evaluations entry 0)
             :next-stride (:stride entry)}
      window (assoc :interval (select-keys window [:unit :span])))))

(defn capture
  "Called after the scheduler stamps a completed render. Keep counters and
  duration units only; retaining endpoint arrays would change GC in the replay."
  [row {:keys [cadence keyframes model]}]
  (let [{:keys [epoch frame effects]} (:state cadence)]
    (if (and (:ready-at-ms row) frame (= epoch (:epoch model)))
      (assoc row :effect-sampling
             (reduce-kv (fn [out id entry]
                          (assoc out id (effect-row frame keyframes id entry)))
                        {} effects))
      row)))

(defn summarize
  "Count observed outcomes on completed renders only. An absent observation
  stays absent; a physical slot with no render cannot have a tween sample."
  [renders]
  (reduce (fn [totals row]
            (reduce-kv (fn [totals _ {:keys [effect-id outcome]}]
                         (update-in totals [effect-id outcome] (fnil inc 0)))
                       totals (:effect-sampling row)))
          {} renders))
