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

(defn- measured-rows [rows {:keys [versions samples]}]
  (reduce-kv (fn [result id {:keys [calls]}]
               (if (contains? result id)
                 (assoc-in result [id :calls-this-render] calls)
                 ;; The worker can produce a final contribution and finish in the
                 ;; same call. The governor has already released its lifetime count.
                 (assoc result id {:effect-id (first (get versions id))
                                   :outcome :retired-sampled :calls-this-render calls})))
             rows samples))

(defn capture
  "Called after the scheduler stamps a completed render. Keep counters and
  duration units only; retaining endpoint arrays would change GC in the replay."
  [row {:keys [cadence keyframes model]}]
  (let [{:keys [epoch frame effects]} (:state cadence)
        observed (:observed-samples cadence)]
    (if (and (:ready-at-ms row) frame (= epoch (:epoch model))
             (or (nil? observed) (= (:frame-number observed) (:frame-number model))))
      (let [rows (reduce-kv (fn [out id entry]
                              (assoc out id (cond-> (effect-row frame keyframes id entry)
                                              observed (assoc :calls-this-render 0))))
                            {} effects)]
        (assoc row :effect-sampling (measured-rows rows observed)))
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
