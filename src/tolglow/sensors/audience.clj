(ns tolglow.sensors.audience
  "Combine admitted sensor channels across Bluetooth gateways in one venue.")

(def motion-axes [:accel-x :accel-y :accel-z :rotation-x :rotation-y :rotation-z])

(def gateway-mapping
  {:version 1
   :timeout-ms 1200
   :channels (mapv (fn [id] {:id id :input id}) (into [:audience-count :dance-effort :pulse-count :pulse-bpm :motion-count] motion-axes))})

(defn- between? [lo hi x]
  (and (number? x) (Double/isFinite (double x)) (<= lo x hi)))

(defn- count-in [limit x]
  (when (and (between? 0 limit x) (== (long x) x)) (long x)))

(defn- reading [{:keys [armed? channels]}]
  (when armed?
    (let [people (count-in 128 (:audience-count channels))
          effort (:dance-effort channels)]
      (when (and people (pos? people) (between? 0 1 effort))
        (let [pulse (:pulse-bpm channels)
              axes (mapv channels motion-axes)]
          {:people people :effort effort :bpm pulse :axes axes
           :wearers (if (between? 30 240 pulse) (or (count-in people (:pulse-count channels)) 0) 0)
           :movers (if (every? #(between? -256 256 %) axes)
                     (or (count-in people (:motion-count channels)) 0) 0)})))))

(defn- weighted-mean [readings weight value participants]
  (when (pos? participants)
    (/ (reduce (fn [sum row]
                 (if (pos? (weight row)) (+ sum (* (weight row) (value row))) sum))
               0.0 readings)
       participants)))

(defn levels
  "Weight each measurement by the participating people who supplied it."
  [sources gateway-ids]
  (let [rows (keep #(reading (get-in sources [% :state])) (distinct gateway-ids))
        total #(reduce + 0 (map % rows))
        people (total :people)
        wearers (total :wearers)
        movers (total :movers)]
    (cond-> {:audience-count people :dance-effort (or (weighted-mean rows :people :effort people) 0.0)
             :pulse-count wearers :motion-count movers}
      (pos? wearers) (assoc :pulse-bpm (weighted-mean rows :wearers :bpm wearers))
      (pos? movers) (into (map-indexed (fn [i axis]
                                         [axis (weighted-mean rows :movers #(nth (:axes %) i) movers)])
                                       motion-axes)))))

(defn project [model]
  (let [audience (get-in model [:variables :audience])
        ids (:gateways audience)]
    (cond
      (and (vector? ids) (<= (count ids) 16))
      (let [mix (levels (get-in model [:sensors :sources]) ids)]
        (if (= mix (:levels audience)) model (assoc-in model [:variables :audience :levels] mix)))

      (and (map? audience) (contains? audience :levels))
      (update-in model [:variables :audience] dissoc :levels)

      :else model)))
