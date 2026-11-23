(ns tolglow.sensors.audience
  "Combine admitted sensor channels across Bluetooth gateways in one venue.")

(def gateway-mapping
  {:version 1
   :timeout-ms 1200
   :channels (mapv (fn [id] {:id id :input id}) [:audience-count :dance-effort :pulse-count :pulse-bpm])})

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
              wearers (or (and (between? 30 240 pulse) (count-in people (:pulse-count channels))) 0)]
          [people (* people (double effort)) wearers (if (pos? wearers) (* wearers (double pulse)) 0.0)])))))

(defn levels
  "Use one vote per participant, and only consenting wearers for the pulse mean.
  Source states already reflect host enrollment, arming and expiry."
  [sources gateway-ids]
  (let [readings (keep #(reading (get-in sources [% :state])) (distinct gateway-ids))
        [people effort wearers pulse] (reduce (partial mapv +) [0 0.0 0 0.0] readings)]
    (cond-> {:audience-count people :dance-effort (if (pos? people) (/ effort people) 0.0)
             :pulse-count wearers}
      (pos? wearers) (assoc :pulse-bpm (/ pulse wearers)))))

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
