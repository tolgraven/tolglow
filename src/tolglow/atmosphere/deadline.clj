(ns tolglow.atmosphere.deadline
  "Expire automatically requested nozzle output even when the renderer stalls.")

(defn remember [memory frame assignments]
  (let [limits (into {} (for [{:keys [until-ms slots]} assignments, slot slots] [slot until-ms]))
        previous (into {} (remove (fn [[_ id]] (< id (- frame 8)))) (:last-owned memory))
        last-owned (reduce #(assoc %1 %2 frame) previous (keys limits))]
    (when (or (seq last-owned) (seq limits))
      {:latest limits :frames (into {frame limits} (remove (fn [[id _]] (<= id (- frame 8)))) (:frames memory))
       :last-owned last-owned})))

(defn guard [memory packet frame now]
  (if-not memory packet
    (let [known? (contains? (:frames memory) frame)
          intervals (get-in memory [:frames frame])
          expired (if known?
                    (keep (fn [[slot deadline]] (when (or (>= now deadline) (not (contains? (:latest memory) slot))) slot)) intervals)
                    (keep (fn [[slot last-owned]] (when (<= frame last-owned) slot)) (:last-owned memory)))]
      (reduce (fn [out [u i]]
                (if-let [^bytes original (get packet u)]
                  (if (zero? (aget original (int i))) out
                    (let [current ^bytes (get out u)
                          owned (if (identical? original current) (aclone original) current)]
                      (aset-byte owned (int i) (byte 0))
                      (assoc out u owned)))
                  out)) packet expired))))
