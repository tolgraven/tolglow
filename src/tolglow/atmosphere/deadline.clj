(ns tolglow.atmosphere.deadline
  "Expire automatically requested nozzle output even when the renderer stalls."
  (:require [afterglow.render.atmosphere :as atmosphere]))

(defn remember [memory frame assignments]
  (let [limits (into {} (for [{:keys [until-ms off-bytes]} assignments, [slot off] off-bytes] [slot {:deadline until-ms :off off}]))
        previous (into {} (remove (fn [[_ id]] (< (:frame id) (- frame 8)))) (:last-owned memory))
        last-owned (reduce-kv (fn [m slot {:keys [off]}] (assoc m slot {:frame frame :off off})) previous limits)]
    (when (or (seq last-owned) (seq limits))
      {:latest limits :frames (into {frame limits} (remove (fn [[id _]] (<= id (- frame 8)))) (:frames memory))
       :last-owned last-owned})))

(defn guard [memory packet frame now]
  (if-not memory packet
    (let [known? (contains? (:frames memory) frame)
          intervals (get-in memory [:frames frame])
          expired (if known?
                    (keep (fn [[slot {:keys [deadline off]}]] (when (or (>= now deadline) (not (contains? (:latest memory) slot))) [slot off])) intervals)
                    (keep (fn [[slot last-owned]] (when (<= frame (:frame last-owned)) [slot (:off last-owned)])) (:last-owned memory)))]
      (reduce (fn [out [[u i] off]]
                (if-let [^bytes original (get packet u)]
                  (if (= off (bit-and 255 (aget original (int i)))) out
                    (let [current ^bytes (get out u)
                          owned (if (identical? original current) (aclone original) current)]
                      (aset-byte owned (int i) (unchecked-byte off))
                      (assoc out u owned)))
                  out)) packet expired))))

(def saved-ids-path [:extensions ::saved-automatic-pumps])

(defn save-ids [model output]
  (if-let [ids (seq (distinct (map :target-id (:fog-demands output))))]
    (assoc-in model saved-ids-path (vec ids))
    model))

(defn restore-packet [patch ids packet]
  (if (empty? ids) packet
    (let [expired (mapv (fn [id] {:until-ms 0 :off-bytes (atmosphere/off-bytes (get-in patch [:by-id id]))}) ids)]
      (guard (remember nil 0 expired) packet 0 0))))
