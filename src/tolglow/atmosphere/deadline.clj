(ns tolglow.atmosphere.deadline
  "Expire automatically requested nozzle output even when the renderer stalls."
  (:require [afterglow.render.atmosphere :as atmosphere]))

(defn remember [memory frame assignments]
  (let [limits (into {} (for [{:keys [target-id until-ms off-bytes]} assignments, [slot off] off-bytes]
                         [slot {:target-id target-id :deadline until-ms :off off}]))
        previous (into {} (remove (fn [[_ id]] (< (:frame id) (- frame 8)))) (:last-owned memory))
        last-owned (reduce-kv (fn [m slot {:keys [off]}] (assoc m slot {:frame frame :off off})) previous limits)]
    (when (or (seq last-owned) (seq limits))
      {:latest limits :frames (into {frame limits} (remove (fn [[id _]] (<= id (- frame 8)))) (:frames memory))
       :last-owned last-owned})))

(defn apply-off-bytes [packet off-bytes]
  (reduce (fn [out [[u i] off]]
            (if-let [^bytes original (get packet u)]
              (if (= off (bit-and 255 (aget original (int i)))) out
                (let [current ^bytes (get out u)
                      owned (if (identical? original current) (aclone original) current)]
                  (aset-byte owned (int i) (unchecked-byte off))
                  (assoc out u owned)))
              out)) packet off-bytes))

(defn guard-source
  "Use leases attached to the source being presented, even after newer renders
  evict its frame history. Apply them only while their patched pump still owns
  the same addresses. Retirement supplies off values for withdrawn addresses."
  [memory patch packet assignments now stale-source?]
  (apply-off-bytes packet
                   (for [{:keys [target-id until-ms off-bytes]} assignments
                         [slot off] (atmosphere/off-bytes (get-in patch [:by-id target-id]))
                         :when (and (contains? off-bytes slot)
                                    (or stale-source? (>= now until-ms)
                                        (not= target-id (get-in memory [:latest slot :target-id]))))]
                     [slot off])))

(defn next-cutoff [assignments checked-at-ms]
  (reduce (fn [nearest {:keys [until-ms]}]
            (if (and (< checked-at-ms until-ms) (or (nil? nearest) (< until-ms nearest)))
              until-ms
              nearest))
          nil assignments))

(defn guard [memory packet frame now]
  (if-not memory packet
    (let [known? (contains? (:frames memory) frame)
          intervals (get-in memory [:frames frame])
          expired (if known?
                    (keep (fn [[slot {:keys [deadline off]}]] (when (or (>= now deadline) (not (contains? (:latest memory) slot))) [slot off])) intervals)
                    (keep (fn [[slot last-owned]] (when (<= frame (:frame last-owned)) [slot (:off last-owned)])) (:last-owned memory)))]
      (apply-off-bytes packet expired))))

(def saved-ids-path [:extensions ::saved-automatic-pumps])

(defn save-ids [model output]
  (if-let [ids (seq (distinct (map :target-id (:fog-demands output))))]
    (assoc-in model saved-ids-path (vec ids))
    model))

(defn restore-packet [patch ids packet]
  (if (empty? ids) packet
    (let [expired (mapv (fn [id] {:until-ms 0 :off-bytes (atmosphere/off-bytes (get-in patch [:by-id id]))}) ids)]
      (guard (remember nil 0 expired) packet 0 0))))
