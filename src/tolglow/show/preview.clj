(ns tolglow.show.preview
  "Bounded Studio projection of a committed Afterglow frame."
  (:require [afterglow.render.frame :as render])
  (:import [java.util PriorityQueue]))

(def ^:private active-limit 128)
(def ^:private idle-limit 16)

(defn- emission-score [rich compact]
  (if rich
    (fn [_ id]
      (let [{:keys [emitted-color channels]} (get rich id)
            level (if emitted-color (double (reduce max emitted-color)) 0.0)]
        (if (and (pos? level) (contains? channels :pan) (contains? channels :tilt))
          (+ 2.0 level) level)))
    ;; Read just the three committed emitter components. Only the selected
    ;; rows will be expanded into ordinary maps with pose and channel data.
    (let [fields (:fields compact)
          ^doubles colors (get-in fields [:emitted-color :values])
          ^booleans valid (get-in fields [:emitted-color :valid])
          ^booleans pan (get-in fields [:pan :valid])
          ^booleans tilt (get-in fields [:tilt :valid])]
      (fn [i _]
        (if (and valid (aget valid i))
          (let [base (* 3 (int i))
                level (max (aget colors base) (aget colors (inc base)) (aget colors (+ base 2)))]
            (if (and (pos? level) pan tilt (aget pan i) (aget tilt i)) (+ 2.0 level) level))
          0.0)))))

(defn- consider! [^PriorityQueue candidates limit level tie i]
  ;; Only allocate a heap entry when it can survive the cutoff.
  (let [full? (= limit (.size candidates))
        weakest (when full? (.peek candidates))]
    (when (or (not full?) (> level (double (first weakest)))
              (and (== level (double (first weakest))) (> tie (long (nth weakest 1)))))
      (when full? (.poll candidates))
      (.add candidates [level tie i]))))

(defn- preview-indices [order score]
  (if (<= (count order) idle-limit)
    (range (count order))
    (let [active (PriorityQueue.) fixed (PriorityQueue.)]
      (dotimes [i (count order)]
        (let [id (nth order i)
              level (double (score i id))]
          (when (pos? level)
            (let [tie (hash id)]
              (consider! active active-limit level tie i)
              ;; Preserve some strip/static emitters even in a wall of movers.
              (when (<= level 1.0) (consider! fixed idle-limit level tie i))))))
      (let [lit (if (= active-limit (.size active))
                  (into #{} (comp (map peek) (distinct) (take active-limit))
                        (concat fixed (sort #(compare %2 %1) active)))
                  (into #{} (map peek) active))]
        (if (< (count lit) idle-limit)
          (into lit (take (- idle-limit (count lit)) (remove lit (range idle-limit))))
          lit)))))

(defn values
  "Prefer emitting moving heads, then the brightest emitting pixels. Keep a
  small idle selection for inspection. Never changes packaged universe data."
  [{:keys [target-order compact] rich :values} fixtures]
  (when (or rich compact)
    (let [indices (preview-indices target-order (emission-score rich compact))
          rows (if rich (map #(get rich (nth target-order %)) indices)
                   (render/project-compact compact indices))
          by-id (delay (into {} (map (juxt :id identity)) fixtures))]
      (into {}
            (keep identity)
            (map (fn [i row]
                   (when row
                     (let [id (nth target-order i)
                           same-slot (nth fixtures i nil)
                           fixture (if (= id (:id same-slot)) same-slot (get @by-id id))
                           pose (when fixture (render/fixture-pose fixture row))]
                       [id (cond-> row pose (assoc :pose pose))])))
                 indices rows)))))
