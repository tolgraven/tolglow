(ns tolglow.show.preview
  "Bounded Studio projection of a committed Afterglow frame."
  (:require [afterglow.render.frame :as render]
            [afterglow.render.photometry :as photometry])
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

(defn brightness-gains
  "Compile optional full-white gains once when the installed rig changes."
  [fixtures]
  (into {} (keep (fn [{:keys [max-watts measured-relative-brightness id] :as fixture}]
                   (when (or max-watts measured-relative-brightness)
                     [id (photometry/relative-brightness fixture)]))) fixtures))

(defn- calibrated-score [score gains]
  (if (empty? gains) score
    (fn [index id]
      (let [raw (double (score index id))
            mover? (> raw 1.0)
            emitted (* (if mover? (- raw 2.0) raw) (double (get gains id 1.0)))]
        ;; Keep both classes of score distinct even for gains greater than one.
        (if (pos? emitted)
          (+ (if mover? 2.0 0.0) (/ emitted (+ 1.0 emitted)))
          0.0)))))

(defn values
  "Prefer emitting moving heads, then the brightest emitting pixels. Keep a
  small idle selection for inspection. Never changes packaged universe data."
  ([output fixtures] (values output fixtures (brightness-gains fixtures)))
  ([{:keys [target-order compact] rich :values} fixtures gains]
  (when (or rich compact)
    (let [indices (preview-indices target-order (calibrated-score (emission-score rich compact) gains))
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
                 indices rows))))))
