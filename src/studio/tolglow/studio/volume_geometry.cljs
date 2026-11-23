(ns tolglow.studio.volume-geometry
  "World-space editing guides sampled from Afterglow's signed distance field."
  (:require [afterglow.spatial.shape :as shape]))

(defn primitive [kind]
  (case kind
    :sphere {:shape :sphere :position [0 0 0] :radius 1}
    :rounded-box {:shape :rounded-box :position [0 0 0] :size [2 2 2] :rounding 0.35}
    :box {:shape :rounded-box :position [0 0 0] :size [2 2 2] :rounding 0}
    :ball {:shape :ball :position [0 0 0] :radius 0.6 :velocity [1 0 0] :restitution 0.85}))

(defn nodes
  ([spec] (nodes [] spec))
  ([path spec]
   (into (cond-> [[path spec]] (:geometry spec) (into (nodes (conj path :geometry) (:geometry spec))))
         (mapcat (fn [[i part]] (nodes (conj path :parts i) part)))
         (map-indexed vector (:parts spec)))))

(def control-fields [:strength :falloff :selection :group])

(defn combine [node operation kind]
  (merge (select-keys node control-fields)
         {:shape operation
          :parts [(apply dissoc node control-fields)
                  (assoc (primitive kind) :position (mapv + (:position node [0 0 0]) [0.6 0 0]))]}))

(defn- midpoint [a b] (mapv #(* 0.5 (+ %1 %2)) a b))

(defn- crossing [[pa a] [pb b]]
  (when (not= (neg? a) (neg? b))
    (let [ratio (/ a (- a b))]
      (mapv (fn [lo hi] (+ lo (* ratio (- hi lo)))) pa pb))))

(defn- cell-edges [corners]
  (let [[a b c d] corners
        crossings (keep identity [(crossing a b) (crossing b c) (crossing c d) (crossing d a)])]
    (mapcat identity (partition 2 crossings))))

(defn- steps [lo hi n]
  (mapv #(+ lo (* (/ % n) (- hi lo))) (range (inc n))))

(defn- contour-slice [distance bounds axis level samples coordinate]
  (let [[u v] (remove #{axis} (range 3))
        us (steps (get (:min bounds) u) (get (:max bounds) u) samples)
        vs (steps (get (:min bounds) v) (get (:max bounds) v) samples)
        sample (fn [a b]
                 (let [p (assoc [0 0 0] axis coordinate u a v b)]
                   [p (- (distance p) level)]))
        grid (mapv (fn [b] (mapv #(sample % b) us)) vs)]
    (into [] (mapcat (fn [[a b]]
                       (mapcat (fn [[p q r s]] (cell-edges [p q r s]))
                               (map vector a (rest a) (rest b) b))))
          (partition 2 1 grid))))

(defn- contours [distance {:keys [min max]} level]
  (let [padding (mapv (fn [a b] (+ level (clojure.core/max 0.001 (* 0.035 (- b a))))) min max)
        bounds {:min (mapv - min padding) :max (mapv + max padding)}]
    (into [] (mapcat (fn [axis]
                       (let [slices (rest (butlast (steps (get (:min bounds) axis) (get (:max bounds) axis) 6)))]
                         (mapcat #(contour-slice distance bounds axis level 14 %) slices))))
          (range 3))))

(defn guides [volume]
  (when volume
    (try
      (let [{:keys [bounds distance]} (shape/prepare volume)
            {:keys [min max]} bounds
            outer (get-in volume [:falloff :outer-m] 0)]
        (when (and bounds (every? true? (map < min max)))
          {:center (midpoint min max) :min min :max max :surface (contours distance bounds 0)
           :outer (when (pos? outer) (contours distance bounds outer))}))
      (catch :default _ nil))))

(defn translate [spec delta]
  (update spec :translation #(mapv + (or % [0 0 0]) delta)))

(defn moving [spec enabled?]
  (let [controls (select-keys spec control-fields)
        body (apply dissoc spec control-fields)]
    (if enabled?
      (let [{:keys [min max]} (:bounds (shape/prepare body))
            center (midpoint min max)
            radius (* 0.5 (apply clojure.core/max (mapv - max min)))]
        (merge controls {:shape :ball :position center :radius radius :velocity [1 0 0] :restitution 0.85
                         :geometry (translate body (mapv - center))}))
      (merge controls (translate (or (:geometry body) {:shape :sphere :position [0 0 0] :radius (:radius body)})
                                 (:position body))))))

(defn stretch [spec axis delta half-extent]
  (let [scale (:scale spec [1 1 1])
        factor (max 0.05 (+ 1 (/ delta (max 0.01 half-extent))))]
    (assoc spec :scale (update scale axis * factor))))
