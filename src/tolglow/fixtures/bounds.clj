(ns tolglow.fixtures.bounds
  "Declared local fixture and head extents in metres."
  (:require [tolglow.calibration.geometry :as geometry]))

(defn errors [fixture]
  (concat
   (for [key [:bounds-m :fixture-bounds-m]
         :when (contains? fixture key)
         :let [bounds (get fixture key)
               lo (:min bounds)
               hi (:max bounds)]
         :when (not (and (map? bounds) (= #{:min :max} (set (keys bounds)))
                         (geometry/v3? lo) (geometry/v3? hi)
                         (every? true? (map <= lo hi))))]
     {:path [key] :message "Bounds need finite :min and :max XYZ vectors in metres, with min <= max"})
   (let [pose (:fixture-bounds-pose fixture)]
     (when (and (contains? fixture :fixture-bounds-pose)
                (not (and (contains? fixture :fixture-bounds-m)
                          (map? pose) (= #{:position :rotation-deg} (set (keys pose)))
                          (geometry/v3? (:position pose)) (geometry/v3? (:rotation-deg pose)))))
       [{:path [:fixture-bounds-pose]
         :message "Fixture bounds pose needs body bounds and finite XYZ position and rotation vectors"}]))))

(defn- cosine-sine [degrees]
  ;; Reduce in degrees before converting to radians so full turns and
  ;; complementary angles reuse the same coefficients.
  (let [angle (StrictMath/IEEEremainder (double degrees) 360.0)
        residual (StrictMath/IEEEremainder angle 90.0)
        quadrant (mod (long (/ (- angle residual) 90.0)) 4)
        reference (Math/abs residual)
        [c s] (cond
                (zero? reference) [1.0 0.0]
                (== reference 30.0) [(StrictMath/sqrt 0.75) 0.5]
                (== reference 45.0) (let [diagonal (StrictMath/sqrt 0.5)] [diagonal diagonal])
                :else (let [radians (StrictMath/toRadians reference)]
                        [(StrictMath/cos radians) (StrictMath/sin radians)]))
        s (Math/copySign (double s) residual)]
    (case quadrant
      0 [c s]
      1 [(- s) c]
      2 [(- c) (- s)]
      3 [s (- c)])))

(defn- rotation-matrices [rotation]
  (let [[[cx sx] [cy sy] [cz sz]] (mapv cosine-sine rotation)]
    [[[1 0 0] [0 cx (- sx)] [0 sx cx]]
     [[cy 0 sy] [0 1 0] [(- sy) 0 cy]]
     [[cz (- sz) 0] [sz cz 0] [0 0 1]]]))

(defn- rotate-point [matrices point]
  (reduce (fn [point matrix] (geometry/rotate matrix point)) point matrices))

(defn world-position [origin rotation local]
  (geometry/add origin (rotate-point (rotation-matrices rotation) local)))

(defn world-span
  "Project the oriented box onto a single world axis by index."
  [{:keys [id position rotation-deg] :or {rotation-deg [0 0 0]}} {lo :min hi :max} axis]
  (let [matrices (rotation-matrices rotation-deg)
        origin (nth (or position [0 0 0]) axis)
        [xs ys zs] (map vector lo hi)
        coordinates (for [x xs y ys z zs]
                      (nth (rotate-point matrices [x y z]) axis))
        lower (apply min coordinates)
        upper (apply max coordinates)
        span [(+ origin lower) (+ origin upper)]]
    (when-not (and (every? geometry/finite? coordinates) (every? geometry/finite? span))
      (throw (ex-info "Fixture bounds exceed finite world coordinates" {:fixture-id id})))
    span))
