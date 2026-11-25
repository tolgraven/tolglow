(ns tolglow.fixtures.bounds
  "Declared local fixture and head extents in metres."
  (:require [tolglow.calibration.geometry :as geometry]))

(defn errors [fixture]
  (for [key [:bounds-m :fixture-bounds-m]
        :when (contains? fixture key)
        :let [bounds (get fixture key)
              lo (:min bounds)
              hi (:max bounds)]
        :when (not (and (map? bounds) (= #{:min :max} (set (keys bounds)))
                        (geometry/v3? lo) (geometry/v3? hi)
                        (every? true? (map <= lo hi))))]
    {:path [key] :message "Bounds need finite :min and :max XYZ vectors in metres, with min <= max"}))

(defn- quarter-turn [degrees]
  (let [angle (rem (double degrees) 360.0)]
    (cond
      (zero? angle) 0
      (or (== angle 90.0) (== angle -270.0)) 1
      (or (== angle 180.0) (== angle -180.0)) 2
      (or (== angle 270.0) (== angle -90.0)) 3)))

(defn- cosine-sine [degrees turn]
  (case turn
    0 [1.0 0.0]
    1 [0.0 1.0]
    2 [-1.0 0.0]
    3 [0.0 -1.0]
    (let [angle (StrictMath/toRadians (double degrees))]
      [(StrictMath/cos angle) (StrictMath/sin angle)])))

(defn- rotation-matrices [rotation turns]
  (let [[[cx sx] [cy sy] [cz sz]] (mapv cosine-sine rotation turns)]
    [[[1 0 0] [0 cx (- sx)] [0 sx cx]]
     [[cy 0 sy] [0 1 0] [(- sy) 0 cy]]
     [[cz (- sz) 0] [sz cz 0] [0 0 1]]]))

(defn- rotate-point [matrices point]
  (reduce (fn [point matrix] (geometry/rotate matrix point)) point matrices))

(defn world-position [origin rotation local]
  (geometry/add origin (rotate-point (rotation-matrices rotation (mapv quarter-turn rotation)) local)))

(defn relative-to
  "Express fixture-local bounds relative to a head's local origin."
  [bounds head-offset]
  (update-vals bounds #(geometry/sub % head-offset)))

(defn- axis-aligned? [turns axis]
  ;; Trace the world axis back through Z, Y and X. A rotation around the
  ;; current axis leaves it alone. Odd quarter turns swap the other two axes.
  (loop [turn-axis 2 axis axis]
    (if (neg? turn-axis)
      true
      (let [turn (nth turns turn-axis)]
        (cond
          (= axis turn-axis) (recur (dec turn-axis) axis)
          (nil? turn) false
          :else (recur (dec turn-axis) (if (odd? turn) (- 3 axis turn-axis) axis)))))))

(defn- project-span
  [{:keys [id position rotation-deg] :or {rotation-deg [0 0 0]}} {lo :min hi :max} axis selection?]
  (let [turns (mapv quarter-turn rotation-deg)
        matrices (rotation-matrices rotation-deg turns)
        origin (nth (or position [0 0 0]) axis)
        [xs ys zs] (map vector lo hi)
        coordinates (for [x xs y ys z zs]
                      (nth (rotate-point matrices [x y z]) axis))
        lower (apply min coordinates)
        upper (apply max coordinates)
        span [(+ origin lower) (+ origin upper)]]
    (when-not (and (every? geometry/finite? coordinates) (every? geometry/finite? span))
      (throw (ex-info "Fixture bounds exceed finite world coordinates" {:fixture-id id})))
    (if (and selection? (not (axis-aligned? turns axis)))
      (let [outward [(+ origin (Math/nextDown (double lower)))
                     (+ origin (Math/nextUp (double upper)))]]
        (mapv (fn [edge computed] (if (geometry/finite? edge) edge computed)) outward span))
      span)))

(defn world-span
  "Project the oriented box onto a single world axis by index."
  [fixture box axis]
  (project-span fixture box axis false))

(defn selection-span
  "Return inclusive comparison edges. Allow one floating-point step on an
  oblique axis before adding the fixture's world position."
  [fixture box axis]
  (project-span fixture box axis true))
