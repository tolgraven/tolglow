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

(defn world-position [origin rotation local]
  (let [[x y z] (map #(Math/toRadians (double %)) rotation)
        c #(Math/cos %) s #(Math/sin %)]
    (->> local
         (geometry/rotate [[1 0 0] [0 (c x) (- (s x))] [0 (s x) (c x)]])
         (geometry/rotate [[(c y) 0 (s y)] [0 1 0] [(- (s y)) 0 (c y)]])
         (geometry/rotate [[(c z) (- (s z)) 0] [(s z) (c z) 0] [0 0 1]])
         (geometry/add origin))))

(defn relative-to
  "Express fixture-local bounds relative to a head's local origin."
  [bounds head-offset]
  (update-vals bounds #(geometry/sub % head-offset)))

(defn world-span
  "Project the oriented box onto a single world axis by index."
  [{:keys [id position rotation-deg] :or {rotation-deg [0 0 0]}} {lo :min hi :max} axis]
  (let [[xs ys zs] (map vector lo hi)
        coordinates (for [x xs y ys z zs]
                      (nth (world-position (or position [0 0 0]) rotation-deg [x y z]) axis))]
    (when-not (every? geometry/finite? coordinates)
      (throw (ex-info "Fixture bounds exceed finite world coordinates" {:fixture-id id})))
    [(apply min coordinates) (apply max coordinates)]))
