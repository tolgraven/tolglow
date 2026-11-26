(ns tolglow.visualization.vector-space-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.visualization.vector-space :as space]))

(defn- near?
  ([a b] (near? a b 1.0e-9))
  ([a b tolerance]
   (if (sequential? a)
     (and (= (count a) (count b)) (every? true? (map #(near? %1 %2 tolerance) a b)))
     (< (Math/abs (double (- a b))) tolerance))))

(defn- centroid [points]
  (mapv #(/ (reduce + %) (count points)) (apply map vector points)))

(defn- face
  "The four projected corners of the face where one axis sits at one bound."
  [axis bound]
  (let [[a b] (remove #{axis} [0 1 2])]
    (for [u [-1.0 1.0] v [-1.0 1.0]]
      (space/project (-> [0.0 0.0 0.0] (assoc axis bound) (assoc a u) (assoc b v))))))

(def unit [[-1 1] [-1 1] [-1 1]])

(def directions
  [[1 0 0] [0 1 0] [0 0 1] [-1 0 0] [0 -1 0] [0 0 -1] [1 1 1] [-1 1 -1] [0.3 -0.8 0.5] [0.7 0.1 -0.7]])

(defn- scaled [direction amount] (mapv #(* amount %) direction))

(deftest axis-labels-sit-on-the-projected-centre-of-their-high-face
  (is (= [0 1 2] (mapv :axis space/axes)))
  (doseq [{:keys [axis from to label]} space/axes]
    (is (near? (centroid (face axis 1.0)) label) (str "label of axis " axis))
    (is (near? (centroid (face axis -1.0)) from) (str "axis " axis " starts on its low face centre"))
    (is (near? label to) "the axis ends where its label sits")
    (is (near? (space/project [0 0 0]) (centroid [from to])) "the axis passes through the box centre"))
  (let [[x y z] (map :label space/axes)
        [cx cy] (space/project [0 0 0])]
    (is (and (> (first x) cx) (> (second x) cy)) "X runs right and toward the viewer")
    (is (and (near? (first y) cx) (< (second y) cy)) "Y runs straight up")
    (is (and (< (first z) cx) (> (second z) cy)) "Z runs left and toward the viewer")))

(deftest one-parallel-projection-draws-every-element
  (let [origin (space/project [0 0 0])
        offset (fn [p] (mapv - (space/project p) origin))]
    (doseq [a directions b directions]
      (is (near? (offset (mapv + a b)) (mapv + (offset a) (offset b))) "the projection is linear"))
    (doseq [axis [0 1 2]
            :let [edge (fn [from] (mapv - (space/project (update from axis + 2.0)) (space/project from)))
                  [a b] (remove #{axis} [0 1 2])
                  starts (for [u [-1.0 1.0] v [-1.0 1.0]] (-> [0.0 0.0 0.0] (assoc axis -1.0) (assoc a u) (assoc b v)))]]
      (is (apply near? (map edge (take 2 starts))))
      (is (every? #(near? (edge (first starts)) (edge %)) starts) "parallel edges stay parallel and equal"))
    (let [[left top right bottom] space/view-box
          corners (for [x [-1 1] y [-1 1] z [-1 1]] (space/project [x y z]))]
      (is (every? (fn [[x y]] (and (< left x right) (< top y bottom))) corners) "the cube fits the drawing"))))

(deftest arrow-length-is-proportional-to-magnitude
  (doseq [direction directions
          :let [arrows (mapv #(space/arrow unit (scaled direction %)) [0.25 0.5 0.75 1.0])
                [quarter half three-quarters full] arrows
                unit-length (Math/sqrt (reduce + (map * direction direction)))]]
    (is (near? (* 0.25 unit-length) (:magnitude quarter)))
    (is (near? unit-length (:magnitude full)))
    (doseq [[arrow amount] (map vector arrows [0.25 0.5 0.75 1.0])]
      (is (near? (* amount (:screen-length full)) (:screen-length arrow) 1.0e-7) (pr-str direction amount))
      (is (near? (* amount (:length full)) (:length arrow)))
      (is (near? (/ (:magnitude arrow) (Math/sqrt 3)) (:extent arrow)) "extent is relative to the bounds"))
    (is (near? (* 2 (:screen-length quarter)) (:screen-length half) 1.0e-7))
    (is (near? (* 3 (:screen-length quarter)) (:screen-length three-quarters) 1.0e-7))
    (is (pos? (:screen-length quarter)))
    (is (not-any? :clamped? arrows))))

(deftest the-same-value-draws-shorter-in-wider-bounds
  (let [value [0.6 -0.2 0.4]
        narrow (space/arrow unit value)
        wide (space/arrow [[-2 2] [-2 2] [-2 2]] value)]
    (is (near? (:magnitude narrow) (:magnitude wide)))
    (is (near? (/ (:screen-length narrow) 2) (:screen-length wide) 1.0e-7))
    (is (near? (/ (:extent narrow) 2) (:extent wide)))
    (is (near? (space/project [0 0 0]) (:from narrow)) "a signed range starts at the centre")))

(deftest the-arrow-starts-at-the-zero-of-the-bounds
  (let [positive (space/arrow [[0 10] [0 10] [0 10]] [3 4 0])
        offset (space/arrow [[2 5] [2 5] [2 5]] [5 2 2])]
    (is (near? [-1.0 -1.0 -1.0] (:origin positive)) "zero is the low corner of a positive range")
    (is (near? 5.0 (:magnitude positive)))
    (is (near? (/ 5.0 (* 10 (Math/sqrt 3))) (:extent positive)))
    (is (near? [-1.0 -1.0 -1.0] (:origin offset)) "a range above zero starts at its nearest face")
    (is (near? 3.0 (:magnitude offset)))
    (is (near? [1.0 -1.0 -1.0] (:tip offset)))))

(deftest values-outside-the-bounds-stop-at-the-walls
  (let [inside (space/arrow unit [1 0 0])
        outside (space/arrow unit [4 0 0])]
    (is (:clamped? outside))
    (is (false? (:clamped? inside)))
    (is (near? (:to inside) (:to outside)))
    (is (near? 1.0 (:magnitude outside)))
    (is (<= (:extent outside) 1.0))))

(deftest the-arrowhead-points-along-the-arrow-and-leaves-a-shaft
  (doseq [direction directions amount [0.05 0.4 1.0]
          :let [{:keys [from to shaft-to head screen-length]} (space/arrow unit (scaled direction amount))
                [tip left right] head
                along (fn [p] (/ (reduce + (map * (map - p from) (map - to from))) (* screen-length screen-length)))]]
    (is (near? to tip))
    (is (near? shaft-to (centroid [left right])) "the head sits on the end of the shaft")
    (is (<= (- 0.55 1.0e-9) (along shaft-to) 1.0) "more than half of a short arrow is shaft")
    (is (near? (along left) (along right)) "the head is symmetric about the shaft"))
  (let [{:keys [shadow foot origin tip screen-length]} (space/arrow unit [-0.5 -0.35 -0.7])
        [from to] shadow
        shadow-length (Math/hypot (- (first to) (first from)) (- (second to) (second from)))]
    (is (near? foot to))
    (is (near? (space/project (assoc origin 1 -1.0)) from) "the shadow lies on the floor under the arrow")
    (is (near? (space/project (assoc tip 1 -1.0)) to))
    (is (< screen-length 2.0 shadow-length) "a vector along the view stays visible by its shadow"))
  (let [zero (space/arrow unit [0 0 0])]
    (is (zero? (:screen-length zero)))
    (is (zero? (:magnitude zero)))
    (is (near? (:from zero) (:to zero)))))

(deftest the-three-far-walls-are-shaded-by-one-light
  (let [by-id (into {} (map (juxt :id identity)) space/walls)]
    (is (= #{:floor :side :back} (set (keys by-id))))
    (is (= 3 (count (distinct (map :shade space/walls)))) "each wall has its own shade")
    (is (apply < (map :shade space/walls)) "walls paint from darkest to brightest")
    (is (= :floor (:id (last space/walls))) "the floor faces the light most")
    (is (every? #(< 0.3 (:shade %) 1.0) space/walls))
    (doseq [[id axis] {:side 0 :floor 1 :back 2}
            :let [wall (by-id id)]]
      (is (= axis (:axis wall)))
      (is (every? #(= -1.0 (nth % axis)) (:corners wall)) "a far wall lies on the low bound of its axis")
      (is (= (mapv space/project (:corners wall)) (:points wall)))
      (is (= 6 (count (:grid wall)))))
    (is (near? (centroid (apply concat (map (fn [wall] (filter #(= [-1.0 -1.0 -1.0] %) (:corners wall))) space/walls)))
               [-1.0 -1.0 -1.0])
        "the three walls share the far corner")
    (is (= (space/project [-1 -1 -1]) (:at space/far-corner)))
    (is (pos? (:radius space/far-corner)))
    (is (= "1.0,2.0 3.5,4.0" (space/points [[1.0 2.0] [3.5 4.0]])))))

(deftest invalid-values-and-bounds-draw-no-arrow
  (is (nil? (space/arrow unit [0 Double/NaN 0])))
  (is (nil? (space/arrow unit [0 0])))
  (is (nil? (space/arrow [[-1 1] [1 1] [-1 1]] [0 0 0])))
  (is (nil? (space/arrow [[-1 1] [-1 1]] [0 0 0]))))
