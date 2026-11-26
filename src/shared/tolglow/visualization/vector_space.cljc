(ns tolglow.visualization.vector-space
  "Geometry of the 3D vector widget as data. The parameter bounds form a cube
  seen through one parallel projection. Every element goes through `project`:
  the shaded far walls, the axes between opposite face centres, their labels
  and the value arrow. The projection is linear, so along any one direction
  the drawn arrow length is proportional to the vector's magnitude.")

(def view-box [0 0 96 80])

(def ^:private azimuth (* 35 (/ #?(:clj Math/PI :cljs js/Math.PI) 180)))

(def ^:private elevation (* 22 (/ #?(:clj Math/PI :cljs js/Math.PI) 180)))

(def ^:private scale 25.0)

(def ^:private center [48.0 40.0])

(defn- cos [x] #?(:clj (Math/cos x) :cljs (js/Math.cos x)))

(defn- sin [x] #?(:clj (Math/sin x) :cljs (js/Math.sin x)))

(defn- sqrt [x] #?(:clj (Math/sqrt x) :cljs (js/Math.sqrt x)))

(defn- finite? [x]
  #?(:clj (and (number? x) (Double/isFinite (double x)))
     :cljs (and (number? x) (js/isFinite x))))

(defn- norm [v] (sqrt (reduce + (map * v v))))

(defn project
  "Box coordinates in [-1,1] on each axis to drawing coordinates. X runs right
  and toward the viewer, Z left and toward the viewer, Y up."
  [[x y z]]
  (let [toward (+ (* x (sin azimuth)) (* z (cos azimuth)))
        [cx cy] center]
    [(+ cx (* scale (- (* x (cos azimuth)) (* z (sin azimuth)))))
     (- cy (* scale (- (* y (cos elevation)) (* toward (sin elevation)))))]))

(def ^:private light
  "Direction to the light, above and in front of the far corner."
  (let [v [0.2 0.85 0.55] length (norm v)] (mapv #(/ % length) v)))

(defn- wall
  "One far wall of the cube: the face where `axis` is at its low bound."
  [id axis]
  (let [[a b] (remove #{axis} [0 1 2])
        corner (fn [u v] (-> [0.0 0.0 0.0] (assoc axis -1.0) (assoc a u) (assoc b v)))
        corners [(corner -1.0 -1.0) (corner 1.0 -1.0) (corner 1.0 1.0) (corner -1.0 1.0)]
        ;; The inward normal is the axis itself, so Lambert's term is one component.
        lit (max 0.0 (nth light axis))]
    {:id id :axis axis :corners corners :points (mapv project corners)
     :shade (+ 0.3 (* 0.7 lit))
     :grid (vec (for [n [-0.5 0.0 0.5]
                      [from to] [[(corner n -1.0) (corner n 1.0)] [(corner -1.0 n) (corner 1.0 n)]]]
                  {:from (project from) :to (project to)}))}))

(def walls
  "The three walls behind the value, darkest first for painting."
  (vec (sort-by :shade [(wall :side 0) (wall :floor 1) (wall :back 2)])))

(def far-corner
  "Where the three walls meet, the deepest point of the box. Shading darkens
  toward it over `:radius` drawing units."
  {:at (project [-1.0 -1.0 -1.0]) :radius (* 2.2 scale)})

(def axes
  "Each axis from the centre of its low face to the centre of its high face.
  The label sits on the projected centre of the high face."
  (mapv (fn [axis]
          (let [low (assoc [0.0 0.0 0.0] axis -1.0)
                high (assoc [0.0 0.0 0.0] axis 1.0)]
            {:axis axis :low low :high high
             :from (project low) :to (project high) :label (project high)}))
        [0 1 2]))

(defn- normalized
  "A value on one axis as a box coordinate, clamped to the bounds."
  [[low high] value]
  (- (* 2.0 (max 0.0 (min 1.0 (/ (- value low) (- high low))))) 1.0))

(defn arrow
  "The value as an arrow from the zero of the bounds. `bounds` holds one
  [low high] pair per axis. A zero outside the bounds starts at the nearest
  face. `:magnitude` is the Euclidean length in parameter units, `:extent` its
  share of the longest vector the bounds allow, and `:length` the arrow in box
  units. With equal bounds on the three axes, `:length` and the drawn
  `:screen-length` are proportional to `:magnitude` along any one direction.
  A vector along the view direction projects short, so the model also carries
  its shadow on the floor."
  [bounds value]
  (when (and (= 3 (count bounds) (count value)) (every? finite? value)
             (every? (fn [[low high]] (and (finite? low) (finite? high) (< low high))) bounds))
    (let [zero (mapv (fn [[low high]] (max low (min high 0.0))) bounds)
          clamped (mapv (fn [[low high] v] (max low (min high v))) bounds value)
          origin (mapv normalized bounds zero)
          tip (mapv normalized bounds clamped)
          farthest (mapv (fn [[low high] z] (max (- high z) (- z low))) bounds zero)
          magnitude (norm (map - clamped zero))
          longest (norm farthest)
          [x1 y1 :as from] (project origin)
          [x2 y2 :as to] (project tip)
          drawn (norm [(- x2 x1) (- y2 y1)])
          head (min 5.0 (* 0.45 drawn))
          [dx dy] (if (pos? drawn) [(/ (- x2 x1) drawn) (/ (- y2 y1) drawn)] [0.0 0.0])
          base [(- x2 (* head dx)) (- y2 (* head dy))]
          half (* 0.42 head)]
      {:origin origin :tip tip :from from :to to
       :length (norm (map - tip origin)) :screen-length drawn
       :magnitude magnitude :extent (if (pos? longest) (/ magnitude longest) 0.0)
       :clamped? (boolean (some false? (map == clamped value)))
       :shaft-to base
       :head [to
              [(- (first base) (* half dy)) (+ (second base) (* half dx))]
              [(+ (first base) (* half dy)) (- (second base) (* half dx))]]
       ;; Where the tip stands over the floor, for the drop line and its foot.
       ;; The floor shadow keeps a vector that points along the view readable.
       :foot (project (assoc tip 1 -1.0))
       :shadow [(project (assoc origin 1 -1.0)) (project (assoc tip 1 -1.0))]})))

(defn points
  "SVG polygon or polyline text for projected points."
  [projected]
  (apply str (interpose " " (map (fn [[x y]] (str x "," y)) projected))))
