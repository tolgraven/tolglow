(ns tolglow.bench-movers-reference
  "Independent moving-rig geometry and physical-channel oracle for profiling.")

(defn fixtures [n]
  (mapv (fn [i]
          (let [slot (+ 506 (* 12 i))]
            {:id (str "moving-" i) :benchmark/index i
             :position [(- (* 2.3 (mod i 8)) 8.0) (+ 3.5 (* 0.3 (mod i 5)))
                        (- (* 2.1 (quot i 8)) 6.0)]
             :rotation-deg [(- (* 7.0 (mod i 7)) 21.0)
                            (- (* 17.0 (mod i 13)) 102.0)
                            (- (* 11.0 (mod i 9)) 44.0)]
             :calibration {:pan {:min-deg -270.0 :max-deg 270.0}
                           :tilt {:min-deg -135.0 :max-deg 135.0}}
             :universe (inc (quot slot 512)) :address (inc (mod slot 512)) :footprint 12
             :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5
                        :pan [6 7] :tilt [8 9] :focus 10 :zoom 11}})) (range n)))

(def layer-values
  [{:color [0.9 0.1 0.15] :white 0.04 :dimmer 0.65 :focus 0.2 :zoom 0.15}
   {:color [0.1 0.8 0.25] :white 0.12 :dimmer 0.8 :focus 0.6 :zoom 0.45}
   {:color [0.15 0.2 0.95] :white 0.23 :dimmer 0.95 :focus 0.85 :zoom 0.75}])

;; Explicit endpoint, rise/fall and concurrent-layer combinations repeat during
;; warmup and measurement. The first sixteen preview targets are not special.
(def alpha-cycle
  [[1.0 0.0 0.0] [1.0 0.25 0.0] [1.0 0.5 0.0] [1.0 0.75 0.0] [1.0 1.0 0.0]
   [1.0 0.75 0.25] [1.0 0.5 0.5] [1.0 0.25 0.75] [1.0 0.0 1.0]
   [0.75 0.25 0.75] [0.5 0.5 0.5] [0.25 0.75 0.25] [0.0 1.0 0.0]
   [0.0 0.0 1.0] [0.0 0.0 0.0] [0.25 0.0 0.0] [0.5 0.0 0.0]
   [0.75 0.0 0.0] [1.0 1.0 1.0] [0.4 0.7 0.2]])

(defn alphas [i] (nth alpha-cycle (mod i (count alpha-cycle))))
(defn effect-id [layer group] (keyword "moving-profile" (str "layer-" layer "-group-" group)))
(defn- op [kind & args] {:op kind :args (vec args)})

(defn definitions [fixtures]
  (vec (for [layer (range 3) group (range 8)
             :let [phase (op :add (op :add (op :multiply {:op :position :axis :x} 0.137)
                                                       (op :multiply {:op :position :axis :z} 0.19))
                                 (+ (* group 0.2) (* layer 0.7)))
                   theta (op :add (op :multiply {:op :beat} (+ 0.35 (* group 0.02) (* layer 0.1))) phase)]]
         {:id (effect-id layer group) :version 1 :priority (* layer 10)
          :targets (mapv :id (filter #(= group (mod (:benchmark/index %) 8)) fixtures))
          :outputs (assoc (nth layer-values layer) :aim
                          (op :vec3
                              (op :add (op :multiply 5.0 (op :sin theta))
                                  (op :multiply {:op :position :axis :x} 0.1))
                              (op :add 1.2 (op :multiply 0.6 (op :cos (op :multiply theta 0.7))))
                              (op :add (+ 2.0 layer) (op :multiply 4.0 (op :cos theta)))))})))

(defn expected-aim [fixture layer beat]
  (let [[x _ z] (:position fixture) group (mod (:benchmark/index fixture) 8)
        phase (+ (+ (* x 0.137) (* z 0.19)) (+ (* group 0.2) (* layer 0.7)))
        theta (+ (* beat (+ 0.35 (* group 0.02) (* layer 0.1))) phase)]
    [(+ (* 5.0 (Math/sin theta)) (* x 0.1))
     (+ 1.2 (* 0.6 (Math/cos (* theta 0.7))))
     (+ (+ 2.0 layer) (* 4.0 (Math/cos theta)))]))

(defn expected-assigned [fixture beat alpha]
  (reduce (fn [values layer]
            (let [gain (nth alpha layer)]
              (if (zero? gain) values
                  (reduce-kv (fn [values key value]
                               (let [old (get values key (if (vector? value) [0.0 0.0 0.0] 0.0))
                                     blend (fn [a b] (if (= gain 1.0) b (+ a (* gain (- b a)))))]
                                 (assoc values key (if (vector? value) (mapv blend old value) (blend old value)))))
                             values (assoc (nth layer-values layer) :aim (expected-aim fixture layer beat))))))
          {} (range 3)))

(defn- dot [a b] (reduce + (map * a b)))
(defn- transpose [matrix] (apply mapv vector matrix))
(defn- matrix-product [a b] (mapv (fn [row] (mapv #(dot row %) (transpose b))) a))
(defn- local-vector [rotation world]
  (let [[x y z] (map #(Math/toRadians (double %)) rotation)
        cx (Math/cos x) sx (Math/sin x) cy (Math/cos y) sy (Math/sin y) cz (Math/cos z) sz (Math/sin z)
        rx [[1.0 0.0 0.0] [0.0 cx (- sx)] [0.0 sx cx]]
        ry [[cy 0.0 sy] [0.0 1.0 0.0] [(- sy) 0.0 cy]]
        rz [[cz (- sz) 0.0] [sz cz 0.0] [0.0 0.0 1.0]]
        world-basis (matrix-product rz (matrix-product ry rx))]
    ;; Forward placement matrix is inverted by transposition for rigid rotation.
    (mapv #(dot % world) (transpose world-basis))))

(defn- normalized [fixture axis angle previous]
  (let [{lo :min-deg hi :max-deg} (get-in fixture [:calibration axis])
        convert #(/ (- % lo) (- hi lo))
        candidates (mapv convert (if (= :pan axis) [angle (- angle 360.0) (+ angle 360.0)] [angle]))
        legal (filterv #(<= 0.0 % 1.0) candidates)
        chosen (first (sort-by #(Math/abs (- (double %) (double (or previous (convert 0.0)))))
                              (if (seq legal) legal candidates)))]
    (max 0.0 (min 1.0 chosen))))

(defn reference-geometry [fixture aim previous]
  (let [world (mapv - aim (:position fixture))
        [x y z] (local-vector (:rotation-deg fixture) world)
        size (Math/sqrt (dot world world))]
    {:pan (normalized fixture :pan (Math/toDegrees (Math/atan2 x z)) (:pan previous))
     :tilt (normalized fixture :tilt (Math/toDegrees (Math/atan2 y (Math/hypot x z))) (:tilt previous))
     :direction (mapv #(/ % size) world)}))

(defn- check! [expected actual details]
  (when-not (if (number? expected)
              (and (number? actual) (<= (Math/abs (- (double expected) (double actual))) 1e-9))
              (= expected actual))
    (throw (ex-info "Moving-head reference mismatch" (assoc details :expected expected :actual actual)))))

(defn- expected-bytes [level fine?]
  (let [scaled (* 255.0 (double level)) coarse (long scaled)]
    (if fine? [coarse (Math/round (* 255.0 (- scaled coarse)))] [(Math/round scaled)])))

(defn- slot-byte [universes fixture offset]
  (let [slot (+ (dec (:address fixture)) (dec offset))
        universe (+ (:universe fixture) (quot slot 512))]
    (bit-and 255 (aget ^bytes (get universes universe) (mod slot 512)))))

(defn check-frame! [fixtures beat alpha previous-preview preview universes]
  (check! (count fixtures) (count preview) {:field :all-head-preview})
  (doseq [[index fixture] (map-indexed vector fixtures)]
    (let [actual (nth preview index) assigned (expected-assigned fixture beat alpha)
          geometry (when (:aim assigned) (reference-geometry fixture (:aim assigned) (get previous-preview index)))
          color (:color assigned [0.0 0.0 0.0]) white (:white assigned 0.0) dimmer (:dimmer assigned 1.0)
          values (merge {:pan 0.0 :tilt 0.0 :focus 0.0 :zoom 0.0 :white white :dimmer dimmer}
                        (zipmap [:red :green :blue] color) assigned geometry)]
      (check! (:id fixture) (:id actual) {:fixture index :field :id})
      (doseq [field [:pan :tilt :focus :zoom]]
        (check! (get values field) (get actual field) {:fixture index :field field}))
      (doseq [field [:aim :direction]]
        (if-let [expected (get values field)]
          (doseq [component (range 3)]
            (check! (nth expected component) (get-in actual [field component]) {:fixture index :field field :component component}))
          (check! nil (get actual field) {:fixture index :field field})))
      (doseq [[field offsets] (:channels fixture)
              :let [offsets (if (vector? offsets) offsets [offsets])
                    bytes (expected-bytes (get values field) (= 2 (count offsets)))]
              [offset expected] (map vector offsets bytes)]
        (check! expected (slot-byte universes fixture offset) {:fixture index :field field :channel-offset offset}))))
  {:checked-heads (count fixtures) :checked-heads-after-16 (max 0 (- (count fixtures) 16))
   :universe-checksum (reduce (fn [checksum [_ bytes]]
                               (unchecked-add-int (unchecked-multiply-int 31 checksum) (java.util.Arrays/hashCode ^bytes bytes)))
                             1 (sort-by key universes))})

(defn self-check! []
  (let [fixture (assoc (first (fixtures 1)) :position [0.0 0.0 0.0] :rotation-deg [0.0 0.0 0.0])
        forward (reference-geometry fixture [0.0 0.0 1.0] nil)
        rotated (reference-geometry (assoc fixture :rotation-deg [0.0 90.0 0.0]) [1.0 0.0 0.0] nil)
        pitched (reference-geometry (assoc fixture :rotation-deg [90.0 0.0 0.0]) [0.0 -1.0 0.0] nil)
        rolled (reference-geometry (assoc fixture :rotation-deg [0.0 0.0 90.0]) [0.0 1.0 0.0] nil)]
    (check! 0.5 (:pan forward) {:case :forward})
    (check! 0.5 (:tilt forward) {:case :forward})
    (check! 0.5 (:pan rotated) {:case :rotated-forward})
    (check! 0.5 (:tilt pitched) {:case :pitched-forward})
    (check! (/ 2.0 3.0) (:pan rolled) {:case :rolled-right})
    (check! [127 128] (expected-bytes 0.5 true) {:case :fractional-255})
    (check! [255 0] (expected-bytes 1.0 true) {:case :full-scale})
    (doseq [[alpha layer] [[[1.0 0.0 0.0] 0] [[1.0 1.0 0.0] 1] [[1.0 1.0 1.0] 2]]]
      (check! (nth layer-values layer) (dissoc (expected-assigned fixture 1.0 alpha) :aim) {:case :full-layer-replaces}))
    (check! {} (expected-assigned fixture 1.0 [0.0 0.0 0.0]) {:case :all-transparent})
    true))
