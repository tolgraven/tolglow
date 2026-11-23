(ns tolglow.perf.venue-rig
  "Deterministic metre-space benchmark venue: ascending ribbons and a tilted truss.")

(defn pixel-position [i]
  (let [strand (mod i 24)
        t (* 0.015 (quot i 24))
        azimuth (+ t (* strand (/ Math/PI 12.0)))
        radius (+ 7.0 (* 2.0 (Math/sin (+ (* 2.0 t) strand))))]
    [(* radius (Math/cos azimuth))
     (+ 4.0 (* 3.0 (Math/sin (+ (* 0.7 t) strand))))
     (* radius (Math/sin azimuth))]))

(defn mover-pose [i n]
  (let [azimuth (* 2.0 Math/PI (/ i (double n)))]
    {:position [(* 8.0 (Math/cos azimuth))
                (+ 5.0 (Math/sin (* 3.0 azimuth)))
                (* 8.0 (Math/sin azimuth))]
     :rotation-deg [(double (- (mod (* 7 i) 33) 16))
                    (Math/toDegrees azimuth)
                    (double (- (mod (* 11 i) 37) 18))]}))

(defn- projected-position []
  (reduce (fn [sum [axis weight]]
            {:op :add :args [sum {:op :multiply :args [{:op :position :axis axis} weight]}]})
          {:op :bar} [[:x 0.31] [:y -0.27] [:z 0.19]]))

(defn spatial-color [layout]
  (let [phase (if (= layout :sculpture)
                (projected-position)
                {:op :add :args [{:op :multiply :args [{:op :position :axis :x} 0.05]}
                                 {:op :bar}]})
        sine {:op :sin :args [phase]}
        red {:op :multiply :args [0.5 {:op :add :args [1.0 sine]}]}]
    {:op :vec3 :args [red {:op :param :key :level} 0.25]}))
