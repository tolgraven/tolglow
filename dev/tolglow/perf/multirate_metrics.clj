(ns tolglow.perf.multirate-metrics
  "Validation outside the wall/CPU timing boundary. RGB triplets are linear.")

(defn distance [a b]
  (Math/sqrt (reduce + (map (fn [x y] (let [d (- x y)] (* d d))) a b))))

(defn angle [position a b]
  (let [u (mapv - a position) v (mapv - b position)
        magnitude (fn [q] (Math/sqrt (reduce + (map * q q))))
        lengths (* (magnitude u) (magnitude v))]
    (when-not (pos? lengths) (throw (ex-info "Undefined beam direction" {:position position})))
    (if (= a b) 0.0 (Math/toDegrees (Math/acos (max -1.0 (min 1.0 (/ (reduce + (map * u v)) lengths))))))))

(defn- oklab [r g b]
  ;; CSS Color 4: linear-light sRGB to OKLab (D65).
  (let [l (Math/cbrt (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)))
        m (Math/cbrt (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)))
        s (Math/cbrt (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))]
    [(+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s))
     (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s))
     (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s))]))

(defn rgb-error [^doubles expected ^doubles actual]
  (loop [i 0 sq 0.0 peak 0.0 delta 0.0]
    (if (= i (alength expected))
      {:rgb-mse (/ sq (alength expected)) :rgb-max peak :oklab-max delta}
      (let [a (mapv #(aget expected (+ i %)) [0 1 2])
            b (mapv #(aget actual (+ i %)) [0 1 2])
            errors (mapv #(Math/abs (double (- %1 %2))) a b)
            d (if (= a b) 0.0 (distance (apply oklab a) (apply oklab b)))]
        (recur (+ i 3) (+ sq (reduce + (map * errors errors)))
               (max peak (reduce max errors)) (max delta d))))))

(defn dmx-error [left right]
  (when-not (and (= (set (keys left)) (set (keys right))) (seq left))
    (throw (ex-info "DMX universe sets differ" {})))
  (reduce-kv (fn [peak u ^bytes xs]
               (let [^bytes ys (get right u)]
                 (when-not (= (alength xs) (alength ys))
                   (throw (ex-info "DMX universe sizes differ" {:universe u})))
                 (loop [i 0 peak (long peak)]
                   (if (= i (alength xs)) peak
                     (recur (inc i) (max peak (Math/abs (long (- (bit-and 255 (aget xs i))
                                                               (bit-and 255 (aget ys i)))))))))))
             0 left))
