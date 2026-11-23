(ns tolglow.perf.beam-admission
  "Offline interpolation-error bounds for a single aim program above a horizontal floor.")

(defn- finite? [n] (and (number? n) (Double/isFinite (double n))))

(defn- scalar [value]
  {:low value :high value :speed 0.0 :curvature 0.0})

(defn- fixed? [{:keys [low high]}] (= low high))

(defn- scale [factor {:keys [low high speed curvature]}]
  (if (zero? factor) (scalar 0.0)
    (let [a (* factor low) b (* factor high) magnitude (abs factor)]
      {:low (min a b) :high (max a b)
       :speed (* magnitude speed) :curvature (* magnitude curvature)})))

(defn- sum [a b]
  (merge-with + a b))

(defn- oscillation [op {:keys [speed curvature low] :as phase}]
  (if (fixed? phase) (scalar (if (= op :sin) (Math/sin low) (Math/cos low)))
    {:low -1.0 :high 1.0 :speed speed :curvature (+ (* speed speed) curvature)}))

(defn- bounds [form position]
  (cond
    (number? form) (when (finite? form) (scalar (double form)))
    (= :const (:op form)) (when (finite? (:value form)) (scalar (double (:value form))))
    (= :position (:op form)) (when-some [value (position (:axis form))] (scalar value))
    (= :beat (:op form)) {:low ##-Inf :high ##Inf :speed 1.0 :curvature 0.0}
    :else
    (let [{:keys [op args]} form
          [a b :as children] (mapv #(bounds % position) args)]
      (when (every? some? children)
        (case (count children)
          1 (when (#{:sin :cos} op) (oscillation op a))
          2 (case op
              :add (sum a b)
              :subtract (sum a (scale -1.0 b))
              :multiply (cond (fixed? a) (scale (:low a) b)
                              (fixed? b) (scale (:low b) a))
              nil)
          nil)))))

(defn- excursion [{:keys [low high]} origin]
  (max (abs (- low origin)) (abs (- high origin))))

(defn- axis-error [encoding]
  ;; Default calibrated axis spans are 360° and 180°. Either axis can round by
  ;; up to half of one measured wire step. Rotating both adds at most their sum.
  (/ (Math/toRadians 540.0) (* 2.0 (:max-code encoding))))

(defn- footprint-transfer [height lens half-angle max-slope axis-error]
  (let [inclination (+ (Math/atan max-slope) axis-error)]
    (when (< (+ inclination half-angle) (/ Math/PI 2))
      (let [slope (Math/tan inclination)
            q (Math/hypot 1.0 slope)
            s (Math/sin half-angle) ss (* s s)
            apex (/ lens (Math/tan half-angle))
            n (+ height (* apex ss q)) d (- 1.0 (* ss q q))
            ;; Ellipse centre offset is slope-vector times F = N/D.
            ;; The radial derivative F + r dF/dr bounds its planar sensitivity.
            derivative (+ (/ n d) (* slope slope ss (+ (/ apex (* q d)) (/ (* 2.0 n) (* d d)))))
            min-radius (+ lens (* height (Math/tan half-angle)))]
        {:radii-per-slope (/ derivative min-radius)
         :quantization-slope (* 2.0 axis-error q q)}))))

(defn prepare
  "Inspect the recorded program and fixture. A nil result cannot admit a
  predicted interval. The experiment uses a single moving head, linear
  interpolation, constant tempo, fixed lens and Afterglow's default axes."
  [program fixture encoding]
  (let [[ox height oz] (:position fixture) lens (:lens-radius-m fixture) angle (:beam-angle fixture)]
    (when (and (= :vec3 (:op program)) (= 3 (count (:args program)))
               (= 3 (count (:position fixture))) (every? finite? (:position fixture))
               (every? zero? (get fixture :rotation-deg [0 0 0])) (nil? (:calibration fixture))
               (nil? (:zoom-range-deg fixture)) (finite? lens) (<= 0 lens) (> height lens)
               (finite? angle) (< 0 angle 180) (pos-int? (:max-code encoding)))
      (let [position {:x ox :y height :z oz}
            [x y z :as axes] (mapv #(bounds % position) (:args program))]
        (when (and (every? some? axes) (fixed? y) (< (:low y) height))
          (let [depth (- height (:low y))
                radius (Math/hypot (excursion x ox) (excursion z oz))
                acceleration (Math/hypot (:curvature x) (:curvature z))]
            (when (and (Double/isFinite radius) (Double/isFinite acceleration))
              (when-let [transfer (footprint-transfer height lens (Math/toRadians (/ angle 2))
                                                      (/ radius depth) (axis-error encoding))]
                (assoc transfer :curvature-per-depth (/ acceleration depth))))))))))

(defn max-error
  "Upper bound on displacement in full-rate reference footprint radii."
  [plan stride beats-per-frame]
  (cond
    (= stride 1) 0.0
    (nil? plan) ##Inf
    :else (let [beats (* stride beats-per-frame)
                native (/ (* (:curvature-per-depth plan) beats beats) 8.0)]
            (* (:radii-per-slope plan) (+ native (:quantization-slope plan))))))
