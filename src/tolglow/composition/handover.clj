(ns tolglow.composition.handover
  "Velocity-continuous takeover for eased moves (#896). A move that takes over
  from another keeps its authored easing e(p) and adds a decaying Hermite term:

    v(p) = from + (to - from)·e(p) + (m0 - (to - from)·e'(0))·h(p)
    h(p) = p(1 - p)²

  h is zero at both ends, has unit slope at 0 and zero slope at 1, so the move
  starts with slope m0 and still reaches to at p = 1 with its authored end
  slope. m0 is the incoming velocity per unit of the new move's progress (the
  outgoing slope times the new span), so it does not depend on tempo, frame
  rate or a clock rebase.

  Values are numbers, numeric vectors (colours, positions, ranges) or vectors
  of {:position n :color [...]} gradient stops: every leaf moves on its own.
  Everything here is pure and samples any progress without previous frames."
  (:require [afterglow.render.easing :as easing]))

(defn hermite
  "The handover basis p(1 - p)²."
  ^double [^double p]
  (let [u (- 1.0 p)] (* p u u)))

(defn hermite-slope
  "dh/dp = (1 - p)(1 - 3p)."
  ^double [^double p]
  (* (- 1.0 p) (- 1.0 (* 3.0 p))))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- shape-of [curve] (if (map? curve) (:type curve) (or curve :linear)))

(def ^:private numeric-step
  "Central-difference step for curves without a closed-form slope (cubic
  bezier, spring). The bezier evaluator resolves x to 2^-36, so this keeps both
  the truncation and the resolution error near 1e-7."
  1.0e-4)

(defn- numeric-slope [curve p]
  (let [e (easing/evaluator curve)
        lo (max 0.0 (- p numeric-step))
        hi (min 1.0 (+ p numeric-step))]
    (/ (- (double (e hi)) (double (e lo))) (- hi lo))))

(defn- bezier-start-slope
  "dy/dx at the start of a cubic bezier from (0,0) through (x1,y1), (x2,y2) to
  (1,1): the first control point that leaves the origin sets the tangent. nil
  for a vertical start."
  [[x1 y1 x2 y2]]
  (let [[x1 y1 x2 y2] (map double [x1 y1 x2 y2])]
    (cond
      (pos? x1) (/ y1 x1)
      (not (zero? y1)) nil
      (pos? x2) (/ y2 x2)
      (not (zero? y2)) nil
      :else 1.0)))

(defn start-slope
  "e'(0) of an easing curve, or nil when the curve cannot start with a finite
  velocity: :step holds and then jumps, :power below 1 and a bezier with a
  vertical start leave with infinite slope. Such a curve keeps its authored
  start, as no handover term can make it velocity-continuous."
  [curve]
  (case (shape-of curve)
    :linear 1.0
    (:smoothstep :sine :ease-in :ease-in-out :spring) 0.0
    :ease-out 2.0
    :step nil
    :power (let [k (double (:exponent curve))] (cond (> k 1.0) 0.0 (== k 1.0) 1.0 :else nil))
    :cubic-bezier (bezier-start-slope (:points curve))
    nil))

(defn slope
  "e'(p) of an easing curve for p in [0, 1], or nil where it is not finite.
  Named curves use their closed form; a bezier or spring is differentiated
  numerically."
  [curve p]
  (let [p (max 0.0 (min 1.0 (double p)))]
    (if (zero? p)
      (start-slope curve)
      (case (shape-of curve)
        :linear 1.0
        :smoothstep (* 6.0 p (- 1.0 p))
        :sine (* 0.5 Math/PI (Math/sin (* Math/PI p)))
        :ease-in (* 2.0 p)
        :ease-out (* 2.0 (- 1.0 p))
        :ease-in-out (if (< p 0.5) (* 4.0 p) (* 4.0 (- 1.0 p)))
        :step 0.0
        :power (let [k (double (:exponent curve))] (* k (Math/pow p (dec k))))
        (:cubic-bezier :spring) (numeric-slope curve p)
        nil))))

;; Value trees ---------------------------------------------------------------------

(defn same-shape?
  "Two values move together leaf by leaf: numbers, vectors of equal length or
  maps with the same keys, recursively."
  [a b]
  (cond
    (and (number? a) (number? b)) true
    (and (vector? a) (vector? b)) (and (= (count a) (count b)) (every? true? (map same-shape? a b)))
    (and (map? a) (map? b)) (and (= (set (keys a)) (set (keys b))) (every? #(same-shape? (get a %) (get b %)) (keys a)))
    :else false))

(defn zip-with
  "Apply f leaf by leaf to equal-shaped value trees."
  [f & trees]
  (let [t (first trees)]
    (cond
      (number? t) (apply f trees)
      (vector? t) (apply mapv (partial zip-with f) trees)
      (map? t) (into {} (map (fn [k] [k (apply zip-with f (map #(get % k) trees))])) (keys t))
      :else (throw (ex-info "Handover values need equal numeric shapes" {:values trees})))))

(defn finite-tree?
  "A number, or a vector or map of finite numbers, recursively."
  [x]
  (cond
    (number? x) (finite? x)
    (vector? x) (and (seq x) (every? finite-tree? x))
    (map? x) (and (seq x) (every? finite-tree? (vals x)))
    :else false))

(defn scale [tree k] (zip-with #(* (double %) (double k)) tree))

(defn movable?
  "from and to can carry a velocity: equal-shaped finite value trees."
  [from to]
  (and (finite-tree? from) (finite-tree? to) (same-shape? from to)))

(defn- correction
  "m0 - (to - from)·e'(0), leaf by leaf."
  [from to e0 m0]
  (zip-with (fn [a b m] (- (double m) (* (- (double b) (double a)) e0))) from to m0))

(defn value
  "The unconstrained value at progress p of a move from -> to on curve, taking
  over with incoming velocity m0 (per unit progress; nil for none). At p = 1 it
  is exactly to. Bounds and integer rounding belong to the destination schema."
  [from to curve m0 p]
  (let [p (max 0.0 (min 1.0 (double p)))
        e (double (easing/sample curve p))]
    (cond
      (>= p 1.0) to
      (nil? m0) (zip-with (fn [a b] (+ (double a) (* e (- (double b) (double a))))) from to)
      :else (let [h (hermite p) e0 (double (start-slope curve))]
              (zip-with (fn [a b c] (+ (double a) (* e (- (double b) (double a))) (* h (double c))))
                        from to (correction from to e0 m0))))))

(defn velocity
  "dv/dp at progress p, leaf by leaf, or nil where the curve has no finite
  slope there."
  [from to curve m0 p]
  (when-let [e' (slope curve p)]
    (let [p (max 0.0 (min 1.0 (double p)))]
      (if (nil? m0)
        (zip-with (fn [a b] (* (double e') (- (double b) (double a)))) from to)
        (let [h' (hermite-slope p) e0 (double (start-slope curve))]
          (zip-with (fn [a b c] (+ (* (double e') (- (double b) (double a))) (* h' (double c))))
                    from to (correction from to e0 m0)))))))

(defn fraction
  "The move's progress along from -> to as one number per p, for a display that
  draws a single curve: the value's projection on the from -> to direction,
  (v - from)·(to - from) / |to - from|². For a scalar this is the eased
  fraction plus the handover term; nil when from equals to."
  [from to curve m0 p]
  (let [leaves (fn leaves [x] (cond (number? x) [(double x)] (vector? x) (mapcat leaves x)
                                    :else (mapcat leaves (map x (sort (keys x))))))
        a (leaves from) b (leaves to) v (leaves (value from to curve m0 p))
        d (map - b a)
        norm (reduce + (map * d d))]
    (when (pos? norm)
      (/ (reduce + (map * (map - v a) d)) norm))))
