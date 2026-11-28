(ns tolglow.composition.follower
  "A second-order follower (#914): a slew with bounded acceleration. A move
  from position x0 with velocity v0 reaches its target at rest in the least
  time with |acceleration| <= a and, once it has turned towards the target,
  |velocity| <= v. Its plan is a short list of constant-acceleration segments
  fixed when the move starts, so the position is a closed form of elapsed time
  (any frame rate gives the same value at the same instant), it is continuous
  in position and velocity, and it ends exactly on the target.

  Units are the caller's: a linked knob uses its 0-1 range and beats, so the
  speed limit is range per beat and the acceleration range per beat². Every
  function here is pure and samples any instant without previous frames.")

(defn- toward
  "Segments [[duration acceleration]] that bring a move dist (>= 0) ahead,
  starting at velocity u along that direction, to rest there."
  [dist u v a]
  (let [stop (/ (* u u) (* 2.0 a))]
    (if (and (pos? u) (> stop dist))
      ;; Too fast to stop in time: brake to rest past the target, then come back.
      (into [[(/ u a) (- a)]] (map (fn [[dt acc]] [dt (- acc)])) (toward (- stop dist) 0.0 v a))
      (let [peak (Math/sqrt (+ (* a dist) (/ (* u u) 2.0)))]
        (if (<= peak v)
          [[(/ (- peak u) a) a] [(/ peak a) (- a)]]
          (let [first-a (if (< u v) a (- a))
                d1 (/ (- (* v v) (* u u)) (* 2.0 first-a))
                d3 (/ (* v v) (* 2.0 a))]
            [[(/ (Math/abs (- v u)) a) first-a] [(/ (- dist d1 d3) v) 0.0] [(/ v a) (- a)]]))))))

(defn plan
  "The segments [[duration acceleration] ...] of a move from x0 at velocity v0
  to target with speed limit v (> 0) and acceleration limit a (> 0). Empty
  when it is already at rest on the target."
  [x0 v0 target v a]
  (let [x0 (double x0) v0 (double v0) target (double target) v (double v) a (double a)
        d (- target x0)]
    (if (and (zero? d) (zero? v0))
      []
      (let [s (if (zero? d) (if (pos? v0) 1.0 -1.0) (Math/signum d))]
        (into [] (comp (remove (fn [[dt _]] (<= dt 0.0))) (map (fn [[dt acc]] [dt (* s acc)])))
              (toward (Math/abs d) (* s v0) v a))))))

(defn duration
  "How long a planned move takes."
  [segments]
  (reduce + 0.0 (map first segments)))

(defn state
  "[position velocity] at elapsed time t (>= 0) of a planned move from x0 at
  v0. From the end of the plan on, exactly [target 0.0]."
  [x0 v0 target segments t]
  (let [t (max 0.0 (double t))]
    (if (>= t (duration segments))
      [(double target) 0.0]
      (loop [x (double x0) v (double v0) t t [[dt acc] & more :as pending] segments]
        (cond
          (empty? pending) [(double target) 0.0]
          (< t dt) [(+ x (* v t) (* 0.5 acc t t)) (+ v (* acc t))]
          :else (recur (+ x (* v dt) (* 0.5 acc dt dt)) (+ v (* acc dt)) (- t dt) more))))))
