(ns tolglow.composition.step-schedule
  "Piecewise-constant values along one linear move (#667).

  A linear a→b move that drives a stepped destination changes the
  destination only where it crosses a step boundary. `schedule` takes the
  move's boundary positions (fractions 0–1 along the move, found
  analytically by the caller) and a value function f, and evaluates f once
  per segment, at its midpoint. `value-at` then answers any position by
  lookup. Within `edge` of a boundary it calls f itself, so the result is
  always exactly f at that position: a schedule never disagrees with
  per-frame evaluation, it only evaluates less often. Pure data.")

(def edge
  "Positions this close to a boundary are evaluated directly, so float
  rounding in a boundary position can never change a committed value."
  1e-6)

(defn- normalized-bounds [boundaries]
  (vec (dedupe (sort (filter #(and (< edge %) (< % (- 1.0 edge))) (map double boundaries))))))

(defn- midpoint [bounds i]
  (let [a (if (zero? i) 0.0 (nth bounds (dec i))) b (if (= i (count bounds)) 1.0 (nth bounds i))]
    (/ (+ a b) 2.0)))

(defn schedule
  "{:bounds [p...] :values [v...]}: sorted distinct boundary positions inside
  (0, 1) and the value of f on each of the (inc (count bounds)) segments."
  [boundaries f]
  (let [bounds (normalized-bounds boundaries)]
    {:bounds bounds :values (mapv #(f (midpoint bounds %)) (range (inc (count bounds))))}))

(defn plan
  "A schedule whose segment values are filled in only when a frame first
  reaches them (`lookup`), so a move that is retargeted early never evaluates
  the segments it did not reach."
  [boundaries]
  {:bounds (normalized-bounds boundaries) :values {}})

(defn- segment
  "Index of the segment holding p: the number of bounds at or below it."
  [bounds p]
  (loop [lo 0 hi (count bounds)]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (<= (nth bounds mid) p) (recur (inc mid) hi) (recur lo mid)))
      lo)))

(defn near-boundary? [{:keys [bounds]} p]
  (boolean (some #(< (Math/abs (- (double p) (double %))) edge) bounds)))

(defn value-at
  "The value at position p, by lookup away from boundaries. Near a boundary
  or the move's ends it calls exact, a thunk that evaluates the destination
  at the frame's own coordinate (f at p when none is given), so float error
  in p cannot pick the wrong side of a boundary. p is clamped to 0–1."
  ([s f p] (value-at s f p #(f (max 0.0 (min 1.0 (double p))))))
  ([{:keys [bounds values] :as s} _f p exact]
   (let [p (max 0.0 (min 1.0 (double p)))]
     (if (or (near-boundary? s p) (< p edge) (> p (- 1.0 edge)))
       (exact)
       (nth values (segment bounds p))))))

(defn lookup
  "[value schedule] at position p for a `plan`: a filled segment by lookup,
  an unfilled one by f at its midpoint (kept in the returned schedule), and
  exact near a boundary or the move's ends, as for `value-at`."
  [{:keys [bounds values] :as s} f p exact]
  (let [p (max 0.0 (min 1.0 (double p)))]
    (if (or (near-boundary? s p) (< p edge) (> p (- 1.0 edge)))
      [(exact) s]
      (let [i (segment bounds p)]
        (if (contains? values i)
          [(get values i) s]
          (let [v (f (midpoint bounds i))] [v (assoc-in s [:values i] v)]))))))

(defn index-at
  "Segment index for p, or nil within `edge` of a boundary (call f there)."
  [{:keys [bounds] :as s} p]
  (when-not (near-boundary? s p) (segment bounds (max 0.0 (min 1.0 (double p))))))
