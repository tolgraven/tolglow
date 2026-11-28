(ns tolglow.composition.handover-test
  "The velocity-continuous handover term (#896) on its own: endpoints, start
  and end slopes, value trees and curves that cannot carry a velocity."
  (:require [afterglow.render.easing :as easing]
            [clojure.test :refer [deftest is testing]]
            [tolglow.composition.handover :as handover]))

(def ^:private curves
  [:linear :smoothstep :sine :ease-in :ease-out :ease-in-out
   {:type :power :exponent 2.0} {:type :power :exponent 1.0}
   {:type :cubic-bezier :points [0.3 0.1 0.6 1.0]} {:type :cubic-bezier :points [0.0 0.0 0.5 1.0]}
   {:type :spring} {:type :spring :frequency 3.0 :damping 1.2}])

(defn- close? [a b tolerance] (<= (Math/abs (- (double a) (double b))) tolerance))

(defn- finite-difference [f p]
  (let [d 1.0e-6 lo (max 0.0 (- p d)) hi (min 1.0 (+ p d))]
    (/ (- (f hi) (f lo)) (- hi lo))))

(deftest every-curve-keeps-its-endpoints-and-starts-with-the-carried-slope
  (doseq [curve curves m0 [-1.7 0.0 0.4 3.0]]
    (let [from 0.2 to 0.9
          v #(handover/value from to curve m0 %)]
      (testing (pr-str curve m0)
        (is (close? from (v 0.0) 1.0e-12))
        (is (= to (v 1.0)) "the target is reached exactly at the planned end")
        (is (close? m0 (handover/velocity from to curve m0 0.0) 1.0e-9) "the move leaves with the carried velocity")
        (is (close? m0 (finite-difference v 0.0) 1.0e-3) "the sampled value agrees with that slope")
        (is (close? (* (- to from) (handover/slope curve 1.0)) (handover/velocity from to curve m0 1.0) 1.0e-9)
            "it arrives with the curve's own end slope")))))

(deftest closed-form-slopes-match-the-shared-easing
  (doseq [curve curves p [0.03 0.25 0.5 0.61 0.97]]
    (is (close? (finite-difference #(easing/sample curve %) p) (handover/slope curve p) 2.0e-3)
        (pr-str curve p))))

(deftest curves-without-a-finite-start-slope-carry-nothing
  (is (nil? (handover/start-slope :step)) "a step holds and then jumps")
  (is (nil? (handover/start-slope {:type :power :exponent 0.5})))
  (is (nil? (handover/start-slope {:type :cubic-bezier :points [0.0 0.5 0.5 1.0]})) "a vertical start")
  (is (= 1.0 (handover/start-slope {:type :cubic-bezier :points [0.0 0.0 1.0 1.0]})))
  (is (= 2.0 (handover/start-slope :ease-out))))

(deftest vectors-and-gradient-stops-move-leaf-by-leaf
  (let [from [0.0 0.5 1.0] to [1.0 0.5 0.0] m0 [0.3 -0.2 0.0]]
    (is (every? true? (map #(close? %1 %2 1.0e-9) m0 (handover/velocity from to :linear m0 0.0))))
    (is (= to (handover/value from to :linear m0 1.0))))
  (let [stops (fn [a b] [{:position 0.0 :color [a 0.0 0.0]} {:position b :color [0.0 0.0 a]}])
        from (stops 0.0 0.5) to (stops 1.0 1.0)
        m0 [{:position 0.0 :color [0.5 0.0 0.0]} {:position 0.25 :color [0.0 0.0 0.5]}]]
    (is (handover/movable? from to))
    (is (not (handover/movable? from (conj to {:position 1.0 :color [1.0 1.0 1.0]}))) "unequal stop counts switch, they do not move")
    (is (close? 0.25 (get-in (handover/velocity from to :ease-in-out m0 0.0) [1 :position]) 1.0e-12))
    (is (= to (handover/value from to :ease-in-out m0 1.0)))))

(deftest a-scalar-fraction-is-the-eased-fraction-plus-the-term
  (let [from 0.5 to 0.2 m0 2.0]
    (doseq [p [0.0 0.2 0.5 0.9 1.0]]
      (is (close? (/ (- (handover/value from to :linear m0 p) from) (- to from))
                  (handover/fraction from to :linear m0 p) 1.0e-12)))
    (is (nil? (handover/fraction 0.4 0.4 :linear 1.0 0.5)) "no direction to project on")
    (is (close? 0.5 (handover/fraction [0.0 0.0] [1.0 1.0] :linear nil 0.5) 1.0e-12))))
