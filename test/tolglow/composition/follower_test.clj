(ns tolglow.composition.follower-test
  "The bounded-acceleration follower (#914) on its own: every plan ends at rest
  exactly on its target, never exceeds its acceleration, keeps position and
  velocity continuous and samples the same at any instant."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.composition.follower :as follower]))

(def ^:private cases
  "[x0 v0 target speed acceleration]: from rest, cruising, too fast to stop in
  time, moving away, faster than the limit, already on the target but moving."
  (for [x0 [0.0 0.3] v0 [0.0 0.4 -1.5 3.0] target [0.3 0.9 0.0] speed [1.0 0.25] accel [2.0 8.0]]
    [x0 v0 target speed accel]))

(defn- close? [a b tolerance] (<= (Math/abs (- (double a) (double b))) tolerance))

(deftest every-plan-arrives-at-rest-on-its-target
  (doseq [[x0 v0 target speed accel :as c] cases
          :let [segments (follower/plan x0 v0 target speed accel)
                end (follower/duration segments)
                ;; Integrate the plan to its end instead of trusting the exact return.
                [x v] (reduce (fn [[x v] [dt a]] [(+ x (* v dt) (* 0.5 a dt dt)) (+ v (* a dt))]) [x0 v0] segments)]]
    (testing (pr-str c)
      (is (every? (fn [[dt a]] (and (pos? dt) (<= (Math/abs a) (+ accel 1.0e-12)))) segments)
          "constant-acceleration pieces within the limit")
      (is (close? target x 1.0e-9) "the pieces end on the target")
      (is (close? 0.0 v 1.0e-9) "at rest")
      (is (= [target 0.0] (follower/state x0 v0 target segments end)) "exactly, from the planned end on")
      (is (= [target 0.0] (follower/state x0 v0 target segments (+ end 5.0))))
      (is (= [(double x0) (double v0)] (follower/state x0 v0 target segments 0.0)) "it starts where it was, as fast")
      (is (every? #(<= (Math/abs (second (follower/state x0 v0 target segments %)))
                       (+ (max speed (Math/abs v0)) 1.0e-9))
                  (range 0.0 end 0.01))
          "never faster than the limit or the speed it took over with"))))

(deftest position-and-velocity-are-continuous-and-frame-independent
  (doseq [[x0 v0 target speed accel :as c] cases
          :let [segments (follower/plan x0 v0 target speed accel)
                at #(follower/state x0 v0 target segments %)
                dt 1.0e-4]]
    (testing (pr-str c)
      (doseq [t (range 0.0 (+ (follower/duration segments) 0.05) 0.003)]
        (let [[x1 v1] (at t) [x2 v2] (at (+ t dt))]
          (is (<= (Math/abs (- x2 x1)) (+ (* (max speed (Math/abs v0)) dt) 1.0e-9)))
          (is (<= (Math/abs (- v2 v1)) (+ (* accel dt) 1.0e-9)) "the velocity changes no faster than the acceleration allows")))
      (is (= (at 0.123) (at 0.123)) "a pure function of elapsed time"))))

(deftest a-move-at-rest-on-its-target-has-no-plan
  (is (= [] (follower/plan 0.4 0.0 0.4 1.0 2.0)))
  (is (seq (follower/plan 0.4 0.5 0.4 1.0 2.0)) "moving through the target, it brakes and comes back"))
