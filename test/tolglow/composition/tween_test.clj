(ns tolglow.composition.tween-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition.tween :as tween]))

(def ramp {:keyframes [{:at-ms 0 :value 0} {:at-ms 1000 :value 1}]})

(deftest musical-keyframes-use-explicit-elapsed-beats
  (let [spec {:keyframes [{:at {:beats 0} :value 0} {:at {:bars 1} :value 1}]
              :mode :ping-pong :repeat-count 2}
        sample (tween/compile-tween spec)
        clock {:elapsed-beats 1.5 :elapsed-ms 10000 :meter {:beats-per-bar 3}}]
    (is (= 0.5 (:value (sample clock))))
    (is (= (sample clock) (sample (assoc clock :elapsed-ms 900000))))
    (is (= 1.0 (:value (sample (assoc clock :elapsed-beats 3)))))
    (is (= 0.5 (:value (sample (assoc clock :elapsed-beats 4.5)))))
    (is (:done? (sample (assoc clock :elapsed-beats 6)))))
  (is (= 0.5 (:value (tween/sample {:keyframes [{:at {:ms 0} :value 0}
                                              {:at {:ms 100} :value 1}]} {:elapsed-ms 50})))))

(deftest tweens-sample-logical-time-without-frame-history
  (let [sample (tween/compile-tween ramp)]
    (is (= 0.25 (:value (sample 250))))
    (doseq [t (range 0 1001 100)]
      (is (= (:value (sample t)) (:value ((tween/compile-tween ramp) t)))))
    (is (= {:value 1.0 :phase 1.0 :iteration 0 :done? true} (sample 1000)))
    (is (= (sample 1000) (sample 5000)))
    (is (= 0.0 (:value (sample -1))))))

(deftest loop-ping-pong-and-finite-repeat-boundaries
  (let [looped (tween/compile-tween (assoc ramp :mode :loop))
        bounced (tween/compile-tween (assoc ramp :mode :ping-pong :repeat-count 2))]
    (is (= 0.0 (:value (looped 1000))))
    (is (= 0.75 (:value (bounced 1250))))
    (is (= 0.0 (:value (bounced 2000))))
    (is (:done? (bounced 2000)))))

(deftest curves-vectors-and-invalid-keyframes
  (is (= [0.5 1.0 1.5] (tween/interpolate [0 0 0] [1 2 3] 0.5)))
  (is (= 0.25 (tween/curve :ease-in 0.5)))
  (is (= 0.75 (tween/curve :ease-out 0.5)))
  (is (= 0.0 (tween/curve :step 0.9)))
  (doseq [spec [{:keyframes []}
                {:keyframes [{:at-ms 0 :value 0} {:at-ms 0 :value 1}]}
                {:keyframes [{:at-ms 0 :value [0 0]} {:at-ms 2 :value [1]}]}
                (assoc ramp :repeat-count 0)]]
    (is (thrown? clojure.lang.ExceptionInfo (tween/compile-tween spec)))))
