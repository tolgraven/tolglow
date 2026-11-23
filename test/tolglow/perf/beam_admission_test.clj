(ns tolglow.perf.beam-admission-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.perf.beam-admission :as admission]))

(def fixture {:position [0.0 6.0 0.0] :beam-angle 5.0 :lens-radius-m 0.005})
(def encoding {:max-code 65025})
(def beat {:op :beat})
(defn- op [name & args] {:op name :args (vec args)})
(defn- plan [x y z rig] (admission/prepare (op :vec3 x y z) rig encoding))

(deftest affine-time-with-unknown-position-and-dynamic-depth-cannot-be-admitted
  (doseq [result [(plan beat 0.0 1.0 fixture)
                  (plan 1.0 (op :cos beat) 0.0 fixture)
                  (plan {:op :param :key :dancer} 0.0 0.0 fixture)
                  (plan 1.0 6.01 0.0 fixture)
                  (plan 1.0 5.999 0.0 fixture)
                  (plan 1.0 0.0 0.0 (assoc fixture :zoom-range-deg [4 50]))]]
    (is (nil? result))
    (is (= ##Inf (admission/max-error result 2 0.05)))
    (is (zero? (admission/max-error result 1 0.05)))))

(deftest temporal-slope-and-curvature-can-distinguish-a-fast-orbit
  (let [trajectory #(op :multiply 0.55 (op :sin (op :multiply % beat)))
        slow (plan (trajectory 5.0) 0.0 0.25 (assoc fixture :beam-angle 40.0))
        fast (plan (trajectory (* 10.0 Math/PI)) 0.0 0.25 (assoc fixture :beam-angle 40.0))]
    (is (< (admission/max-error slow 8 0.05) 0.25))
    (is (> (admission/max-error fast 4 0.05) 0.25))
    (is (> (admission/max-error slow 4 0.1) (admission/max-error slow 4 0.05)))))
