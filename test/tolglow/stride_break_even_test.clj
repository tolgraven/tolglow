(ns tolglow.stride-break-even-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.perf.stride-break-even :as admission]))

(deftest expensive-endpoints-can-make-the-first-profitable-interval-larger-than-two
  (let [costs (into {} (map (juxt identity #(admission/estimate 1.0 3.0 0.2 %)) admission/strides))]
    (is (> (costs 2) (costs 1)))
    (is (< (costs 4) (costs 1)))
    (is (= [1 1 4 4] (mapv #(admission/first-saving-stride costs %) admission/strides)))))

(deftest the-first-saving-interval-respects-ties-the-cap-and-expensive-blends
  (is (= 1 (admission/first-saving-stride {1 1, 2 1, 4 1, 8 1} 8)))
  (is (= 1 (admission/first-saving-stride {1 1, 2 2, 4 1.2, 8 0.9} 4)))
  (is (= 2 (admission/first-saving-stride {1 1, 2 0.8, 4 1.1, 8 1.3} 8))))

(deftest observed-loss-and-an-artist-cap-remain-visible-in-validation
  (let [trained {:by-cap {1 1, 2 2, 4 4, 8 4}}
        observed {:cost-ms {1 1.0, 2 1.2, 4 0.85, 8 0.8}
                  :by-cap {1 1, 2 1, 4 4, 8 4}}
        result (admission/compare-decisions trained observed)]
    (is (neg? (get-in result [2 :measured-saving-ms])))
    (is (false? (get-in result [2 :missed-by-stopping?])))
    (is (= [1 4 true] ((juxt :stop-after-two :first-measured-saving :missed-by-stopping?) (result 8))))))
