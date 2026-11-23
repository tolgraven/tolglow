(ns tolglow.studio.block-trace-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.block-trace :as trace]
            [tolglow.studio.push-display :as push]))

(defn sample [t]
  {:time-ms t :base [1 2] :stages [{:block :optics/through-water :offset [0 -0.1] :value [1 1.9]}]
   :result [1 1.9]})

(deftest observed-clock-controls-the-history
  (let [samples (reduce trace/append-sample [] (map sample (range 110)))]
    (is (= (range 14 110) (map :time-ms samples)))
    (is (= 96 (count (trace/append-sample samples (sample 109)))))
    (is (= [(sample 7)] (trace/append-sample samples (sample 7))))
    (is (empty? (trace/append-sample samples nil)))
    (is (= [[2] [-0.1] [1.9] [1.9]]
           (mapv last (trace/lines [(sample 0)] 0 1))))))

(deftest passive-display-keeps-history-only-for-the-same-live-binding
  (let [v {:runtime-id "a" :epoch 2 :instance-id "2:1" :display-mode :block-trace
           :block-trace {:binding-path [:params :size] :target-id :front :sample (sample 1)}}
        first-view (push/observe-view nil v)
        next-view (push/observe-view first-view (assoc-in v [:block-trace :sample] (sample 2)))]
    (is (= [1 2] (mapv :time-ms (:trace-samples next-view))))
    (is (= [1] (mapv :time-ms (:trace-samples (push/observe-view next-view (assoc v :epoch 3))))))
    (is (nil? (:trace-samples (push/observe-view next-view (assoc v :display-mode :audio)))))))
