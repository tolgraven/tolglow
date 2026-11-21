(ns tolglow.automation.random-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.automation.random :as random]))

(deftest explicit-seed-and-selection-make-randomization-replayable
  (let [schemas {:level {:type :number :default 0.5 :bounds [0 1]
                         :recommended [0.2 0.8] :random {:enabled? true :max-change 0.1}}
                 :locked {:type :number :default 7 :bounds [0 10]}
                 :boolean {:type :boolean :default false :random {:enabled? true}}}
        result (random/randomize-params schemas {} 42)
        selected (random/randomize-params schemas {} 42 {:keys [:boolean]})]
    (is (= result (random/randomize-params schemas {} 42)))
    (is (<= 0.4 (get-in result [:params :level]) 0.6))
    (is (= 7 (get-in result [:params :locked])))
    (is (= 0.5 (get-in selected [:params :level])))
    (is (boolean? (get-in result [:params :boolean])))))

(deftest numeric-ranges-colors-enums-and-zero-change
  (let [schemas {:range {:type :range :default [2 8] :bounds [0 10] :random {:enabled? true}}
                 :color {:type :color :default [0 0 0] :random {:enabled? true}}
                 :choice {:type :enum :default :a :values [:a :b] :random {:enabled? true}}
                 :fixed {:type :number :default 0.75 :bounds [0 1] :random {:enabled? true :max-change 0}}}
        values (:params (random/randomize-params schemas {} 5))]
    (is (apply <= (:range values)))
    (is (every? #(<= 0 % 1) (:color values)))
    (is (#{:a :b} (:choice values)))
    (is (= 0.75 (:fixed values))))
  (is (thrown? clojure.lang.ExceptionInfo
               (random/randomize-value 1 {:type :integer :default 1 :bounds [0 2]
                                          :random {:enabled? true :range [0.1 0.2]}} 1))))

(deftest shuffle-is-a-deterministic-permutation
  (let [[seed shuffled] (random/shuffle-values 42 (range 10))]
    (is (= (set (range 10)) (set shuffled)))
    (is (= [seed shuffled] (random/shuffle-values 42 (range 10))))
    (is (= [42 []] (random/shuffle-values 42 [])))
    (is (= [42 [:only]] (random/shuffle-values 42 [:only])))))
