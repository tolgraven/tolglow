(ns tolglow.studio.push-display-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.push-display :as display]
            [tolglow.studio.parameter-widgets :as widgets]))

(deftest passive-display-snapshot-is-bounded-and-emits-no-effects
  (reset! db/app-db {:unrelated :preserved})
  (let [view {:parameters (repeat 20 {:name "Level" :value 0.5}) :cells (range 16) :labels (range 10)
              :cue {:name "Ember"} :effect {:envelope {:attack {:beats 0.5}}} :ignored "not display data"}
        result (:effects (interceptor/execute [:push-display/update view]
                           (remove #(= :do-fx (:id %)) (registrar/get-handler :event :push-display/update))))]
    (is (= #{:db} (set (keys result))))
    (is (= :preserved (get-in result [:db :unrelated])))
    (is (= [8 8 8] (mapv #(count (get-in result [:db :push-display %])) [:parameters :cells :labels])))
    (is (nil? (get-in result [:db :push-display :ignored])))
    (is (= {:attack {:beats 0.5}} (get-in result [:db :push-display :effect :envelope])))))

(deftest typed-values-share-labels-with-web-controls
  (is (= "Triangle" (widgets/value-label {:choices [{:value 1 :label "Triangle"}]} 1)))
  (is (= "On" (widgets/value-label {:type :boolean} true)))
  (is (= "0.5 · -1" (widgets/value-label {:type :vector} [0.5 -1])))
  (is (= "0.5 beats" (widgets/duration-label {:beats 0.5})))
  (is (= "rgb(255,0,128)" (widgets/color-css [2 -1 0.5])))
  (is (= 0.75 (widgets/proportion {:bounds [-1 1]} 0.5)))
  (is (= 1 (widgets/proportion {:bounds [0 1]} 2)))
  (is (nil? (widgets/proportion {:bounds [1 1]} 1))))

(deftest traces-require-real-finite-samples
  (is (nil? (widgets/sample-points nil)))
  (is (nil? (widgets/sample-points [0 js/NaN 1])))
  (is (= "0,16 100,16" (widgets/sample-points [1 1])))
  (is (= "0,28 50,4 100,28" (widgets/sample-points [{:value 0} {:value 1} {:value 0}]))))
