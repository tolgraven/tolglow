(ns tolglow.studio.camera-controls-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.camera-controls :as camera]))

(defn effects [event]
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest camera-controls-only-address-owned-preview-and-validate-input
  (reset! db/app-db {:show {:master 0.4}})
  (is (= {:studio/preview [:navigation-mode "tour"]} (effects [:camera-navigation/mode "tour"])))
  (is (nil? (:studio/preview (effects [:camera-navigation/mode "fly-light-fixture"]))))
  (is (= {:studio/preview [:navigation-speed 3]} (effects [:camera-navigation/speed 3])))
  (doseq [speed [0 9 js/NaN "2"]] (is (nil? (:studio/preview (effects [:camera-navigation/speed speed])))))
  (is (= {:studio/preview [:navigation-move ["forward" true]]} (effects [:camera-navigation/move "forward" true])))
  (is (= {:studio/preview [:navigation-move ["forward" false]]} (effects [:camera-navigation/move "forward" false])))
  (is (nil? (:studio/preview (effects [:camera-navigation/move "pan" true])))))

(deftest engine-observations-reconcile-manual-takeover-without-changing-show
  (reset! db/app-db {:show {:master 0.4}})
  (let [next (:db (effects [:camera-navigation/observed {:mode "walk" :speed 2 :interacting false
                                                       :tour-fixture nil :unknown "discard"}]))]
    (is (= "walk" (:mode (camera/state next))))
    (is (= {:master 0.4} (:show next)))
    (is (nil? (get-in next [:camera-navigation :unknown])))))
