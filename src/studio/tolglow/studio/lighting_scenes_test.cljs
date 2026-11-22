(ns tolglow.studio.lighting-scenes-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.lighting-scenes :as scene]))

(def fixtures [{:id :front/wash :groups [:rig/washes]} {:id :rear/wash :groups [:rig/washes]}
               {:id "warn" :groups [:rig/washes]} {:id :stage/fog :groups [:rig/washes]}])
(def ids [:front/wash :rear/wash "warn"])
(def identity-key {:epoch 3 :show-id :club :runtime-id "test-runtime"})
(def db {:connection {:connected? true}
         :lighting-scene-ui {:identity identity-key :selected "group:rig/washes"}
         :server-projection {:view (assoc identity-key :rig {:fixtures fixtures}
                                          :lighting-scenes {:eligible-fixture-ids ids :groups []})}})

(deftest groups-use-authoritative-emitter-membership-and-full-typed-ids
  (let [eligible (scene/eligible-fixtures fixtures ids)
        chosen (scene/selection (:lighting-scene-ui db) (scene/choices eligible []) [])]
    (is (= ids (mapv :id eligible)))
    (is (= [:rig/washes ids] ((juxt :id :ids) chosen)))
    (is (= {:op :apply :group-id :rig/washes :fixture-ids ids :target 0.72 :expected-epoch 3 :runtime-id "test-runtime"}
           (get-in (scene/request db :apply) [:studio/http :body])))))

(deftest live-scenes-vary-release-and-disallow-overlapping-groups
  (let [active {:group-id :rig/washes :fixture-ids ids :target 0.25 :actual 0.19 :status :limited}
        live (assoc-in db [:server-projection :view :lighting-scenes :groups] [active])]
    (is (= 0.25 (get-in (scene/request live :vary) [:studio/http :body :target])))
    (is (= {:op :release :group-id :rig/washes :expected-epoch 3 :runtime-id "test-runtime"}
           (get-in (scene/request live :release) [:studio/http :body])))
    (is (nil? (scene/request (assoc-in live [:lighting-scene-ui :selected] "group:studio/all-lights") :apply)))
    (is (nil? (scene/request db :vary)))
    (is (nil? (scene/request (assoc-in live [:connection :connected?] false) :release)))))

(deftest new-scene-selection-requires-real-fixtures-and-a-name
  (let [custom (update db :lighting-scene-ui merge {:selected "custom" :name "Back wall" :picked [:rear/wash]})]
    (is (= {:op :apply :group-id :studio.scene/back-wall :fixture-ids [:rear/wash]
            :expected-epoch 3 :runtime-id "test-runtime" :target 0.72}
           (get-in (scene/request custom :apply) [:studio/http :body])))
    (is (nil? (scene/request (assoc-in custom [:lighting-scene-ui :name] " ") :apply)))
    (is (nil? (scene/request (assoc-in custom [:lighting-scene-ui :picked] [:no/light]) :apply)))
    (is (nil? (scene/request (:db (scene/request custom :apply)) :apply)))
    (testing "replacement shows discard prior fixture selections"
      (let [next-show (assoc-in custom [:server-projection :view :epoch] 4)]
        (is (= :studio/all-lights (get-in (scene/request next-show :apply) [:studio/http :body :group-id])))))))
