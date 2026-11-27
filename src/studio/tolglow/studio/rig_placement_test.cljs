(ns tolglow.studio.rig-placement-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.rig.placement :as placement]
            [tolglow.studio.events]
            [tolglow.studio.rig-editor :as editor]
            [tolglow.studio.rig-placement :as rp]))

(def spot {:id "room-spot" :version 1 :name "Room spot" :footprint 6 :rgbw-policy "none"
           :channels {:dimmer 1 :red 2 :green 3 :blue 4 :pan [5 6] :tilt 7}
           :calibration {:pan {:min-deg -270 :max-deg 270} :tilt {:min-deg -135 :max-deg 135}}})

(def par {:id "par" :version 1 :footprint 3 :channels {:red 1 :green 2 :blue 3}})

(def rig {:room {:width 12 :height 5 :depth 10}
          :fixtures [{:id "head" :position [0 3 0] :rotation-deg [0 0 0] :universe 1 :address 1 :footprint 12
                      :channels {:pan [1 2] :tilt [3 4]} :calibration {:pan {:min-deg -180 :max-deg 180} :tilt {:min-deg -90 :max-deg 90}}}]})

(def base {:show {:show-id "show" :runtime-id "runtime" :epoch 2 :rig rig :fixture-types [spot par]
                  :output {:values {:head {:pan 0.25 :tilt 0.75}}}}
           :ui {:rig-draft rig}})

(defn- tool [db name & [more]] (editor/put-editor db (merge (assoc (editor/editor db) :tool name) more)))

(defn- fx [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(defn- run [db event]
  (reset! rf-db/app-db db)
  (rf/dispatch-sync event)
  @rf-db/app-db)

(defn- interaction [db detail]
  (run db [:rig-editor/interaction (merge {:identity (editor/identity-key db) :token 1} detail)]))

(deftest placing-a-library-type-is-one-undoable-draft-edit
  (let [db (tool base "place" {:place-type "room-spot"})
        placed (interaction db {:type "place" :point [1 5 -2] :normal [0 -1 0] :kind "surface" :surface-id "ceiling"})
        fixture (last (get-in placed [:ui :rig-draft :fixtures]))
        state (editor/editor placed)]
    (is (= "room-spot-1" (:id fixture)))
    (is (= [1 13] [(:universe fixture) (:address fixture)]) "Directly after the existing 12-channel head")
    (is (= [1 4.75 -2] (:position fixture)))
    (is (= :hanging (placement/mounting (:rotation-deg fixture))))
    (is (= "none" (:rgbw-policy fixture)) "Draft values stay in presentation form")
    (is (= {:kind "fixtures" :id "room-spot-1"} (:selection state)))
    (is (re-find #"ceiling" (:message state)))
    (is (true? (get-in placed [:ui :rig-dirty?])))
    (is (= rig (get-in placed [:show :rig])) "Nothing is committed until Save rig")
    (let [undone (run placed [:rig-editor/undo])]
      (is (= rig (get-in undone [:ui :rig-draft]))))
    (testing "A truss hit hangs the fixture below the truss"
      (let [hung (interaction db {:type "place" :point [0 4 0] :normal [0 -1 0] :kind "truss" :truss-id "t1"})]
        (is (re-find #"truss t1" (:message (editor/editor hung))))))
    (testing "Without a chosen type or with a stale identity nothing is placed"
      (is (= rig (get-in (interaction (tool base "place") {:type "place" :point [0 0 0] :normal [0 1 0]}) [:ui :rig-draft])))
      (is (= rig (get-in (run db [:rig-editor/interaction {:type "place" :identity "old" :point [0 0 0] :normal [0 1 0]}])
                         [:ui :rig-draft]))))
    (testing "Another tool ignores a stray place event"
      (is (= rig (get-in (interaction (tool base "move" {:place-type "room-spot"}) {:type "place" :point [0 0 0] :normal [0 1 0]})
                         [:ui :rig-draft]))))))

(deftest mounting-range-and-reversal-edit-the-selected-draft-fixture
  (let [db (tool base "mount" {:selection {:kind "fixtures" :id "head"}})
        hung (run db [:rig-editor/mount :hanging])
        ranged (run hung [:rig-editor/axis-range :pan :min-deg -270])
        reversed (run ranged [:rig-editor/reverse-axis :pan true])
        head #(get-in % [:ui :rig-draft :fixtures 0])]
    (is (= [0 0 180] (:rotation-deg (head hung))))
    (is (= {:min-deg -270 :max-deg 180} (get-in (head ranged) [:calibration :pan])))
    (is (= {:offset 1 :fine-offset 2 :inverted-from 0} (get-in (head reversed) [:channels :pan])))
    (is (= 3 (count (:undo (editor/editor reversed)))))
    (is (= (head ranged) (head (run reversed [:rig-editor/undo]))))
    (is (re-find #"below its maximum" (:message (editor/editor (run reversed [:rig-editor/axis-range :pan :min-deg 400]))))))
  (testing "The Aim home drag spins the fixture about its mount towards the pointer"
    (let [db (tool base "mount" {:plane "xz"})
          begun (interaction db {:type "begin" :selection {:kind "fixtures" :id "head"} :point [0 3 0]})
          moved (interaction begun {:type "move" :point [3 3 0]})
          rotation (get-in moved [:ui :rig-draft :fixtures 0 :rotation-deg])]
      (is (every? #(< (js/Math.abs %) 1e-9) (map - [1 0 0] (:forward (placement/axes rotation)))))
      (is (= :standing (placement/mounting rotation)))
      (is (= rig (get-in (interaction moved {:type "cancel"}) [:ui :rig-draft]))))))

(deftest aim-calibration-requests-are-read-only-and-fenced
  (let [db (tool base "calibrate" {:selection {:kind "fixtures" :id "head"}})
        targeted (interaction db {:type "surface" :point [2 0 1] :surface-id "floor"})
        state (editor/editor targeted)]
    (is (= [2 0 1] (get-in state [:calibration :target])))
    (is (= rig (get-in targeted [:ui :rig-draft])) "Tapping a target never moves a fixture")
    (let [{:keys [db studio/http]} (fx targeted [:rig-editor/aim-check])
          request (get-in (editor/editor db) [:calibration :checking])]
      (is (= "/api/fixtures/aim-check" (:path http)))
      (is (= {:fixture-id "head" :point [2 0 1] :position [0 3 0] :rotation-deg [0 0 0]
              :calibration {:pan {:min-deg -180 :max-deg 180} :tilt {:min-deg -90 :max-deg 90}}} (:body http)))
      (let [answered (run db [:rig-editor/calibration-result (editor/identity-key db) request true {:reachable? true :pan-deg 63.4}])]
        (is (= 63.4 (get-in (editor/editor answered) [:calibration :check :pan-deg]))))
      (is (= db (run db [:rig-editor/calibration-result (editor/identity-key db) "stale" true {:reachable? true}]))))
    (testing "Observations record the head's committed pan/tilt and fit with the verified flag"
      (let [recorded (-> targeted (run [:rig-editor/record-sample])
                         (interaction {:type "surface" :point [-2 0 1]}) (run [:rig-editor/record-sample])
                         (interaction {:type "surface" :point [0 0 4]}) (run [:rig-editor/record-sample])
                         (run [:rig-editor/sample-value 2 :tilt 51]))
            samples (get-in (editor/editor recorded) [:calibration :samples])
            {:keys [studio/http]} (fx (run recorded [:rig-editor/set [:calibration :verified?] true]) [:rig-editor/aim-fit])]
        (is (= [{:target [2 0 1] :pan 0.25 :tilt 0.75} {:target [-2 0 1] :pan 0.25 :tilt 0.75} {:target [0 0 4] :pan 0.25 :tilt 0.2}] samples))
        (is (= "/api/fixtures/aim-fit" (:path http)))
        (is (true? (get-in http [:body :axis-verified?])))
        (is (= 3 (count (get-in http [:body :samples]))))
        (let [db (editor/put-editor recorded (assoc-in (editor/editor recorded) [:calibration :fit] {:fixture-id "head" :rotation-deg [0 0 180] :accepted? true}))
              applied (run db [:rig-editor/apply-fit])]
          (is (= [0 0 180] (get-in applied [:ui :rig-draft :fixtures 0 :rotation-deg])))
          (is (= rig (get-in (run applied [:rig-editor/undo]) [:ui :rig-draft]))))))
    (testing "A live test aim needs a saved rig and goes through the surface-aim command"
      (let [{:keys [studio/http]} (fx targeted [:rig-editor/test-aim])]
        (is (= "/api/surface-aim" (:path http)))
        (is (= {:mode "together" :targets ["head"] :point [2 0 1] :surface-id nil}
               (select-keys (:body http) [:mode :targets :point :surface-id]))))
      (let [{:keys [db studio/http]} (fx (assoc-in targeted [:ui :rig-dirty?] true) [:rig-editor/test-aim])]
        (is (nil? http))
        (is (re-find #"Save or discard" (:message (editor/editor db))))))))

(deftest placement-helpers
  (is (= ["pan" "tilt"] (rp/missing-calibration {:channels {:pan 1 :tilt 2} :calibration {}})))
  (is (nil? (rp/missing-calibration {:channels {:red 1}})))
  (is (= {:pan 0.25 :tilt 0.75} (rp/output-sample base "head")))
  (is (nil? (rp/output-sample base "other")))
  (is (= "room-spot" (:id (rp/find-type base :room-spot)))))

(def two-heads
  (update rig :fixtures conj {:id "other" :position [2 3 0] :rotation-deg [0 0 0] :universe 1 :address 20
                              :channels {:pan {:offset 1 :inverted-from 0} :tilt 3}
                              :calibration {:pan {:min-deg -180 :max-deg 180} :tilt {:min-deg -90 :max-deg 90}}}))

(deftest calibration-state-belongs-to-the-selected-fixture
  (let [db (-> base (assoc-in [:show :rig] two-heads) (assoc-in [:ui :rig-draft] two-heads)
               (tool "calibrate" {:selection {:kind "fixtures" :id "head"}}))
        targeted (-> db (interaction {:type "surface" :point [2 0 1]}) (run [:rig-editor/record-sample]))
        {in-flight :db} (fx targeted [:rig-editor/aim-check])
        request (get-in (editor/editor in-flight) [:calibration :checking])
        fitted (editor/put-editor in-flight (assoc-in (editor/editor in-flight) [:calibration :fit]
                                                      {:fixture-id "head" :rotation-deg [0 0 180] :accepted? true}))
        other (run fitted [:rig-editor/set [:selection] {:kind "fixtures" :id "other"}])
        state (editor/editor other)]
    (is (= 1 (count (get-in (editor/editor targeted) [:calibration :samples]))))
    (is (= {:fixture-id "other" :samples [] :target [2 0 1]} (:calibration state))
        "Another head starts without the first head's observations, check or fit; the target stays")
    (is (= two-heads (get-in (run other [:rig-editor/apply-fit]) [:ui :rig-draft])) "Nothing to apply to the other head")
    (is (= state (editor/editor (run other [:rig-editor/calibration-result (editor/identity-key other) request true {:reachable? true}])))
        "A check that left for the first head is dropped")
    (testing "A fit for another fixture is never applied"
      (let [mismatched (editor/put-editor other (assoc-in (editor/editor other) [:calibration :fit] {:fixture-id "head" :rotation-deg [0 0 180]}))]
        (is (= two-heads (get-in (run mismatched [:rig-editor/apply-fit]) [:ui :rig-draft])))))
    (testing "Selecting the first head again finds its state only while the selection matches"
      (is (empty? (get-in (editor/editor (run other [:rig-editor/set [:selection] {:kind "fixtures" :id "head"}])) [:calibration :samples]))))
    (testing "Observation fields hold DMX sent; a reversed pan converts to the model value"
      (let [recorded (run other [:rig-editor/record-sample])
            typed (run recorded [:rig-editor/sample-value 0 :pan 51])]
        (is (= 0.8 (get-in (editor/editor typed) [:calibration :samples 0 :pan])))
        (is (= 51 (placement/dmx-sent (get-in two-heads [:fixtures 1 :channels :pan]) 0.8)))))))

(deftest placements-never-reuse-a-committed-id-and-are-marked
  (let [committed (update rig :fixtures conj {:id "room-spot-1" :universe 2 :address 1 :channels {:dimmer 1}})
        db (-> base (assoc-in [:show :rig] committed) (tool "place" {:place-type "room-spot"}))
        placed (last (get-in (interaction db {:type "place" :point [0 0 0] :normal [0 1 0]}) [:ui :rig-draft :fixtures]))]
    (is (= "room-spot-2" (:id placed)) "room-spot-1 was removed from this draft but is still committed")
    (is (true? (:placement? placed))))
  (let [legacy {:channels {:pan 1 :tilt 2} :calibration {:pan [100 -90] :tilt {:min-deg -90 :max-deg 90}}}
        edited (rp/set-range legacy :pan :max-deg 210)]
    (is (nil? (rp/missing-calibration legacy)) "A reversed legacy calibration is not missing")
    (is (= {:min-deg -310 :max-deg 210} (update (get-in edited [:calibration :pan]) :min-deg js/Math.round)))
    (is (placement/reversed? (get-in edited [:channels :pan])) "Its reversal moves to the channel")))
