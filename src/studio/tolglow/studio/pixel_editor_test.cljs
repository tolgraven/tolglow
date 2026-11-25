(ns tolglow.studio.pixel-editor-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.pixel-editor :as pixels]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.rig-editor :as editor]))

(def rig {:fixtures [{:id "spot" :universe 2 :address 1 :position [0 2 0]}]})

(def db {:show {:runtime-id "r" :show-id "s" :epoch 1 :rig rig}
         :ui {:rig-draft rig}})

(defn pointer [db type fields]
  (editor/interaction db (merge {:type type :identity (editor/identity-key db) :token 4} fields)))

(deftest pixel-anchors-edit-without-touching-running-rig-or-aim-path
  (let [[rig' state] (pixels/edit rig (assoc (editor/editor db) :points [[1 1 1]]) [:new :path])
        start (-> db (assoc-in [:ui :rig-draft] rig') (editor/put-editor state))
        first-anchor (-> start (pointer "begin" {:point [0 1 0]}) (pointer "end" {}))
        drawn (-> first-anchor (pointer "begin" {:point [2 1 0]}) (pointer "end" {}))
        drag (-> drawn (pointer "begin" {:point [0 1 0] :selection {:kind "points" :index 0}})
                 (pointer "move" {:point [0 2 0]}))
        cancel (pointer drag "cancel" {})]
    (is (= rig (get-in drag [:show :rig])))
    (is (= [[1 1 1]] (:points (editor/editor drag))))
    (is (= [[0 2 0] [2 1 0]] (get-in drag [:ui :rig-draft :pixel-layouts 0 :points])))
    (is (= (get-in drawn [:ui :rig-draft]) (get-in cancel [:ui :rig-draft])))
    (is (= (:undo (editor/editor drawn)) (:undo (editor/editor cancel))))
    (is (= drag (pointer drag "move" {:point [7 7 7] :token 5})))
    (is (= 3 (get-in drag [:ui :rig-draft :pixel-layouts 0 :universe])))))

(deftest large-shape-samples-and-wire-indices-stay-together
  (let [spec {:id "ceiling" :shape "grid" :color-layout "rgbw" :rows 250 :columns 400
              :origin [0 0 0] :column-step [0.01 0 0] :row-step [0 0 0.01] :universe 2 :address 509}
        state (assoc (pixels/activate {} "ceiling") :pixel-mode :pixel :pixel-index 713)
        [rig' _] (pixels/edit (assoc rig :pixel-layouts [spec]) state [:pixel 713 [1 2 3]])
        shape (pixels/selected rig' state)
        geometry (pixels/geometry shape 713)
        projection (pixels/projection state shape geometry)]
    (is (= 100000 (:pixel-count geometry)))
    (is (= 2048 (count (:pixel-positions projection))))
    (is (= [0 99999] ((juxt first peek) (:pixel-indices projection))))
    (is (= [1 2 3] (:pixel-position projection)))
    (is (= 713 (:selected-pixel projection)))
    (is (= [2 509] (get-in geometry [:patch :start])))
    (is (= [784 124] (get-in geometry [:patch :end])))
    (is (= (dissoc shape :position-overrides)
           (dissoc (pixels/selected (first (pixels/edit rig' state [:reset-pixel 713])) state) :position-overrides)))))

(deftest compact-save-restores-typed-fields-and-keeps-manual-fixtures
  (let [spec {:id :ceiling/strip :color-layout :rgbw :group :rig/floor :shape :path
              :pixels 2 :points [[0 0 0] [0 1 0]]}
        stored (assoc rig :pixel-layouts [spec])
        draft (-> (projection/presentation stored)
                  (update :fixtures conj {:id "ceiling/strip-head-1" :pixel-layout-id "ceiling/strip"})
                  (assoc-in [:pixel-layouts 0 :position-overrides 1] [3 2 1]))
        compact (projection/rig-request draft stored)]
    (is (= (:fixtures rig) (:fixtures compact)))
    (is (= (assoc spec :position-overrides {1 [3 2 1]}) (first (:pixel-layouts compact))))
    (is (= [] (:pixel-layouts (projection/rig-request (assoc draft :pixel-layouts []) stored))))))

(deftest invalid-and-conflicting-drafts-remain-editable
  (let [[draft state] (pixels/edit rig {:new-pixel-id "main-strip"} [:new :path])
        [same error-state] (pixels/edit draft (assoc state :new-pixel-id "main-strip") [:new :helix])]
    (is (= "main-strip" (:pixel-id state)))
    (is (some? (:error (pixels/geometry (pixels/selected draft state) 0))))
    (is (= draft same))
    (is (string? (:pixel-error error-state)))))

(deftest imported-paths-limit-canvas-handles-and-retain-the-selected-anchor
  (let [points (mapv #(vector (* 0.01 %) 2 0) (range 1000))
        spec {:id "curve" :shape :path :points points :pixels 1000 :universe 1 :address 1}
        state (assoc (pixels/activate {} "curve") :anchor-index 751)
        view (pixels/projection state spec (pixels/geometry spec 0))
        selected (:selected-point view)]
    (is (<= (count (:points view)) 256))
    (is (= 751 (get (:point-indices view) selected)))
    (is (= (get points 751) (get (:points view) selected)))))

(deftest new-layout-avoids-the-final-universe-of-straddling-manual-fixtures
  (let [rig {:fixtures [{:id "manual" :universe 3 :address 510 :channels {:pan [4 5]}}]}
        [rig' state] (pixels/edit rig {} [:new :grid])]
    (is (= 5 (:universe (pixels/selected rig' state))))))
