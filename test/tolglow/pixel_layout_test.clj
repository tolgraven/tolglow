(ns tolglow.pixel-layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.rig.pixel-layout :as layout]))

(def ribbon
  {:id :ceiling/strip :shape :path :color-layout :rgbw
   :universe 1 :address 511 :pixels 5
   :points [[0 0 0] [0 0 1] [3 0 1]]})

(deftest paths-use-distance-and-keep-the-patch-in-wire-order
  (is (= [[0.0 0.0 0.0] [0.0 0.0 1.0] [1.0 0.0 1.0] [2.0 0.0 1.0] [3.0 0.0 1.0]]
         (layout/positions ribbon)))
  (is (= {:start [1 511] :end [2 18]} (layout/patch-range ribbon)))
  (is (= [[1.0 0.0 1.0]] (layout/positions (assoc ribbon :pixels 1))))
  (let [layout (assoc ribbon :pixels 100000 :position-overrides {"19" [8 7 6]})]
    (is (= [[0.0 0.0 0.0] [8.0 7.0 6.0] [3.0 0.0 1.0]]
           (layout/positions layout [0 19 99999])))
    (is (thrown? clojure.lang.ExceptionInfo (layout/positions layout [2 1]))))
  (let [loop-path (assoc ribbon :closed? true :points [[0 0 0] [0 0 0] [2 0 0]] :pixels 4)]
    (is (= [[0.0 0.0 0.0] [1.0 0.0 0.0] [2.0 0.0 0.0] [1.0 0.0 0.0]]
           (layout/positions loop-path)))))

(deftest generated-surfaces-retain-individual-sculpting
  (let [grid {:id "grid" :shape :grid :rows 2 :columns 3 :origin [1 2 3]
              :column-step [1 0 0] :row-step [0 1 1] :serpentine? true}
        helix {:id "helix" :shape :helix :origin [1 2 3] :radius 2 :height 4 :turns 1 :pixels 5}]
    (is (= 6 (layout/pixel-count grid)))
    (is (= [[1.0 2.0 3.0] [2.0 2.0 3.0] [3.0 2.0 3.0] [3.0 3.0 4.0] [2.0 3.0 4.0] [1.0 3.0 4.0]]
           (layout/positions grid)))
    (is (= [99.0 4.0 0.0] (nth (layout/positions (assoc grid :position-overrides {2 [99 4 0]})) 2)))
    (is (every? #(< (abs %) 1.0e-9)
                (map - [1 3 5] (second (layout/positions helix))))))
  (doseq [spec [(assoc ribbon :pixels -3) (assoc ribbon :points [[0 0 0] [0 0 0]])
                (assoc ribbon :points [[0 0 0] [Double/NaN 0 0]])
                (assoc ribbon :position-overrides {5 [0 0 0]})
                (assoc ribbon :color-layout :unknown) (assoc ribbon :address 513)]]
    (is (thrown? clojure.lang.ExceptionInfo (layout/fixtures spec)))))

(deftest reapplying-layouts-preserves-other-authoring
  (let [manual {:id :spot :universe 8 :address 1 :channels {:dimmer 1}}
        rig (layout/materialize {:fixtures [manual] :pixel-layouts [ribbon]})
        edited (assoc-in rig [:fixtures 1 :measured-relative-brightness] 0.8)
        moved (layout/materialize (assoc-in edited [:pixel-layouts 0 :points 2] [5 0 1]))]
    (is (= rig (layout/materialize rig)))
    (is (= :ceiling/strip-head-1 (get-in rig [:fixtures 1 :id])))
    (is (= [2 3] ((juxt :universe :address) (get-in rig [:fixtures 2]))))
    (is (= :ceiling/strip (get-in rig [:fixtures 1 :fixture-id])))
    (is (= 0.8 (get-in moved [:fixtures 1 :measured-relative-brightness])))
    (is (= [5.0 0.0 1.0] (:position (peek (:fixtures moved)))))
    (is (= [manual] (:fixtures (layout/materialize (assoc rig :pixel-layouts [])))))
    (is (thrown? clojure.lang.ExceptionInfo (layout/materialize (assoc rig :pixel-layouts [ribbon ribbon]))))))
