(ns tolglow.studio.interop-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.interop :as interop]
            [tolglow.studio.projection :as projection]))

(deftest equal-local-names-from-different-fixture-groups-reach-separate-preview-meshes
  (let [typed {:rig {:fixtures [{:id :left/spot} {:id :right/spot}]}
               :output {:values {:left/spot {:emitted-color [1 0 0]}
                                 :right/spot {:emitted-color [0 0 1]}}}}
        js-view (interop/preview-data (projection/presentation typed))
        fixtures (.. js-view -rig -fixtures)
        output (.. js-view -output -values)]
    (is (= ["left/spot" "right/spot"] (mapv #(.-id %) (array-seq fixtures))))
    (is (= [1 0 0] (vec (aget (aget output "left/spot") "emitted-color"))))
    (is (= [0 0 1] (vec (aget (aget output "right/spot") "emitted-color"))))))
