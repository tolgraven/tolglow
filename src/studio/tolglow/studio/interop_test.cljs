(ns tolglow.studio.interop-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.interop :as interop]
            [tolglow.transit :as transit]
            [tolglow.studio.projection :as projection]))

(deftest equal-local-names-from-different-fixture-groups-reach-separate-preview-meshes
  (let [typed {:rig {:fixtures [{:id :left/spot} {:id :right/spot}]}
               :output {:values {:left/spot {:emitted-color [1 0 0]}
                                 :right/spot {:emitted-color [0 0 1]}}}}
        js-view (interop/preview-data (projection/presentation typed))
        fixtures (aget js-view "rig" "fixtures")
        output (aget js-view "output" "values")]
    (is (= ["left/spot" "right/spot"] (mapv #(.-id %) (array-seq fixtures))))
    (is (= [1 0 0] (vec (aget (aget output "left/spot") "emitted-color"))))
    (is (= [0 0 1] (vec (aget (aget output "right/spot") "emitted-color"))))))

(deftest fixture-optics-through-typed-presentation-and-preview
  (let [rig {:fixtures [{:id :truss/lamp :channels {:red 1 :green 2 :blue 3} :max-watts 50
                         :beam-angle 8.5 :pseudo-fixture false}
                        {:id :strip/head :measured-relative-brightness 3 :max-watts 50 :emission-share 0.5}
                        {:id :UV :channels {:uv 1} :max-watts 100}]}
        fixtures (aget (interop/preview-rig (projection/presentation (transit/decode (transit/encode rig)))) "fixtures")]
    (is (= [0.5 1.5 0] (mapv #(aget % "relative-brightness") (array-seq fixtures))))
    (is (= "truss/lamp" (.-id (aget fixtures 0))))
    (is (= 8.5 (aget (aget fixtures 0) "beam-angle")))
    (is (false? (aget (aget fixtures 0) "pseudo-fixture")))
    (is (not (contains? (first (:fixtures rig)) :relative-brightness)))))
