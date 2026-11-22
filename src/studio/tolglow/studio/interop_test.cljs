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

(deftest owned-room-projection-forwards-the-accepted-fog-sample
  (let [sample {:epoch 2 :sample-time-ms 2500 :volume-m3 300 :mixed-m3 15 :mixed-density 0.05
                :sources [{:fixture-id :truss/fog :position [0 1 0] :direction [1 0 0]
                           :flow-m3-s 2 :local-m3 4 :emitted-m3 20}]}
        projected (interop/preview-output {:output {:values {} :atmosphere sample :universes {0 [255]}}})
        fog (aget projected "atmosphere")]
    (is (= 2500 (aget fog "sample-time-ms")))
    (is (= 0.05 (aget fog "mixed-density")))
    (is (= "truss/fog" (aget (aget (aget fog "sources") 0) "fixture-id")))
    (is (nil? (aget projected "universes")))))

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

(deftest preview-controls-return-to-mounted-perform-engine-after-rig-unmount
  (let [views {:previews {:rig :rig-view :perform :live-view}}]
    (is (= :rig-view (interop/preview-engine views nil)))
    (is (= :live-view (interop/preview-engine views :perform)))
    (is (= :live-view (interop/preview-engine (update views :previews dissoc :rig) nil)))
    (is (nil? (interop/preview-engine (update views :previews dissoc :rig) :rig)))))
