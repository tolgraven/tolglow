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
    (is (= #{:rig-view :live-view} (set (interop/preview-recipients views :worklight nil))))
    (is (= [:rig-view] (interop/preview-recipients views :burst nil)))
    (is (= [:live-view] (interop/preview-recipients views :worklight :perform)))
    (is (= :rig-view (interop/preview-engine views nil)))
    (is (= :live-view (interop/preview-engine views :perform)))
    (is (= :live-view (interop/preview-engine (update views :previews dissoc :rig) nil)))
    (is (nil? (interop/preview-engine (update views :previews dissoc :rig) :rig)))))

(deftest canvas-rate-counts-accepted-bitmap-intervals-and-resets-with-the-owner
  (let [time (atom 0) rate (atom nil)
        {:keys [present! clear!]} (interop/bitmap-cadence #(deref time) #(reset! rate %))]
    (present! "night" 1)
    (doseq [n (range 1 21)] (reset! time (* 50 n)) (present! "night" 1))
    (is (= 20 @rate))
    (clear!)
    (is (nil? @rate))
    (reset! time 16000)
    (present! "night" 1)
    (reset! time 16200)
    (present! "next show" 1)
    (is (nil? @rate))
    (doseq [n (range 1 11)] (reset! time (+ 16200 (* 100 n))) (present! "next show" 1))
    (is (= 10 @rate))
    (present! "next show" 2)
    (is (nil? @rate))))

(deftest a-show-frame-leaves-the-room-input-unchanged
  (let [show {:runtime-id "r" :show-id "main" :epoch 1 :revision 4 :master 0.8 :blackout? false
              :rig {:fixtures [{:id "head-1"}]} :instances [{:id "1:1"}] :frame-number 10 :time-ms 250
              :transport {:beat 3.5} :schedule {:entries []}
              :status {:frame-ms 1.2 :output-scheduler {:queued 2} :last-intents [{:type "instance/finished"}]}
              :output {:atmosphere {:sources []} :universes {1 [255]}}}
        next-frame (-> show (assoc :frame-number 11 :time-ms 275) (assoc-in [:transport :beat] 3.6)
                       (assoc-in [:status :frame-ms] 1.4) (assoc-in [:status :output-scheduler :queued] 3))
        input (interop/room-input show)]
    (is (= input (interop/room-input next-frame)))
    (is (= {:sources []} (:atmosphere input)))
    (is (= [{:type "instance/finished"}] (:last-intents input)))
    (is (not-any? input [:frame-number :time-ms :status :output :transport]))
    (is (nil? (interop/room-input nil)))))

(deftest a-lane-frame-waits-for-the-room-input-of-its-revision
  (let [input {:runtime-id "r" :show-id "main" :epoch 1 :revision 4}
        lane {:identity {:runtime-id "r" :show-id :main :epoch 1} :revision 4 :value {}}]
    (is (interop/consistent? input lane) "typed and presented identities name the same show")
    (is (interop/consistent? input nil))
    (is (not (interop/consistent? input (assoc lane :revision 5))))
    (is (not (interop/consistent? input (assoc-in lane [:identity :epoch] 2))))))

(deftest lane-values-convert-like-presented-preview-data-and-reuse-unchanged-fixtures
  (let [typed (transit/decode (transit/encode
                               {:truss/spot {:emitted-color [1 0.5 0] :mode :gobo/spin :phase (transit/->BeatFraction "1" "4")
                                             :tags #{:a} :pose {:direction [0 1 0]}}
                                "strip" {:intensity 0.25 :cells [{:id :cell/a :level 1}]}}))
        expected (interop/preview-data (projection/presentation typed))
        converted (interop/values->js typed nil nil)
        js->data #(js->clj (js/JSON.parse (js/JSON.stringify %)))]
    (is (= (js->data expected) (js->data converted)))
    (is (= 0.25 (aget converted "truss/spot" "phase")))
    (is (= "gobo/spin" (aget converted "truss/spot" "mode")))
    (let [moved (assoc-in typed ["strip" :intensity] 0.5)
          next-js (interop/values->js moved typed converted)]
      (is (identical? (aget converted "truss/spot") (aget next-js "truss/spot")) "an unchanged fixture keeps its object")
      (is (not (identical? (aget converted "strip") (aget next-js "strip"))))
      (is (= 0.5 (aget next-js "strip" "intensity")))
      (is (= 0.25 (aget converted "strip" "intensity")) "the previous frame's objects are never mutated"))
    (is (nil? (interop/values->js nil typed converted)))))
