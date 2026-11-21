(ns tolglow.fixtures-test
  "Characterization of the existing declarative fixture API against the actual
  Afterglow fork. Requires the integration dependency lane; no channel stubs."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.fixtures :as fixtures]))

(deftest color-layouts-allocate-sequential-or-explicit-offsets
  (doseq [[layout color-keys] [[:rgb [:red :green :blue]]
                             [:rgbw [:red :green :blue :white]]
                             [:rgbwa [:red :green :blue :white :amber]]]]
    (let [channels (vec (fixtures/colors layout 4))]
      (is (= color-keys (mapv :color channels)))
      (is (= (vec (range 4 (+ 4 (count color-keys))))
             (mapv :offset channels)))))
  (is (= [10 11 9] (mapv :offset (fixtures/colors :rgb [10 11 9])))))

(deftest color-hue-metadata-covers-additional-emitters
  (is (= 45 (:hue (fixtures/color :amber 1))))
  (is (= 270 (:hue (fixtures/color :uv 1))))
  (is (= 50 (:hue (fixtures/color :amber 1 :hue 50)))))

(deftest make-channels-resolves-known-types-layouts-and-fallbacks
  (let [channels (sort-by :offset
                         (fixtures/make-channels
                          {:dimmer 1 :rgbw 2 :control 6}))]
    (is (vector? (fixtures/make-channels {:dimmer 1})))
    (is (= [1 2 3 4 5 6] (mapv :offset channels)))
    (is (= [:dimmer :color :color :color :color :control]
           (mapv :type channels)))
    (is (= [:red :green :blue :white]
           (mapv :color (filter #(= :color (:type %)) channels)))))
  (is (= [11 12 13 14]
         (mapv :offset (fixtures/make-channels {:rgbw 1} 10))))
  (is (nil? (fixtures/make-channels nil)))
  (is (= [] (fixtures/make-channels {}))))

(deftest make-channels-retains-fine-pan-and-tilt-offsets
  (let [channels (fixtures/make-channels (array-map :pan [1 13] :tilt [2 14]))]
    (is (= [{:type :pan :offset 1 :fine-offset 13}
            {:type :tilt :offset 2 :fine-offset 14}]
           (mapv #(select-keys % [:type :offset :fine-offset]) channels)))))

(deftest pixel-head-positions-are-centered-and-include-both-ends
  (is (= [-1.0 0.0 1.0]
         (mapv #(fixtures/head-pos 3 % 2) (range 3))))
  (is (= 0.0 (fixtures/head-pos 1 0 10)))
  (is (= [-2 0 2]
         (mapv #(fixtures/pos-for-head 3 % -2 2) (range 3)))))

(deftest declarative-strip-preserves-pixel-order-and-global-channel-prefix
  (let [fixture (fixtures/create
                 {:name "RGBW test strip"
                  :channels {:dimmer 1}
                  :strip {:color-layout :rgbw :pixels 3 :density 3}})
        heads (vec (:heads fixture))]
    (is (= "RGBW test strip" (:name fixture)))
    (is (= [1] (mapv :offset (:channels fixture))))
    (is (= [-0.5 0.0 0.5] (mapv :x heads)))
    (is (= [0.0 0.0 0.0] (mapv :y heads)))
    (is (= [0.0 0.0 0.0] (mapv :z heads)))
    (is (= [[2 3 4 5] [6 7 8 9] [10 11 12 13]]
           (mapv #(mapv :offset (:channels %)) heads)))
    (is (every? #(= [:red :green :blue :white]
                    (mapv :color (:channels %))) heads))))

(deftest strip-without-global-channels-starts-at-one
  (let [fixture (fixtures/create
                 {:strip {:color-layout :rgb :pixels 1 :density 1}})]
    (is (= "Unnamed fixture" (:name fixture)))
    (is (not (contains? fixture :channels)))
    (is (= [1 2 3] (mapv :offset (:channels (first (:heads fixture))))))
    (is (= 0.0 (:x (first (:heads fixture)))))))

(deftest moving-head-calibration-preserves-zero-and-partial-values
  (is (= {:pan-center 127 :pan-half-circle 85
          :tilt-center 0 :tilt-half-circle 256}
         (fixtures/moving-head-calibration {:pan [127 85] :tilt [0 256]})))
  (is (= {:pan-center 0}
         (fixtures/moving-head-calibration {:pan [0]})))
  (is (nil? (fixtures/moving-head-calibration {})))
  (is (nil? (fixtures/moving-head-calibration nil)))
  (is (= {:name "Mover" :pan-center 127 :pan-half-circle 85}
         (fixtures/create {:name "Mover" :calibration {:pan [127 85]}}))))

(deftest strobe-channel-keeps-open-and-closed-ranges
  (let [channel (fixtures/strobe 7 :open 8 :start 16)]
    (is (= :shutter (:type channel)))
    (is (= 7 (:offset channel)))
    (is (= [{:type :shutter-closed :start 0 :end 7 :range :fixed}
            {:type :shutter-open :start 8 :end 15 :range :fixed}
            {:type :strobe :start 16 :end 255 :range :variable}]
           (mapv #(select-keys % [:type :start :end :range])
                 (:functions channel)))))
  (is (= :strobe (:type (fixtures/strobe 2)))))

(deftest strobe-hertz-scaler-clamps-to-channel-percentage
  (let [spec (fixtures/shutter-strobe-map :hz [2 10])
        scale (:scale-fn spec)]
    (is (= "Strobe (2Hz->10Hz)" (:label spec)))
    (is (= :variable (:range spec)))
    (is (= [0 0 50 100 100] (mapv scale [-5 2 6 10 40]))))
  (is (= "Manual label"
         (:label (fixtures/shutter-strobe-map :label "Manual label"))))
  (is (= "Strobe (Slow->Fast)" (:label (fixtures/shutter-strobe-map)))))
