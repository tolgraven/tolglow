(ns tolglow.authoring.parameter-controls-test
  "The control description Studio's parameter rows and the Live parameter
  device (#625) both read."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.parameter-controls :as controls]))

(def catalog [{:id :sine :name "Sine" :glyph [0 1 0]} {:id :saw :name "Saw" :glyph [0 1]}])

(deftest a-control-names-its-family-and-readout
  (testing "decimals follow the patched channel, as Studio's readouts do"
    (is (= 3 (controls/decimal-places {:type :number :bounds [0 1]})))
    (is (= 5 (controls/decimal-places {:type :number :bounds [0 1] :dmx-bits 16})))
    (is (= 0 (controls/decimal-places {:type :number :bounds [0 255] :dmx-bits 8})))
    (is (= 3 (controls/decimal-places {:type :number :bounds [-180 180] :dmx-bits 16}))))
  (testing "the input step is one DMX code, else a thousandth of the range; integers step by one"
    (is (= 1 (controls/input-step {:type :integer :bounds [0 10]})))
    (is (= (/ 1.0 65535) (controls/input-step {:type :number :bounds [0 1] :dmx-bits 16})))
    (is (= 1/50 (controls/input-step {:type :number :bounds [0 20]}))))
  (testing "the family: waveform selector, shape glyphs, choices, else the storage type"
    (is (= :waveform (controls/control-type {:type :number :semantic/type :waveform-selector :choices [{:value 0 :id :sine}]} catalog)))
    (is (= :shape (controls/control-type {:type :enum :values [:sine :saw]} catalog)))
    (is (= :enum (controls/control-type {:type :enum :values [:soft :hard]} catalog)))
    (is (= :choice (controls/control-type {:type :number :choices [{:value 0 :label "Off"} {:value 1 :label "On"}]} catalog)))
    (is (= :vector (controls/control-type {:type "vector" :size 3} catalog)) "wire strings read as keywords"))
  (testing "one description"
    (is (= {:param :period :label "period" :type :number :places 3 :unit "beats" :bounds [0.5 32]
            :step (/ 31.5 1000) :widget :rotary}
           (controls/control :period {:type :number :bounds [0.5 32] :unit :beats} catalog)))
    (is (= {:param :mode :label "Mode" :type :shape :places 3 :choices [{:value :sine :label "Sine"} {:value :saw :label "Saw"}]}
           (controls/control :mode {:type :enum :name "Mode" :values [:sine :saw]} catalog)))
    (is (= :fader (:widget (controls/control :width {:type :number :ui/widget :fader :bounds [0 1]}))))
    (is (= :field (:widget (controls/control :drift {:type :number}))))))
