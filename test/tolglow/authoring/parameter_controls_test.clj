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
            :step (/ 31.5 1000) :widget :rotary :slot :compact}
           (controls/control :period {:type :number :bounds [0.5 32] :unit :beats} catalog)))
    (is (= {:param :mode :label "Mode" :type :shape :places 3 :choices [{:value :sine :label "Sine"} {:value :saw :label "Saw"}]
            :slot :wide}
           (controls/control :mode {:type :enum :name "Mode" :values [:sine :saw]} catalog)))
    (is (= :fader (:widget (controls/control :width {:type :number :ui/widget :fader :bounds [0 1]}))))
    (is (= :field (:widget (controls/control :drift {:type :number}))))))

(deftest the-expression-type-is-what-a-param-read-gives
  (is (= :number (controls/expression-type {:type :number}) (controls/expression-type {:type :integer})))
  (is (= :vec3 (controls/expression-type {:type :color})))
  (is (= :vec2 (controls/expression-type {:type :range})))
  (is (= :vec4 (controls/expression-type {:type :vector :size 4})))
  (testing "a gradient, enum, boolean or oversized vector has no {:op :param} type"
    (is (every? nil? (map controls/expression-type
                          [{:type :gradient} {:type :enum} {:type :boolean} {:type :vector :size 5} {}])))))

(deftest a-vector-chooses-its-compound-widget
  (testing "two bounded components are an XY pad, three a pad with a Z fader, four nested rings"
    (is (= :xy (controls/compound-widget {:type :vector :size 2 :bounds [-1 1]})))
    (is (= :xyz (controls/compound-widget {:type "vector" :size 3 :bounds [0 1]})) "wire strings read as keywords")
    (is (= :rings (controls/compound-widget {:type :vector :size 4 :bounds [0 1]}))))
  (testing "component schemas give each axis its own bounds"
    (is (= :xyz (controls/compound-widget {:type :vector :size 3
                                           :component-schemas [{:bounds [-180 180]} {:bounds [-90 90]} {:bounds [0 20]}]})))
    (is (= [{:type :number :bounds [-180 180] :unit :degrees} {:type :number :bounds [0 1] :unit :degrees}]
           (controls/component-schemas {:type :vector :size 2 :bounds [0 1] :unit :degrees
                                        :component-schemas [{:bounds [-180 180]}]}))))
  (testing ":ui/widget asks for rings or axes on any size, and a pad only on the size it fits"
    (is (= :rings (controls/compound-widget {:type :vector :size 2 :bounds [0 1] :ui/widget :rings})))
    (is (= :axes (controls/compound-widget {:type :vector :size 3 :bounds [0 1] :ui/widget "axes"})))
    (is (= :xyz (controls/compound-widget {:type :vector :size 3 :bounds [0 1] :ui/widget :xy})))
    (is (= :xy (controls/compound-widget {:type :vector :size 2 :bounds [0 1] :ui/widget :xyz}))))
  (testing "an unbounded component or an unsupported size keeps the component readouts"
    (is (= :axes (controls/compound-widget {:type :vector :size 2})))
    (is (= :axes (controls/compound-widget {:type :vector :size 2 :component-schemas [{:bounds [0 1]} {}]})))
    (is (= :axes (controls/compound-widget {:type :vector :size 5 :bounds [0 1]})))
    (is (= :axes (controls/compound-widget {:type :vector :size 1 :bounds [0 1]}))))
  (testing "colour vectors, ranges and numbers are not compound"
    (is (nil? (controls/compound-widget {:type :vector :size 3 :bounds [0 1] :semantic/type :rgb})))
    (is (nil? (controls/compound-widget {:type :range :bounds [0 1]})))
    (is (nil? (controls/compound-widget {:type :number :bounds [0 1]}))))
  (testing "the description names it, so Studio and Push draw the same widget"
    (is (= :xy (:widget (controls/control :offset {:type :vector :size 2 :bounds [-1 1]}))))
    (is (= :rings (:widget (controls/control :mix {:type :vector :size 4 :bounds [0 1]}))))
    (is (not (contains? (controls/control :tint {:type :color}) :widget)))))

(deftest every-widget-type-has-one-standard-slot
  (let [slot (fn [schema] (:slot (controls/control :p schema catalog)))]
    (testing "knobs, faders, toggles, choices, swatches, fields and closed rings are compact"
      (is (= #{:compact}
             (set (map slot [{:type :number :bounds [0 1]}
                             {:type :integer :bounds [0 8] :ui/widget :fader}
                             {:type :boolean} {:type :enum :values [:soft :hard]}
                             {:type :number :choices [{:value 0 :label "Off"} {:value 1 :label "On"}]}
                             {:type :color} {:type :number}
                             {:type :vector :size 4 :bounds [0 1]}
                             {:type :vector :size 2 :bounds [0 1] :ui/widget :rings}])))))
    (testing "pickers, gradients, ranges and component readouts are wide"
      (is (= #{:wide}
             (set (map slot [{:type :enum :values [:sine :saw]}
                             {:type :number :semantic/type :waveform-selector :choices [{:value 0 :id :sine}]}
                             {:type :gradient} {:type :range :bounds [0 1]}
                             {:type :vector :size 3}
                             {:type :vector :size 2 :bounds [0 1] :ui/widget :axes}
                             {:type :vector :size 3 :semantic/type :rgb :bounds [0 1]}])))))
    (testing "pads are large"
      (is (= #{:large} (set (map slot [{:type :vector :size 2 :bounds [-1 1]}
                                       {:type :vector :size 3 :bounds [0 1]}])))))
    (testing "spans are whole slots of one unit"
      (is (= {:compact [1 1] :wide [2 1] :large [2 2]} controls/slot-sizes))
      (is (= [1 1] (controls/slot-span :unknown))))))
