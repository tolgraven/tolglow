(ns tolglow.visualization.musical-grid-test
  (:require [afterglow.time :as time]
            [clojure.test :refer [deftest is]]
            [tolglow.visualization.musical-grid :as grid]))

(def common {:beats-per-bar 4 :bars-per-phrase 4})

(def seven-eight {:beats-per-bar 3.5 :bars-per-phrase 4})

(defn- marks
  "Beat and unit of each line, the part a renderer tints."
  [spec]
  (mapv (juxt :beat :unit) (:lines spec)))

(defn- cycle
  "One cycle of a period, as a cycle plot draws it."
  [period-beats meter]
  (grid/grid {:span-beats period-beats :period-beats period-beats :meter meter}))

(deftest unit-lengths-agree-with-the-authoritative-duration-conversion
  (doseq [meter [common seven-eight {:beats-per-bar 3 :bars-per-phrase 8}]
          :let [sizes (grid/unit-beats meter)]]
    (is (== (time/duration->beats {:beats 1} meter) (:beat sizes)))
    (is (== (time/duration->beats {:bars 1} meter) (:bar sizes)))
    (is (== (time/duration->beats {:phrases 1} meter) (:phrase sizes)))
    (is (== 1/4 (:sixteenth sizes))))
  (is (= (grid/unit-beats nil) (grid/unit-beats time/default-meter)))
  (is (nil? (grid/unit-beats {:beats-per-bar 0})))
  (is (nil? (grid/unit-beats {:bars-per-phrase 2.5}))))

(deftest a-quarter-beat-period-draws-sixteenths-inside-a-beat
  (let [spec (cycle 0.25 common)]
    (is (= [:sixteenth :beat] (:units spec)))
    (is (= [:sixteenth :beat] ((juxt :minor :major) spec)))
    (is (= [[0.0 :beat] [0.25 :sixteenth]] (marks spec)))
    (is (= [0.0 1.0] (mapv :position (:lines spec))))
    (is (= {:unit :beat :count 0.25 :beats 0.25} (:period spec)))
    (is (= "1/4 beat" (grid/measure-text (:period spec))))))

(deftest a-one-beat-period-draws-sixteenths-between-beat-lines
  (let [spec (cycle 1 common)]
    (is (= [:sixteenth :beat] (:units spec)))
    (is (= [[0.0 :beat] [0.25 :sixteenth] [0.5 :sixteenth] [0.75 :sixteenth] [1.0 :beat]] (marks spec)))
    (is (= [0.0 0.25 0.5 0.75 1.0] (mapv :position (:lines spec))))
    (is (= "1 beat" (grid/measure-text (:period spec))))))

(deftest a-one-bar-period-draws-beats-between-bar-lines
  (let [spec (cycle 4 common)]
    (is (= [:beat :bar] (:units spec)))
    (is (= [[0.0 :bar] [1.0 :beat] [2.0 :beat] [3.0 :beat] [4.0 :bar]] (marks spec)))
    (is (= [0.0 0.25 0.5 0.75 1.0] (mapv :position (:lines spec))))
    (is (= {:unit :bar :count 1.0 :beats 4.0} (:period spec)))
    (is (= "1 bar" (grid/measure-text (:period spec))))))

(deftest a-four-bar-period-draws-bars-and-the-phrases-they-reach
  (let [phrase (cycle 16 common)
        half (cycle 16 {:beats-per-bar 4 :bars-per-phrase 8})]
    (is (= [:bar :phrase] (:units phrase) (:units half)))
    (is (= [[0.0 :phrase] [4.0 :bar] [8.0 :bar] [12.0 :bar] [16.0 :phrase]] (marks phrase)))
    (is (= "1 phrase" (grid/measure-text (:period phrase))))
    (is (= [[0.0 :phrase] [4.0 :bar] [8.0 :bar] [12.0 :bar] [16.0 :bar]] (marks half))
        "four bars of an eight-bar phrase end on a bar line")
    (is (= "4 bars" (grid/measure-text (:period half))))))

(deftest a-one-phrase-period-draws-bars-between-phrase-lines
  (let [spec (cycle 32 {:beats-per-bar 4 :bars-per-phrase 8})]
    (is (= [:bar :phrase] (:units spec)))
    (is (= [:phrase :bar :bar :bar :bar :bar :bar :bar :phrase] (mapv :unit (:lines spec))))
    (is (= (mapv #(/ % 8.0) (range 9)) (mapv :position (:lines spec))))
    (is (= {:unit :phrase :count 1.0 :beats 32.0} (:period spec)))))

(deftest seven-eight-bars-are-three-and-a-half-beats-and-restart-the-beat-lines
  (let [bar (cycle 3.5 seven-eight)
        two-bars (grid/grid {:span-beats 7 :period-beats 3.5 :meter seven-eight})
        beat (cycle 1 seven-eight)
        phrase (cycle 14 seven-eight)]
    (is (= [:beat :bar] (:units bar)))
    (is (= [[0.0 :bar] [1.0 :beat] [2.0 :beat] [3.0 :beat] [3.5 :bar]] (marks bar)))
    (is (= [0.0 (/ 1 3.5) (/ 2 3.5) (/ 3 3.5) 1.0] (mapv :position (:lines bar))))
    (is (= "1 bar" (grid/measure-text (:period bar))))
    (is (= [[0.0 :bar] [1.0 :beat] [2.0 :beat] [3.0 :beat] [3.5 :bar]
            [4.5 :beat] [5.5 :beat] [6.5 :beat] [7.0 :bar]]
           (marks two-bars)))
    (is (= [:sixteenth :beat] (:units beat)))
    (is (= [:bar :phrase] (:units phrase)))
    (is (= [[0.0 :phrase] [3.5 :bar] [7.0 :bar] [10.5 :bar] [14.0 :phrase]] (marks phrase)))
    (is (= [:beat :bar] (grid/scale 3 {:beats-per-bar 3 :bars-per-phrase 4})))
    (is (= [:sixteenth :beat] (grid/scale 3 common)) "three beats are less than a 4/4 bar")))

(deftest the-scale-follows-the-period-relative-to-the-meter
  (doseq [[period expected] [[0.25 [:sixteenth :beat]] [1 [:sixteenth :beat]] [3.99 [:sixteenth :beat]]
                             [4 [:beat :bar]] [6 [:beat :bar]]
                             [8 [:bar :phrase]] [16 [:bar :phrase]] [64 [:bar :phrase]]]]
    (is (= expected (grid/scale period common)) (str period " beats")))
  (is (nil? (grid/scale 0 common)))
  (is (nil? (grid/scale 4 {:beats-per-bar -1}))))

(deftest a-transport-window-also-marks-the-coarser-boundaries-it-crosses
  (let [spec (grid/grid {:start-beat 14.5 :span-beats 2 :period-beats 1 :meter common :aligned? true})
        offset (grid/grid {:start-beat 0 :span-beats 8 :period-beats 4 :meter common :aligned? true
                           :bar-origin-beat 1 :phrase-origin-beat 5})]
    (is (= [:sixteenth :beat :bar :phrase] (:units spec)))
    (is (= [[14.5 :sixteenth] [14.75 :sixteenth] [15.0 :beat] [15.25 :sixteenth] [15.5 :sixteenth]
            [15.75 :sixteenth] [16.0 :phrase] [16.25 :sixteenth] [16.5 :sixteenth]]
           (marks spec)))
    (is (= 0.75 (:position (first (filter #(= :phrase (:unit %)) (:lines spec))))))
    (is (= [[0.0 :beat] [1.0 :bar] [2.0 :beat] [3.0 :beat] [4.0 :beat] [5.0 :phrase] [6.0 :beat] [7.0 :beat] [8.0 :beat]]
           (marks offset))
        "bar and phrase lines follow the transport origins")))

(deftest crowded-units-give-way-to-coarser-ones
  (let [many-cycles (grid/grid {:span-beats 16 :period-beats 0.25 :meter common})
        tight (grid/grid {:span-beats 16 :period-beats 0.25 :meter common :max-lines 4})
        long-window (grid/grid {:start-beat 3 :span-beats 16000 :meter common :aligned? true})]
    (is (= [:beat :bar] (:units many-cycles)) "64 sixteenths across the window are too many")
    (is (= 17 (count (:lines many-cycles))))
    (is (= [:bar :phrase] (:units tight)))
    (is (= [:phrase] (:units long-window)))
    (is (<= 2 (count (:lines long-window)) 9))
    (is (apply < (map :position (:lines long-window))))))

(deftest a-window-without-a-period-chooses-the-finest-sparse-unit
  (doseq [[span units] [[0.5 [:sixteenth :beat :bar :phrase]] [2 [:sixteenth :beat :bar :phrase]]
                        [4 [:beat :bar :phrase]] [8 [:beat :bar :phrase]] [16 [:bar :phrase]]]]
    (let [spec (grid/grid {:start-beat 100 :span-beats span :meter common :aligned? true})]
      (is (= units (:units spec)) (str span " beats"))
      (is (nil? (:period spec))))))

(deftest invalid-windows-and-meters-have-no-grid
  (is (nil? (grid/grid {:span-beats 0 :meter common})))
  (is (nil? (grid/grid {:span-beats 4 :period-beats -1 :meter common})))
  (is (nil? (grid/grid {:span-beats Double/NaN :meter common})))
  (is (nil? (grid/grid {:start-beat Double/POSITIVE_INFINITY :span-beats 4 :meter common})))
  (is (nil? (grid/grid {:span-beats 4 :meter {:beats-per-bar 0 :bars-per-phrase 4}})))
  (is (= [:beat :bar] (:units (grid/grid {:span-beats 4 :period-beats 4})))
      "an absent meter is the transport default"))

(deftest period-text-names-the-largest-whole-unit
  (doseq [[beats meter text] [[0.25 common "1/4 beat"] [0.5 common "1/2 beat"] [1.5 common "3/2 beats"]
                              [2 common "2 beats"] [4 common "1 bar"] [8 common "2 bars"] [6 common "6 beats"]
                              [16 common "1 phrase"] [32 common "2 phrases"]
                              [3.5 seven-eight "1 bar"] [14 seven-eight "1 phrase"] [1 seven-eight "1 beat"]]]
    (is (= text (grid/measure-text (grid/period-measure beats meter))) (str beats)))
  (is (nil? (grid/measure-text nil))))
