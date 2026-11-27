(ns tolglow.authoring.slot-grid-test
  "Deterministic slot-grid placement (#795): fewest empty cells, authored
  order where the optimum allows, reading order for DOM and tab order."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.authoring.slot-grid :as grid]))

(def widget-study
  "Level, Offset (XY), Aim (XYZ), Tone (rings), Steps (fader)."
  (mapv controls/slot-span [:compact :large :large :compact :compact]))

(defn- occupancy [{:keys [columns cells]}]
  (frequencies (for [{:keys [col row w h]} cells dr (range h) dc (range w)]
                 (do (assert (<= (+ col w) columns)) [(+ row dr) (+ col dc)]))))

(deftest the-widget-study-packs-into-the-optimum
  (testing "four columns: 12 cells, not the 16 greedy dense packing used"
    (let [{:keys [columns rows empty cells] :as packed} (grid/pack widget-study 4)]
      (is (= [4 3 1] [columns rows empty]))
      (is (= (grid/optimum widget-study 4) (select-keys packed [:empty :rows])))
      (is (every? #(= 1 %) (vals (occupancy packed))) "no two cards share a cell")
      (is (= (sort-by (juxt :row :col) cells) cells) "cells come in reading order")))
  (testing "six columns fit it in two rows with one empty cell"
    (is (= [6 2 1] ((juxt :columns :rows :empty) (grid/pack widget-study 8)))))
  (testing "three columns on a phone: five rows, not seven in two columns"
    (is (= [3 5 1] ((juxt :columns :rows :empty)
                    (grid/pack (mapv controls/slot-span [:large :large :compact :compact :compact :compact :compact :compact]) 3))))))

(deftest authored-order-leads-and-later-small-cards-fill-holes
  (let [{:keys [cells]} (grid/pack widget-study 4)]
    (is (= 0 (:index (first cells))) "Level, authored first, takes the first cell")
    (is (= [1 2] (filterv #{1 2} (map :index cells))) "the pads keep their order")
    (is (= 2 (count (filter #(= 2 (:w %)) cells)))))
  (testing "eight knobs and nothing else stay in order on one row"
    (let [{:keys [columns rows cells]} (grid/pack (repeat 8 [1 1]) 12)]
      (is (= [8 1] [columns rows]))
      (is (= (range 8) (map :index cells)))))
  (testing "a wide card that does not fit the row end is backfilled by later knobs"
    (let [{:keys [cells empty columns]} (grid/pack [[1 1] [1 1] [1 1] [2 1] [1 1] [1 1] [1 1]] 4)]
      (is (= [4 0] [columns empty]))
      (is (= [0 1 2 4 3 5 6] (map :index cells))))))

(deftest the-column-count-caps-at-the-content
  (is (= [1 1 0] ((juxt :columns :rows :empty) (grid/pack [[1 1]] 9))) "one knob takes one column, not nine")
  (is (= [2 2 0] ((juxt :columns :rows :empty) (grid/pack [[2 2]] 9))))
  (is (= 2 (:columns (grid/pack [[2 1]] 1))) "a wide card still gets two columns")
  (is (= {:columns 1 :rows 0 :empty 0 :cells []} (grid/pack [] 4))))

(deftest packing-matches-the-optimum-across-mixed-banks
  (doseq [spans [[[1 1] [2 2] [1 1] [2 1] [1 1] [1 1] [2 2]]
                 [[2 1] [2 1] [2 1] [1 1]]
                 [[2 2] [2 2] [2 2] [1 1] [1 1] [1 1] [1 1] [2 1]]
                 (mapv controls/slot-span [:large :large :compact :compact :compact :compact :compact :compact])]
          cols [2 3 4 5 6 8]]
    (let [packed (grid/pack spans cols)]
      (is (<= (:columns packed) cols))
      (is (= (grid/optimum spans (:columns packed)) (select-keys packed [:empty :rows])) [spans cols])
      (is (every? #(= 1 %) (vals (occupancy packed))))
      (is (= (count spans) (count (:cells packed))))
      (testing "no column count up to the limit is shorter, and none as short is narrower"
        (let [options (map #(assoc (grid/optimum spans %) :columns %) (range (reduce max (map first spans)) (inc cols)))
              shortest (reduce min (map :rows options))]
          (is (= shortest (:rows packed)))
          (is (= (:columns packed) (reduce min (map :columns (filter #(= shortest (:rows %)) options))))))))))

(deftest large-banks-fall-back-to-first-fit
  (let [{:keys [cells columns]} (grid/pack (repeat 30 [1 1]) 6)]
    (is (= 6 columns))
    (is (= (range 30) (map :index cells)))))
