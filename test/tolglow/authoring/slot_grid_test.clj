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
  (is (= {:columns 1 :rows 0 :empty 0 :cells [] :exact? true} (grid/pack [] 4))))

(defn- brute-force-rows
  "The fewest rows any arrangement of `spans` fits in `cols` columns, by
  trying every position of every card: independent of the packer's search.
  Cards of one shape take increasing positions, which only removes repeats."
  [spans cols]
  (let [spans (vec (sort spans))]
    (first
     (for [rows (iterate inc 1)
           :when (letfn [(place [i occupied last-pos]
                           (or (= i (count spans))
                               (let [[w h] (nth spans i)
                                     same? (and (pos? i) (= (nth spans i) (nth spans (dec i))))]
                                 (some (fn [pos]
                                         (let [r (quot pos cols) c (rem pos cols)
                                               cells (for [dr (range h) dc (range w)] [(+ r dr) (+ c dc)])]
                                           (when (and (<= (+ c w) cols) (<= (+ r h) rows)
                                                      (not-any? occupied cells))
                                             (place (inc i) (into occupied cells) pos))))
                                       (range (if same? last-pos 0) (* rows cols))))))]
                   (place 0 #{} 0))]
       rows))))

(deftest packing-reaches-the-brute-force-optimum-on-small-banks
  (let [shapes [[1 1] [2 1] [2 2]]
        banks (for [a shapes b shapes c shapes d shapes e [nil [1 1] [2 2]]] (vec (remove nil? [a b c d e])))]
    (doseq [spans (distinct banks) cols [2 3 4]
            :let [{:keys [columns rows] :as packed} (grid/pack spans cols {:exact-columns? true})]]
      (is (= cols columns))
      (is (= (brute-force-rows spans cols) rows) [spans cols])
      (is (every? #(= 1 %) (vals (occupancy packed))) [spans cols])
      (is (= (count spans) (count (:cells packed)))))))

(deftest known-optimal-layouts
  (testing "hand-checked optimal layouts"
    (doseq [[spans cols rows] [[widget-study 4 3] [widget-study 6 2] [widget-study 2 6]
                               [(repeat 8 [1 1]) 4 2] [[[2 2] [2 2] [2 2]] 4 4] [[[2 1] [2 1] [2 1] [1 1] [1 1]] 4 2]
                               [[[2 2] [1 1] [1 1] [1 1] [1 1] [1 1] [1 1]] 4 3]]]
      (is (= rows (:rows (grid/pack spans cols {:exact-columns? true}))) [spans cols]))))

(deftest the-shortest-and-then-narrowest-grid-wins
  (doseq [spans [[[1 1] [2 2] [1 1] [2 1] [1 1] [1 1] [2 2]]
                 [[2 1] [2 1] [2 1] [1 1]]
                 (mapv controls/slot-span [:large :large :compact :compact :compact :compact :compact :compact])]
          cols [2 3 4 5 6]]
    (let [packed (grid/pack spans cols)
          heights (into {} (for [c (range (reduce max (map first spans)) (inc cols))]
                             [c (:rows (grid/pack spans c {:exact-columns? true}))]))
          shortest (reduce min (vals heights))]
      (is (<= (:columns packed) cols))
      (is (= shortest (:rows packed)) [spans cols])
      (is (= (:columns packed) (reduce min (keep (fn [[c r]] (when (= r shortest) c)) heights))) [spans cols]))))

(deftest a-board-of-fixed-tracks-keeps-its-column-count
  (is (= 6 (:columns (grid/pack [[3 1]] 6 {:exact-columns? true}))) "a lone medium card is half of six tracks")
  (is (= 3 (:columns (grid/pack [[3 1]] 6))) "a bank of fixed slots narrows to its content"))

(deftest the-step-budget-bounds-large-banks-without-a-cliff
  (testing "thirty knobs still pack in order on full rows"
    (let [{:keys [cells columns rows]} (grid/pack (repeat 30 [1 1]) 6)]
      (is (= [6 5] [columns rows]))
      (is (= (range 30) (map :index cells)))))
  (testing "a 24-card mixed bank finishes quickly and stays a valid placement"
    (let [spans (vec (take 24 (cycle [[1 1] [2 2] [1 1] [2 1] [1 1] [2 2] [1 1]])))
          start (System/nanoTime)
          packed (grid/pack spans 8)
          ms (/ (- (System/nanoTime) start) 1e6)]
      (is (< ms 2000) (str ms " ms"))
      (is (= 24 (count (:cells packed))))
      (is (every? #(= 1 %) (vals (occupancy packed)))))))

(deftest the-budget-goes-to-the-widest-counts-first
  (testing "200 random banks of 13-24 cards match an effectively unlimited search"
    (let [r (java.util.Random. 795)
          shapes [[1 1] [1 1] [1 1] [2 1] [2 2]]]
      (doseq [_ (range 200)
              :let [spans (vec (repeatedly (+ 13 (.nextInt r 12)) #(nth shapes (.nextInt r 5))))
                    cols (+ 4 (.nextInt r 5))
                    packed (grid/pack spans cols)
                    reference (with-redefs [grid/step-budget 3000000] (grid/pack spans cols))]]
        (is (= ((juxt :rows :columns) reference) ((juxt :rows :columns) packed)) [spans cols])))))

(deftest a-quick-pack-is-first-fit-in-authored-order
  (let [{:keys [cells exact?]} (grid/pack widget-study 4 {:quick? true})]
    (is (false? exact?))
    (is (= (range 5) (sort (map :index cells))))
    (is (= 0 (:index (first cells))))))

(deftest an-arranged-board-reads-in-stored-order
  (testing "M, L, S on two tracks: nothing backfills past an earlier card"
    (let [{:keys [cells]} (grid/pack [[1 2] [2 3] [1 1]] 2 {:exact-columns? true :in-order? true})]
      (is (= [0 1 2] (map :index cells)))
      (is (= [[0 0] [0 2] [0 5]] (map (juxt :col :row) cells)))))
  (testing "M, S beside it: the short card grows down into the band it would leave empty"
    (let [packed (grid/pack [[1 2] [1 1] [2 3]] 2 {:exact-columns? true :in-order? true})
          grown (grid/grow-down packed)]
      (is (= [[0 0 1 2] [1 0 1 1] [0 2 2 3]] (map (juxt :col :row :w :h) (:cells packed))))
      (is (= [[0 0 1 2] [1 0 1 2] [0 2 2 3]] (map (juxt :col :row :w :h) (:cells grown))))
      (is (every? #(= 1 %) (vals (occupancy grown))))))
  (testing "growing never widens a card"
    (let [grown (grid/grow-down (grid/pack [[3 2] [2 1]] 6 {:exact-columns? true :in-order? true}))]
      (is (= [3 2] (map :w (:cells grown))))
      (is (= [2 2] (map :h (:cells grown)))))))
