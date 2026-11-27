(ns tolglow.visualization.control-geometry-test
  "Pointer, key and wheel geometry of the shared XY pad, vertical fader,
  nested-ring knob and knob skins."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.visualization.control-geometry :as geometry]))

(defn- near? [a b]
  (if (sequential? a)
    (and (= (count a) (count b)) (every? true? (map near? a b)))
    (< (Math/abs (double (- a b))) 1.0e-9)))

(def x (geometry/axis "X" {:bounds [-1 1]}))

(def y (geometry/axis "Y" {:bounds [0 10]}))

(def steps (geometry/axis "Steps" {:bounds [0 8] :type :integer}))

(deftest an-axis-needs-real-bounds
  (is (= {:label "X" :bounds [-1 1] :integer? false} x))
  (is (true? (:integer? (geometry/axis "N" {:bounds [0 4] :type "integer"}))))
  (is (every? nil? [(geometry/axis "a" {}) (geometry/axis "a" {:bounds [1 1]}) (geometry/axis "a" {:bounds [0 ##Inf]})])))

(deftest the-pad-maps-its-surface-onto-both-axes-with-up-high
  (is (= [0 1] (geometry/pad-amounts 0 0 200 100)) "the top-left corner is X low, Y high")
  (is (= [1 0] (geometry/pad-amounts 200 100 200 100)))
  (is (near? [0.5 0.5] (geometry/pad-amounts 100 50 200 100)))
  (is (= [0 0] (geometry/pad-amounts -40 900 200 100)) "a drag past the edge clamps to it")
  (is (nil? (geometry/pad-amounts 10 10 0 100)) "a collapsed pad sets nothing")
  (testing "a gesture sets the first two components inside each axis's own bounds"
    (is (near? [0.5 2.5] ((juxt #(get % 0) #(get % 1)) (geometry/pad-changes [x y] [0.75 0.25]))))
    (is (= #{0 1} (set (keys (geometry/pad-changes [x y] [0 1])))) "a third component is left to its own control")
    (is (near? [-1 10] ((juxt #(get % 0) #(get % 1)) (geometry/pad-changes [x y] [0 1])))))
  (testing "an integer axis rounds"
    (is (= {0 -1 1 4} (geometry/pad-changes [x steps] [0 0.49]))))
  (testing "the drawn position is the inverse"
    (is (near? [0.75 0.25] (geometry/pad-position [x y] [0.5 2.5])))
    (is (nil? (geometry/pad-position [x y] [0.5 nil])) "a missing value draws no pointer")))

(deftest keys-move-the-pad-one-axis-at-a-time
  (is (= [0 "ArrowUp"] (geometry/pad-key "ArrowRight" false)))
  (is (= [0 "ArrowDown"] (geometry/pad-key "ArrowLeft" false)))
  (is (= [1 "ArrowUp"] (geometry/pad-key "ArrowUp" false)))
  (is (= [1 "PageDown"] (geometry/pad-key "ArrowDown" true)) "Shift makes a large step")
  (is (= [0 "PageUp"] (geometry/pad-key "ArrowRight" true)))
  (is (nil? (geometry/pad-key "Enter" false))))

(deftest the-vertical-fader-is-high-at-the-top
  (is (= 1 (geometry/fader-amount 0 120)))
  (is (= 0 (geometry/fader-amount 120 120)))
  (is (== 0.75 (geometry/fader-amount 30 120)))
  (is (= 1 (geometry/fader-amount -15 120)))
  (testing "bipolar axes fill from zero"
    (is (== 0.5 (geometry/origin x)))
    (is (= 0 (geometry/origin y))))
  (testing "integer spans of sixteen or fewer get one tick per step, every fourth heavier"
    (is (= [0 1 2 3 4 5 6 7 8] (mapv :value (geometry/ticks steps))))
    (is (= [0 4 8] (mapv :value (filter :major? (geometry/ticks steps)))))
    (is (== 0.5 (:amount (nth (geometry/ticks steps) 4))))
    (is (nil? (geometry/ticks (geometry/axis "Wide" {:bounds [0 32] :type :integer}))))
    (is (nil? (geometry/ticks y)) "continuous axes have no ticks")
    (is (= [-6 -5 -4 -3] (mapv :value (geometry/ticks (geometry/axis "Neg" {:bounds [-6.5 -2.5] :type :integer}))))
        "ticks stay inside fractional negative bounds")
    (is (= [-6 -2] (mapv :value (filter :major? (geometry/ticks (geometry/axis "Neg" {:bounds [-6 -1] :type :integer})))))
        "every fourth tick from the low end is heavier")))

(deftest the-dial-reads-an-absolute-angle-with-a-dead-zone
  (testing "the 270 degree sweep starts bottom left and ends bottom right"
    (is (near? 0 (geometry/dial-amount -10 10 :press)))
    (is (near? 0.5 (geometry/dial-amount 0 -10 :press)))
    (is (near? 1 (geometry/dial-amount 10 10 :press))))
  (testing "a fresh Ember press in the bottom gap holds its value"
    (is (nil? (geometry/dial-amount 0 30 :press)))
    (is (nil? (geometry/dial-amount 30 31 :press))))
  (testing "a drag past either end lands exactly on the bound, on both skins"
    (is (< 0.99 (geometry/dial-amount 30 29 :drag) 1) "just inside the arc")
    (is (= 1 (geometry/dial-amount 30 31 :drag)) "just past the high end")
    (is (= 1 (geometry/dial-amount 0.5 30 :drag)) "right of the bottom centre is the high end")
    (is (= 0 (geometry/dial-amount -30 31 :drag)) "just past the low end")
    (is (= 0 (geometry/dial-amount -0.5 30 :drag)))
    (is (= 0 (geometry/dial-amount -1 30)))
    (is (= 1 (geometry/dial-amount 1 30))))
  (testing "a pointer on the centre has no angle while dragging or pressing"
    (is (nil? (geometry/dial-amount 1 -1 :drag)))
    (is (nil? (geometry/dial-amount 1 -1 :press)))
    (is (some? (geometry/dial-amount 10 -10 :press))))
  (testing "the pointer bar turns with the amount"
    (is (= -135 (geometry/pointer-rotation 0)))
    (is (= 0 (geometry/pointer-rotation 1/2)))
    (is (= 135 (geometry/pointer-rotation 1)))))

(deftest the-wheel-turns-in-proportion-to-vertical-distance
  (testing "only vertical scrolling turns a knob; horizontal and zero deltas pass through"
    (is (nil? (geometry/wheel-pixels 40 0 0)) "a horizontal swipe or Shift+wheel")
    (is (nil? (geometry/wheel-pixels 30 -10 0)) "a mostly horizontal swipe")
    (is (nil? (geometry/wheel-pixels 0 ##NaN 0)))
    (is (== 100 (geometry/wheel-pixels 0 -100 0)) "scrolling up raises")
    (is (== -60 (geometry/wheel-pixels 0 3 1)) "three lines are one integer step")
    (is (== 400 (geometry/wheel-pixels 0 -1 2))))
  (testing "a continuous knob turns by distance, however the distance is split"
    (let [one (geometry/wheel-amount 0.2 300)
          many (reduce geometry/wheel-amount 0.2 (repeat 100 3))]
      (is (near? 0.5 one))
      (is (near? one many) "a hundred 3 px trackpad events turn as far as one 300 px event"))
    (is (= 1 (geometry/wheel-amount 0.9 5000)) "the wheel stops at the bounds")
    (is (nil? (geometry/wheel-amount 0.5 0))))
  (testing "an integer knob accumulates distance into whole steps"
    (is (= [0 4] (geometry/wheel-steps 0 4)) "a small trackpad event alone is no step")
    (is (= [1 4] (reduce (fn [[steps r] px] (let [[s r] (geometry/wheel-steps r px)] [(+ steps s) r]))
                         [0 0] (repeat 16 4)))
        "sixteen 4 px events make one 60 px step")
    (is (= [1 40] (geometry/wheel-steps 0 100)) "a 100 px mouse notch is one step")
    (is (= [-1 -10] (geometry/wheel-steps 50 -70)) "reversing drops the carried remainder")))

(deftest rings-nest-one-sub-parameter-each
  (let [axes [x y steps (geometry/axis "W" {:bounds [0 1]})]
        rings (geometry/rings axes [0.5 2.5 8 0])]
    (is (= ["X" "Y" "Steps" "W"] (mapv :label rings)))
    (is (= [38 30 22 14] (mapv :radius rings)) "outermost first, evenly nested")
    (is (near? [0.75 0.25 1 0] (mapv :amount rings)))
    (is (near? [0.5 0 0 0] (mapv :origin rings)) "a bipolar ring fills from its zero")
    (is (= 4 (count (distinct (map :color rings))))))
  (testing "two rings sit further apart; a missing value draws no fill"
    (let [rings (geometry/rings [x y] [nil 5])]
      (is (= [38 28] (mapv :radius rings)))
      (is (nil? (:amount (first rings)))))))

(deftest knob-skins-are-a-closed-preference
  (is (= [:ember :standard] (mapv :id geometry/knob-skins)) "Ember is the default, listed first")
  (is (= :standard (geometry/knob-skin "standard")) "a saved Standard keeps the flat style")
  (is (= :standard (geometry/knob-skin :standard)))
  (is (= :ember (geometry/knob-skin "ember")))
  (is (every? #{:ember} (map geometry/knob-skin [nil "chrome" 4 :unknown])) "anything else is the default"))
