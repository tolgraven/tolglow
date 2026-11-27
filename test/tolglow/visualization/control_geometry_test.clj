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
    (is (nil? (geometry/ticks y)) "continuous axes have no ticks")))

(deftest the-dial-reads-an-absolute-angle-with-a-dead-zone
  (testing "the 270 degree sweep starts bottom left and ends bottom right"
    (is (near? 0 (geometry/dial-amount -10 10 true)))
    (is (near? 0.5 (geometry/dial-amount 0 -10 true)))
    (is (near? 1 (geometry/dial-amount 10 10 true))))
  (testing "in the bottom gap the ember skin holds its value; the standard skin clamps"
    (is (nil? (geometry/dial-amount 0 30 true)))
    (is (nil? (geometry/dial-amount 10 15 true)))
    (is (= 0 (geometry/dial-amount -1 30 false)))
    (is (= 1 (geometry/dial-amount 1 30 false))))
  (testing "a pointer on the centre has no angle"
    (is (nil? (geometry/dial-amount 1 -1 true))
        "radius √2 is inside the centre dead zone")
    (is (some? (geometry/dial-amount 10 -10 true))))
  (testing "the pointer bar turns with the amount"
    (is (= -135 (geometry/pointer-rotation 0)))
    (is (= 0 (geometry/pointer-rotation 1/2)))
    (is (= 135 (geometry/pointer-rotation 1)))))

(deftest the-wheel-accelerates-and-scrolling-up-raises
  (let [small (- (geometry/wheel-amount 0.5 -1) 0.5)
        flick (- (geometry/wheel-amount 0.5 -256) 0.5)]
    (is (near? (/ 1.5 270) small) "one pixel turns one and a half degrees")
    (is (near? (/ 6.0 270) flick) "256 pixels turn four times as far, not 256 times")
    (is (< (geometry/wheel-amount 0.5 40) 0.5) "scrolling down lowers"))
  (is (= 1 (geometry/wheel-amount 0.999 -100)) "the wheel stops at the bounds")
  (is (nil? (geometry/wheel-amount 0.5 0))))

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
  (is (= [:standard :ember] (mapv :id geometry/knob-skins)))
  (is (= :ember (geometry/knob-skin "ember")))
  (is (= :ember (geometry/knob-skin :ember)))
  (is (every? #{:standard} (map geometry/knob-skin [nil "chrome" 4 :unknown]))))
