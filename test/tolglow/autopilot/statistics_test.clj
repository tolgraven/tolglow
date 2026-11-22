(ns tolglow.autopilot.statistics-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [tolglow.autopilot.statistics :as stats]))

(defn- near? [expected actual] (< (abs (- expected actual)) 1e-7))
(defn- advance [state clock window level]
  (stats/advance (:state state) clock window {:brightness level :activity (- 1.0 level)}
                 {:brightness 1 :activity 0} 0.02))

(deftest an-infrequent-flash-counts-only-for-the-time-it-lit-the-room
  (let [clock #(hash-map :time-ms (* % 400) :beat % :epoch 4)
        sample #(advance %1 (clock %2) {:bars 1} %3)
        off (sample nil 0 0)
        flash (sample off 7/2 1)
        report (:report (sample flash 4 0))
        brightness (:brightness report) activity (:activity report)]
    (is (= 1.0 (:coverage report)))
    (is (zero? (:instant brightness)))
    (is (near? 1/8 (:mean brightness)))
    (is (near? 7/8 (:mean activity)))
    (is (= [0.0 1.0] ((juxt :min :max) brightness)))
    (is (near? (Math/sqrt (* 1/8 7/8)) (:stddev brightness)))
    (is (= 0.125 (:target-hit-fraction brightness) (:target-hit-fraction activity) (:lit-fraction brightness)))
    (is (= 0.875 (first (:histogram brightness))))
    (is (= 0.125 (peek (:histogram brightness))))))

(deftest repeated-frames-and-fractional-window-boundaries
  (let [w {:ms 3000}
        sample #(advance %1 {:time-ms %2} w %3)
        observations [[0 0.2] [2123 1.0] [2123 0.7] [3123 0.0] [3900 1.0]]
        run (fn [rows] (reduce (fn [s [t v]] (sample s t v)) nil rows))
        sparse (run observations)
        dense (run (sort-by first (into observations (for [t (range 100 2100 100)] [t 0.2]))))
        expected (/ (+ (* 1223 0.2) (* 1000 0.7)) 3000)]
    (is (near? expected (get-in sparse [:report :brightness :mean])))
    (is (near? expected (get-in dense [:report :brightness :mean])))
    (is (near? 1 (reduce + (get-in sparse [:report :brightness :histogram]))))
    (is (= :ok (when (= (edn/read-string (pr-str (:state sparse))) (:state sparse)) :ok)))))

(deftest phrase-meter-and-tempo-changes-pause-seek-and-epoch
  (let [w {:phrases 1/2}
        clock (fn [t beat playing? epoch] {:time-ms t :beat beat :playing? playing? :epoch epoch
                                           :meter {:beats-per-bar 7/2 :bars-per-phrase 2}})
        a (advance nil (clock 0 0 true 1) w 0)
        b (advance a (clock 200 1 true 1) w 1)
        paused (advance b (clock 1000 2 false 1) w 0)
        resumed (advance paused (clock 5000 2 true 1) w 1)
        c (advance resumed (clock 6600 7/2 true 1) w 0)
        seek (advance c (clock 6700 1 true 1) w 0)
        reload (advance c (clock 6800 4 true 2) w 0)]
    (is (near? (/ 2.0 3.5) (get-in resumed [:report :coverage])))
    (is (near? (/ 2.5 3.5) (get-in c [:report :brightness :mean])))
    (is (= 0.0 (get-in seek [:report :coverage]) (get-in reload [:report :coverage])))))

(deftest bounded-history-and-long-gaps
  (let [run (reduce (fn [s i] (advance s {:time-ms i} {:ms 3000} (double (mod i 2)))) nil (range 3200))
        b (get-in run [:state :buckets])
        huge (advance run {:time-ms 1.0e18} {:ms 3000} 0)]
    (is (<= (count b) 65))
    (is (every? #(<= (count (:spans %)) 16) b))
    (is (< (abs (- 0.5 (get-in run [:report :brightness :mean]))) (/ 1.0 64)))
    (is (= 1.0 (get-in huge [:report :brightness :mean])))
    (is (= 1.0 (get-in huge [:report :coverage])))
    (is (thrown? Exception (advance run {:time-ms 4000} {:beats 0} 1)))))
