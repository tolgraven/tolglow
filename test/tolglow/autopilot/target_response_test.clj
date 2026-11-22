(ns tolglow.autopilot.target-response-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [tolglow.autopilot.target-response :as response]))

(defn- levels [b a] {:brightness b :activity a})
(defn- tick [old clock target seed opts]
  (response/advance (:state old) clock target seed opts))

(deftest exact-arrival-and-retargeting-use-scoped-measurements-for-the-first-frame
  (let [opts {:attack {:beats 1} :release {:ms 200}}
        goal (levels 1 0)
        begin (tick nil {:time-ms 0 :beat 0} goal (levels 0 1) opts)
        half (tick begin {:time-ms 100 :beat 1/2} goal nil opts)
        edited (tick half {:time-ms 150 :beat 3/4} (levels 0 0) nil opts)
        restored (edn/read-string (pr-str edited))
        end (tick restored {:time-ms 300 :beat 1} (levels 0 0) nil opts)]
    (is (= (levels 0.0 1.0) (:targets begin)))
    (is (= (levels 0.5 0.5) (:targets half)))
    (is (= (levels 0.375 0.25) (:targets edited)))
    (is (= (levels 0.0 0.0) (:targets end)))))

(deftest pause-duplicate-and-rewind-preserve-their-own-clock-semantics
  (let [opts {:attack {:blend 1/2} :release {:ms 200}}
        goal (levels 1 0)
        step #(tick %1 {:time-ms %2 :beat (/ %2 500) :playing? %3} goal (levels 0 1) opts)
        a (step nil 0 true) b (step a 50 true) same (step b 50 true)
        paused (step same 500 false) resumed (step paused 800 true)
        c (step resumed 850 true) rewound (step c 0 true)]
    (is (= (levels 0.5 0.75) (:targets b) (:targets same) (:targets paused) (:targets resumed)))
    (is (= (levels 0.75 0.5) (:targets c)))
    (is (= (:targets a) (:targets rewound)))))

(deftest meter-at-segment-start-and-unsmoothed-values
  (let [opts {:attack {:phrases 1/2}}
        clock {:time-ms 0 :beat 0 :epoch 1 :meter {:beats-per-bar 3 :bars-per-phrase 2}}
        a (tick nil clock (levels 1 0.25) {:brightness 0.25} opts)
        b (tick a (assoc clock :time-ms 500 :beat 1 :meter {:beats-per-bar 8}) (levels 1 0.25) nil opts)
        c (tick b (assoc clock :time-ms 1500 :beat 3) (levels 1 0.25) nil opts)]
    (is (= (levels 0.25 0.25) (:targets a)))
    (is (= (levels 0.5 0.25) (:targets b)))
    (is (= (levels 1.0 0.25) (:targets c)))
    (is (= {:state nil :targets (levels 1.0 0.0)} (tick c clock (levels 2 -1) nil nil)))
    (is (= (levels 1.0 0.0) (:targets (tick nil clock (levels 1 0) (levels 0 1) {:attack {:ms 0}}))))
    (is (thrown? Exception (tick nil clock (levels ##NaN 1) nil opts)))
    (doseq [bad [{:attack {:beats -1}} {:release {:blend 1.1}} {:rise {:ms 2}} {:attack {:ms 2 :beats 1}}]]
      (is (thrown? Exception (response/validate! bad))))))
